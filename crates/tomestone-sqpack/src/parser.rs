use std::{
    convert::TryInto,
    fs::File,
    io::{self, BufRead, Read, Seek, SeekFrom},
    path::PathBuf,
};

use nom::{
    branch::alt,
    bytes::streaming::{tag, take, take_while},
    combinator::{complete, map, map_opt, map_parser, map_res, peek, verify},
    error::{ErrorKind, ParseError},
    multi::{count, length_data, length_value},
    number::streaming::{le_u16, le_u32, le_u8},
    sequence::{pair, terminated, tuple},
    Err, IResult, Needed,
};
use sha1::{Digest, Sha1};

use tomestone_common::null_padding;

use crate::{
    compression::decompress_sqpack_block, CollisionEntry, DataBlocks, Error, FilePointer, Index,
    IndexEntry, IndexEntry1, IndexEntry2, IndexHash1, IndexHash2, IndexPointer, IndexSegmentHeader,
    PlatformId, SqPackType, SHA1_OUTPUT_SIZE,
};

fn sqpack_magic(input: &[u8]) -> IResult<&[u8], ()> {
    map(tag(b"SqPack\x00\x00"), |_| ())(input)
}

fn alternate_dat_magic(input: &[u8]) -> IResult<&[u8], ()> {
    map(tag(b"\x80\x00\x00\x00\x00\x00\x00\x00"), |_| ())(input)
}

fn null_padding_greedy<'a, E>(input: &'a [u8]) -> IResult<&'a [u8], (), E>
where
    E: ParseError<&'a [u8]>,
{
    if input.iter().all(|byte| *byte == 0) {
        Ok((b"", ()))
    } else {
        Err(Err::Error(E::from_error_kind(input, ErrorKind::Count)))
    }
}

fn platform_id(input: &[u8]) -> IResult<&[u8], PlatformId> {
    map_res(le_u8, PlatformId::from_u8)(input)
}

fn sqpack_type(input: &[u8]) -> IResult<&[u8], SqPackType> {
    map_res(le_u32, SqPackType::from_u32)(input)
}

/// Parses a SqPack header (excluding its integrity check hash).
///
/// ```text
/// 0x000-0x008: "SqPack\x00\x00" or "\x80\x00\x00\x00\x00\x00\x00\x00"
/// 0x008-0x00C: Platform ID
/// 0x00C-0x010: Size
/// 0x010-0x014: Version (0 or 1)
/// 0x014-0x018: Type (1 for data, 2 for index)
/// 0x018-0x01C: Date
/// 0x01C-0x020: Time
/// 0x020-0x024: "\xff\xff\xff\xff"
/// 0x024-0x3c0: Null bytes
/// ```
fn sqpack_header_inner(input: &[u8]) -> IResult<&[u8], (PlatformId, u32, u32, SqPackType)> {
    map(
        tuple((
            alt((sqpack_magic, alternate_dat_magic)),
            platform_id,
            null_padding(3),
            le_u32, // note that this doesn't seem to match header size for .dat2 files
            verify(le_u32, |version| *version == 0 || *version == 1),
            sqpack_type,
            le_u32,
            le_u32,
            tag(b"\xff\xff\xff\xff"),
            null_padding(0x39c),
        )),
        |(_, platform_id, _, size, version, sqpack_type, _date, _time, _, _)| {
            (platform_id, size, version, sqpack_type)
        },
    )(input)
}

pub fn integrity_checked_header<
    'a,
    LP: FnMut(&'a [u8]) -> IResult<&'a [u8], usize>,
    CP: FnMut(&'a [u8]) -> IResult<&'a [u8], O>,
    O,
>(
    input: &'a [u8],
    length_parser: LP,
    contents_parser: CP,
) -> IResult<&[u8], O> {
    const HASH_OFFSET: usize = 0x3c0;

    map_parser(
        map(
            verify(
                length_value(
                    peek(length_parser),
                    map(
                        tuple((take(HASH_OFFSET), take(SHA1_OUTPUT_SIZE), null_padding_greedy)),
                        |(header_input, hash_input, ()): (&[u8], &[u8], ())| -> (&[u8], &[u8; SHA1_OUTPUT_SIZE]) {
                            (header_input, hash_input.try_into().unwrap())
                        },
                    ),
                ),
                |(header_input, header_hash): &(&[u8], &[u8; SHA1_OUTPUT_SIZE])| {
                    let mut hash = Sha1::new();
                    hash.update(header_input);
                    &*hash.finalize() == *header_hash
                },
            ),
            |(header_input, _header_hash)| header_input,
        ),
        complete(contents_parser),
    )(input)
}

/// Parses a SqPack header.
///
/// ```text
/// 0x000-0x008: "SqPack\x00\x00" or "\x80\x00\x00\x00\x00\x00\x00\x00"
/// 0x008-0x00C: Platform ID
/// 0x00C-0x010: Size
/// 0x010-0x014: Version (0 or 1)
/// 0x014-0x018: Type (1 for data, 2 for index)
/// 0x018-0x01C: Date
/// 0x01C-0x020: Time
/// 0x020-0x024: "\xff\xff\xff\xff"
/// 0x024-0x3c0: Null bytes
/// 0x3c0-0x3d4: SHA-1 hash of the preceding 0x3c0 bytes
/// 0x3d4-0x400: Null bytes
/// ```
pub(crate) fn sqpack_header_outer(
    input: &[u8],
) -> IResult<&[u8], (PlatformId, u32, u32, SqPackType)> {
    integrity_checked_header(input, |_| Ok((b"", 1024usize)), sqpack_header_inner)
}

fn index_segment_header(input: &[u8]) -> IResult<&[u8], IndexSegmentHeader> {
    map(
        tuple((le_u32, le_u32, take(SHA1_OUTPUT_SIZE))),
        |(offset, size, hash): (u32, u32, &[u8])| IndexSegmentHeader {
            offset,
            size,
            hash: hash.try_into().unwrap(),
        },
    )(input)
}

fn index_segment_headers(input: &[u8]) -> IResult<&[u8], (u32, u32, [IndexSegmentHeader; 4])> {
    integrity_checked_header(
        input,
        map(le_u32, |size| size.try_into().unwrap()),
        map(
            tuple((
                le_u32,
                tag(b"\x01\x00\x00\x00"),
                index_segment_header,
                null_padding(44),
                le_u32,
                index_segment_header,
                null_padding(44),
                index_segment_header,
                null_padding(44),
                index_segment_header,
            )),
            |(
                size,
                _,
                segment_header_1,
                _,
                number_of_dat_files,
                segment_header_2,
                _,
                segment_header_3,
                _,
                segment_header_4,
            )| {
                (
                    size,
                    number_of_dat_files,
                    [
                        segment_header_1,
                        segment_header_2,
                        segment_header_3,
                        segment_header_4,
                    ],
                )
            },
        ),
    )
}

#[derive(Debug, Clone, Copy)]
pub enum DataContentType {
    Empty = 1,
    Binary = 2,
    Model = 3,
    Texture = 4,
    Unsupported,
}

impl DataContentType {
    pub fn parse(value: u32) -> Option<DataContentType> {
        match value {
            0 => Some(DataContentType::Unsupported),
            1 => Some(DataContentType::Empty),
            2 => Some(DataContentType::Binary),
            3 => Some(DataContentType::Model),
            4 => Some(DataContentType::Texture),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct DataEntryHeaderCommon {
    content_type: DataContentType,
    _uncompressed_size: u32,
    _block_buffer_size: u32,
    num_blocks: u16,
}

fn data_entry_header_common(input: &[u8]) -> IResult<&[u8], (u32, DataEntryHeaderCommon)> {
    map(
        tuple((
            le_u32,
            map_opt(le_u32, DataContentType::parse),
            le_u32,
            le_u32,
            le_u32,
            le_u16,
            le_u16,
        )),
        |(
            header_length,
            content_type,
            uncompressed_size,
            _unknown,
            block_buffer_size,
            num_blocks,
            _todo,
        )| {
            (
                header_length,
                DataEntryHeaderCommon {
                    content_type,
                    _uncompressed_size: uncompressed_size,
                    _block_buffer_size: block_buffer_size << 7,
                    num_blocks,
                },
            )
        },
    )(input)
}

pub fn type_2_block_table<'a>(
    num_blocks: u16,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Vec<(u32, u16, u16)>, nom::error::Error<&'a [u8]>> {
    count(
        tuple((le_u32, le_u16, le_u16)),
        num_blocks.try_into().unwrap(),
    )
}

fn type_3_block_table<'a>(
    _num_blocks: u16,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], (Vec<u32>, Vec<u32>, Vec<u32>, u16)> {
    tuple((
        count(le_u32, 11),
        count(le_u32, 11),
        count(le_u32, 11),
        le_u16,
        // count(le_u16, TryInto::<usize>::try_into(num_blocks).unwrap()),
    ))
}

fn type_4_block_table<'a>(
    num_blocks: u16,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], (Vec<(u32, u32, u32, u32, u32)>, Vec<u16>)> {
    move |input: &[u8]| {
        let (input, frame_infos) = count(
            tuple((
                le_u32, // frame_offset
                le_u32, // frame_size
                le_u32, // _unknown
                le_u32, // frame_block_size_offset
                le_u32, // frame_block_size_count
            )),
            num_blocks.try_into().unwrap(),
        )(input)?;
        let size_field_count = frame_infos
            .iter()
            .map(|tuple| TryInto::<usize>::try_into(tuple.4).unwrap())
            .sum();
        let (input, frame_block_sizes) = count(le_u16, size_field_count)(input)?;
        Ok((input, (frame_infos, frame_block_sizes)))
    }
}

/// Parses the headers of a data entry. The first several fields are common to all types of
/// entries, and each type defines a different block table that occupies the rest of the headers.
///
/// ```text
/// 0x00-0x04: Header length
/// 0x04-0x08: Type (0: all zeros/tombstone, 1: empty?, 2: binary, 3: model, 4: texture)
/// 0x08-0x0c: Uncompressed size
/// 0x0c-0x10: Unknown
/// 0x10-0x14: Block buffer size (shifted right by 7)
/// 0x14-0x16: Number of blocks
/// 0x16-0x18: Unknown
/// 0x18-end: Block tables (varies)
/// ```
///
/// Type 2/binary entry block table:
/// ```text
/// 0x18-0x1c: block offset
/// 0x1c-0x1e: block size
/// 0x1e-0x20: decompressed data size
/// (repeats)
/// ```
fn data_entry_headers(start_position: u32) -> impl FnMut(&[u8]) -> IResult<&[u8], DataBlocks> {
    move |input: &[u8]| {
        let (input, header_data) = length_data(peek(le_u32))(input)?;
        let (header_data, (header_length, header_common)) =
            complete(data_entry_header_common)(header_data)?;
        let base_position = start_position + header_length;
        let blocks = match header_common.content_type {
            DataContentType::Empty => DataBlocks::Empty,
            DataContentType::Unsupported => DataBlocks::Unsupported,
            DataContentType::Binary => {
                let (_, blocks) =
                    complete(type_2_block_table(header_common.num_blocks))(header_data)?;
                DataBlocks::Binary {
                    base_position,
                    blocks,
                }
            }
            DataContentType::Model => {
                let (_, _) = complete(type_3_block_table(header_common.num_blocks))(header_data)?;
                DataBlocks::Model()
            }
            DataContentType::Texture => {
                let (_, _) = complete(type_4_block_table(header_common.num_blocks))(header_data)?;
                DataBlocks::Texture()
            }
        };
        Ok((input, blocks))
    }
}

fn block_header(input: &[u8]) -> IResult<&[u8], (u32, u32)> {
    let (_, header_length) = le_u32(input)?;
    map_parser(
        take(TryInto::<usize>::try_into(header_length).unwrap()),
        complete(map(
            tuple((le_u32, null_padding(4), le_u32, le_u32)),
            |(_, _, compressed_length, decompressed_length)| {
                (compressed_length, decompressed_length)
            },
        )),
    )(input)
}

fn index_hash_1(input: &[u8]) -> IResult<&[u8], IndexHash1> {
    map(pair(le_u32, le_u32), |(filename_crc, folder_crc)| {
        IndexHash1::new(folder_crc, filename_crc)
    })(input)
}

fn index_hash_2(input: &[u8]) -> IResult<&[u8], IndexHash2> {
    map(le_u32, IndexHash2::new)(input)
}

fn index_entry_1(input: &[u8]) -> IResult<&[u8], IndexEntry1> {
    map(
        tuple((
            index_hash_1,
            map(le_u32, IndexPointer::from_u32),
            null_padding(4),
        )),
        |(hash, pointer, _)| IndexEntry1 { hash, pointer },
    )(input)
}

fn index_entry_2(input: &[u8]) -> IResult<&[u8], IndexEntry2> {
    map(
        pair(index_hash_2, map(le_u32, IndexPointer::from_u32)),
        |(hash, pointer)| IndexEntry2 { hash, pointer },
    )(input)
}

fn collision_entry_1(input: &[u8]) -> IResult<&[u8], CollisionEntry<IndexHash1>> {
    map(
        tuple((
            index_hash_1,
            map(le_u32, FilePointer::from_u32),
            le_u32,
            length_value(
                |input| Ok((input, 240usize)),
                map_res(
                    terminated(take_while(|byte: u8| byte != 0), tag(b"\x00")),
                    std::str::from_utf8,
                ),
            ),
        )),
        |(hash, pointer, collision_index, path)| CollisionEntry {
            hash,
            pointer,
            _maybe_collision_index: collision_index,
            path: path.to_string(),
        },
    )(input)
}

fn collision_entry_2(input: &[u8]) -> IResult<&[u8], CollisionEntry<IndexHash2>> {
    map(
        tuple((
            index_hash_2,
            null_padding(4),
            map(le_u32, FilePointer::from_u32),
            le_u32,
            length_value(
                |input| Ok((input, 240usize)),
                map_res(
                    terminated(take_while(|byte: u8| byte != 0), tag(b"\x00")),
                    std::str::from_utf8,
                ),
            ),
        )),
        |(hash, _, pointer, collision_index, path)| CollisionEntry {
            hash,
            pointer,
            _maybe_collision_index: collision_index,
            path: path.to_string(),
        },
    )(input)
}

/// `GrowableBufReader` adds buffering to a reader, and allows callers to dynamically request a
/// larger buffer on the fly. EOF handling is decoupled from consuming the buffer, so callers can
/// see when the buffer cannot be grown anymore, without having to mark it all as consumed first.
pub struct GrowableBufReader<R: Read> {
    inner: R,
    buf: Vec<u8>,
    pos: usize,
    cap: usize,
}

impl<R: Read> GrowableBufReader<R> {
    pub fn with_capacity(inner: R, capacity: usize) -> GrowableBufReader<R> {
        let mut buf = Vec::with_capacity(capacity);
        buf.resize(buf.capacity(), 0);
        GrowableBufReader {
            inner,
            buf,
            pos: 0,
            cap: 0,
        }
    }

    pub fn new(inner: R) -> GrowableBufReader<R> {
        GrowableBufReader::with_capacity(inner, 1024)
    }

    pub fn buffer_capacity(&self) -> usize {
        self.cap - self.pos
    }

    pub fn fill_buf_required(&mut self, required: usize) -> std::io::Result<(&[u8], bool)> {
        let mut eof = false;
        if self.cap - self.pos <= required {
            if self.buf.len() < required {
                // grow buffer
                self.buf.reserve(required - self.buf.len() + self.pos);
                self.buf.resize(self.buf.capacity(), 0);
            } else if self.pos > 0 {
                self.buf.copy_within(self.pos..self.cap, 0);
                self.cap -= self.pos;
                self.pos = 0;
            }
            while self.cap < self.buf.len() && self.cap - self.pos <= required {
                let n = self.inner.read(&mut self.buf[self.cap..])?;
                self.cap += n;
                if n == 0 {
                    eof = true;
                    break;
                }
            }
        }
        Ok((&self.buf[self.pos..self.cap], eof))
    }
}

impl<R: Read> Read for GrowableBufReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let nread = {
            let mut rem = self.fill_buf()?;
            rem.read(buf)?
        };
        self.consume(nread);
        Ok(nread)
    }
}

impl<R: Read + Seek> Seek for GrowableBufReader<R> {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64, io::Error> {
        if let SeekFrom::Current(_) = pos {
            panic!("Seeking from the current position is unsupported for GrowableBufReader")
        }
        // throw away the entire buffer, no optimizations
        self.pos = 0;
        self.cap = 0;
        self.inner.seek(pos)
    }
}

impl<R: Read> BufRead for GrowableBufReader<R> {
    fn fill_buf(&mut self) -> std::io::Result<&[u8]> {
        if self.pos >= self.cap {
            debug_assert!(self.pos == self.cap);
            self.cap = self.inner.read(&mut self.buf)?;
            self.pos = 0;
        }
        Ok(&self.buf[self.pos..self.cap])
    }

    fn consume(&mut self, amt: usize) {
        self.pos = std::cmp::min(self.pos + amt, self.cap);
    }
}

/// Apply a streaming parser to a stream wrapped in `GrowableBufReader`. If the parser returns an
/// `Incomplete` value, the `GrowableBufReader` will be filled, and the parser will be re-ran,
/// until parsing succeeds. Upon return, the position tracked by `GrowableBufReader` will point
/// after the data that the parser consumed.
pub fn drive_streaming_parser<R, F, O>(
    reader: &mut GrowableBufReader<R>,
    mut parser: F,
) -> Result<O, Error>
where
    R: Read,
    F: FnMut(&[u8]) -> IResult<&[u8], O, nom::error::Error<&[u8]>>,
{
    let mut eof_flag = false;
    loop {
        let data = reader.fill_buf()?;
        match parser(data) {
            Ok((rest_input, output)) => {
                debug_assert!(data.ends_with(rest_input));
                let consume_amount = data.len() - rest_input.len();
                reader.consume(consume_amount);
                return Ok(output);
            }
            Err(Err::Incomplete(Needed::Unknown)) => {
                if eof_flag {
                    return Err(ErrorKind::Eof.into());
                }
                let (_, eof) = reader.fill_buf_required(reader.buffer_capacity() + 1024)?;
                eof_flag |= eof;
            }
            Err(Err::Incomplete(Needed::Size(needed))) => {
                if eof_flag {
                    return Err(ErrorKind::Eof.into());
                }
                let (_, eof) = reader.fill_buf_required(reader.buffer_capacity() + needed.get())?;
                eof_flag |= eof;
            }
            Err(Err::Error(e)) | Err(Err::Failure(e)) => return Err(e.code.into()),
        }
    }
}

/// Apply a streaming parser to a seekable stream. This function uses its own buffer, rather than
/// the one from `GrowableBufReader`. Upon return, the stream's position will be right after the
/// bytes consumed by the parser.
pub fn drive_streaming_parser_smaller<RS, F, O>(mut reader: RS, mut parser: F) -> Result<O, Error>
where
    RS: Read + Seek,
    F: FnMut(&[u8]) -> IResult<&[u8], O, nom::error::Error<&[u8]>>,
{
    let mut buf = Vec::with_capacity(4);
    loop {
        let expected_length = buf.capacity();
        let read_length = buf.capacity() - buf.len();
        let mut take = reader.take(read_length.try_into().unwrap());
        take.read_to_end(&mut buf)?;
        let at_eof = buf.len() < expected_length;
        reader = take.into_inner();
        match parser(&buf) {
            Ok((rest, output)) => {
                if !rest.is_empty() {
                    reader.seek(SeekFrom::Current(
                        -TryInto::<i64>::try_into(rest.len()).unwrap(),
                    ))?;
                }
                return Ok(output);
            }
            Err(Err::Incomplete(Needed::Unknown)) => {
                if at_eof {
                    return Err(ErrorKind::Eof.into());
                }
                buf.reserve(256);
            }
            Err(Err::Incomplete(Needed::Size(needed))) => {
                if at_eof {
                    return Err(ErrorKind::Eof.into());
                }
                buf.reserve(needed.get());
            }
            Err(Err::Error(e)) | Err(Err::Failure(e)) => return Err(e.code.into()),
        }
    }
}

fn load_index_reader<
    I: IndexEntry,
    EP: Fn(&[u8]) -> IResult<&[u8], I>,
    CP: Fn(&[u8]) -> IResult<&[u8], CollisionEntry<I::Hash>>,
>(
    bufreader: &mut GrowableBufReader<File>,
    entry_parser: EP,
    collision_parser: CP,
) -> Result<Index<I>, Error> {
    let file_header = drive_streaming_parser(bufreader, sqpack_header_outer)?;
    let size = file_header.1;

    bufreader.seek(SeekFrom::Start(size.into()))?;
    let index_header = drive_streaming_parser(bufreader, index_segment_headers)?;
    let first_segment_header = &index_header.2[0];
    let second_segment_header = &index_header.2[1];

    bufreader.seek(SeekFrom::Start(first_segment_header.offset.into()))?;
    let entry_count = first_segment_header.size / I::SIZE;
    let mut index_entries = Vec::with_capacity(entry_count.try_into().unwrap());
    for _ in 0..entry_count {
        let index_entry = drive_streaming_parser(bufreader, &entry_parser)?;
        index_entries.push(index_entry);
    }

    bufreader.seek(SeekFrom::Start(second_segment_header.offset.into()))?;
    let entry_count = second_segment_header.size / 256 - 1;
    let mut collision_entries = Vec::with_capacity(entry_count.try_into().unwrap());
    for _ in 0..entry_count {
        let entry = drive_streaming_parser(bufreader, &collision_parser)?;
        collision_entries.push(entry);
    }

    Ok(Index::new(index_entries, collision_entries))
}

pub fn load_index_1(path: PathBuf) -> Result<Index<IndexEntry1>, Error> {
    let file = File::open(path)?;
    let mut bufreader = GrowableBufReader::new(file);
    load_index_reader(&mut bufreader, index_entry_1, collision_entry_1)
}

pub fn load_index_2(path: PathBuf) -> Result<Index<IndexEntry2>, Error> {
    let file = File::open(path)?;
    let mut bufreader = GrowableBufReader::new(file);
    load_index_reader(&mut bufreader, index_entry_2, collision_entry_2)
}

pub fn decompress_file(mut file: &mut File, data_entry_offset: u32) -> Result<Vec<u8>, Error> {
    // Note that file decompression could be parallelized by splitting different blocks across
    // threads. This is probably why the file format has multiple blocks per entry.
    file.seek(SeekFrom::Start(data_entry_offset.into()))?;
    let blocks = drive_streaming_parser_smaller(&mut *file, data_entry_headers(data_entry_offset))?;
    let mut compressed = Vec::new();
    let mut decompressed = Vec::new();
    for block_offset in blocks.all_blocks() {
        file.seek(SeekFrom::Start(block_offset.into()))?;
        let (compressed_length, decompressed_length) =
            drive_streaming_parser_smaller(&mut *file, block_header)?;
        if compressed_length == 32000 {
            let mut take = file.take(decompressed_length.try_into().unwrap());
            take.read_to_end(&mut decompressed)?;
            file = take.into_inner();
        } else {
            let mut take = file.take(compressed_length.try_into().unwrap());
            take.read_to_end(&mut compressed)?;
            let block_decompressed =
                decompress_sqpack_block(&compressed, decompressed_length.try_into().unwrap())?;
            decompressed.extend_from_slice(&block_decompressed);
            compressed.clear();
            file = take.into_inner();
        }
    }
    Ok(decompressed)
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use nom::{
        error::{ErrorKind, ParseError},
        Err, Needed,
    };

    use super::{sqpack_header_inner, sqpack_header_outer};
    use crate::{IndexPointer, PlatformId, SqPackType};

    #[test]
    fn test_null_padding() {
        use super::null_padding;
        let mut three_byte_parser = null_padding(3);
        assert_eq!(three_byte_parser(b"\x00\x00\x00"), Ok((&b""[..], ())));
        assert_eq!(
            three_byte_parser(b"\x00\x00\x00\x00"),
            Ok((&b"\x00"[..], ()))
        );
        assert_eq!(
            three_byte_parser(b"\x00\x00"),
            Err(Err::Incomplete(Needed::Size(NonZeroUsize::new(1).unwrap())))
        );
        assert_eq!(
            three_byte_parser(b"\x00\x00\x01"),
            Err(Err::Error((&b"\x00\x00\x01"[..], ErrorKind::Count)))
        )
    }

    #[test]
    fn test_platform_id() {
        use super::platform_id;
        assert_eq!(
            platform_id(&[PlatformId::Win32 as u8][..]).unwrap().1,
            PlatformId::Win32
        );
        assert_eq!(
            platform_id(&[PlatformId::Ps3 as u8][..]).unwrap().1,
            PlatformId::Ps3
        );
        assert_eq!(
            platform_id(&[PlatformId::Ps4 as u8][..]).unwrap().1,
            PlatformId::Ps4
        );
        assert_eq!(
            platform_id(b"\xff").unwrap_err(),
            Err::Error(nom::error::Error::from_error_kind(
                &b"\xff"[..],
                ErrorKind::MapRes,
            )),
        );
        assert!(matches!(platform_id(b"").unwrap_err(), Err::Incomplete(_)));
    }

    #[test]
    fn test_sqpack_type() {
        use super::sqpack_type;
        assert_eq!(
            sqpack_type(&[SqPackType::Sqdb as u8, 0, 0, 0][..])
                .unwrap()
                .1,
            SqPackType::Sqdb
        );
        assert_eq!(
            sqpack_type(&[SqPackType::Data as u8, 0, 0, 0][..])
                .unwrap()
                .1,
            SqPackType::Data
        );
        assert_eq!(
            sqpack_type(&[SqPackType::Index as u8, 0, 0, 0][..])
                .unwrap()
                .1,
            SqPackType::Index
        );
        assert_eq!(
            sqpack_type(b"1234").unwrap_err(),
            Err::Error(nom::error::Error::from_error_kind(
                &b"1234"[..],
                ErrorKind::MapRes
            ))
        );
        assert!(matches!(sqpack_type(b"").unwrap_err(), Err::Incomplete(_)));
    }

    #[test]
    fn test_sqpack_header() {
        let data = b"\
            SqPack\x00\x00\
            \x00\
            \x00\x00\x00\
            \x00\x04\x00\x00\
            \x01\x00\x00\x00\
            \x02\x00\x00\x00\
            \x00\x00\x00\x00\
            \x00\x00\x00\x00\
            \xff\xff\xff\xff\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\
            \x14\x03\x16\xfb\x3d\x2f\x7a\x61\xd8\xd9\x51\x20\x12\xe4\x4a\xf6\xa1\xe1\x45\x2e\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\x00\x00\x00\x00\
            \x00\x00\x00\x00\
        ";
        assert_eq!(
            sqpack_header_inner(data).unwrap().1,
            (PlatformId::Win32, 1024, 1, SqPackType::Index)
        );
        assert_eq!(
            sqpack_header_outer(data).unwrap(),
            (&b""[..], (PlatformId::Win32, 1024, 1, SqPackType::Index))
        );
    }

    #[test]
    fn test_pointer_roundtrip() {
        let pointer = IndexPointer::from_u32(0x260);
        assert_eq!(pointer.to_u32(), 0x260);
    }
}

#[cfg(test)]
mod tests_game_data;
