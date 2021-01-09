use std::{
    convert::TryInto,
    io::{self, BufRead, Read, Seek, SeekFrom},
    num::NonZeroUsize,
};

use nom::{
    branch::alt,
    bytes::streaming::{tag, take},
    combinator::{all_consuming, complete, map, map_opt, map_parser, map_res, verify},
    error::{Error, ErrorKind, ParseError},
    number::streaming::{le_u32, le_u8},
    sequence::{pair, tuple},
    Err, IResult, InputLength, InputTake, Needed,
};
use sha1::{Digest, Sha1};

use crate::{PlatformId, SqPackType};

const SHA1_OUTPUT_SIZE: usize = 20;

fn sqpack_magic(input: &[u8]) -> IResult<&[u8], ()> {
    map(tag(b"SqPack\x00\x00"), |_| ())(input)
}

fn alternate_dat_magic(input: &[u8]) -> IResult<&[u8], ()> {
    map(tag(b"\x80\x00\x00\x00\x00\x00\x00\x00"), |_| ())(input)
}

fn null_padding<'a, E>(length: usize) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], (), E>
where
    E: ParseError<&'a [u8]>,
{
    move |input: &'a [u8]| {
        if let Some(needed) = length
            .checked_sub(input.input_len())
            .and_then(NonZeroUsize::new)
        {
            Err(Err::Incomplete(Needed::Size(needed)))
        } else {
            let (rest, padding) = input.take_split(length);
            if padding.iter().all(|byte| *byte == 0) {
                Ok((rest, ()))
            } else {
                Err(Err::Error(E::from_error_kind(padding, ErrorKind::Count)))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
struct PlatformIdParseError;

fn platform_id(input: &[u8]) -> IResult<&[u8], PlatformId> {
    map_res(le_u8, |byte: u8| PlatformId::from_u8(byte))(input)
}

fn sqpack_type(input: &[u8]) -> IResult<&[u8], SqPackType> {
    map_res(le_u32, |value| SqPackType::from_u32(value))(input)
}

fn sqpack_header_inner(input: &[u8]) -> IResult<&[u8], (PlatformId, u32, u32, SqPackType)> {
    map(
        tuple((
            alt((sqpack_magic, alternate_dat_magic)),
            platform_id,
            null_padding(3),
            le_u32,
            le_u32,
            sqpack_type,
            le_u32,
            le_u32,
            tag(b"\xff\xff\xff\xff"),
            null_padding(0x39c),
        )),
        |(_, platform_id, _, size, version, sqpack_type, _date, _unknown, _, _)| {
            (platform_id, size, version, sqpack_type)
        },
    )(input)
}

pub fn sqpack_header_outer(input: &[u8]) -> IResult<&[u8], (PlatformId, u32, u32, SqPackType)> {
    map_parser(
        map(
            verify(
                pair(take(0x3c0usize), take(SHA1_OUTPUT_SIZE)),
                |(header_input, header_hash)| {
                    let mut hash = Sha1::new();
                    hash.update(header_input);
                    &*hash.finalize() == *header_hash
                },
            ),
            |(header_input, _header_hash)| header_input,
        ),
        complete(all_consuming(sqpack_header_inner)),
    )(input)
}

#[derive(Debug)]
pub enum IndexType {
    ZERO = 0,
    FILES = 1,
    TWO = 2,
    THREE = 3,
    FOUR = 4,
    FIVE = 5,
}

impl IndexType {
    pub fn parse(value: u32) -> Option<IndexType> {
        match value {
            0 => Some(IndexType::ZERO),
            1 => Some(IndexType::FILES),
            2 => Some(IndexType::TWO),
            3 => Some(IndexType::THREE),
            4 => Some(IndexType::FOUR),
            5 => Some(IndexType::FIVE),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct IndexSegmentHeader {
    pub index_type: IndexType,
    pub offset: u32,
    pub size: u32,
    pub hash: [u8; SHA1_OUTPUT_SIZE],
}

fn index_segment_header(input: &[u8]) -> IResult<&[u8], IndexSegmentHeader> {
    map(
        tuple((
            map_opt(le_u32, |value| IndexType::parse(value)),
            le_u32,
            le_u32,
            take(SHA1_OUTPUT_SIZE),
        )),
        |(index_type, offset, size, hash): (IndexType, u32, u32, &[u8])| IndexSegmentHeader {
            index_type,
            offset,
            size,
            hash: hash.try_into().unwrap(),
        },
    )(input)
}

pub fn index_segment_headers(input: &[u8]) -> IResult<&[u8], (u32, [IndexSegmentHeader; 4])> {
    map(
        tuple((
            le_u32,
            index_segment_header,
            null_padding(44),
            index_segment_header,
            null_padding(40),
            index_segment_header,
            null_padding(40),
            index_segment_header,
        )),
        |(
            size,
            segment_header_1,
            _,
            segment_header_2,
            _,
            segment_header_3,
            _,
            segment_header_4,
        )| {
            (
                size,
                [
                    segment_header_1,
                    segment_header_2,
                    segment_header_3,
                    segment_header_4,
                ],
            )
        },
    )(input)
}

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
            buf: buf,
            pos: 0,
            cap: 0,
        }
    }

    pub fn new(inner: R) -> GrowableBufReader<R> {
        GrowableBufReader::with_capacity(inner, 1024)
    }

    fn fill_buf_required(&mut self, required: usize) -> std::io::Result<(&[u8], bool)> {
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

pub fn drive_streaming_parser<R, F, O, E>(
    reader: &mut GrowableBufReader<R>,
    mut parser: F,
) -> Result<Result<O, ErrorKind>, std::io::Error>
where
    R: Read,
    F: FnMut(&[u8]) -> IResult<&[u8], O, Error<&[u8]>>,
{
    loop {
        let data = reader.fill_buf()?;
        match parser(data) {
            Ok((rest_input, output)) => {
                debug_assert!(data.ends_with(rest_input));
                let consume_amount = data.len() - rest_input.len();
                reader.consume(consume_amount);
                return Ok(Ok(output));
            }
            Err(Err::Incomplete(Needed::Unknown)) => {
                reader.fill_buf_required(1024)?;
            }
            Err(Err::Incomplete(Needed::Size(needed))) => {
                reader.fill_buf_required(needed.get())?;
            }
            Err(Err::Error(e)) | Err(Err::Failure(e)) => {
                dbg!(&e);
                return Ok(Err(e.code));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use nom::{error::ErrorKind, Err, Needed};

    use crate::{PlatformId, SqPackType};

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
            platform_id(&[PlatformId::PS3 as u8][..]).unwrap().1,
            PlatformId::PS3
        );
        assert_eq!(
            platform_id(&[PlatformId::PS4 as u8][..]).unwrap().1,
            PlatformId::PS4
        );
    }

    #[test]
    fn test_sqpack_type() {
        use super::sqpack_type;
        assert_eq!(
            sqpack_type(&[SqPackType::SQDB as u8, 0, 0, 0][..])
                .unwrap()
                .1,
            SqPackType::SQDB
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
    }
}
