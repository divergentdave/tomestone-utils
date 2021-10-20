use std::io::Read;

use flate2::{read::DeflateEncoder, Compression};
use miniz_oxide::inflate::{
    core::{
        decompress, inflate_flags::TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF, DecompressorOxide,
    },
    TINFLStatus,
};

pub fn decompress_sqpack_block(
    input: &[u8],
    decompressed_size: usize,
) -> Result<Vec<u8>, TINFLStatus> {
    let mut decompressed = vec![0; decompressed_size];
    let mut decompressor = DecompressorOxide::new();
    let (status, _in_count, _out_count) = decompress(
        &mut decompressor,
        input,
        &mut decompressed,
        0,
        TINFL_FLAG_USING_NON_WRAPPING_OUTPUT_BUF,
    );
    if status != TINFLStatus::Done {
        Err(status)
    } else {
        Ok(decompressed)
    }
}

pub fn compress_sqpack_block(data: &[u8]) -> Result<Vec<u8>, std::io::Error> {
    // This uses libz instead of miniz so that recompressed streams will match the original files,
    // simplifying comparisons.
    let mut deflater = DeflateEncoder::new(data, Compression::best());
    let mut compressed = Vec::new();
    deflater.read_to_end(&mut compressed)?;
    Ok(compressed)
}
