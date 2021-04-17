#![no_main]
use libfuzzer_sys::fuzz_target;

use tomestone_string_interp::Text;

fuzz_target!(|data: &[u8]| {
    let _ = Text::parse(data);
});
