#[test]
fn cli_tests() {
    trycmd::TestCases::new()
        .default_bin_path(trycmd::cargo_bin!("tomestone-dump"))
        .case("tests/cmd/*.trycmd");
}
