#[test]
fn cli_tests() {
    // Change directories to a new empty temporary directory, so that dotenv doesn't pick up the
    // developer's .env file, and clap doesn't display values from it in the help output.

    let current_dir_before = std::env::current_dir().unwrap();
    let test_dir = tempfile::tempdir().unwrap();
    std::env::set_current_dir(&test_dir).unwrap();

    trycmd::TestCases::new()
        .default_bin_path(current_dir_before.join(trycmd::cargo::cargo_bin!("tomestone-dump")))
        .case(current_dir_before.join("tests/cmd/*.trycmd"))
        .run();

    std::env::set_current_dir(&current_dir_before).unwrap();
}
