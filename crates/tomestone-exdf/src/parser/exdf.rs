use nom::{
    bytes::streaming::tag, combinator::map, number::streaming::le_u32, sequence::tuple, IResult,
};

fn exdf_header(input: &[u8]) -> IResult<&[u8], ()> {
    map(tuple((tag("EXDF"), le_u32, le_u32)), |_| ())(input)
}

#[cfg(test)]
mod tests {
    use tomestone_sqpack::GameData;

    use super::exdf_header;

    #[test]
    fn exdf_game_data() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();
        const EXH_PATH: &str = "exd/fcauthority.exh";
        const EXD_PATH: &str = "exd/fcauthority_0_en.exd";

        let exd_data = game_data.lookup_path_data(&EXD_PATH).unwrap().unwrap();
        exdf_header(&exd_data).unwrap();
    }
}
