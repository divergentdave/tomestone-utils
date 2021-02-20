use nom::{
    bytes::streaming::tag, combinator::map, number::streaming::le_u32, sequence::tuple, IResult,
};

fn exhf_header(input: &[u8]) -> IResult<&[u8], ()> {
    map(tuple((tag("EXHF"), le_u32, le_u32)), |_| ())(input)
}

#[cfg(test)]
mod tests {
    use tomestone_sqpack::GameData;

    use super::exhf_header;

    #[test]
    fn exhf_game_data() {
        dotenv::dotenv().ok();
        // Don't test anything if the game directory isn't provided
        let root = if let Ok(root) = std::env::var("FFXIV_INSTALL_DIR") {
            root
        } else {
            return;
        };
        let game_data = GameData::new(root).unwrap();
        const EXH_PATH: &str = "exd/fcauthority.exh";

        let exh_data = game_data.lookup_path_data(EXH_PATH).unwrap().unwrap();
        exhf_header(&exh_data).unwrap();
    }
}
