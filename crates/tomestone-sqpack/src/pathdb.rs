use std::fs;

use directories::ProjectDirs;
use rusqlite::{Connection, Statement, NO_PARAMS};

const FILENAME: &str = "paths.db";

pub enum DbError {}

pub struct PathDb {
    conn: Connection,
}

pub struct PreparedStatements<'a> {
    index_1_directory_lookup_stmt: Statement<'a>,
    index_1_directory_insert_stmt: Statement<'a>,
    index_1_filename_lookup_stmt: Statement<'a>,
    index_1_filename_insert_stmt: Statement<'a>,
    index_2_lookup_stmt: Statement<'a>,
    index_2_insert_stmt: Statement<'a>,
}

impl PathDb {
    pub fn new() -> Result<PathDb, DbError> {
        let project_dirs =
            ProjectDirs::from("party.davidsherenowitsa", "David Cook", "Tomestone").unwrap();
        let dir = project_dirs.data_dir();
        fs::create_dir(dir).unwrap();
        let conn = Connection::open(dir.join(FILENAME)).unwrap();
        conn.execute(
            "CREATE TABLE IF NOT EXISTS index_1_directory (
                crc INTEGER NOT NULL
                path TEXT NOT NULL PRIMARY KEY
            )",
            NO_PARAMS,
        )
        .unwrap();
        conn.execute(
            "CREATE TABLE IF NOT EXISTS index_1_filename (
                crc INTEGER NOT NULL
                path TEXT NOT NULL PRIMARY KEY
            )",
            NO_PARAMS,
        )
        .unwrap();
        conn.execute(
            "CREATE TABLE IF NOT EXISTS index_2_path (
                crc INTEGER NOT NULL
                path TEXT NOT NULL PRIMARY KEY
            )",
            NO_PARAMS,
        )
        .unwrap();

        conn.execute(
            "CREATE INDEX IF NOT EXISTS index_1_directory_crc ON index_1_directory (crc)",
            NO_PARAMS,
        )
        .unwrap();
        conn.execute(
            "CREATE INDEX IF NOT EXISTS index_1_filename_crc ON index_1_filename (crc)",
            NO_PARAMS,
        )
        .unwrap();
        conn.execute(
            "CREATE INDEX IF NOT EXISTS index_2_path_crc ON index_2_path (crc)",
            NO_PARAMS,
        )
        .unwrap();

        Ok(PathDb { conn })
    }

    pub fn prepare<'a>(&'a self) -> Result<PreparedStatements<'a>, DbError> {
        let index_1_directory_lookup_stmt = self
            .conn
            .prepare("SELECT path FROM index_1_directory WHERE crc = ?")
            .unwrap();
        let index_1_directory_insert_stmt = self
            .conn
            .prepare("INSERT OR IGNORE INTO index_1_directory (crc, path) VALUES (?, ?)")
            .unwrap();
        let index_1_filename_lookup_stmt = self
            .conn
            .prepare("SELECT path FROM index_1_filename WHERE crc = ?")
            .unwrap();
        let index_1_filename_insert_stmt = self
            .conn
            .prepare("INSERT OR IGNORE INTO index_1_filename (crc, path) VALUES (?, ?)")
            .unwrap();
        let index_2_lookup_stmt = self
            .conn
            .prepare("SELECT path FROM index_2_path WHERE crc = ?")
            .unwrap();
        let index_2_insert_stmt = self
            .conn
            .prepare("INSERT OR IGNORE INTO index_2_path (crc, path) VALUES (?, ?)")
            .unwrap();
        Ok(PreparedStatements {
            index_1_directory_lookup_stmt,
            index_1_directory_insert_stmt,
            index_1_filename_lookup_stmt,
            index_1_filename_insert_stmt,
            index_2_lookup_stmt,
            index_2_insert_stmt,
        })
    }
}
