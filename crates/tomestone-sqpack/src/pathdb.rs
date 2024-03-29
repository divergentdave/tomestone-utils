use std::{fmt, fs, io};

use directories::ProjectDirs;
use r2d2::PooledConnection;
use r2d2_sqlite::SqliteConnectionManager;
use rusqlite::{params, Statement};

use crate::{IndexHash, IndexHash1, IndexHash2};

const FILENAME: &str = "paths.db";

#[derive(Debug)]
pub enum DbError {
    Sqlite(rusqlite::Error),
    Pool(r2d2::Error),
    Io(io::Error),
    NoDirectories,
}

impl fmt::Display for DbError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DbError::Sqlite(e) => e.fmt(f),
            DbError::Pool(e) => e.fmt(f),
            DbError::Io(e) => e.fmt(f),
            DbError::NoDirectories => write!(f, "Home directory path could not be found"),
        }
    }
}

impl From<rusqlite::Error> for DbError {
    fn from(e: rusqlite::Error) -> Self {
        DbError::Sqlite(e)
    }
}

impl From<r2d2::Error> for DbError {
    fn from(e: r2d2::Error) -> Self {
        DbError::Pool(e)
    }
}

impl From<io::Error> for DbError {
    fn from(e: io::Error) -> Self {
        DbError::Io(e)
    }
}

pub struct PathDb {
    pool: r2d2::Pool<SqliteConnectionManager>,
}

impl PathDb {
    pub fn open() -> Result<PathDb, DbError> {
        let project_dirs = ProjectDirs::from("party.davidsherenowitsa", "David Cook", "Tomestone")
            .ok_or(DbError::NoDirectories)?;
        let dir = project_dirs.data_dir();
        if !dir.is_dir() {
            fs::create_dir_all(dir)?;
        }
        let manager = SqliteConnectionManager::file(dir.join(FILENAME));
        let pool = r2d2::Pool::new(manager)?;
        let conn = pool.get()?;
        conn.execute(
            "CREATE TABLE IF NOT EXISTS index_1_folder (
                crc INTEGER NOT NULL,
                path TEXT NOT NULL PRIMARY KEY
            )",
            [],
        )?;
        conn.execute(
            "CREATE TABLE IF NOT EXISTS index_1_filename (
                crc INTEGER NOT NULL,
                path TEXT NOT NULL PRIMARY KEY
            )",
            [],
        )?;
        conn.execute(
            "CREATE TABLE IF NOT EXISTS index_2_path (
                crc INTEGER NOT NULL,
                path TEXT NOT NULL PRIMARY KEY
            )",
            [],
        )?;

        conn.execute(
            "CREATE INDEX IF NOT EXISTS index_1_folder_crc ON index_1_folder (crc)",
            [],
        )?;
        conn.execute(
            "CREATE INDEX IF NOT EXISTS index_1_filename_crc ON index_1_filename (crc)",
            [],
        )?;
        conn.execute(
            "CREATE INDEX IF NOT EXISTS index_2_path_crc ON index_2_path (crc)",
            [],
        )?;

        Ok(PathDb { pool })
    }

    pub fn get_connection(&self) -> Result<PooledConnection<SqliteConnectionManager>, r2d2::Error> {
        self.pool.get()
    }

    pub fn prepare(
        connection: &PooledConnection<SqliteConnectionManager>,
    ) -> Result<PreparedStatements<'_>, DbError> {
        let index_1_folder_lookup_stmt =
            connection.prepare("SELECT path FROM index_1_folder WHERE crc = ?")?;
        let index_1_folder_insert_stmt =
            connection.prepare("INSERT OR IGNORE INTO index_1_folder (crc, path) VALUES (?, ?)")?;
        let index_1_filename_lookup_stmt =
            connection.prepare("SELECT path FROM index_1_filename WHERE crc = ?")?;
        let index_1_filename_insert_stmt = connection
            .prepare("INSERT OR IGNORE INTO index_1_filename (crc, path) VALUES (?, ?)")?;
        let index_2_lookup_stmt =
            connection.prepare("SELECT path FROM index_2_path WHERE crc = ?")?;
        let index_2_insert_stmt =
            connection.prepare("INSERT OR IGNORE INTO index_2_path (crc, path) VALUES (?, ?)")?;
        Ok(PreparedStatements {
            index_1_folder_lookup_stmt,
            index_1_folder_insert_stmt,
            index_1_filename_lookup_stmt,
            index_1_filename_insert_stmt,
            index_2_lookup_stmt,
            index_2_insert_stmt,
        })
    }
}

pub struct PreparedStatements<'a> {
    index_1_folder_lookup_stmt: Statement<'a>,
    index_1_folder_insert_stmt: Statement<'a>,
    index_1_filename_lookup_stmt: Statement<'a>,
    index_1_filename_insert_stmt: Statement<'a>,
    index_2_lookup_stmt: Statement<'a>,
    index_2_insert_stmt: Statement<'a>,
}

impl<'a> PreparedStatements<'a> {
    pub fn index_1_lookup(
        &mut self,
        hash: IndexHash1,
    ) -> Result<(Vec<String>, Vec<String>), DbError> {
        Ok((
            self.index_1_folder_lookup_stmt
                .query_map([hash.folder_crc], |row| row.get::<_, String>(0))?
                .collect::<Result<Vec<String>, rusqlite::Error>>()?,
            self.index_1_filename_lookup_stmt
                .query_map([hash.filename_crc], |row| row.get::<_, String>(0))?
                .collect::<Result<Vec<String>, rusqlite::Error>>()?,
        ))
    }

    pub fn add_path(&mut self, path: &str) -> Result<(), DbError> {
        {
            let hash = IndexHash1::hash(path);
            let (folder, filename) = IndexHash1::split_path(path);
            self.index_1_folder_insert_stmt
                .execute(params![hash.folder_crc, folder])?;
            self.index_1_filename_insert_stmt
                .execute(params![hash.filename_crc, filename])?;
        }
        {
            let hash = IndexHash2::hash(path);
            self.index_2_insert_stmt
                .execute(params![hash.path_crc, path.to_lowercase()])?;
        }
        Ok(())
    }

    pub fn add_folder(&mut self, path: &str) -> Result<(), DbError> {
        let lowercase = path.to_lowercase();
        let crc = crate::crc32(lowercase.as_bytes());
        self.index_1_folder_insert_stmt
            .execute(params![crc, lowercase])?;
        Ok(())
    }

    pub fn index_2_lookup(&mut self, hash: IndexHash2) -> Result<Vec<String>, DbError> {
        Ok(self
            .index_2_lookup_stmt
            .query_map([hash.path_crc], |row| row.get::<_, String>(0))?
            .collect::<Result<Vec<String>, rusqlite::Error>>()?)
    }
}
