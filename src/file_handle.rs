//! This module is supposed to abstract file handling from other modules.

use std::fs::File;
use std::fmt;
use std::io::prelude::*;
use std::io::Error;

/// The file handle represents a file within the compiler.
#[derive(PartialEq, Eq)]
pub struct FileHandle {
    /// The path to the file.
    pub path: String,
    /// The content of the file.
    pub content: String 
}

impl FileHandle {
    /// Creates a new file handle from the given file path.
    pub fn new(path: String) -> Result<FileHandle, Error> {
        let mut file = File::open(&path)?;
        let mut content = String::new();

        file.read_to_string(&mut content)?;

        Ok(FileHandle {
            path,
            content
        })
    }

    /// Creates a new file handle with a fake file for testing purposes.
    #[cfg(test)]
    pub fn test_new(path: String, content: String) -> FileHandle {
        FileHandle {
            path,
            content
        }
    }
}

impl fmt::Debug for FileHandle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FileHandle")
            .field("path", &self.path)
            .finish()
    }
}
