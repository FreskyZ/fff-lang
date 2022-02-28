///! source::fs: abstract file system for test (and should also be useful for compiler api)

use std::io;
use std::path::{Path, PathBuf};

pub trait FileSystem {
    fn canonicalize(&self, path: impl AsRef<Path>) -> io::Result<PathBuf>;
    fn read_to_string(&self, path: impl AsRef<Path>) -> io::Result<String>;
}

#[derive(Debug)]
pub struct DefaultFileSystem;

impl Default for DefaultFileSystem {
    fn default() -> Self { 
        Self
    }
}
impl FileSystem for DefaultFileSystem {
    fn canonicalize(&self, path: impl AsRef<Path>) -> io::Result<PathBuf> {
        std::fs::canonicalize(path)
    }
    fn read_to_string(&self, path: impl AsRef<Path>) -> io::Result<String> {
        std::fs::read_to_string(path)
    }
}

// virtual file system for test
#[cfg(test)]
#[derive(Debug)]
pub struct VirtualFileSystem {
    pub files: std::collections::HashMap<PathBuf, String>,
}

#[cfg(test)]
impl FileSystem for VirtualFileSystem {
    fn canonicalize(&self, path: impl AsRef<Path>) -> io::Result<PathBuf> {
        Ok(path.as_ref().into())
    }
    fn read_to_string(&self, path: impl AsRef<Path>) -> io::Result<String> {
        self.files.get(path.as_ref().into()).map(|v| v.clone()).ok_or_else(|| io::ErrorKind::NotFound.into())
    }
}
