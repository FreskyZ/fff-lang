///! source::fs: abstract file system

use std::io;
use std::path::{Path, PathBuf};

pub trait FileSystem {
    fn get_current_dir(&self) -> io::Result<PathBuf> {
        std::env::current_dir()
    }
    fn canonicalize(&self, path: impl AsRef<Path>) -> io::Result<PathBuf> {
        std::fs::canonicalize(path)
    }
    fn read_to_string(&self, path: impl AsRef<Path>) -> io::Result<String> {
        std::fs::read_to_string(path)
    }
}

#[derive(Debug, Default)]
pub struct DefaultFileSystem;

// trait default to use real file system
impl FileSystem for DefaultFileSystem {}

// virtual file system for test (and should also be useful for compiler api)
#[cfg(test)]
#[derive(Debug)]
pub struct VirtualFileSystem {
    pub cwd: PathBuf,
    pub files: std::collections::HashMap<PathBuf, String>,
}

#[cfg(test)]
impl FileSystem for VirtualFileSystem {
    fn get_current_dir(&self) -> io::Result<PathBuf> {
        Ok(self.cwd.clone())
    }
    fn canonicalize(&self, path: impl AsRef<Path>) -> io::Result<PathBuf> {
        Ok(path.as_ref().into())
    }
    fn read_to_string(&self, path: impl AsRef<Path>) -> io::Result<String> {
        match self.files.get(path.as_ref().into()) {
            Some(content) if content.starts_with("permission") => Err(io::ErrorKind::PermissionDenied.into()),
            Some(content) => Ok(content.clone()),
            None => Err(io::ErrorKind::NotFound.into()),
        }
    }
}
