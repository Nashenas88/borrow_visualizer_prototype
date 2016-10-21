use std;

error_chain! {
    types {
        Error, ErrorKind, ChainErr, Result;
    }

    foreign_links {
        std::io::Error, IoError;
        std::num::ParseIntError, ParseIntError;
    }
}