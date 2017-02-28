use clap::{self, Arg, App, SubCommand};
use borrowcalls::{ BorrowCalls, ParseError };
use error;
use log::{ self, LogRecord, LogMetadata, LogLevel };
use std::sync::Mutex;
use std::cell::RefCell;
use std::fs::{ File, OpenOptions };
use std::io::{ self, Write };
use syntax_pos::BytePos;

struct SimpleFileLogger {
    file: Mutex<RefCell<File>>
}

impl SimpleFileLogger {
    fn new(file: File) -> Self {
        SimpleFileLogger { file: Mutex::new(RefCell::new(file)) }
    }
}

impl log::Log for SimpleFileLogger {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= LogLevel::Debug
    }

    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) {
            let file = self.file.lock().unwrap();
            if let Err(e) = write!(file.borrow_mut(), "{}\n", record.args()) {
                if let Err(e) = write!(io::stderr(), "Failed to write to log file: {}", e) {
                    panic!("Something went horribly, horribly wrong: {}", e);
                }
            };
        }
    }
}

fn parse_compiler_args<'a: 'b, 'b, F>(matches: &'a clap::ArgMatches, f: F) -> Result<(BorrowCalls<'b>, Vec<String>), ParseError<'b>>
    where F: FnOnce(Vec<String>) -> (BorrowCalls<'b>, Vec<String>)
{
    matches.values_of("args")
        .map(|args| f(args.map(str::to_string).collect()))
        .ok_or(error::gen_error("Compile args are missing").into())
}

pub fn parse_input<'a: 'b, 'b>(matches: &'a clap::ArgMatches) -> Result<(BorrowCalls<'b>, Vec<String>), ParseError<'b>> {
    match matches.subcommand() {
        ("bytes", Some(sub_m)) => {
            let offset = try!(sub_m.value_of("offset").unwrap().parse::<u32>());
            let line_start = try!(sub_m.value_of("line_start").unwrap().parse::<u32>());
            let line_end = try!(sub_m.value_of("line_end").unwrap().parse::<u32>());
            if let Some(output) = sub_m.value_of("log_out") {
                try!(log::set_logger(move |_| {
                    let file = OpenOptions::new()
                        .read(true)
                        .write(true)
                        .create(true)
                        .open(output);
                    box SimpleFileLogger::new(file.unwrap())
                }));
            }

            parse_compiler_args(sub_m, |args| {
                let line = BytePos(line_start)..BytePos(line_end);
                let borrow_calls = BorrowCalls::with_bytes(BytePos(offset), line);
                (borrow_calls, args)
            })
        },
        ("line", Some(sub_m)) => {
            let file_name = sub_m.value_of("file_name").unwrap();
            let line = try!(sub_m.value_of("line").unwrap().parse::<u32>());
            let column = try!(sub_m.value_of("column").unwrap().parse::<u32>());
            if let Some(output) = sub_m.value_of("log_out") {
                try!(log::set_logger(move |max_log_level| {
                    max_log_level.set(log::LogLevelFilter::Debug);
                    let file = OpenOptions::new()
                        .read(true)
                        .append(true)
                        .create(true)
                        .open(output);
                    box SimpleFileLogger::new(file.unwrap())
                }));
            }

            parse_compiler_args(sub_m, |mut args| {
                let borrow_calls = BorrowCalls::with_line_info(file_name, line, column);
                args.push(file_name.to_owned());
                (borrow_calls, args)
            })
        },
        (cmd, _) => Err(error::gen_error(format!("Unrecognized subcommand: {}", cmd)).into())
    }
}

fn extend_args<'a>(app: App<'a, 'a>) -> App<'a, 'a> {
    app.arg_from_usage("[args]... 'Arguments to pass to the compiler'")
}

pub fn get_matches<'a>() -> clap::ArgMatches<'a> {
    App::new("Borrow Visualizer")
        .version("0.1")
        .author("Paul D. Faria")
        .about("Poorly finds borrow spans")
        .subcommand(extend_args(SubCommand::with_name("bytes")
            .arg(Arg::with_name("offset")
                .value_name("OFFSET_BYTES")
                .help("The number of bytes from the start of the file to the item to analyze.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("line_start")
                .value_name("LINE_START_BYTES")
                .help("The number of bytes from the start of the file to the start of the line of the item to analyze.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("line_end")
                .value_name("LINE_END_BYTES")
                .help("The number of bytes from the start of the file to the end of the line of the item to anaylize.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("log_out")
                .value_name("OUTPUT_FILE")
                .help("The file to write logs to.")
                .takes_value(true)
                .required(false))))
        .subcommand(extend_args(SubCommand::with_name("line")
            .arg(Arg::with_name("file_name")
                .value_name("FILE_NAME")
                .help("The name of the file that's being analyzed.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("line")
                .value_name("LINE_NUMBER")
                .help("The line number for the item to analyze.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("column")
                .value_name("COLUMN_NUMBER")
                .help("The column number for the item to analyze.")
                .takes_value(true)
                .required(true))
            .arg(Arg::with_name("log_out")
                .value_name("OUTPUT_FILE")
                .help("The file to write logs to.")
                .takes_value(true)
                .required(false))))
        .get_matches()
}