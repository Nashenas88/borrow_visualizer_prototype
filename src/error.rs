use std::borrow::Cow;

pub fn gen_error<'a, T>(message: T) -> String where T: Into<Cow<'a, str>> {
    let message = message.into().to_owned().escape_default();
    format!("[{{\"kind\":\"error\",\"error\":\"{}\"}}]", message)
}

pub fn print_error<'a, T>(message: T) where T: Into<Cow<'a, str>> {
    println!("{}", gen_error(message));
}