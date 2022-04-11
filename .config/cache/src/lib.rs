use anyhow::Result;
use cradle::prelude::*;
use redis::{Commands, Connection};

pub fn curl(url: &str) -> String {
    cache_function(uncached, url).unwrap()
}

fn uncached(url: &str) -> Result<String> {
    let StdoutUntrimmed(response) = (("curl", "--fail", "--silent"), url).run_result()?;
    Ok(response)
}

fn cache_function(f: fn(&str) -> Result<String>, arg: &str) -> Result<String> {
    let mut redis = connect()?;
    let key = format!("shahn/colortheme/{}", arg);
    let cached: Option<String> = redis.get(&key)?;
    match cached {
        Some(response) => Ok(response),
        None => {
            let result = f(arg)?;
            redis.set(&key, &result)?;
            Ok(result)
        }
    }
}

fn connect() -> Result<Connection> {
    let client = redis::Client::open("redis://127.0.0.1/")?;
    Ok(client.get_connection()?)
}
