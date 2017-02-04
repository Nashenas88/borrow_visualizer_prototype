use std::collections::HashMap;

fn usage<T>(x: T) {}

fn main() {
    let mut target = HashMap::new();
    let key = String::from("foo");
    let value = {
        let map_ref1 = &mut target;
        match map_ref1.get_mut(&key) {
            Some(value) => value,
            None => {
                target.insert(key.clone(), 5);
                let map_ref2 = &mut target;
                map_ref2.get_mut(&key).unwrap()
            }
        }
    };
    usage(value);
    // potentially more code
}