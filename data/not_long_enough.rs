struct Foo;

fn max<'a>(x: &'a Foo, y: &'a Foo) -> &'a Foo {
    x
}

fn main() {
    let a = Foo;
    let y: &Foo;
    {
        let target = Foo;
        y = max(&a, &target);
        // error: `b` does not live long enough
    }
    // potentially more code
}
