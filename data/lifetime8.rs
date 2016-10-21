struct Foo;

struct Bar<'b> {
    f: Option<&'b i32>
}

fn restrict<'a, 'b>(x: &'a Foo) -> Bar<'b> {
    unimplemented!()
}

fn borrow(x: &Foo) {}

fn main() {
    let mut target = Foo;
    let b = restrict(&mut target);
    borrow(&target);
}