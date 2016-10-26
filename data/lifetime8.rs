#[derive(Debug)]
struct Foo;

struct Bar<'b> {
    f: Option<&'b i32>
}

fn restrict<'a, 'b>(xyz: &'a Foo) -> Bar<'b> {
    let z = &xyz;
    unimplemented!()
}

fn restrict_mut<'a, 'b>(y: &'a mut Bar<'b>, x: &'b Foo) {
    unimplemented!()
}

fn borrow(x: &Foo) {}

fn main() {
    let mut target = Foo;
    let b = restrict(&mut target);
    borrow(&target);

    let target2 = Foo;
    {
        let b = &target2;
        println!("{:?}", b);
    }

    let mut target3 = Foo;
    {
        let mut b = Bar { f: None };
        restrict_mut(&mut b, &target3);
    borrow(&mut target3);
}
