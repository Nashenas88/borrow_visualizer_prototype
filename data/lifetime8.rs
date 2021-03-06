#[derive(Debug)]
struct Foo;

#[derive(Debug)]
struct Bar<'b> {
    f: Option<&'b str>
}

struct MutBar<'b> {
    f: Option<&'b mut str>
}

impl<'b> Bar<'b> {
    fn unclear_method_name(&mut self) -> Option<&str> {
        self.f
    }
}

fn restrict<'a, 'b>(xyz: &'a Foo) -> Bar<'b> {
    let z = &xyz;
    unimplemented!()
}

fn restrict_mut<'a, 'b>(y: &'a mut Bar<'b>, x: &'b Foo) {
    unimplemented!()
}

fn restrict_mut2<'a, 'b>(y: &'a mut MutBar<'b>, x: &'b Foo) {
    unimplemented!()
}

fn borrow(x: &Foo) {}

fn move_f(f: String) {
    println!("{:?}", f);
}

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
    }
    borrow(&mut target3);

    if let Some(x) = Some("abc123") {
        let v = &x.chars();
        println!("{:?}", v);
    }

    match Some("123456") {
        Some(x) => println!("{}", x),
        None => println!("Noting"),
    }

    let mut b_target = Bar { f: Some("123") };
    let f = b_target.unclear_method_name();
    let g = b_target.unclear_method_name();
    let x = 4;
    let mut message = "I'm moving!".to_owned();
    {
        let borrowed_msg = &message;
        println!("borrowed: {}", borrowed_msg);

        let borrowed_msg2 = &message;
        println!("borrowed 2: {}", borrowed_msg2);
    }
    {
        restrict_mut2(&mut MutBar { f: Some(&mut message)}, &target3);
    }
    move_f(message);
    drop(x);
    move_f(message);
    let y = x*3;
    let f2 = b_target.unclear_method_name();
    println!("b_target: {:?}", b_target);
}
