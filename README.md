# borrow_visualizer_prototype

This is a prototype, and one that will only be used temporarily. To run this, first compile https://github.com/Nashenas88/rust/tree/borrow_visualizer with `./configure` and `make rustc-stage3`. Wait a long time, then use rustup to create a new toolchain with `rustup toolchain link bv-rust <path-to-rust-src>/<host-eg-x86_64_apple_darwin>/stage2`. In the folder for this repo then type `rustup override set bv-rust`. The run `cargo build` and run with `cargo run -- data/lifetime8.rs --sysroot=$(rustc --print sysroot)`. The current output should look something like this:

```
    Finished debug [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/borrow_bounds data/lifetime8.rs --sysroot=/Users/paul/programming/rust/x86_64-apple-darwin/stage2`
warning: field is never used: `f`, #[warn(dead_code)] on by default
 --> data/lifetime8.rs:4:5
  |
4 |     f: Option<&'b i32>
  |     ^^^^^^^^^^^^^^^^^^

warning: unused variable: `x`, #[warn(unused_variables)] on by default
 --> data/lifetime8.rs:7:21
  |
7 | fn restrict<'a, 'b>(x: &'a Foo) -> Bar<'b> {
  |                     ^

warning: unused variable: `x`, #[warn(unused_variables)] on by default
  --> data/lifetime8.rs:11:11
   |
11 | fn borrow(x: &Foo) {}
   |           ^

warning: unused variable: `b`, #[warn(unused_variables)] on by default
  --> data/lifetime8.rs:15:9
   |
15 |     let b = restrict(&mut target);
   |         ^

Looking at nodeid 39
lo: 168, hi: 178
found matching block: NodeLocal(pat(39: mut target))
Found 2 loans within fn identified by 39:
NodeLocal(pat(39: mut target))
NodeItem(Item { name: main(83), attrs: [], id: NodeId(35), node: ItemFn(FnDecl { inputs: [], output: DefaultReturn(data/lifetime8.rs:13:11: 13:11), variadic: false }, Normal, NotConst, Rust, Generics { lifetimes: [], ty_params: [], where_clause: WhereClause { id: NodeId(36), predicates: [] }, span: data/lifetime8.rs:1:1: 1:1 }, Block { stmts: [Spanned { node: stmt(70: let mut target = Foo;), span: data/lifetime8.rs:14:5: 14:26 }, Spanned { node: stmt(71: let b = restrict(&mut target);), span: data/lifetime8.rs:15:5: 15:35 }, Spanned { node: stmt(72: borrow(&target);), span: data/lifetime8.rs:16:5: 16:21 }], expr: None, id: NodeId(37), rules: DefaultBlock, span: data/lifetime8.rs:13:11: 17:2 }), vis: Inherited, span: data/lifetime8.rs:13:1: 17:2 })
Loan_0($(local mut target (id=39)), MutBorrow, CodeExtent(45/Misc(NodeId(44)))-CodeExtent(43/Misc(NodeId(46))), [$(local mut target (id=39))])
MutBorrow: Some(data/lifetime8.rs:15:27: 15:33)-Some(data/lifetime8.rs:15:13: 15:34)
Loan_1($(local mut target (id=39)), ImmBorrow, CodeExtent(51/Misc(NodeId(48)))-CodeExtent(49/Misc(NodeId(50))), [$(local mut target (id=39))])
ImmBorrow: Some(data/lifetime8.rs:16:13: 16:19)-Some(data/lifetime8.rs:16:5: 16:20)
```
