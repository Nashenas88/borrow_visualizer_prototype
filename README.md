# borrow_visualizer_prototype

This is a prototype, and one that will only be used temporarily.

Dependencies:
* https://github.com/Nashenas88/rust/tree/borrow_visualizer
* rustup (optional but makes things easier. Also, there are no instructions for doing this without rustup ;)

To run, perform the following actions:
* First compile https://github.com/Nashenas88/rust/tree/borrow_visualizer with `./configure` and `make rustc-stage3`
* Wait a long time
* Use rustup to create a new toolchain with `rustup toolchain link bv-rust <path-to-rust-src>/<host-eg-x86_64_apple_darwin>/stage2`
* Make sure you replace the above params in "<>" with the values for your system
* In the folder for this repo then type `rustup override set bv-rust`
* Run `cargo build`
* Run with `target/debug/borrow_bounds line data/lifetime8.rs 13 12 data/lifetime8.rs --sysroot=$(rustc --print sysroot)`

The current output should look like this:
```
[{"kind":"mut", "start":"212", "end":"218"},{"kind":"imm", "start":"233", "end":"239"}]
```

Type `target/debug/borrow_bound help` to see usage instructions.