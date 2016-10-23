# borrow_visualizer_prototype

This is a prototype, and one that will only be used temporarily.

Dependencies:
* https://github.com/Nashenas88/rust/tree/borrow_visualizer
* rustup (optional but makes things easier. Also, there are no instructions for doing this without rustup ;)

To run this, perform the following actions in order:
* First compile https://github.com/Nashenas88/rust/tree/borrow_visualizer with `./configure` and `make rustc-stage3`
* Wait a long time
* Use rustup to create a new toolchain with `rustup toolchain link bv-rust <path-to-rust-src>/<host-eg-x86_64_apple_darwin>/stage2`
* Make sure you replace the above params in "<>" with the values for your system
* In the folder for this repo then type `rustup override set bv-rust`
* Run `cargo build`
* Run with `target/debug/borrow_bounds -o 173 -s 160 -e 185 data/lifetime8.rs --sysroot=$(rustc --print sysroot)`

The current output should look something like this:
```
[{"kind":"mut", "start":"212", "end":"218"},{"kind":"imm", "start":"233", "end":"239"}]
```

This is the usage of the program:
```
Borrow Visualizer 0.1
Paul D. Faria
Poorly finds borrow spans

USAGE:
    borrow_bounds <args>... --offset <OFFSET_BYTES> --start <BYTES> --end <BYTES>

FLAGS:
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -e, --end <BYTES>              The number of bytes from the start of the file to the end of the line of the item to anaylize.
    -s, --start <BYTES>            The number of bytes from the start of the file to the start of the line of the item to analyze.
    -o, --offset <OFFSET_BYTES>    The number of bytes from the start of the file to the item to analyze.

ARGS:
    <args>...    args to pass to the compiler
```