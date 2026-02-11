# Cirq

A small, interpreted programming language built with Rust. Cirq compiles source code to bytecode and runs it on a register-based virtual machine.

## Features

- Variables (`var`, `const`) and functions (`fun`)
- Modules (`mod`) and classes with `self`
- Control flow: `if`/`else`, `while`, `for`, `break`, `continue`
- Arrays with built-in methods (`push`, `pop`, `len`, `map`, `filter`, etc.)
- String interpolation with `\(...)`
- Bitwise operators, compound assignment (`+=`, `-=`, ...), `++`/`--`
- Built-in modules: `io`, `math`, `time`, `env`
- Interactive REPL
- Error reporting with source location and stack traces

## Getting Started

Build from source:

```
cargo build --release
```

Run a script:

```
./target/release/cirq script.cq
```

Start the REPL:

```
./target/release/cirq
```

## Examples

Hello world:

```
io.printn("hello world");
```

Functions:

```
fun greet(name) {
    io.printn("hello, \(name)");
}

greet("cirq");
```

Classes:

```
class Counter {
    init(start) {
        self.value = start;
    }

    increment() {
        self.value++;
    }

    display() {
        io.printn("count: \(self.value)");
    }
}

var c = Counter(0);
c.increment();
c.increment();
c.display(); // count: 2
```

## Built-in Modules

| Module | Description |
|--------|-------------|
| `io`   | Print, input, file read/write |
| `math` | Trig, logarithms, `random()`, constants (`PI`, `E`) |
| `time` | DateTime, `sleep()`, `clock()`, timestamps |
| `env`  | Environment variables, CLI args, `cwd()`, OS info |

Modules are accessed directly by name:

```
io.printn(math.sqrt(2));

var now = time.now();
io.printn(now.to_iso());
```

## License

ISC