<div align="center">
  <img src="docs/icon.svg" height="200" alt="Cirq Logo" />
  <h1>Cirq</h1>
  <p>A small, interpreted programming language built with Rust.</p>
</div>

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

```swift
io.printn("hello world");
```

Functions:

```swift
fun greet(name) {
    io.printn("hello, \(name)");
}

greet("cirq");
```

Classes:

```swift
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

## License

ISC