# Chord Parser

Parser for musical chord signatures. Successful outputs return a parsed chord with a built-in abstract representation for chords.

## Usage

```rust
let mut parser = ChordParser::new();
 
let result = parser.parse("Cmaj9");
 
match result {
    ChordParseResult::Success(chord) => println!("{:?}", chord.seventh),
    ChordParseResult::Failure(kind) => panic!("Expected successful parse!"),
};
 
let result = parser.parse("E7(b9,b13)");
 
// Do something else...
```

## Advanced

For more, visit the official [documentation](https://docs.rs/crate/chord-parser/latest/).
