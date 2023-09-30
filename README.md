# Chord Parser

Parser for musical chord signatures. Successful outputs return a parsed chord with a built-in abstract representation for chords.

## Features

- Full chord parsing with additional support for unicode symbols! (i.e. Δ, °)
- Ambiguous notation to concrete notes conversion (maj13 -> maj7 + 9 + 13, sus -> sus4, etc.)
- Input normalization (space trimming, etc.)
- Fully documented and convenient to use!

## Usage

```rust
use chord_parser::*;

let mut parser = ChordParser::new();
 
let result = parser.parse("Cmaj9");
 
match result {
    ChordParseResult::Success(chord) => println!("{:?}", chord.alterations.seventh),
    ChordParseResult::Failure(kind) => panic!("Expected successful parse!"),
};
 
let result = parser.parse("E7(b9,b13)");
 
// Do something else...
```

## Advanced

For more, visit the official [documentation](https://docs.rs/crate/chord-parser/latest/).