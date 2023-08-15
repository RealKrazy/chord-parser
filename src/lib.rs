//! This cargo provides a parser for musical chord signatures. Successful outputs return a parsed chord
//! using a built-in representation.
//! 
//! # Simple Example
//! ```
//! use chord_parser::*;
//! 
//! let mut parser = ChordParser::new();
//! 
//! let result = parser.parse("Cmaj9");
//! 
//! match result {
//!     ChordParseResult::Success(chord) => println!("{:?}", chord.alterations.seventh),
//!     ChordParseResult::Failure(kind) => panic!("Expected successful parse!"),
//! };
//! 
//! let result = parser.parse("E7(b9,b13)");
//! 
//! // Do something else...
//! ```
//! 
//! # Advanced
//! 
//! For everything you can do with parsing, visit [`ChordParser`].
//! 
//! To examine the abstract representation of chord elements, visit [`chord`].

/// Abstract representations for elements of a chord signature.
pub mod chord;
/// Utilities for parsing required by the crate.
pub mod utils;

use crate::chord::*;
use crate::utils::*;

/// The type of error returned by [`ChordParser`] parsing failure
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum ChordParseErrorKind {
    /// Couldn't parse the root note
    InvalidRoot,
    /// Invalid alterations syntax
    InvalidAlterations,
    /// Currently is not returned; reserved.
    Unknown, // reserved
}

/// Result of the [`ChordParser`]'s parse.
pub enum ChordParseResult {
    /// Successfully parsed the chord signature
    Success(Chord),
    /// Invalid input
    Failure(ChordParseErrorKind),
}

/// The chord parser used for parsing chord signatures into abstract representations defined in [`chord`] module
pub struct ChordParser {
    reader: TextReader,
}

impl ChordParser {
    /// Creates a new instance of `ChordParser`.
    /// Use `ChordParser::parse(&str)` to execute parsing on an input.
    pub fn new() -> Self {
        ChordParser { reader: TextReader::new() }
    }

    /// Parse a chord signature directly from the input.
    /// Shorter than having to create an instance via `new` function.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::*;
    /// 
    /// let result = ChordParser::from_str("C#7b9"); // Parse directly from the input
    /// 
    /// match result {
    ///     ChordParseResult::Success(chord) => (),
    ///     ChordParseResult::Failure(error_kind) => (),
    /// }
    /// ```
    pub fn from_str(s: &str) -> ChordParseResult {
        ChordParser::new().parse(s)
    }

    fn new_reader(&mut self, s: &str) {
        self.reader = TextReader::from_text(s);
    }

    fn try_read_number(&mut self) -> Option<usize> {
        let mut str = String::new();

        while self.reader.is_end() == false {
            let ch = self.reader.next().unwrap();

            if ch.is_ascii_digit() {
                str.push(ch);
                continue;
            }

            self.reader.rollback(1).unwrap();
            break;
        }

        if str.is_empty() || str.len() > 2 { // we don't need numbers more than 2 digits for intervals
            self.reader.rollback(str.len()).unwrap();
            return None;
        }

        Some(str.parse().unwrap())
    }

    fn try_read_interval(&mut self) -> Option<AlteredInterval> {
        let num = match self.try_read_number() {
            Some(num) => num,
            None => return None,
        };

        match AlteredInterval::from_usize(num) {
            Some(interval) => Some(interval),
            None => {
                if num > 9 {
                    self.reader.rollback(2).unwrap();
                } else {
                    self.reader.rollback(1).unwrap();
                }

                None
            }
        }
    }

    fn try_read_alter_accidental(&mut self) -> Option<Accidental> {
        if self.reader.is_end() {
            return None;
        }

        let fst = self.reader.next().unwrap();

        if fst == '+' {
            return Some(Accidental::Sharp);
        } else if fst == '-' {
            return Some(Accidental::Flat);
        }

        let mut s = String::with_capacity(2);
        s.push(fst);

        if let Some(acc) = Accidental::from_str(s.as_str()) {
            if self.reader.is_end() == false {
                s.push(self.reader.next().unwrap());

                if let Some(acc) = Accidental::from_str(s.as_str()) { // double accidental
                    return Some(acc);
                }

                self.reader.rollback(1).unwrap();
            }

            return Some(acc);
        }

        self.reader.rollback(1).unwrap();

        None
    }

    fn try_read_note_alter(&mut self, require_accidental: bool) -> Option<ChordNoteAlter> {
        let accidental = match self.try_read_alter_accidental() {
            Some(accidental) => accidental,
            None => {
                if require_accidental {
                    return None;
                }

                Accidental::Natural
            }
        };

        let interval = match self.try_read_interval() {
            Some(interval) => interval,
            None => return None,
        };

        Some(ChordNoteAlter { accidental, interval })
    }

    fn try_read_note(&mut self) -> Option<Note> {
        let note = match self.reader.next() {
            Some(note) => note,
            None => return None,
        };

        let pitch = match Pitch::from_char(&note) {
            Some(pitch) => pitch,
            None => {
                self.reader.rollback(1).unwrap();
                return None;
            }
        };

        let accidental = match self.reader.is_end() {
            true => Accidental::Natural,
            false => {
                let fst = self.reader.next().unwrap();

                if let Some(res) = Accidental::from_str(&fst.to_string()) {
                    if self.reader.is_end() {
                        res
                    } else {
                        let snd = self.reader.next().unwrap();
                        let mut full = String::new();

                        full.push(fst);
                        full.push(snd);

                        if let Some(res) = Accidental::from_str(&full) {
                            res
                        } else {
                            self.reader.rollback(1).unwrap();

                            res
                        }
                    }
                } else {
                    self.reader.rollback(1).unwrap();

                    Accidental::Natural
                }
            }
        };

        Some(Note { pitch, accidental })
    }

    fn is_one_of(&mut self, elems: &mut Vec<&str>, insensitive: bool) -> bool {
        elems.sort_by(|&a, &b| a.chars().count().cmp(&b.chars().count()));
        elems.reverse();                                                    // make sure we start with the longest one
                                                                            // to account for potential substring options

        for &el in elems.iter() {
            if let Some(s) = self.reader.try_read(el.chars().count()) {
                if insensitive && el.to_lowercase() == s.to_lowercase() {
                    return true;
                } else if insensitive == false && el == s {
                    return true;
                } else {
                    self.reader.rollback(el.chars().count()).unwrap();
                }
            }
        }

        false
    }

    fn parse_alterations(&mut self) -> Option<Alterations> {
        let mut alters = Alterations::new();

        // ambiguous notation conversion rules:
        // (dom/maj)2 = (dom/maj)7 + 2
        // (dom/maj)7 = (dom/maj)7 (no conversion; not ambiguous)
        // (dom/maj)9 = (dom/maj)7 + 9
        // (dom/maj)11 = (dom/maj)7 + sus4 (might be a bit controversial but this is most common)
        // (dom/maj)13 = (dom/maj)9 + 13
        //
        // sus = sus4
        // sus2/4 = sus2 + add9
        // sus13 = dom13 + sus4

        if self.is_one_of(&mut vec!["Δ", "ma", "maj"], true)
        || self.is_one_of(&mut vec!["M"], false) {
            alters.seventh = Seventh::Major;
        }

        if self.is_one_of(&mut vec!["dom"], true) {
            alters.seventh = Seventh::Flat;
        }

        if let Some(interval) = self.try_read_interval() {
            if alters.seventh == Seventh::None {
                alters.seventh = Seventh::Flat;
            }

            match interval {
                AlteredInterval::Second => alters.set_note(&ChordNoteAlter 
                    { interval: AlteredInterval::Second,
                      accidental: Accidental::Natural }),
                AlteredInterval::Sixth => {
                    alters.seventh = Seventh::None;
                    alters.set_note(&ChordNoteAlter {
                        interval: AlteredInterval::Sixth,
                        accidental: Accidental::Natural
                    });
                }
                AlteredInterval::Seventh => (),
                AlteredInterval::Ninth =>
                    alters.set_note(&ChordNoteAlter 
                        { interval: AlteredInterval::Ninth, 
                          accidental: Accidental::Natural }),
                AlteredInterval::Eleventh => {
                    alters.set_suspension(&AlteredInterval::Fourth);
                }
                AlteredInterval::Thirteenth => {
                    alters.set_note(&ChordNoteAlter 
                        { interval: AlteredInterval::Ninth, 
                          accidental: Accidental::Natural });
                    alters.set_note(&ChordNoteAlter 
                        { interval: AlteredInterval::Thirteenth, 
                          accidental: Accidental::Natural });
                }
                _ => return None,
            }
        }

        while self.reader.is_end() == false {
            if let Some(s) = self.reader.try_read(3) {
                if s.to_lowercase() == "add" {
                    let alter = match self.try_read_note_alter(false) {
                        Some(alter) => alter,
                        None => return None,
                    };

                    if let Some(_) = alters.get_note(&alter.interval) { // duplicate
                        return None;
                    }

                    alters.set_note(&alter);
                    continue;
                } else if s.to_lowercase() == "sus" {
                    if let Some(_) = alters.get_suspension() { // duplicate
                        return None;
                    }
                    if self.reader.is_end() {
                        alters.set_suspension(&AlteredInterval::Fourth);
                        break;
                    }

                    let interval = self.try_read_interval();

                    match interval {
                        Some(interval) => {
                            if interval == AlteredInterval::Thirteenth {
                                alters.seventh = Seventh::Flat;

                                alters.set_note(&ChordNoteAlter 
                                    { interval: AlteredInterval::Ninth, 
                                      accidental: Accidental::Natural });
                                alters.set_note(&ChordNoteAlter 
                                    { interval: AlteredInterval::Thirteenth, 
                                    accidental: Accidental::Natural });
                                
                                alters.set_suspension(&AlteredInterval::Fourth);
                                break;
                            }

                            alters.set_suspension(&interval);
                        }
                        None => return None,
                    }

                    continue;
                }

                self.reader.rollback(3).unwrap();
            }

            if self.is_one_of(&mut vec!["no", "no."], true) {
                if alters.no != No::None { // duplicate
                    return None;
                }

                let interval = match self.try_read_number() {
                    Some(num) => match num {
                        3 => No::Third,
                        5 => No::Fifth,
                        _ => return None,
                    },
                    None => return None,
                };

                alters.no = interval;
                continue;
            }

            if let Some(alter) = self.try_read_note_alter(true) {
                if let Some(_) = alters.get_note(&alter.interval) {
                    return None;
                }

                alters.set_note(&alter);
                continue;
            }

            let ch = self.reader.next().unwrap();

            if ch == '(' { // alteration enumeration
                while self.reader.is_end() == false {
                    if self.is_one_of(&mut vec!["ma", "maj"], true) ||
                       self.is_one_of(&mut vec!["Δ", "M"], false) { // must be maj7
                        if self.reader.is_end() || self.reader.next().unwrap() != '7' {
                            return None;
                        }
                        if alters.seventh != Seventh::None {
                            return None; // duplicate
                        }

                        alters.seventh = Seventh::Major;
                    } else if self.is_one_of(&mut vec!["no", "no."], true) {
                        let interval = match self.try_read_number() {
                            Some(num) => match num {
                                3 => No::Third,
                                5 => No::Fifth,
                                _ => return None,
                            },
                            None => return None,
                        };

                        if alters.no != No::None { // duplicate
                            return None;
                        }

                        alters.no = interval;
                    } else {
                        let alter = match self.try_read_note_alter(false) {
                            Some(alter) => alter,
                            None => return None,
                        };

                        if alter.interval == AlteredInterval::Seventh {
                            alters.seventh = Seventh::Flat;
                        } else {
                            if let Some(_) = alters.get_note(&alter.interval) { // duplicate
                                return None;
                            }
    
                            alters.set_note(&alter);
                        }
                    }

                    let next_char = match self.reader.next() {
                        Some(ch) => ch,
                        None => return None,
                    };

                    if next_char == ')' {
                        break;
                    }
                    if next_char != ',' {
                        return None;
                    }
                }

                continue;
            } else if ch == '/' || ch == '\\' {
                let alter = self.try_read_note_alter(false);

                if let Some(alter) = alter {
                    if let Some(_) = alters.get_note(&alter.interval) { // duplicate
                        return None;
                    }
    
                    alters.set_note(&alter);
                    continue;
                }

                // slash chord
                let note = match self.try_read_note() {
                    Some(note) => note,
                    None => return None,
                };

                if alters.slash != None { // duplicate
                    return None;
                }

                alters.slash = Some(note);
                continue;
            }

            return None;
        }

        Some(alters)
    }

    fn parse_chord_type(&mut self) -> ChordTriadType {
        // acceptable parsable chord types
        // *nothing, ("M", "ma", "maj") (- followed by nothing) -> Major
        // "m", "mi", "min", "-" -> Minor
        // "+", "aug" -> Augmented
        // "dim", "°", "o" -> Diminished

        if self.reader.is_end() {
            return ChordTriadType::Major;
        }

        let partial_maj = "ma";
        let partial_min = "mi";

        let full_maj = "maj";
        let full_min = "min";
        let full_aug = "aug";
        let full_dim = "dim"; // all equal in length

        if let Some(s) = self.reader.try_read(full_maj.len()) {
            if s.to_lowercase() == full_maj {
                if self.reader.is_end() == false {
                    self.reader.rollback(full_maj.len()).unwrap();
                }

                return ChordTriadType::Major;
            }
            if s.to_lowercase().get(..partial_maj.len()).unwrap() == partial_maj {
                self.reader.rollback(full_maj.len()).unwrap();
                return ChordTriadType::Major;
            }
            if s.to_lowercase() == full_min {
                return ChordTriadType::Minor;
            }
            if s.to_lowercase().get(..partial_min.len()).unwrap() == partial_min {
                self.reader.rollback(full_min.len() - partial_min.len()).unwrap();
                return ChordTriadType::Minor;
            }
            if s.to_lowercase() == full_aug {
                return ChordTriadType::Augmented;
            }
            if s.to_lowercase() == full_dim {
                return ChordTriadType::Diminished;
            }

            self.reader.rollback(full_maj.len()).unwrap();
        }

        if let Some(s) = self.reader.try_read(partial_maj.len()) {
            if s.to_lowercase() == partial_maj {
                if self.reader.is_end() == false {
                    self.reader.rollback(partial_maj.len()).unwrap();
                }
                
                return ChordTriadType::Major;
            }
            
            self.reader.rollback(partial_maj.len()).unwrap();
        }

        let fst = self.reader.next().unwrap();

        if fst == '+' {
            return ChordTriadType::Augmented;
        }
        if fst == '-' {
            return ChordTriadType::Minor;
        }
        if fst == 'm' {
            return ChordTriadType::Minor;
        }
        if fst == 'M' {
            if self.reader.is_end() {
                return ChordTriadType::Major;
            }
        }
        if fst == '°' || fst == 'o' {
            return ChordTriadType::Diminished;
        }

        self.reader.rollback(1).unwrap();

        ChordTriadType::Major
    }

    /// Parse a chord signature from a string slice.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::*;
    /// 
    /// let mut parser = ChordParser::new();
    /// 
    /// let result = parser.parse("C#7b9"); // Parse from the input
    /// 
    /// match result {
    ///     ChordParseResult::Success(chord) => (),
    ///     ChordParseResult::Failure(error_kind) => (),
    /// }
    /// ```
    pub fn parse(&mut self, s: &str) -> ChordParseResult {
        self.new_reader(s);

        let note = match self.try_read_note() {
            Some(note) => note,
            None => return ChordParseResult::Failure(ChordParseErrorKind::InvalidRoot),
        };

        let chord_type = self.parse_chord_type();

        let alterations = match self.parse_alterations() {
            Some(alterations) => alterations,
            None => return ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations),
        };

        ChordParseResult::Success(Chord {
            note,
            chord_type,
            alterations,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_parsing() {
        let mut parser = ChordParser::new();

        parser.new_reader("15");
        assert_eq!(Some(15), parser.try_read_number());
        parser.new_reader("12asf");
        assert_eq!(Some(12), parser.try_read_number());

        parser.new_reader("asghjka");
        assert_eq!(None, parser.try_read_number());
        parser.new_reader("");
        assert_eq!(None, parser.try_read_number());
        parser.new_reader("123"); // too long for interval needs
        assert_eq!(None, parser.try_read_number());
    }

    #[test]
    fn alter_interval_parsing() {
        let mut parser = ChordParser::new();

        parser.new_reader("13");
        assert_eq!(Some(AlteredInterval::Thirteenth), parser.try_read_interval());
        parser.new_reader("9");
        assert_eq!(Some(AlteredInterval::Ninth), parser.try_read_interval());
        parser.new_reader("2");
        assert_eq!(Some(AlteredInterval::Second), parser.try_read_interval());

        parser.new_reader("15");
        assert_eq!(None, parser.try_read_interval());
        parser.new_reader("");
        assert_eq!(None, parser.try_read_interval());
        parser.new_reader("1326");
        assert_eq!(None, parser.try_read_interval());
    }

    #[test]
    fn note_alteration_parsing() {
        let mut parser = ChordParser::new();

        parser.new_reader("+9");
        assert_eq!(
            ChordNoteAlter { accidental: Accidental::Sharp, interval: AlteredInterval::Ninth },
            parser.try_read_note_alter(true).unwrap()
        );
        parser.new_reader("#9");
        assert_eq!(
            ChordNoteAlter { accidental: Accidental::Sharp, interval: AlteredInterval::Ninth },
            parser.try_read_note_alter(true).unwrap()
        );
        parser.new_reader("##9");
        assert_eq!(
            ChordNoteAlter { accidental: Accidental::DoubleSharp, interval: AlteredInterval::Ninth },
            parser.try_read_note_alter(true).unwrap()
        );
        parser.new_reader("bB9");
        assert_eq!(
            ChordNoteAlter { accidental: Accidental::DoubleFlat, interval: AlteredInterval::Ninth },
            parser.try_read_note_alter(true).unwrap()
        );

        parser.new_reader("b15");
        assert_eq!(None, parser.try_read_note_alter(true));
        parser.new_reader("####13");
        assert_eq!(None, parser.try_read_note_alter(true));
        parser.new_reader("13");
        assert_eq!(None, parser.try_read_note_alter(true));
        parser.new_reader("b");
        assert_eq!(None, parser.try_read_note_alter(true));
        parser.new_reader("gibberish");
        assert_eq!(None, parser.try_read_note_alter(true));
        parser.new_reader("");
        assert_eq!(None, parser.try_read_note_alter(true));
    }

    #[test]
    fn root_note_parsing() {
        let mut parser = ChordParser::new();

        match parser.parse("C") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { note, .. }) => {
                assert_eq!(note.pitch, Pitch::C);
                assert_eq!(note.accidental, Accidental::Natural);
            }
        };

        match parser.parse("d##") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { note, .. }) => {
                assert_eq!(note.pitch, Pitch::D);
                assert_eq!(note.accidental, Accidental::DoubleSharp);
            }
        };

        match parser.parse("dm") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { note, .. }) => {
                assert_eq!(note.pitch, Pitch::D);
                assert_eq!(note.accidental, Accidental::Natural);
            }
        };

        match parser.parse("F#m") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { note, .. }) => {
                assert_eq!(note.pitch, Pitch::F);
                assert_eq!(note.accidental, Accidental::Sharp);
            }
        };

        match parser.parse("Bbbm") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { note, .. }) => {
                assert_eq!(note.pitch, Pitch::B);
                assert_eq!(note.accidental, Accidental::DoubleFlat);
            }
        };

        match parser.parse("kdsk") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidRoot) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };
    }

    #[test]
    fn chord_type_parsing() {
        let mut parser = ChordParser::new();

        match parser.parse("C") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Major);
            }
        };

        match parser.parse("C7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Major);
            }
        };

        match parser.parse("CM") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Major);
            }
        };

        match parser.parse("C#Maj7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Major);
            }
        };

        match parser.parse("Cbb-7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Minor);
            }
        };

        match parser.parse("Ebm11") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Minor);
            }
        };
        
        match parser.parse("Dbmi13") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Minor);
            }
        };

        match parser.parse("G##min9") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Minor);
            }
        };

        match parser.parse("C+9") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Augmented);
            }
        };

        match parser.parse("Caug9") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Augmented);
            }
        };

        match parser.parse("C°7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Diminished);
            }
        };

        match parser.parse("Co7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Diminished);
            }
        };

        match parser.parse("Cdim7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Diminished);
            }
        };
    }

    #[test]
    fn chord_alteration_parsing_add() {
        let mut parser = ChordParser::new();

        match parser.parse("Cadd9") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                })]);
            }
        };

        match parser.parse("Cadd+9addbb4") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Sharp,
                }),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Fourth,
                        accidental: Accidental::DoubleFlat,
                })]);
            }
        };

        match parser.parse("C/+9/bb11/13") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Sharp,
                }),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Eleventh,
                        accidental: Accidental::DoubleFlat,
                }),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Thirteenth,
                        accidental: Accidental::Natural,
                    }
                )]);
            }
        };

        match parser.parse("Cadd") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };
    }

    #[test]
    fn chord_alteration_parsing_sus() {
        let mut parser = ChordParser::new();

        match parser.parse("Csus") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Suspended(
                    AlteredInterval::Fourth,
                )]);
            }
        };

        match parser.parse("Csus4") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Suspended(
                    AlteredInterval::Fourth,
                )]);
            }
        };

        match parser.parse("Csus2") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Suspended(
                    AlteredInterval::Second,
                )]);
            }
        };

        match parser.parse("Csus13") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                }),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Thirteenth,
                        accidental: Accidental::Natural,
                }),
                ChordAlter::Suspended(
                    AlteredInterval::Fourth
                )]);
            }
        };

        match parser.parse("Csus2/4") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Suspended(
                    AlteredInterval::Second,
                ),
                ChordAlter::Add(ChordNoteAlter {
                    interval: AlteredInterval::Fourth,
                    accidental: Accidental::Natural,
                })]);
            }
        };

        match parser.parse("Csusb4") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };

        match parser.parse("Fsuswoahgibberish") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };
    }

    #[test]
    fn chord_alteration_parsing_maj() {
        let mut parser = ChordParser::new();

        match parser.parse("Cmaj7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.alters().clone(), vec![]);
            }
        };

        match parser.parse("CΔ9") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                })]);
            }
        };

        match parser.parse("Cmaj11") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Suspended(AlteredInterval::Fourth)]);
            }
        };

        match parser.parse("Cmaj13") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                }),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Thirteenth,
                        accidental: Accidental::Natural,
                })]);
            }
        };

        match parser.parse("Cmaj") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::None);
                assert_eq!(alterations.alters().clone(), vec![]);
            }
        };

        match parser.parse("Cmaj10") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };
    }

    #[test]
    fn chord_alteration_parsing_dom() {
        let mut parser = ChordParser::new();

        match parser.parse("C2") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Flat);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Second,
                        accidental: Accidental::Natural,
                })]);
            }
        };

        match parser.parse("Cdom7") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Flat);
                assert_eq!(alterations.alters().clone(), vec![]);
            }
        };

        match parser.parse("C9") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Flat);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                })]);
            }
        };

        match parser.parse("C11") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Flat);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Suspended(AlteredInterval::Fourth)]);
            }
        };

        match parser.parse("C13") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Flat);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                }),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Thirteenth,
                        accidental: Accidental::Natural,
                })]);
            }
        };

        match parser.parse("C10") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };
    }

    #[test]
    fn chord_alteration_parsing_alters() {
        let mut parser = ChordParser::new();

        match parser.parse("Cmaj7##5b13+9-10") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Fifth,
                        accidental: Accidental::DoubleSharp,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Thirteenth,
                        accidental: Accidental::Flat,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Sharp,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Tenth,
                        accidental: Accidental::Flat,
                    }
                )]);
            }
        };

        match parser.parse("C7#25") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };
    }

    #[test]
    fn chord_alteration_parsing_6() {
        let mut parser = ChordParser::new();

        match parser.parse("Cm6") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::None);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Sixth,
                        accidental: Accidental::Natural,
                    }
                )]);
            }
        };

        match parser.parse("Fmaj6/9") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::None);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Sixth,
                        accidental: Accidental::Natural,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                    }
                )]);
            }
        };
    }

    #[test]
    fn chord_alteration_parsing_alters_list() {
        let mut parser = ChordParser::new();

        match parser.parse("Cm(maj7,9,b13,+11)") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, alterations, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Minor);
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Thirteenth,
                        accidental: Accidental::Flat,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Eleventh,
                        accidental: Accidental::Sharp,
                    }
                )]);
            }
        };

        match parser.parse("C7(9,13,b9)") {
            ChordParseResult::Failure(ChordParseErrorKind::InvalidAlterations) => (),
            ChordParseResult::Failure(f) => panic!("Wrong failure, got: {:?}", f),
            ChordParseResult::Success(_) => panic!("Expected failure"),
        };
    }

    #[test]
    fn chord_alteration_parsing_no() {
        let mut parser = ChordParser::new();

        match parser.parse("C7(no3)") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Flat);
                assert_eq!(alterations.no, No::Third);
            }
        }

        match parser.parse("Fmaj11no5") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.no, No::Fifth);
            }
        }
    }

    #[test]
    fn chord_alteration_parsing_slash() {
        let mut parser = ChordParser::new();

        match parser.parse("C7/9/E") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Flat);
                assert_eq!(alterations.slash, Some(Note { pitch: Pitch::E, accidental: Accidental::Natural }));
            }
        }

        match parser.parse("Fmaj11no5") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { alterations, .. }) => {
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.no, No::Fifth);
            }
        }
    }

    #[test]
    fn chord_alteration_parsing_mix() {
        let mut parser = ChordParser::new();

        match parser.parse("C+(maj7,9,b13,+11)add+2sus4") {
            ChordParseResult::Failure(_) => panic!("Expected success"),
            ChordParseResult::Success(Chord { chord_type, alterations, .. }) => {
                assert_eq!(chord_type, ChordTriadType::Augmented);
                assert_eq!(alterations.seventh, Seventh::Major);
                assert_eq!(alterations.alters().clone(), vec![ChordAlter::Add(ChordNoteAlter 
                    {
                        interval: AlteredInterval::Ninth,
                        accidental: Accidental::Natural,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Thirteenth,
                        accidental: Accidental::Flat,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Eleventh,
                        accidental: Accidental::Sharp,
                    }
                ),
                ChordAlter::Add(
                    ChordNoteAlter {
                        interval: AlteredInterval::Second,
                        accidental: Accidental::Sharp,
                    }
                ),
                ChordAlter::Suspended(
                    AlteredInterval::Fourth
                )]);
            }
        };
    }
}