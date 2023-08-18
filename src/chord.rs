/// Represents basic musical pitch types (excluding accidentals)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum Pitch {
    /// Base A pitch
    A,
    /// Base B pitch
    B,
    /// Base C pitch
    C,
    /// Base D pitch
    D,
    /// Base E pitch
    E,
    /// Base F pitch
    F,
    /// Base G pitch
    G,
}

impl Pitch {
    /// Tries to parse a single character into `Pitch`. Case insensitive.
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// assert_eq!(Some(Pitch::A), Pitch::from_char(&'a')); // Case insensitive
    /// assert_eq!(None, Pitch::from_char(&' ')); // Invalid input
    /// ```
    pub fn from_char(ch: &char) -> Option<Self> {
        let formatted = match ch.to_lowercase().next() {
            Some(ch) => ch,
            None => return None,
        };
        
        match formatted {
            'a' => Some(Self::A),
            'b' => Some(Self::B),
            'c' => Some(Self::C),
            'd' => Some(Self::D),
            'e' => Some(Self::E),
            'f' => Some(Self::F),
            'g' => Some(Self::G),
            _ => None,
        }
    }
}

impl ToString for Pitch {
    fn to_string(&self) -> String {
        match self {
            Self::A => "A".into(),
            Self::B => "B".into(),
            Self::C => "C".into(),
            Self::D => "D".into(),
            Self::E => "E".into(),
            Self::F => "F".into(),
            Self::G => "G".into(),
        }
    }
}

/// Represents an accidental alteration on a note.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Accidental {
    /// Natural (no accidental). '♮' symbol.
    Natural,
    /// Sharp accidental. '♯', '#' symbols.
    Sharp,
    /// Double sharp accidental. '𝄪', '##' symbols.
    DoubleSharp,
    /// Flat accidental. '♭', 'b' symbols.
    Flat,
    /// Double flat accidental. '𝄫', 'bb' symbols.
    DoubleFlat,
}

impl Accidental {
    /// Tries to parse a string into an accidental. Case insensitive (for both flat alterations).
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// assert_eq!(Some(Accidental::Flat), Accidental::from_str("B")); // Case insensitive
    /// assert_eq!(Some(Accidental::DoubleFlat), Accidental::from_str("𝄫")); // Unicode works
    /// assert_eq!(None, Accidental::from_str("as")); // Invalid input
    /// ```
    pub fn from_str(s: &str) -> Option<Self> {
        let len = s.chars().count();

        if len > 2 || s.is_empty() {
            return None;
        }

        let fst = s.chars().next().unwrap();

        match fst {
            '♮' => return Some(Self::Natural),
            '♯' => return Some(Self::Sharp),
            '𝄪' => return Some(Self::DoubleSharp),
            '♭' => return Some(Self::Flat),
            '𝄫' => return Some(Self::DoubleFlat),
            _ => (),
        }
        
        let is_sharp = match fst {
            '#' => true,
            'b' | 'B' => false,
            _ => return None,
        };

        if len > 1 {
            let snd = s.chars().nth(1).unwrap();

            match is_sharp {
                true => {
                    if snd != '#' {
                        return None;
                    }

                    Some(Self::DoubleSharp)
                }
                false => {
                    if snd != 'b' && snd != 'B' {
                        return None;
                    }

                    Some(Self::DoubleFlat)
                }
            }
        } else {
            match is_sharp {
                true => Some(Self::Sharp),
                false => Some(Self::Flat),
            }
        }
    }
}

impl ToString for Accidental {
    fn to_string(&self) -> String {
        match self {
            Self::Natural => "♮".into(),
            Self::Sharp => "♯".into(),
            Self::DoubleSharp => "𝄪".into(),
            Self::Flat => "♭".into(),
            Self::DoubleFlat => "𝄫".into(),
        }
    }
}

/// Represents a chord note.
/// 
/// # Note
/// `pitch` and `accidental` can be used to calculate the precise note or a frequency in Hz,
/// however would require additional input of octave.
/// Since octave is not a part of a note on a chord SIGNATURE, this property is therefore omitted.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Note {
    /// The pitch of the note
    pub pitch: Pitch,
    /// The accidental of the note
    pub accidental: Accidental,
}

impl ToString for Note {
    fn to_string(&self) -> String {
        let mut s = self.pitch.to_string();
        s.push_str(self.accidental.to_string().as_str());

        s
    }
}

/// Represents a triad type of a chord.
/// 
/// # Note
/// Chord signatures like "C7#5" or "Cm7b5" would NOT be considered augmented and diminished triad respectively.
/// This enum refers to the triad type right after the specified pitch.
/// Therefore, the provided examples in this note would be considered
/// `Self::Major` and `Self::Minor` type respectively.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum ChordTriadType {
    /// Major triad
    Major,
    /// Minor triad
    Minor,
    /// Augmented triad
    Augmented,
    /// Diminished triad
    Diminished,
}

impl ToString for ChordTriadType {
    fn to_string(&self) -> String {
        match self {
            Self::Major => "".into(),
            Self::Minor => "m".into(),
            Self::Augmented => "aug".into(),
            Self::Diminished => "dim".into(),
        }
    }
}

/// Represents the type of seventh in a chord. Usually a part of [`Alterations`] struct.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Seventh {
    /// No seventh present in a chord
    None,
    /// Flatten (regular) seventh. Used for dominant and regular minor chords.
    Flat,
    /// Major (sharpened) seventh. In a chord signature - "maj7".
    Major,
}

impl ToString for Seventh {
    fn to_string(&self) -> String {
        match self {
            Self::None => "".into(),
            Self::Flat => "7".into(),
            Self::Major => "maj7".into(),
        }
    }
}

/// Represents the interval of an alteration. Used in [`ChordAlter`] enum.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum AlteredInterval {
    /// "2" interval
    Second,
    /// "4" interval
    Fourth,
    /// "5" interval
    Fifth,
    /// "6" interval
    Sixth,
    /// "7" interval
    Seventh,
    /// "9" interval
    Ninth,
    /// "10" interval
    Tenth,
    /// "11" interval
    Eleventh,
    /// "13" interval
    Thirteenth,
}

impl AlteredInterval {
    /// Tries to parse a [`usize`] numer into `AlteredInterval`.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// assert_eq!(Some(AlteredInterval::Second), AlteredInterval::from_usize(2));
    /// assert_eq!(Some(AlteredInterval::Ninth), AlteredInterval::from_usize(9));
    /// assert_eq!(Some(AlteredInterval::Thirteenth), AlteredInterval::from_usize(13));
    /// assert_eq!(None, AlteredInterval::from_usize(15)); // Invalid input
    /// ```
    pub fn from_usize(num: usize) -> Option<AlteredInterval> {
        match num {
            2 => Some(Self::Second),
            4 => Some(Self::Fourth),
            5 => Some(Self::Fifth),
            6 => Some(Self::Sixth),
            7 => Some(Self::Seventh),
            9 => Some(Self::Ninth),
            10 => Some(Self::Tenth),
            11 => Some(Self::Eleventh),
            13 => Some(Self::Thirteenth),
            _ => None,
        }
    }
}

impl ToString for AlteredInterval {
    fn to_string(&self) -> String {
        match self {
            Self::Second => "2".into(),
            Self::Fourth => "4".into(),
            Self::Fifth => "5".into(),
            Self::Sixth => "6".into(),
            Self::Seventh => "7".into(),
            Self::Ninth => "9".into(),
            Self::Tenth => "10".into(),
            Self::Eleventh => "11".into(),
            Self::Thirteenth => "13".into(),
        }
    }
}

/// Represents an ålteration of a note
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ChordNoteAlter {
    /// The interval note being altered
    pub interval: AlteredInterval,
    /// The new accidental of the interval note
    pub accidental: Accidental,
}

impl ToString for ChordNoteAlter {
    fn to_string(&self) -> String {
        let mut s = self.interval.to_string();
        s.push_str(self.accidental.to_string().as_str());

        s
    }
}

/// Represents an alteration in a chord.
/// 
/// Typically used in [`Alterations.alters(&self)`] vector array.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ChordAlter {
    /// Added note interval alteration
    Add(ChordNoteAlter),
    /// Suspension intreval alteration
    Suspended(AlteredInterval),
}

impl ToString for ChordAlter {
    fn to_string(&self) -> String {
        match self {
            Self::Add(alter) => {
                "add".to_string() + alter.to_string().as_str()
            }
            Self::Suspended(interval) => {
                "sus".to_string() + interval.to_string().as_str()
            }
        }
    }
}

/// Represents a "no." notation of chords that a tone is missing.
/// 
/// "5" chords are technically "no3" chords.
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum No {
    /// "no." is not present in a chord
    None,
    /// Omit 3rd. "no3"
    Third,
    /// Omit 5th. "no5"
    Fifth,
}

impl ToString for No {
    fn to_string(&self) -> String {
        match &self {
            Self::None => "".into(),
            Self::Third => "no3".into(),
            Self::Fifth => "no5".into(),
        }
    }
}

/// Represents a list of all the alterations presented in a chord.
/// 
/// This struct provides a simple and intuitive way to add alterations using the safe included methods.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Alterations {
    /// Represents an omitted tone in the
    pub no: No,
    /// Represents a seventh in the chord
    pub seventh: Seventh,
    /// Represents the bass note override (slash chord)
    pub slash: Option<Note>,
    
    alters: Vec<ChordAlter>,
}

impl Alterations {
    /// Creates a new empty instance of `Alterations`.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// let mut alterations = Alterations::new();
    /// 
    /// // Do whatever
    /// alterations.set_suspension(&AlteredInterval::Fourth);
    /// ```
    pub fn new() -> Self {
        Alterations { no: No::None, seventh: Seventh::None, slash: None, alters: vec![] }
    }

    /// Returns a list of all the added alterations.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// let mut alterations = Alterations::new();
    /// 
    /// alterations.set_note(&ChordNoteAlter {
    ///     interval: AlteredInterval::Ninth,
    ///     accidental: Accidental::Sharp,
    /// });
    /// 
    /// alterations.set_suspension(&AlteredInterval::Fourth);
    /// 
    /// alterations.alters(); // Returns the added ninth and suspended fourth
    /// ```
    pub fn alters(&self) -> &Vec<ChordAlter> {
        &self.alters
    }

    /// Tries to get a note alteration associated with the passed interval.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// let mut alterations = Alterations::new();
    /// 
    /// alterations.set_note(&ChordNoteAlter {
    ///     interval: AlteredInterval::Ninth,
    ///     accidental: Accidental::Sharp,
    /// });
    /// 
    /// alterations.get_note(&AlteredInterval::Ninth); // Returns the interval and accidental
    /// ```
    pub fn get_note(&self, interval: &AlteredInterval) -> Option<&ChordNoteAlter> {
        for alter in self.alters.iter() {
            match alter {
                ChordAlter::Add(a) => if a.interval == *interval {
                    return Some(a);
                }
                _ => continue,
            }
        }

        None
    }

    /// Tries to get the accidental altered for an interval.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// let mut alterations = Alterations::new();
    /// 
    /// alterations.set_note(&ChordNoteAlter {
    ///     interval: AlteredInterval::Ninth,
    ///     accidental: Accidental::Sharp,
    /// });
    /// 
    /// alterations.get_accidental(&AlteredInterval::Ninth); // Returns Accidental::Sharp
    /// ```
    pub fn get_accidental(&self, interval: &AlteredInterval) -> Option<Accidental> {
        if let Some(alter) = self.get_note(interval) {
            return Some(alter.accidental.clone());
        }

        None
    }

    /// Set a new alteration for an interval.
    /// 
    /// If an alteration for the interval already exists, the accidental will be overwritten.
    /// If it doesn't exist, a new alteration is added.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// let mut alterations = Alterations::new();
    /// 
    /// alterations.set_note(&ChordNoteAlter {
    ///     interval: AlteredInterval::Ninth,
    ///     accidental: Accidental::Sharp,
    /// });
    /// 
    /// // Now the 9th is flatten
    /// alterations.set_note(&ChordNoteAlter {
    ///     interval: AlteredInterval::Ninth,
    ///     accidental: Accidental::Flat,
    /// });
    /// 
    /// alterations.set_note(&ChordNoteAlter {
    ///     interval: AlteredInterval::Eleventh,
    ///     accidental: Accidental::Sharp,
    /// });
    /// 
    /// // Get back all the 2 alterations added
    /// alterations.alters();
    /// ```
    pub fn set_note(&mut self, alter: &ChordNoteAlter) {
        for other in self.alters.iter_mut() {
            match other {
                ChordAlter::Add(other) => if other.interval == alter.interval {
                    other.accidental = alter.accidental.clone();
                    return;
                }
                _ => continue,
            }
        }

        self.alters.push(ChordAlter::Add( alter.clone()));
    }

    /// Tries to get the suspended note alteration in the chord
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// let mut alterations = Alterations::new();
    /// 
    /// alterations.set_suspension(&AlteredInterval::Fourth);
    /// alterations.get_suspension(); // Returns Some(&AlteredInterval::Fourth)
    /// ```
    pub fn get_suspension(&self) -> Option<&AlteredInterval> {
        for alter in self.alters.iter() {
            match alter {
                ChordAlter::Suspended(interval) => return Some(interval),
                _ => (),
            }
        }

        None
    }

    /// Set the new suspended interval.
    /// 
    /// Old suspended interval will be replaced with the new one,
    /// if the old interval is present.
    /// 
    /// # Examples
    /// ```
    /// use chord_parser::chord::*;
    /// 
    /// let mut alterations = Alterations::new();
    /// 
    /// alterations.set_suspension(&AlteredInterval::Second);
    /// alterations.get_suspension(); // Returns Some(&AlteredInterval::Second)
    /// ```
    pub fn set_suspension(&mut self, interval: &AlteredInterval) {
        for (i, alter) in self.alters.iter_mut().enumerate() {
            match alter {
                ChordAlter::Suspended(_) => {
                    self.alters[i] = ChordAlter::Suspended(interval.clone());
                    return;
                }
                _ => (),
            }
        }

        self.alters.push(ChordAlter::Suspended(interval.clone()));
    }
}

impl ToString for Alterations {
    fn to_string(&self) -> String {
        let mut s = self.no.to_string();
        s.push_str(self.seventh.to_string().as_str());

        self.alters.iter().for_each(|a| s.push_str(a.to_string().as_str()));

        if let Some(slash) = &self.slash {
            let slash_str = "/".to_string() + slash.to_string().as_str();
            s.push_str(slash_str.as_str());
        }

        s
    }
}

/// Represents a full chord signature.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Chord {
    /// Root note
    pub note: Note,
    /// Triad type
    pub chord_type: ChordTriadType,
    /// Alterations
    pub alterations: Alterations,
}

impl ToString for Chord {
    fn to_string(&self) -> String {
        let mut s = self.note.to_string();
        s.push_str(self.chord_type.to_string().as_str());
        s.push_str(self.alterations.to_string().as_str());

        s
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pitch_quality() {
        assert_eq!(Pitch::C, Pitch::from_char(&'C').unwrap());
        assert_eq!(Pitch::D, Pitch::from_char(&'D').unwrap());
        assert_eq!(Pitch::E, Pitch::from_char(&'E').unwrap());
        assert_eq!(Pitch::F, Pitch::from_char(&'F').unwrap());
        assert_eq!(Pitch::G, Pitch::from_char(&'G').unwrap());
        assert_eq!(Pitch::A, Pitch::from_char(&'A').unwrap());
        assert_eq!(Pitch::B, Pitch::from_char(&'B').unwrap());

        assert_eq!(None, Pitch::from_char(&'w'));
        assert_eq!(None, Pitch::from_char(&'1'));
    }

    #[test]
    fn pitch_insensitivity() {
        assert_eq!(Pitch::C, Pitch::from_char(&'C').unwrap());
        assert_eq!(Pitch::B, Pitch::from_char(&'b').unwrap());
        assert_eq!(Pitch::B, Pitch::from_char(&'B').unwrap());
    }

    #[test]
    fn accidental_from() {
        assert_eq!(Accidental::Sharp, Accidental::from_str("#").unwrap());
        assert_eq!(Accidental::DoubleSharp, Accidental::from_str("##").unwrap());
        assert_eq!(Accidental::Flat, Accidental::from_str("b").unwrap());

        assert_eq!(Accidental::DoubleFlat, Accidental::from_str("bb").unwrap());
        assert_eq!(Accidental::DoubleFlat, Accidental::from_str("bB").unwrap());
        assert_eq!(Accidental::DoubleFlat, Accidental::from_str("Bb").unwrap());
        assert_eq!(Accidental::DoubleFlat, Accidental::from_str("BB").unwrap());

        assert_eq!(None, Accidental::from_str("b#"));
        assert_eq!(None, Accidental::from_str("#b"));

        assert_eq!(None, Accidental::from_str(""));
        assert_eq!(None, Accidental::from_str("nonsense"));
    }

    #[test]
    fn alterations_struct_fns() {
        let mut a = Alterations::new();

        assert_eq!(None, a.get_note(&AlteredInterval::Ninth));

        a.set_note(&ChordNoteAlter { interval: AlteredInterval::Ninth, accidental: Accidental::Sharp });

        assert_eq!(Some(Accidental::Sharp), a.get_accidental(&AlteredInterval::Ninth));

        a.set_note(&ChordNoteAlter { interval: AlteredInterval::Ninth, accidental: Accidental::Flat });

        assert_eq!(Some(Accidental::Flat), a.get_accidental(&AlteredInterval::Ninth));
        assert_eq!(1, a.alters.len());
    }

    #[test]
    fn interval_from() {
        assert_eq!(AlteredInterval::Second, AlteredInterval::from_usize(2).unwrap());
        assert_eq!(AlteredInterval::Thirteenth, AlteredInterval::from_usize(13).unwrap());
        assert_eq!(AlteredInterval::Eleventh, AlteredInterval::from_usize(11).unwrap());

        assert_eq!(None, AlteredInterval::from_usize(0));
        assert_eq!(None, AlteredInterval::from_usize(999));
    }
}