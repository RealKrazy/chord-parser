/// Provides a way to effectively read an object piece by piece.
pub trait Reader<T>: Iterator {
    /// Length of the object being read
    fn len(&self) -> usize;
    /// Current index position of the reader
    fn pos(&self) -> usize;
    /// Reads back several pieces
    fn rollback(&mut self, amount: usize) -> Result<(), ()>;

    /// Whether the reader has reached the end of the object
    fn is_end(&self) -> bool {
        self.len() == 0 || self.len() <= self.pos()
    }

    /// Reads several pieces and encapsulates into one object
    fn try_read(&mut self, amount: usize) -> Option<T>;
}

/// Reads a string character by character.
pub struct TextReader {
    pos: usize,
    text: Vec<char>,
}

impl Iterator for TextReader {
    type Item = char;

    /// Reads the next character from the current position
    fn next(&mut self) -> Option<Self::Item> {
        let current = self.text.get(self.pos).copied();
        
        self.pos += 1;

        current
    }
}

impl Reader<String> for TextReader {
    fn len(&self) -> usize {
        self.text.len()
    }

    fn pos(&self) -> usize {
        self.pos
    }

    fn rollback(&mut self, amount: usize) -> Result<(), ()> {
        if self.pos < amount {
            return Err(());
        }

        self.pos -= amount;

        Ok(())
    }

    fn try_read(&mut self, amount: usize) -> Option<String> {
        let mut s = String::with_capacity(amount);

        let mut left = amount;
        while left > 0 {
            if self.is_end() {
                self.pos -= amount - left;
                
                return None;
            }

            let ch = self.next().unwrap();
            s.push(ch);

            left -= 1;
        }

        Some(s)
    }
}

impl TextReader {
    /// Creates an empty reader with empty contents. Isn't particularly useful.
    pub fn new() -> Self {
        TextReader { pos: 0, text: vec![] }
    }

    /// Creates a new reader from the passed text.
    /// 
    /// The provided string is copied, and not used since.
    pub fn from_text(s: &str) -> Self {
        TextReader { pos: 0, text: s.chars().collect() }
    }

    /// Reads until a character is encountered.
    /// 
    /// If no character was found, everything that has been read by the method is rollbacked.
    pub fn read_until_char(&mut self, ch: char) -> Option<String> {
        let mut s = String::new();

        while self.is_end() == false {
            let read_ch = self.next().unwrap();
            s.push(read_ch);

            if read_ch == ch {
                return Some(s);
            }
        }

        self.rollback(s.len()).unwrap();
        None
    }
}