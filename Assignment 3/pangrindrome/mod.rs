#[allow(dead_code)]
pub fn is_palindrome(s: &str) -> bool {
    let lower_s = s.to_lowercase();
    lower_s == lower_s.chars().rev().collect::<String>()
}

#[allow(dead_code)]
pub fn is_pangram(s: &str) -> bool {
    let lower_s = s.to_lowercase();
    ('a'..='z').all(|c| lower_s.contains(c))
}

#[cfg(test)]
mod tests {
    use crate::pangrindrome::{is_palindrome, is_pangram};

    #[test]
    fn basic_is_palindrome() {
        assert_eq!(true, is_palindrome("r"));
        assert_eq!(true, is_palindrome("abba"));
        assert_eq!(true, is_palindrome("abcba"));
        assert_eq!(false, is_palindrome("abc"));
        assert_eq!(false, is_palindrome("I am Ling , ling am i"));
        assert_eq!(true, is_palindrome("I am Ling , gnil ma i"));
    }

    #[test]
    fn basic_pangram() {
        let quick_brown_fox = "The quick brown fox jumps over the lazy Dog";
        assert_eq!(true, is_pangram(quick_brown_fox));
        let quick_prairie_dog = "The quick prairie dog jumps over the lazy fox";
        assert_eq!(false, is_pangram(quick_prairie_dog));
        let test1 = "abcdefghijklmnopqrstuvwxy";
        assert_eq!(false, is_pangram(test1));
        let test2 = "abcdefghijklmnopqrstuvwxyz";
        assert_eq!(true, is_pangram(test2));
    }
}