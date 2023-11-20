use std::collections::HashMap;

#[allow(dead_code)]
pub fn to_roman(n: u16) -> String {
    let roman_numerals = vec![(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1,"I")];   
    let mut ans = String::new();
    let mut remain = n.clone();
    for &(num, c) in roman_numerals.iter() {
        while remain > 0 {
            if num <= remain {
                remain -= num;
                ans.push_str(c);
            } else {
                break;
            }
        }
    }
    ans
}

#[allow(dead_code)]
pub fn parse_roman(roman_number: &str) -> u16 {

    let mut numeral_values = HashMap::new();
    numeral_values.insert('I', 1);
    numeral_values.insert('V', 5);
    numeral_values.insert('X', 10);
    numeral_values.insert('L', 50);
    numeral_values.insert('C', 100);
    numeral_values.insert('D', 500);
    numeral_values.insert('M', 1000);

    let mut ans = 0;
    let mut prev_val = 0;

    for c in roman_number.chars().rev() {
        let val = numeral_values.get(&c).unwrap();
        if *val >= prev_val { ans += val; } 
        else { ans -= val; }
        prev_val = *val
    }

    ans

}

#[cfg(test)]
mod tests {
    use super::{parse_roman, to_roman};

    #[test]
    fn basic_digits() {
        assert_eq!("I", to_roman(1));
        assert_eq!("V", to_roman(5));
        assert_eq!("X", to_roman(10));
        assert_eq!("L", to_roman(50));
        assert_eq!("C", to_roman(100));
    }

    #[test]
    fn basic_mixture() {
        assert_eq!("II", to_roman(2));
        assert_eq!("IV", to_roman(4));
        assert_eq!("IX", to_roman(9));
        assert_eq!("XII", to_roman(12));
        assert_eq!("XIV", to_roman(14));
        assert_eq!("DXLIII", to_roman(543));
        assert_eq!("MCCXXXIV", to_roman(1234));
        assert_eq!("MCMLIV", to_roman(1954));
        assert_eq!("MMMCMXCIX", to_roman(3999))
    }

    #[test]
    fn basic_parsing() {
        assert_eq!(3, parse_roman("III"));
        assert_eq!(4, parse_roman("IV"));
        assert_eq!(8, parse_roman("VIII"));
        assert_eq!(19, parse_roman("XIX"));
        assert_eq!(900, parse_roman("CM"));
        assert_eq!(1994, parse_roman("MCMXCIV"));
        assert_eq!(2022, parse_roman("MMXXII"));
    }
}
