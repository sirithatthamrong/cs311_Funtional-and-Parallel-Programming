use std::char;
use std::collections::HashMap;
use rayon::{iter::{IntoParallelRefIterator, ParallelIterator, IndexedParallelIterator}, str::ParallelString};


fn binary_to_index(binary: &str) -> usize {
        let size: usize = binary.len() - 1;
        binary
            .chars()
            .enumerate()
            .fold(0, |acc, (nth, b)| acc + if b == '1' { 2_i32.pow((size - nth) as u32) } else { 0 })
            as usize
    }

#[allow(dead_code)]
pub fn par_encode_base64(bytes: &[u8]) -> String {

    let base64_chars: Vec<char> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".chars().collect();

    let mut unadjusted_binary: String = bytes
        .par_iter()
        .map(|byte| format!("{:08b}", byte))
        .collect();

    let padding = match unadjusted_binary.len() % 6 {
        n if n != 0 => {
            unadjusted_binary += &("0".repeat(6-n));
            // 00 = , 0000 ==
            "=".repeat((6-n)/2)
        },
        _ => String::new()
    };

    let adjusted_binary: Vec<String> = unadjusted_binary
        .chars()
        .collect::<Vec<char>>()
        .chunks(6)
        .map(|c| c.iter().collect())
        .collect::<Vec<String>>();

    let to_return: String = adjusted_binary
        .par_iter()
        .map(|chunk| base64_chars[binary_to_index(&chunk)])
        .collect();

    to_return + &padding
}


#[allow(dead_code)]
pub fn par_decode_base64(code: &str) -> Option<Vec<u8>> {

    let base64_chars: Vec<char> = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/".chars().collect();

    fn to_binary06 (byte: usize) -> String { format!("{:06b}", byte ) }

    fn cut_end(s: &str) -> String {
        s.chars()
            .rev()
            .skip_while(|&c| c == '=')
            .collect::<String>()
            .chars()
            .rev()
            .collect()
    }

    let removed_padding: String = cut_end(code);
    let valid_check: Vec<char> = removed_padding
        .par_chars()
        .filter(|char| !base64_chars.contains(char))
        .collect();

    if removed_padding.is_empty() || !valid_check.is_empty() || code.len() - removed_padding.len() > 4 { return None; }
    
    let count_added_zero: usize = (code.len() - removed_padding.len()) * 2;

    let matching_with_index: Vec<(char, usize)> = base64_chars
        .par_iter()
        .enumerate()
        .map(|(n, c)| (c.clone(), n))
        .collect();

    let base64: HashMap<char, usize> = matching_with_index.into_iter().collect();

    let mut unadjusted_bi: String = removed_padding
        .par_chars()
        .fold(String::new, |acc, each| {
            acc + &(to_binary06(*base64.get(&each).unwrap()))
        }).collect();

    unadjusted_bi = unadjusted_bi[..unadjusted_bi.len() - count_added_zero].to_string();

    let mut adjusted_bi: Vec<String> = Vec::new();

    unadjusted_bi.par_chars()
        .collect::<Vec<char>>()
        .chunks(8)
        .into_iter()
        .for_each(|c| adjusted_bi.push(c.par_iter().collect()));

    let to_return: Vec<u8> = adjusted_bi
        .par_iter()
        .map(|st| binary_to_index(st) as u8)
        .collect();

    Some(to_return)
   
}


#[cfg(test)]
mod tests {
    use crate::base64::{par_encode_base64, par_decode_base64};

    #[test]
    fn basic_encode() {
        assert_eq!(&"aGVsbG8xNTAxKys9", &par_encode_base64(b"hello1501++="));
        assert_eq!(&"bGlnaHQgd29yaw==", &par_encode_base64(b"light work"));
        assert_eq!(&"IBBnAwJnZw==", &par_encode_base64(b"\x20\x10g\x03\x02gg"));
        assert_eq!(&"bGlnaHQgdw==", &par_encode_base64(b"light w"));
        assert_eq!(&"bGlnaHQgd28=", &par_encode_base64(b"light wo"));
        assert_eq!(&"bGlnaHQgd29yay4=", &par_encode_base64(b"light work."));
    }

    #[test]
    fn basic_decode() {
        assert_eq!(Some(b"light work".to_vec()), par_decode_base64("bGlnaHQgd29yaw=="));
        assert_eq!(Some(b"hello1501++=".to_vec()), par_decode_base64("aGVsbG8xNTAxKys9"));
        assert_eq!(Some(b"\x20\x10g\x03\x02gg".to_vec()), par_decode_base64("IBBnAwJnZw=="));
        assert_eq!(None, par_decode_base64("IBBnAwJnZw-="));
        assert_eq!(None, par_decode_base64("IBBnAw=JnZw="));
        assert_eq!(None, par_decode_base64(""));
        assert_eq!(None, par_decode_base64("Hi Charlie"));
        assert_eq!(None, par_decode_base64("==="));
        assert_eq!(None, par_decode_base64("dfg====="));
    }
}