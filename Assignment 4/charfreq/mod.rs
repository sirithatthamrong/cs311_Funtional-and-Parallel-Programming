use chashmap::CHashMap;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::collections::HashMap;

pub fn par_char_freq(chars: &[u8]) -> HashMap<u8, u32> {
    let histo: CHashMap<u8, u32> = CHashMap::new();
    chars.par_iter().for_each(|c| histo.upsert(*c, || 1, |v| *v += 1));
    histo.into_iter().collect::<HashMap<u8, u32>>()
}


#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use crate::charfreq::par_char_freq;
    #[test]
    fn basic_tests() {
        let map1:HashMap<u8, u32> = [(98,1), (110,2), (97,3)].into();
        assert_eq!(map1,par_char_freq("banana".as_bytes()));
        let map2:HashMap<u8, u32> = [(115,4), (109,1), (105,4), (112,2)].into();
        assert_eq!(map2,par_char_freq("mississippi".as_bytes()));
    }
}
