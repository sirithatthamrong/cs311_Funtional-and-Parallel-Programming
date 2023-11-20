use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;

#[allow(dead_code)]
pub fn par_is_prime(n: u64) -> bool {
    let sqrt_n = (n as f64).sqrt() as u64;
    n>1 && (2..=sqrt_n).into_par_iter().all(|k| n%k != 0 )
}

#[allow(dead_code)]
pub fn par_count_primes(n: u32) -> usize {
    (2..=n).into_par_iter().filter(|k| par_is_prime(*k as u64)).count()
}

#[cfg(test)]
mod tests {
    use crate::numprimes::{par_count_primes, par_is_prime};

    #[test]
    fn basic_is_prime() {
        assert_eq!(false, par_is_prime(0));
        assert_eq!(false, par_is_prime(1));
        assert_eq!(true, par_is_prime(2));
        assert_eq!(true, par_is_prime(3));
        assert_eq!(false, par_is_prime(6));
        assert_eq!(false, par_is_prime(25));
        assert_eq!(true, par_is_prime(41));
    }
    #[test]
    fn basic_count_primes() {
        assert_eq!(0, par_count_primes(1));
        assert_eq!(25, par_count_primes(100));
        assert_eq!(78498, par_count_primes(1_000_000));
    }
}
