#[allow(dead_code)]
/**
 * W(n) = 2*W(n/2) + O(1) = O(n)
 * S(n) = max{S(n/2), S(n/2)} + O(1) = O(log n)
 */
pub fn par_lin_search<T: Eq + Sync>(xs: &[T], k: &T) -> Option<usize> {
    if xs.len() == 1 {
        if xs[0] == *k { return Some(0); }
        else { return None; }
    }
    let half = xs.len()/2;
    let (lr, rr) = rayon::join(
        || par_lin_search( &xs[0..half], k), 
        || par_lin_search(&xs[half..half*2], k));
    match (lr, rr) {
        (Some(lr_n), None) => Some(lr_n),
        (None, Some(rr_n)) => Some(half + rr_n),
        (Some(lr_n), Some(rr_n)) => {
            if lr_n <= half + rr_n { Some(lr_n)}
            else { Some(half + rr_n) }
        },
        (None, None) => None
    }
    
}

#[cfg(test)]
mod tests {
    use crate::linsearch::par_lin_search;

    #[test]
    fn basic_lin_search() {
        let xs = vec![3, 1, 4, 2, 7, 3, 1, 9];
        assert_eq!(par_lin_search(&xs, &3), Some(0));
        assert_eq!(par_lin_search(&xs, &5), None);
        assert_eq!(par_lin_search(&xs, &1), Some(1));
        assert_eq!(par_lin_search(&xs, &2), Some(3));
    }
}
