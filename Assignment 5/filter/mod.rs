mod cwslice;
use rayon::iter::*;

#[allow(dead_code)]
pub fn par_filter<F>(xs: &[i32], p: F) -> Vec<i32>
where 
    F: Fn(i32) -> bool + Send + Sync
{
    //    xs = [2, 5, 8, 6, 7, 4, 9, 1, 16]
    // flags = [0, 1, 0, 0, 1, 0, 1, 1, 0]
    let flags = xs.par_iter()
        .map(|elem| p(*elem) as i32)
        .collect::<Vec<i32>>();

    // plus_scan = [0, 0, 1, 1, 1, 2, 2, 3, 4], 4
    fn plus_scan(xs: &[i32]) -> (Vec<i32>, i32) {
        if xs.is_empty() { return (vec![], 0); }
        let half = xs.len()/2;
        let (c_prefix, mut c_sum) = plus_scan(&(0..half).into_par_iter()
            .map(|i| xs[2*i] + xs[2*i+1])
            .collect::<Vec<i32>>());
        let mut pfs: Vec<i32> = (0..half).into_par_iter()
            .flat_map(|i| vec![c_prefix[i], c_prefix[i]+xs[2*i]])
            .collect();
        if xs.len() % 2 == 1 { pfs.push(c_sum); c_sum += xs[xs.len()-1]; }
        (pfs, c_sum)
    }
    
    //    plus_scan_vec = [0, 0, 1, 1, 1, 2, 2, 3, 4]
    // num_target_index = 4
    let (plus_scan_vec, num_target_index) = plus_scan(&flags);

    let mut output = Vec::with_capacity(num_target_index as usize);
    unsafe { output.set_len(num_target_index as usize) }

    // create an instance of UnsafeSlice pointing to the mutable slice 'output'
    let output_ptr = cwslice::UnsafeSlice::new(output.as_mut_slice());

    plus_scan_vec.par_iter()
        .enumerate()
        .into_par_iter()
        .for_each(|(index, elem)| {
            if flags[index] == 1 {
                // *elem as usize -> used as the index for the write operation [0, 1, 2, 3, 4]
                // xs[index] -> it will be written to the 'output' slice 
                unsafe { output_ptr.write(*elem as usize, xs[index]) }
            }
        });

    // [5, 7, 9, 1]
    output

}


fn is_odd(x: i32) -> bool { x % 2 != 0 }
fn is_even(x: i32) -> bool { x % 2 == 0 }

#[cfg(test)]
mod tests {
    use crate::filter::{is_odd, par_filter, is_even};

    #[test]
    fn par_filter_test() {
        assert_eq!(vec![5, 7, 9, 1], par_filter(&[2, 5, 8, 6, 7, 4, 9, 1, 16], is_odd));
        assert_eq!(vec![2, 8, 6, 4, 16], par_filter(&[2, 5, 8, 6, 7, 4, 9, 1, 16], is_even))
    }
}