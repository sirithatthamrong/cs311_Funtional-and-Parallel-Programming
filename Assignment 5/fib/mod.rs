use num_bigint::ToBigUint;
use rayon::iter::*;

#[allow(dead_code)]
/**
 * Because 'par_fib_seq' using 'plus_scan'
 * So, W(n) = W(n/2) + O(n) and S(n) =  S(n/2) + O(logn) = O(log^2 n)
 */
pub fn par_fib_seq(n: u32) -> Vec<num_bigint::BigUint> {
    match n {
        0 => return Vec::new(),
        1 => return vec![1.to_biguint().unwrap()],
        _ => {
            let initial = vec![(1,1,1,0); (n - 1) as usize];
            let (mut matrix, _) = plus_scan(&initial);
            matrix.push((1,1,1,0));
            let mut to_ret:Vec<num_bigint::BigUint> = matrix.into_iter()
                .map(|x| x.0.to_biguint().unwrap())
                .collect();
            to_ret.sort();
            return to_ret;

        }
    }

    fn plus_scan(xs: &Vec<(i32,i32,i32,i32)>) -> (Vec<(i32,i32,i32,i32)>,(i32,i32,i32,i32)) {
        if xs.is_empty() { return (vec![],(1,1,1,0)); }
        let half = xs.len()/2;
        let contracted = &(0..half).into_par_iter()
            .map(|i| multiply(xs[2*i],xs[2*i+1] ))
            .collect::<Vec<(i32,i32,i32,i32)>>();
        let (c_prefix, mut c_sum) = plus_scan(contracted);
    
        let mut pfs: Vec<(i32,i32,i32,i32)> = (0..half).into_par_iter()
            .flat_map(|i| vec![c_prefix[i], multiply(c_prefix[i], xs[2*i])]).collect();
    
        if xs.len()%2 == 1 {
            pfs.push(c_sum);  // O(1)
            c_sum = multiply(c_sum.clone(), xs[xs.len()-1]);
        }
        (pfs,c_sum)
    }

    fn multiply(first:(i32,i32,i32,i32), second: (i32, i32, i32, i32)) -> (i32, i32, i32, i32) {
        let top_left = (first.0 * second.0) + (first.1 * second.2);
        let bottom_left = (first.2 * second.0) + (first.3 * second.2);
        let top_right = (first.0 * second.1) + (first.1 * second.3);
        let bottom_right = (first.2 * second.1) + (first.3 * second.3);
        (top_left, top_right, bottom_left, bottom_right)
    }

}

#[cfg(test)]
mod tests {
    use num_bigint::ToBigUint;
    use crate::fib::par_fib_seq;

    #[test]
    fn par_fib_seq_test() {
        assert_eq!(vec![1.to_biguint().unwrap(),
                        1.to_biguint().unwrap(),
                        2.to_biguint().unwrap(),
                        3.to_biguint().unwrap(),
                        5.to_biguint().unwrap(),
                        8.to_biguint().unwrap(),
                        13.to_biguint().unwrap(),
                        21.to_biguint().unwrap()], par_fib_seq(8));
    }
}