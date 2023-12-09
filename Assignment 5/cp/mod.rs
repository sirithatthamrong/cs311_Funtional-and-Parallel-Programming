use rayon::{iter::*, slice::ParallelSliceMut};
use std::cmp::min;


#[allow(dead_code)]
pub fn par_closest_distance(points: &[(i32, i32)]) -> i64 {

    fn distance(point_a: (i32, i32), point_b: (i32, i32)) -> i64 {
        let dis = (point_a.0 as i64 - point_b.0 as i64).pow(2) + (point_a.1 as i64 - point_b.1 as i64).pow(2);
        dis
    }

    fn cp_helper(xs: &[(i32, i32)]) -> (i64, Vec<(i32,i32)>) {
        let xs_length = xs.len();

        if xs_length <= 3 {
            let mut min_distance =  std::i64::MAX; // default value
            for i in 0..xs_length {
                for j in i + 1..xs_length {
                    let current_distance = distance(xs[i], xs[j]);
                    min_distance = min(min_distance, current_distance);
                }
            }
            let mut sorted = xs.to_vec();
            sorted.sort_by_key(|elem| elem.1);
            return (min_distance, sorted);
        }

        else {
            let (left, right) = xs.split_at(xs_length / 2);
            let ((left_min, left_sorted), (right_min, right_sorted)) =
            rayon::join(|| cp_helper(left), || cp_helper(right));

            let mut sorted = Vec::with_capacity(xs_length);
            unsafe { sorted.set_len(xs_length); }

            sorted = [left_sorted, right_sorted].concat();

            let d = min(left_min, right_min);

            let band: Vec<(i32, i32)> = sorted
                .par_iter()
                .filter(|(x, _y)| ((xs[xs_length/2].0 - x).abs() as i64) < d)
                .map(|t| *t)
                .collect();

            let dd = (0..band.len())
                .into_par_iter()
                .map(|i| {
                    let bound = 
                        if i + 7 > band.len() { band.len() } 
                        else { i + 7 };
                    (i + 1..bound).into_par_iter()
                        .map(|j| distance(band[j], band[i]))
                        .min().unwrap_or(i64::max_value())
            }).reduce(|| d, |x, y| i64::min(x, y));

            return (min(d, dd), sorted);
        }
    }

    let mut sorted_vec = points.par_iter()
        .map(|elem| *elem)
        .collect::<Vec<(i32, i32)>>();

    sorted_vec.par_sort_unstable_by_key(|elem| elem.0);

    let (min_distance, _) = cp_helper(&sorted_vec);
    min_distance
}
