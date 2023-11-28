use std::io;
use std::fs;
use chashmap::CHashMap;
use rayon::str::*;
use rayon::iter::*;

#[derive(Debug, PartialEq)]
struct FlightRecord {
    unique_carrier: String,
    actual_elapsed_time: i32,
    arrival_delay: i32,
}

#[allow(dead_code)]
fn parse_line(line: &str) -> Option<FlightRecord> {
    let split_line = par_split(line, ',');
    let flight_record = FlightRecord{
        unique_carrier: String::from(split_line[8]),
        actual_elapsed_time: if split_line[11].parse::<i32>().is_ok() { split_line[11].parse().unwrap() } 
                             else { 0 },
        arrival_delay: if split_line[14].parse::<i32>().is_ok() { split_line[14].parse().unwrap() } 
                       else { 0 },
    };
    Some(flight_record)
}


#[allow(dead_code)]
pub fn ontime_rank(filename: &str) -> Result<Vec<(String, f64)>, io::Error> {
    let file_contents = fs::read_to_string(filename)?;
    let store_data = CHashMap::new();
    let all_lines = file_contents.lines().collect::<Vec<&str>>();

    all_lines.into_par_iter()
        .for_each( |line|{
            // check whether the 'Option' contains 'Some' variant
            if parse_line(line).is_some() {
                let record = parse_line(line).unwrap();
                // on-time flights
                if record.arrival_delay <= 0 {
                    store_data.upsert(record.unique_carrier,
                                            // initial counts = (1, 1)
                                    ||(1, 1),
                                    |(on_time, total)| {
                                                *on_time += 1;
                                                *total += 1;
                                            }
                                    )
                } 
                // delayed flights
                else {
                    store_data.upsert(record.unique_carrier,
                                            // initial counts = (0, 1), don't add on-time
                                    ||(0, 1),
                                    |(_, total)| *total += 1)
                }
            }
        });

    store_data.remove("UniqueCarrier");
    Ok(
        store_data.into_iter()
            .map(|(key, (on_time,total))| (key, on_time as f64 / total as f64))
            .collect()
    )
}


#[allow(dead_code)]
pub fn plus_scan(xs: &[i32]) -> (Vec<i32>, i32) {
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


#[allow(dead_code)]
pub fn par_split<'a>(st_buf: &'a str, split_char: char) -> Vec<&'a str> {
    let flags = st_buf
        .par_chars()
        .into_par_iter()
        .map(|char|{ ( char == split_char) as i32 })
        .collect::<Vec<i32>>();

    let (index, _ ) = plus_scan(&flags);

    let mut to_ret = Vec::new();
    let mut count = 0;
    for i in 1..index.len()-1 {
        if index[i] < index[i+1] {
            let word = &st_buf[count..i];
            to_ret.push(word);
            count = i + 1;
        }
    }
    to_ret.push(&st_buf[count..index.len()]);
    to_ret
}


#[cfg(test)]
mod tests {

    use crate::ontime::par_split;

    #[test]
    fn par_split_test() {
        assert_eq!(vec!["a", "hhh", "ab", "hello", "world", "", "meh"], par_split("a,hhh,ab,hello,world,,meh", ','));
    }

}
