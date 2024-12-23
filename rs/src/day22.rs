use std::iter;
use std::sync::atomic::{AtomicU32, Ordering};

use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

fn step(num: u32) -> u32 {
    let num = (num ^ num << 6) & 16777215;
    let num = (num ^ num >> 5) & 16777215;
    (num ^ num << 11) & 16777215
}

pub fn part1(data: &str) -> u64 {
    data.lines()
        .collect::<Vec<_>>()
        .par_iter()
        .filter_map(|line| iter::successors(line.parse().ok(), |num| Some(step(*num))).nth(2000))
        .map(Into::<u64>::into)
        .sum()
}

pub fn part2(data: &str) -> u32 {
    let results = &[const { AtomicU32::new(0) }; 19 * 19 * 19 * 19];
    data.lines()
        .collect::<Vec<_>>()
        .par_iter()
        .filter_map(|line| {
            let mut seen = [false; 19 * 19 * 19 * 19];
            iter::successors(line.parse().ok(), |num| Some(step(*num)))
                .take(2001)
                .map(|num| num % 10)
                .tuple_windows()
                .filter_map(|(a, b, c, d, e)| {
                    let key = ((((9 + a - b) * 19 + 9 + b - c) * 19 + 9 + c - d) * 19 + 9 + d - e)
                        as usize;
                    if !seen[key] {
                        seen[key] = true;
                        Some(results[key].fetch_add(e, Ordering::AcqRel) + e)
                    } else {
                        None
                    }
                })
                .max()
        })
        .max()
        .unwrap_or_default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        1
        10
        100
        2024
    "};

    static EXAMPLE_2: &str = indoc! {"
        1
        2
        3
        2024
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(37327623, part1(EXAMPLE_1));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(23, part2(EXAMPLE_2));
    }
}
