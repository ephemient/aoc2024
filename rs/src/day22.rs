use std::collections::BTreeMap;
use std::iter;

use itertools::Itertools;

fn step(num: u32) -> u32 {
    let num = (num ^ num << 6) & 16777215;
    let num = (num ^ num >> 5) & 16777215;
    (num ^ num << 11) & 16777215
}

pub fn part1(data: &str) -> u64 {
    data.lines()
        .filter_map(|line| iter::successors(line.parse().ok(), |num| Some(step(*num))).nth(2000))
        .map(Into::<u64>::into)
        .sum()
}

pub fn part2(data: &str) -> Option<u32> {
    let mut sequences = BTreeMap::<_, BTreeMap<_, _>>::new();
    for (i, line) in data.lines().enumerate() {
        for (a, b, c, d, e) in iter::successors(line.parse().ok(), |num| Some(step(*num)))
            .take(2001)
            .tuple_windows()
        {
            sequences
                .entry((
                    (a % 10) as i8 - (b % 10) as i8,
                    (b % 10) as i8 - (c % 10) as i8,
                    (c % 10) as i8 - (d % 10) as i8,
                    (d % 10) as i8 - (e % 10) as i8,
                ))
                .or_default()
                .entry(i)
                .or_insert(e % 10);
        }
    }
    sequences
        .into_values()
        .map(|values| values.into_values().sum())
        .max()
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
        assert_eq!(Some(23), part2(EXAMPLE_2));
    }
}
