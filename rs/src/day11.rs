use std::collections::BTreeMap;
use std::num::ParseIntError;

pub fn part1(data: &str) -> Result<u64, ParseIntError> {
    solve::<25>(data)
}

pub fn part2(data: &str) -> Result<u64, ParseIntError> {
    solve::<75>(data)
}

fn solve<const N: usize>(data: &str) -> Result<u64, ParseIntError> {
    Ok((0..N)
        .fold(
            {
                let mut counts = BTreeMap::<u64, u64>::new();
                for word in data.split_ascii_whitespace() {
                    *counts.entry(word.parse()?).or_default() += 1;
                }
                counts
            },
            |counts, _| {
                let mut next = BTreeMap::new();
                for (num, count) in counts {
                    if num == 0 {
                        *next.entry(1).or_default() += count;
                    } else {
                        let length = num.ilog10() + 1;
                        if length % 2 == 0 {
                            let divisor = 10u64.pow(length / 2);
                            *next.entry(num / divisor).or_default() += count;
                            *next.entry(num % divisor).or_default() += count;
                        } else {
                            *next.entry(2024 * num).or_default() += count;
                        }
                    }
                }
                next
            },
        )
        .into_values()
        .sum())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        0 1 10 99 999
    "};

    static EXAMPLE_2: &str = indoc! {"
        125 17
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Ok(7), solve::<1>(EXAMPLE_1));
        assert_eq!(Ok(22), solve::<6>(EXAMPLE_2));
        assert_eq!(Ok(55312), solve::<25>(EXAMPLE_2));
    }
}
