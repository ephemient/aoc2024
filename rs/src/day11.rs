use std::collections::BTreeMap;
use std::num::ParseIntError;

pub fn part1(data: &str) -> Result<u64, ParseIntError> {
    solve(data, 25)
}

pub fn part2(data: &str) -> Result<u64, ParseIntError> {
    solve(data, 75)
}

fn solve(data: &str, n: usize) -> Result<u64, ParseIntError> {
    Ok((0..n)
        .fold(
            {
                let mut counts = BTreeMap::new();
                for word in data.split_ascii_whitespace() {
                    counts
                        .entry(word.parse::<u64>()?)
                        .and_modify(|e| *e += 1)
                        .or_insert(1u64);
                }
                counts
            },
            |counts, _| {
                let mut next = BTreeMap::new();
                for (num, count) in counts {
                    if num == 0 {
                        next.entry(1).and_modify(|e| *e += count).or_insert(count);
                    } else {
                        let length = num.ilog10() + 1;
                        if length % 2 == 0 {
                            let divisor = 10u64.pow(length / 2);
                            next.entry(num / divisor)
                                .and_modify(|e| *e += count)
                                .or_insert(count);
                            next.entry(num % divisor)
                                .and_modify(|e| *e += count)
                                .or_insert(count);
                        } else {
                            next.entry(2024 * num)
                                .and_modify(|e| *e += count)
                                .or_insert(count);
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
        assert_eq!(Ok(7), solve(EXAMPLE_1, 1));
        assert_eq!(Ok(22), solve(EXAMPLE_2, 6));
        assert_eq!(Ok(55312), solve(EXAMPLE_2, 25));
    }
}
