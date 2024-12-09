fn solve<F, I>(data: &str, op: F) -> usize
where
    F: Fn(usize, usize) -> I,
    I: IntoIterator<Item = usize>,
{
    data.lines()
        .filter_map(|line| {
            let (lhs, rhs) = line.split_once(": ")?;
            let lhs = lhs.parse::<usize>().ok()?;
            let rhs = rhs
                .split_ascii_whitespace()
                .map(|value| value.parse())
                .collect::<Result<Vec<_>, _>>()
                .ok()?;
            let mut stack = vec![(lhs, rhs)];
            while let Some((x, mut rest)) = stack.pop() {
                let y = rest.pop()?;
                if rest.is_empty() {
                    if x == y {
                        return Some(lhs);
                    }
                    continue;
                }
                for z in op(x, y) {
                    stack.push((z, rest.clone()));
                }
            }
            None
        })
        .sum()
}

pub fn part1(data: &str) -> usize {
    solve(data, |x, y| {
        let mut values = vec![];
        if let Some(z) = x.checked_sub(y) {
            values.push(z);
        }
        if x % y == 0 {
            values.push(x / y);
        }
        values
    })
}

pub fn part2(data: &str) -> usize {
    solve(data, |x, y| {
        let mut values = vec![];
        if let Some(z) = x.checked_sub(y) {
            values.push(z);
        }
        if x % y == 0 {
            values.push(x / y);
        }
        if x > y {
            let d = 10usize.pow(y.checked_ilog10().unwrap_or(0) + 1);
            if x % d == y {
                values.push(x / d);
            }
        }
        values
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        190: 10 19
        3267: 81 40 27
        83: 17 5
        156: 15 6
        7290: 6 8 6 15
        161011: 16 10 13
        192: 17 8 14
        21037: 9 7 18 13
        292: 11 6 16 20
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(3749, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(11387, part2(EXAMPLE));
    }
}
