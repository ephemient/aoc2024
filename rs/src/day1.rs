use itertools::Itertools;

fn parse(data: &str) -> (Vec<i32>, Vec<i32>) {
    data.lines()
        .filter_map(|line| {
            let mut iter = line.split_ascii_whitespace();
            let first: i32 = iter.next()?.parse().ok()?;
            let second: i32 = iter.next()?.parse().ok()?;
            Some((first, second))
        })
        .unzip()
}

pub fn part1(data: &str) -> i32 {
    let (mut first, mut second) = parse(data);
    first.sort();
    second.sort();
    first
        .into_iter()
        .zip(second.iter())
        .map(|(x, y)| (x - y).abs())
        .sum()
}

pub fn part2(data: &str) -> i32 {
    let (first, second) = parse(data);
    let second = second.into_iter().counts();
    first
        .into_iter()
        .map(|x| x * *second.get(&x).unwrap_or(&0) as i32)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        3   4
        4   3
        2   5
        1   3
        3   9
        3   3
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(11, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(31, part2(EXAMPLE));
    }
}
