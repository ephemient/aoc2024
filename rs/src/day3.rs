use std::iter::successors;

pub fn part1(data: &str) -> i32 {
    data.split("mul(")
        .skip(1)
        .filter_map(|data| {
            let (a, data) = data.split_once(',')?;
            let a = a.parse::<i32>().ok()?;
            let (b, _) = data.split_once(')')?;
            let b = b.parse::<i32>().ok()?;
            Some(a * b)
        })
        .sum()
}

pub fn part2(data: &str) -> i32 {
    fn split_end(data: &str) -> (&str, &str) {
        data.split_once("don't()").unwrap_or((data, ""))
    }
    successors(Some(split_end(data)), |(_, data)| {
        Some(split_end(data.split_once("do()")?.1))
    })
    .map(|(data, _)| part1(data))
    .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE1: &str = indoc! {"
        xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
    "};
    static EXAMPLE2: &str = indoc! {"
        xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(161, part1(EXAMPLE1));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(48, part2(EXAMPLE2));
    }
}
