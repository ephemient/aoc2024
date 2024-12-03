use if_chain::if_chain;

pub fn part1(data: &str) -> i32 {
    const PREFIX: &str = "mul(";

    let mut data = data;
    let mut total = 0;
    while let Some(offset) = data.find(PREFIX) {
        data = data[offset..].strip_prefix(PREFIX).unwrap();
        if_chain! {
            if let Some((a, b)) = data.split_once(',');
            if let Ok(a) = a.parse::<i32>();
            if let Some((b, c)) = b.split_once(')');
            if let Ok(b) = b.parse::<i32>();
            then {
                data = c;
                total += a * b;
            }
        }
    }
    total
}

pub fn part2(data: &str) -> i32 {
    let mut data = data;
    let mut total = 0;
    while !data.is_empty() {
        let end = data.find("don't()").unwrap_or(data.len());
        total += part1(&data[..end]);
        let Some(start) = data[end..].find("do()") else {
            break;
        };
        data = &data[end + start..];
    }
    total
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
