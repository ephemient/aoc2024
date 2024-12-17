struct Machine {
    button_a: (usize, usize),
    button_b: (usize, usize),
    prize: (usize, usize),
}

fn parse(data: &str) -> impl Iterator<Item = Machine> + use<'_> {
    data.split("\n\n").filter_map(|stanza| {
        let mut iter = stanza.lines();
        let line = iter.next()?.strip_prefix("Button A: ")?;
        let (ax, line) = line.strip_prefix("X+")?.split_once(", ")?;
        let ay = line.strip_prefix("Y+")?;
        let line = iter.next()?.strip_prefix("Button B: ")?;
        let (bx, line) = line.strip_prefix("X+")?.split_once(", ")?;
        let by = line.strip_prefix("Y+")?;
        let line = iter.next()?.strip_prefix("Prize: ")?;
        let (x, line) = line.strip_prefix("X=")?.split_once(", ")?;
        let y = line.strip_prefix("Y=")?;
        Some(Machine {
            button_a: (ax.parse().ok()?, ay.parse().ok()?),
            button_b: (bx.parse().ok()?, by.parse().ok()?),
            prize: (x.parse().ok()?, y.parse().ok()?),
        })
    })
}

fn solve(
    &Machine {
        button_a: (ax, ay),
        button_b: (bx, by),
        prize: (x, y),
    }: &Machine,
) -> Option<usize> {
    let a_numerator = (x * by) as isize - (y * bx) as isize;
    let a_denominator = (ax * by) as isize - (bx * ay) as isize;
    let b_numerator = (x * ay) as isize - (y * ax) as isize;
    let b_denominator = (ay * bx) as isize - (by * ax) as isize;
    if a_numerator.checked_rem(a_denominator)? != 0 || b_numerator.checked_rem(b_denominator)? != 0
    {
        return None;
    }
    let a = a_numerator.checked_div(a_denominator)? as usize;
    let b = b_numerator.checked_div(b_denominator)? as usize;
    Some(3 * a + b)
}

pub fn part1(data: &str) -> usize {
    parse(data).filter_map(|machine| solve(&machine)).sum()
}

pub fn part2(data: &str) -> usize {
    parse(data)
        .filter_map(|machine @ Machine { prize: (x, y), .. }| {
            solve(&Machine {
                prize: (x + 10000000000000, y + 10000000000000),
                ..machine
            })
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        Button A: X+94, Y+34
        Button B: X+22, Y+67
        Prize: X=8400, Y=5400

        Button A: X+26, Y+66
        Button B: X+67, Y+21
        Prize: X=12748, Y=12176

        Button A: X+17, Y+86
        Button B: X+84, Y+37
        Prize: X=7870, Y=6450

        Button A: X+69, Y+23
        Button B: X+27, Y+71
        Prize: X=18641, Y=10279
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(480, part1(EXAMPLE));
    }
}
