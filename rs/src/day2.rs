fn parse(data: &str) -> Vec<Vec<i32>> {
    data.lines()
        .flat_map(|line| {
            line.split_ascii_whitespace()
                .map(|level| level.parse::<i32>().ok())
                .collect::<Option<Vec<_>>>()
        })
        .collect::<Vec<_>>()
}

fn is_safe_1(report: &[i32]) -> bool {
    let mut decreasing = false;
    let mut increasing = false;
    report
        .iter()
        .zip(report.iter().skip(1))
        .all(|(x, y)| match x - y {
            -3..=-1 => {
                decreasing = true;
                !increasing
            }
            1..=3 => {
                increasing = true;
                !decreasing
            }
            _ => false,
        })
}

fn is_safe_2(report: &[i32]) -> bool {
    if report.is_empty() {
        return true;
    }
    let mut report2 = vec![0; report.len() - 1];
    report2.copy_from_slice(&report[1..]);
    is_safe_1(&report2)
        || (0..report2.len()).any(|i| {
            report2[i] = report[i];
            is_safe_1(&report2)
        })
}

pub fn part1(data: &str) -> usize {
    parse(data)
        .into_iter()
        .filter(|report| is_safe_1(report))
        .count()
}

pub fn part2(data: &str) -> usize {
    parse(data)
        .into_iter()
        .filter(|report| is_safe_2(report))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        7 6 4 2 1
        1 2 7 8 9
        9 7 6 2 1
        1 3 2 4 5
        8 6 4 4 1
        1 3 6 7 9
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(2, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(4, part2(EXAMPLE));
    }
}
