use itertools::izip;

pub fn part1(data: &str) -> usize {
    let lines = data.lines().collect::<Vec<_>>();
    let mut result = 0;
    for (y, line) in lines.iter().enumerate() {
        for x in 0..line.len() {
            for dy in -1..=1 {
                for dx in -1..=1 {
                    if "XMAS".bytes().enumerate().all(|(i, b)| {
                        y.checked_add_signed(i as isize * dy)
                            .and_then(|y| lines.get(y))
                            .and_then(|line| {
                                x.checked_add_signed(i as isize * dx)
                                    .and_then(|x| line.as_bytes().get(x))
                            })
                            == Some(&b)
                    }) {
                        result += 1;
                    }
                }
            }
        }
    }
    result
}

pub fn part2(data: &str) -> usize {
    let lines = data.lines().collect::<Vec<_>>();
    let mut result = 0;
    for (above, line, below) in izip!(&lines[..], &lines[1..], &lines[2..]) {
        for (nw, ne, b, sw, se) in izip!(
            above.bytes(),
            above.bytes().skip(2),
            line.bytes().skip(1),
            below.bytes(),
            below.bytes().skip(2)
        ) {
            if b == b'A'
                && (nw == b'M' && se == b'S' || se == b'M' && nw == b'S')
                && (ne == b'M' && sw == b'S' || sw == b'M' && ne == b'S')
            {
                result += 1;
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        MMMSXXMASM
        MSAMXMSMSA
        AMXSXMAAMM
        MSAMASMSMX
        XMASAMXAMM
        XXAMMXXAMA
        SMSMSASXSS
        SAXAMASAAA
        MAMMMXMMMM
        MXMXAXMASX
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(18, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(9, part2(EXAMPLE));
    }
}
