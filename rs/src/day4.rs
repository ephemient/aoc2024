use itertools::izip;

pub fn part1(data: &str) -> usize {
    let lines = &data.lines().collect::<Vec<_>>()[..];
    lines
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            (0..line.len()).flat_map(move |x| {
                (-1..=1).flat_map(move |dy| {
                    (-1..=1).filter(move |dx| {
                        "XMAS".bytes().enumerate().all(|(i, b)| {
                            y.checked_add_signed(i as isize * dy)
                                .and_then(|y| lines.get(y))
                                .zip(x.checked_add_signed(i as isize * dx))
                                .and_then(|(line, x)| line.as_bytes().get(x))
                                == Some(&b)
                        })
                    })
                })
            })
        })
        .count()
}

pub fn part2(data: &str) -> usize {
    let lines = data.lines().collect::<Vec<_>>();
    izip!(&lines[..], &lines[1..], &lines[2..])
        .flat_map(|(above, line, below)| {
            izip!(
                above.bytes(),
                above.bytes().skip(2),
                line.bytes().skip(1),
                below.bytes(),
                below.bytes().skip(2)
            )
            .filter(|&(nw, ne, b, sw, se)| {
                b == b'A'
                    && (nw == b'M' && se == b'S' || se == b'M' && nw == b'S')
                    && (ne == b'M' && sw == b'S' || sw == b'M' && ne == b'S')
            })
        })
        .count()
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
