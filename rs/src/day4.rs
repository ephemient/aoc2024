use array_util::try_from_fn;
use itertools::izip;

pub fn part1(data: &str) -> usize {
    const XMAS: [[u8; 4]; 2] = [*b"XMAS", *b"SAMX"];

    let lines = &data.lines().collect::<Vec<_>>()[..];
    lines
        .iter()
        .enumerate()
        .flat_map(|(y, line)| {
            (0..line.len()).flat_map(move |x| {
                [(1, 0), (1, 1), (0, 1), (-1, 1)]
                    .iter()
                    .filter_map(move |(dx, dy)| {
                        try_from_fn(|i| {
                            lines
                                .get(y + i * dy)
                                .zip(x.checked_add_signed(i as isize * dx))
                                .and_then(|(line, x)| line.as_bytes().get(x).copied())
                        })
                    })
            })
        })
        .filter(|s| XMAS.contains(s))
        .count()
}

pub fn part2(data: &str) -> usize {
    const XMAS: [[u8; 5]; 4] = [*b"MMASS", *b"MSAMS", *b"SMASM", *b"SSAMM"];

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
        })
        .filter(|&(nw, ne, b, sw, se)| XMAS.contains(&[nw, ne, b, sw, se]))
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
