use std::collections::BTreeSet;
use std::iter::once;

use itertools::Itertools;

fn solve<I>(data: &str, multiples: I) -> usize
where
    I: Clone + Iterator<Item = isize>,
{
    let (mut height, mut width) = (0, 0);
    data.lines()
        .enumerate()
        .flat_map(|(y, line)| {
            height = y + 1;
            width = width.max(line.len());
            line.bytes().enumerate().filter_map(move |(x, b)| match b {
                b'.' => None,
                _ => Some((b, (y, x))),
            })
        })
        .into_grouping_map_by(|(b, _)| *b)
        .fold_with(
            |_, _| vec![],
            |mut acc, _, (_, point)| {
                acc.push(point);
                acc
            },
        )
        .values()
        .flat_map(|points| {
            points.iter().flat_map(|point0 @ (y0, x0)| {
                points
                    .iter()
                    .filter(move |point1| point0 != *point1)
                    .flat_map(|(y1, x1)| {
                        let (dy, dx) = (*y1 as isize - *y0 as isize, *x1 as isize - *x0 as isize);
                        multiples.clone().map_while(move |i| {
                            Some((
                                y1.wrapping_add_signed(i * dy),
                                x1.wrapping_add_signed(i * dx),
                            ))
                            .filter(|(y, x)| *y < height && *x < width)
                        })
                    })
            })
        })
        .collect::<BTreeSet<_>>()
        .len()
}

pub fn part1(data: &str) -> usize {
    solve(data, once(1))
}

pub fn part2(data: &str) -> usize {
    solve(data, 0..)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        ............
        ........0...
        .....0......
        .......0....
        ....0.......
        ......A.....
        ............
        ............
        ........A...
        .........A..
        ............
        ............
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(14, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(34, part2(EXAMPLE));
    }
}
