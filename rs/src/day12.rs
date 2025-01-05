use std::collections::{BTreeMap, BTreeSet};

use if_chain::if_chain;
use itertools::Itertools;

struct Data<'a> {
    lines: Vec<&'a str>,
}

impl Data<'_> {
    fn new(data: &str) -> Data {
        Data {
            lines: data.lines().collect(),
        }
    }

    fn groups(&self) -> impl Iterator<Item = BTreeSet<(usize, usize)>> + use<'_> {
        self.lines
            .iter()
            .enumerate()
            .flat_map(|(y, line)| line.bytes().enumerate().map(move |(x, b)| ((y, x), b)))
            .scan(BTreeSet::new(), |visited, (pos, b)| {
                if !visited.insert(pos) {
                    return Some(None);
                }
                let mut group = BTreeSet::new();
                let mut stack = vec![pos];
                while let Some(pos @ (y, x)) = stack.pop() {
                    group.insert(pos);
                    for (dy, dx) in [(-1, 0), (0, -1), (0, 1), (1, 0)] {
                        let pos @ (y, x) = (y.wrapping_add_signed(dy), x.wrapping_add_signed(dx));
                        if_chain! {
                            if let Some(line) = self.lines.get(y);
                            if line.as_bytes().get(x) == Some(&b);
                            if visited.insert(pos);
                            then {
                                stack.push(pos);
                            }
                        }
                    }
                }
                Some(Some(group))
            })
            .flatten()
    }
}

fn perimeter1(group: &BTreeSet<(usize, usize)>) -> usize {
    let mut edges = BTreeMap::<_, u8>::new();
    for (y, x) in group.iter() {
        for edge in [
            (2 * y, 2 * x + 1),
            (2 * y + 1, 2 * x),
            (2 * y + 1, 2 * x + 2),
            (2 * y + 2, 2 * x + 1),
        ] {
            *edges.entry(edge).or_default() += 1;
        }
    }
    edges.into_values().filter(|value| *value == 1).count()
}

pub fn part1(data: &str) -> usize {
    Data::new(data)
        .groups()
        .map(|group| group.len() * perimeter1(&group))
        .sum()
}

fn perimeter2(group: &BTreeSet<(usize, usize)>) -> usize {
    let mut edges = BTreeMap::<_, u8>::new();
    for (y, x) in group.iter() {
        *edges.entry((2 * y, 2 * x + 1)).or_default() |= 1;
        *edges.entry((2 * y + 1, 2 * x)).or_default() |= 2;
        *edges.entry((2 * y + 1, 2 * x + 2)).or_default() |= 8;
        *edges.entry((2 * y + 2, 2 * x + 1)).or_default() |= 4;
    }
    let mut lines = BTreeMap::<_, BTreeSet<_>>::new();
    for ((y, x), n) in edges {
        if n & (n - 1) == 0 {
            lines
                .entry(if n.ilog2() % 2 == 0 { y + 1 } else { x })
                .or_default()
                .insert((x + y, n));
        }
    }
    lines
        .into_values()
        .map(|line| {
            line.into_iter()
                .tuple_windows()
                .filter(|((p, b), (q, d))| p.abs_diff(*q) > 2 || b != d)
                .count()
                + 1
        })
        .sum()
}

pub fn part2(data: &str) -> usize {
    Data::new(data)
        .groups()
        .map(|group| group.len() * perimeter2(&group))
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        AAAA
        BBCD
        BBCC
        EEEC
    "};

    static EXAMPLE_2: &str = indoc! {"
        OOOOO
        OXOXO
        OOOOO
        OXOXO
        OOOOO
    "};

    static EXAMPLE_3: &str = indoc! {"
        RRRRIICCFF
        RRRRIICCCF
        VVRRRCCFFF
        VVRCCCJFFF
        VVVVCJJCFE
        VVIVCCJJEE
        VVIIICJJEE
        MIIIIIJJEE
        MIIISIJEEE
        MMMISSJEEE
    "};

    static EXAMPLE_4: &str = indoc! {"
        EEEEE
        EXXXX
        EEEEE
        EXXXX
        EEEEE
    "};

    static EXAMPLE_5: &str = indoc! {"
        AAAAAA
        AAABBA
        AAABBA
        ABBAAA
        ABBAAA
        AAAAAA
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(140, part1(EXAMPLE_1));
        assert_eq!(772, part1(EXAMPLE_2));
        assert_eq!(1930, part1(EXAMPLE_3));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(80, part2(EXAMPLE_1));
        assert_eq!(436, part2(EXAMPLE_2));
        assert_eq!(236, part2(EXAMPLE_4));
        assert_eq!(368, part2(EXAMPLE_5));
        assert_eq!(1206, part2(EXAMPLE_3));
    }
}
