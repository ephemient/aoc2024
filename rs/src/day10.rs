use std::array;
use std::collections::{BTreeMap, BTreeSet};

fn parse(data: &str) -> [BTreeSet<(usize, usize)>; 10] {
    let mut elevations = array::from_fn(|_| BTreeSet::new());
    for (y, line) in data.lines().enumerate() {
        for (x, c) in line.char_indices() {
            if let Some(h) = c.to_digit(10) {
                elevations[h as usize].insert((y, x));
            }
        }
    }
    elevations
}

fn step<T, F>(
    acc: &BTreeMap<(usize, usize), T>,
    points: &BTreeSet<(usize, usize)>,
    plus: F,
) -> BTreeMap<(usize, usize), T>
where
    T: Clone,
    F: Fn(&T, &T) -> T,
{
    let mut ret = BTreeMap::new();
    for ((y, x), value) in acc {
        for (dy, dx) in [(-1, 0), (0, -1), (0, 1), (1, 0)] {
            if let Some(point) = y
                .checked_add_signed(dy)
                .zip(x.checked_add_signed(dx))
                .filter(|point| points.contains(point))
            {
                ret.entry(point)
                    .and_modify(|e: &mut T| *e = plus(value, e))
                    .or_insert_with(|| value.clone());
            }
        }
    }
    ret
}

fn solve<T, Init, Plus>(data: &str, init: Init, plus: Plus) -> impl Iterator<Item = T>
where
    T: Clone,
    Init: Fn((usize, usize)) -> T,
    Plus: Fn(&T, &T) -> T,
{
    let elevations = parse(data);
    elevations[1..]
        .iter()
        .fold(
            elevations[0]
                .iter()
                .map(|point| (*point, init(*point)))
                .collect(),
            |acc, points| step(&acc, points, &plus),
        )
        .into_values()
}

pub fn part1(data: &str) -> usize {
    solve(
        data,
        |point| -> BTreeSet<_> { [point].into() },
        |x, y| x.union(y).copied().collect(),
    )
    .map(|value| value.len())
    .sum()
}

pub fn part2(data: &str) -> usize {
    solve(data, |_| 1, |x, y| x + y).sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        89010123
        78121874
        87430965
        96549874
        45678903
        32019012
        01329801
        10456732
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(36, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(81, part2(EXAMPLE));
    }
}
