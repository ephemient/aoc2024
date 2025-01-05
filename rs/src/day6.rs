use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::collections::BTreeSet;
use std::str::FromStr;

#[derive(Clone, Debug)]
struct Data {
    initial_pos: (usize, usize),
    max_bounds: (usize, usize),
    obstacles: BTreeSet<(usize, usize)>,
}

impl FromStr for Data {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut initial_pos = None;
        let mut max_bounds = (0, 0);
        let mut obstacles = BTreeSet::new();
        for (y, line) in value.lines().enumerate() {
            max_bounds = (y + 1, max_bounds.1.max(line.len()));
            for (x, b) in line.bytes().enumerate() {
                match b {
                    b'^' => match initial_pos {
                        None => initial_pos = Some((y, x)),
                        Some(_) => return Err(()),
                    },
                    b'#' => {
                        obstacles.insert((y, x));
                    }
                    _ => {}
                }
            }
        }
        Ok(Data {
            initial_pos: initial_pos.ok_or(())?,
            max_bounds,
            obstacles,
        })
    }
}

#[derive(Clone)]
struct Visitor<'a> {
    state: Option<((usize, usize), (isize, isize))>,
    max_bounds: (usize, usize),
    obstacles: &'a BTreeSet<(usize, usize)>,
}

impl Iterator for Visitor<'_> {
    type Item = ((usize, usize), (isize, isize));

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            Some((pos, dir)) => {
                let state = (*pos, *dir);
                let y = pos.0.wrapping_add_signed(dir.0);
                let x = pos.1.wrapping_add_signed(dir.1);
                if y < self.max_bounds.0 && x < self.max_bounds.1 {
                    if self.obstacles.contains(&(y, x)) {
                        *dir = (dir.1, -dir.0);
                    } else {
                        *pos = (y, x);
                    }
                } else {
                    self.state = None;
                }
                Some(state)
            }
            None => None,
        }
    }
}

impl Data {
    fn iter(&self) -> Visitor {
        Visitor {
            state: Some((self.initial_pos, (-1, 0))),
            max_bounds: self.max_bounds,
            obstacles: &self.obstacles,
        }
    }
}

pub fn part1(data: &str) -> Option<usize> {
    Some(
        data.parse::<Data>()
            .ok()?
            .iter()
            .map(|(pos, _)| pos)
            .collect::<BTreeSet<_>>()
            .len(),
    )
}

pub fn part2(data: &str) -> Option<usize> {
    let data = data.parse::<Data>().ok()?;
    let mut candidates = data.iter().map(|(pos, _)| pos).collect::<BTreeSet<_>>();
    candidates.remove(&data.initial_pos);
    Some(
        candidates
            .into_par_iter()
            .filter(|candidate| {
                let mut data = data.clone();
                assert!(data.obstacles.insert(*candidate));
                let mut last_dy = 0;
                let mut visited = BTreeSet::new();
                !data.iter().all(|(pos, (dy, _))| {
                    let ok = last_dy == -1 || dy != -1 || visited.insert(pos);
                    last_dy = dy;
                    ok
                })
            })
            .count(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        ....#.....
        .........#
        ..........
        ..#.......
        .......#..
        ..........
        .#..^.....
        ........#.
        #.........
        ......#...
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some(41), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(6), part2(EXAMPLE));
    }
}
