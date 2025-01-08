use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::collections::BTreeSet;
use std::str::FromStr;

#[derive(Clone, Debug)]
struct Data {
    initial_pos: (usize, usize),
    max_bounds: (usize, usize),
    obstacles: Vec<bool>,
}

impl FromStr for Data {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let lines = value.lines().collect::<Vec<_>>();
        let mut initial_pos = None;
        let max_bounds = (
            lines.len(),
            lines.iter().map(|line| line.len()).max().unwrap_or(0),
        );
        let mut obstacles = vec![false; max_bounds.0 * max_bounds.1];
        for (y, line) in value.lines().enumerate() {
            for (x, b) in line.bytes().enumerate() {
                match b {
                    b'^' => {
                        if initial_pos.replace((y, x)).is_some() {
                            return Err(());
                        }
                    }
                    b'#' => {
                        obstacles[y * max_bounds.1 + x] = true;
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
struct Visitor<F> {
    state: Option<((usize, usize), (isize, isize))>,
    max_bounds: (usize, usize),
    is_obstacle: F,
}

impl<F> Iterator for Visitor<F>
where
    F: Fn(usize, usize) -> bool,
{
    type Item = ((usize, usize), (isize, isize));

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.state {
            Some((pos, dir)) => {
                let state = (*pos, *dir);
                let y = pos.0.wrapping_add_signed(dir.0);
                let x = pos.1.wrapping_add_signed(dir.1);
                if y < self.max_bounds.0 && x < self.max_bounds.1 {
                    if (self.is_obstacle)(y, x) {
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

pub fn part1(data: &str) -> Option<usize> {
    let data = data.parse::<Data>().ok()?;
    Some(
        Visitor {
            state: Some((data.initial_pos, (-1, 0))),
            max_bounds: data.max_bounds,
            is_obstacle: |y, x| data.obstacles[y * data.max_bounds.1 + x],
        }
        .map(|(pos, _)| pos)
        .collect::<BTreeSet<_>>()
        .len(),
    )
}

pub fn part2(data: &str) -> Option<usize> {
    let data = data.parse::<Data>().ok()?;
    let mut candidates = Visitor {
        state: Some((data.initial_pos, (-1, 0))),
        max_bounds: data.max_bounds,
        is_obstacle: |y, x| data.obstacles[y * data.max_bounds.1 + x],
    }
    .map(|(pos, _)| pos)
    .collect::<BTreeSet<_>>();
    candidates.remove(&data.initial_pos);
    Some(
        candidates
            .into_par_iter()
            .filter(|candidate| {
                let mut last_dy = 0;
                let mut visited = BTreeSet::new();
                !Visitor {
                    state: Some((data.initial_pos, (-1, 0))),
                    max_bounds: data.max_bounds,
                    is_obstacle: |y, x| {
                        (y, x) == *candidate || data.obstacles[y * data.max_bounds.1 + x]
                    },
                }
                .all(|(pos, (dy, _))| {
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
