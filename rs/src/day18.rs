use std::collections::{BTreeSet, VecDeque};

fn parse(data: &str) -> impl Iterator<Item = (usize, usize)> + use<'_> {
    data.lines().filter_map(|line| {
        let (x, y) = line.split_once(',')?;
        x.parse().ok().zip(y.parse().ok())
    })
}

pub struct Day18<const SIZE: usize, const N: usize> {}

pub type Default = Day18<70, 1024>;

impl<const SIZE: usize, const N: usize> Day18<SIZE, N> {
    fn find_path(obstacles: &[(usize, usize)]) -> Option<Vec<(usize, usize)>> {
        let mut visited = obstacles.iter().copied().collect::<BTreeSet<_>>();
        let mut queue: VecDeque<_> = [((0, 0), vec![])].into();
        while let Some((pos @ (x, y), mut path)) = queue.pop_front() {
            if !visited.insert(pos) {
                continue;
            }
            path.push(pos);
            if x == SIZE && y == SIZE {
                return Some(path);
            }
            if let Some(x) = x.checked_sub(1) {
                queue.push_back(((x, y), path.clone()));
            }
            if let Some(y) = y.checked_sub(1) {
                queue.push_back(((x, y), path.clone()));
            }
            if x < SIZE {
                queue.push_back(((x + 1, y), path.clone()));
            }
            if y < SIZE {
                queue.push_back(((x, y + 1), path.clone()));
            }
        }
        None
    }

    pub fn part1(data: &str) -> Option<usize> {
        Some(Self::find_path(&parse(data).take(N).collect::<Vec<_>>())?.len() - 1)
    }

    pub fn part2(data: &str) -> Option<String> {
        let obstacles = parse(data).collect::<Vec<_>>();
        let mut i = 0;
        while i < obstacles.len() {
            let Some(path) = Self::find_path(&obstacles[..i + 1]) else {
                let (x, y) = obstacles[i];
                return Some(format!("{},{}", x, y));
            };
            let path = path.into_iter().collect::<BTreeSet<_>>();
            i = obstacles
                .iter()
                .enumerate()
                .skip(i)
                .find(|(_, pos)| path.contains(pos))?
                .0;
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        5,4
        4,2
        4,5
        3,0
        2,1
        6,3
        2,4
        1,5
        0,6
        3,3
        2,6
        5,1
        1,2
        5,5
        2,5
        6,5
        1,4
        0,4
        6,4
        1,1
        6,1
        1,0
        0,5
        1,6
        2,0
    "};

    type Test = Day18<6, 12>;

    #[test]
    fn part1_examples() {
        assert_eq!(Some(22), Test::part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some("6,1".to_string()), Test::part2(EXAMPLE));
    }
}
