use std::collections::VecDeque;
use std::mem::replace;

fn parse(data: &str) -> impl Iterator<Item = ((usize, usize), &str)> + use<'_> {
    data.lines().filter_map(|line| {
        let (x, y) = line.split_once(',')?;
        x.parse().ok().zip(y.parse().ok()).map(|pos| (pos, line))
    })
}

pub fn part1(data: &str) -> Option<usize> {
    part1_helper::<70, 1024>(data)
}

fn part1_helper<const SIZE: usize, const N: usize>(data: &str) -> Option<usize> {
    let mut visited = vec![false; (SIZE + 1) * (SIZE + 1)];
    visited[0] = true;
    for ((x, y), _) in parse(data).take(N) {
        visited[x * (SIZE + 1) + y] = true;
    }
    let mut queue: VecDeque<_> = [((0, 0), 0)].into();
    while let Some(((x, y), t)) = queue.pop_front() {
        if x == SIZE && y == SIZE {
            return Some(t);
        }
        for pos @ (x, y) in [
            (x.wrapping_sub(1), y),
            (x, y.wrapping_sub(1)),
            (x, y + 1),
            (x + 1, y),
        ] {
            if x <= SIZE && y <= SIZE && !visited[x * (SIZE + 1) + y] {
                visited[x * (SIZE + 1) + y] = true;
                queue.push_back((pos, t + 1));
            }
        }
    }
    None
}

pub fn part2(data: &str) -> Option<&str> {
    part2_helper::<70>(data)
}

fn part2_helper<const SIZE: usize>(data: &str) -> Option<&str> {
    let mut obstacles = vec![false; (SIZE + 1) * (SIZE + 1)];
    let candidates = parse(data)
        .filter(|((x, y), _)| {
            obstacles
                .get_mut(x * (SIZE + 1) + y)
                .is_some_and(|seen| !replace(seen, true))
        })
        .collect::<Vec<_>>();
    let mut sets = (0..(SIZE + 1) * (SIZE + 1)).collect::<Vec<_>>();
    fn root(sets: &mut [usize], mut key: usize) -> usize {
        let mut value = sets[key];
        while key != value {
            let next = sets[value];
            sets[key] = next;
            (key, value) = (value, next);
        }
        value
    }
    fn union(sets: &mut [usize], i: usize, j: usize) {
        let i = root(sets, i);
        let j = root(sets, j);
        sets[i] = j;
    }
    for x in 0..=SIZE {
        for y in 0..=SIZE {
            let i = x * (SIZE + 1) + y;
            if obstacles[i] {
                continue;
            }
            for (x, y) in [(x, y + 1), (x + 1, y)] {
                if x <= SIZE && y <= SIZE {
                    let j = x * (SIZE + 1) + y;
                    if !obstacles[j] {
                        union(&mut sets, i, j);
                    }
                }
            }
        }
    }
    candidates
        .into_iter()
        .rev()
        .find(|((x, y), _)| {
            let i = x * (SIZE + 1) + y;
            obstacles[i] = false;
            for (x, y) in [
                (x.wrapping_sub(1), *y),
                (*x, y.wrapping_sub(1)),
                (*x, y + 1),
                (x + 1, *y),
            ] {
                if x <= SIZE && y <= SIZE {
                    let j = x * (SIZE + 1) + y;
                    if !obstacles[j] {
                        union(&mut sets, i, j);
                    }
                }
            }
            root(&mut sets, 0) == root(&mut sets, (SIZE + 1) * (SIZE + 1) - 1)
        })
        .map(|(_, line)| line)
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

    #[test]
    fn part1_examples() {
        assert_eq!(Some(22), part1_helper::<6, 12>(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some("6,1"), part2_helper::<6>(EXAMPLE));
    }
}
