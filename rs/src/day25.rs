use itertools::Itertools;

pub fn part1(data: &str) -> usize {
    let data = data
        .split("\n\n")
        .filter_map(|group| {
            let mut lines = group.lines();
            let line1 = lines.next()?.as_bytes();
            let mut counts = line1.iter().map(|b| (*b, 1)).collect::<Vec<_>>();
            for line in lines {
                for (i, b) in line.bytes().enumerate() {
                    match counts.get_mut(i) {
                        Some((c, n)) => {
                            if b == *c {
                                *n += 1
                            }
                        }
                        None => counts.push((b, 1)),
                    }
                }
            }
            Some(counts)
        })
        .collect::<Vec<_>>();
    data.iter()
        .cartesian_product(data.iter())
        .filter(|(key, lock)| {
            key.len() == lock.len()
                && key
                    .iter()
                    .zip(lock.iter())
                    .all(|((b, i), (c, j))| b > c && i >= j)
        })
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        #####
        .####
        .####
        .####
        .#.#.
        .#...
        .....

        #####
        ##.##
        .#.##
        ...##
        ...#.
        ...#.
        .....

        .....
        #....
        #....
        #...#
        #.#.#
        #.###
        #####

        .....
        .....
        #.#..
        ###..
        ###.#
        ###.#
        #####

        .....
        .....
        .....
        #....
        #.#..
        #.#.#
        #####
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(3, part1(EXAMPLE));
    }
}
