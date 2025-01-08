use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

fn count(keys: &[&str], target: &str) -> usize {
    let mut counts = Vec::with_capacity(target.len());
    (1..=target.len()).fold(1, |n, i| {
        counts.push(n);
        debug_assert_eq!(i, counts.len());
        keys.iter()
            .filter_map(|key| {
                if target.as_bytes()[..i].ends_with(key.as_bytes()) {
                    Some(counts[i - key.len()])
                } else {
                    None
                }
            })
            .sum()
    })
}

pub fn solve(data: &str) -> Option<(usize, usize)> {
    let mut iter = data.lines();
    let keys = iter.next()?.split(", ").collect::<Vec<_>>();
    if keys.iter().any(|key| key.is_empty()) {
        return None;
    }
    iter.collect::<Vec<_>>()
        .par_iter()
        .map(|target| {
            if target.is_empty() {
                return (0, 0);
            }
            let count = count(&keys[..], target);
            (if count != 0 { 1 } else { 0 }, count)
        })
        .reduce_with(|(a, b), (c, d)| (a + c, b + d))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        r, wr, b, g, bwu, rb, gb, br

        brwrr
        bggr
        gbbr
        rrbgbr
        ubwu
        bwurrg
        brgr
        bbrgwb
    "};

    #[test]
    fn examples() {
        assert_eq!(Some((6, 16)), solve(EXAMPLE));
    }
}
