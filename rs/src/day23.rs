use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

fn parse(data: &str) -> BTreeMap<&str, BTreeSet<&str>> {
    let mut graph = BTreeMap::<_, BTreeSet<_>>::new();
    for line in data.lines() {
        let Some((a, b)) = line.split_once('-') else {
            continue;
        };
        graph.entry(a.min(b)).or_default().insert(a.max(b));
        graph.entry(a.max(b)).or_default();
    }
    graph
}

pub fn part1(data: &str) -> usize {
    let graph = parse(data);
    graph
        .iter()
        .flat_map(|(a, bs)| {
            let graph = &graph;
            bs.iter().flat_map(move |b| {
                graph[b].intersection(bs).filter(|c| {
                    bs.contains(*c)
                        && (a.starts_with('t') || b.starts_with('t') || c.starts_with('t'))
                })
            })
        })
        .count()
}

pub fn part2(data: &str) -> String {
    let graph = parse(data);
    fn max_complete<'a>(
        graph: &'a BTreeMap<&'a str, BTreeSet<&'a str>>,
        used: BTreeSet<&'a str>,
        remaining: BTreeSet<&'a str>,
    ) -> BTreeSet<&'a str> {
        remaining
            .iter()
            .map(|a| {
                let mut used = used.clone();
                used.insert(*a);
                max_complete(
                    graph,
                    used,
                    remaining.intersection(&graph[a]).copied().collect(),
                )
            })
            .max_by_key(BTreeSet::len)
            .unwrap_or(used)
    }
    graph
        .par_iter()
        .map(|(a, remaining)| max_complete(&graph, [*a].into(), remaining.clone()))
        .max_by_key(BTreeSet::len)
        .map_or_else(String::new, |set| set.into_iter().sorted().join(","))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        kh-tc
        qp-kh
        de-cg
        ka-co
        yn-aq
        qp-ub
        cg-tb
        vc-aq
        tb-ka
        wh-tc
        yn-cg
        kh-ub
        ta-co
        de-co
        tc-td
        tb-wq
        wh-td
        ta-ka
        td-qp
        aq-cg
        wq-ub
        ub-vc
        de-ta
        wq-aq
        wq-vc
        wh-yn
        ka-de
        kh-ta
        co-tc
        wh-qp
        tb-vc
        td-yn
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(7, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!("co,de,ka,ta".to_string(), part2(EXAMPLE));
    }
}
