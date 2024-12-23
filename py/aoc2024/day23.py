"""
Day 23: LAN Party
"""

from collections import defaultdict

SAMPLE_INPUT = """
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
"""


def _parse(data: str) -> dict[str, set[str]]:
    graph = defaultdict(set)
    for line in data.splitlines():
        if "-" not in line:
            continue
        a, b = line.split("-", maxsplit=1)
        graph[min(a, b)].add(max(a, b))
        graph[max(a, b)]
    return graph


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    7
    """
    graph = _parse(data)
    return sum(
        a.startswith("t") or b.startswith("t") or c.startswith("t")
        for a, bs in graph.items()
        for b in bs
        for c in graph[b]
        if c in bs
    )


def part2(data: str) -> str:
    """
    >>> part2(SAMPLE_INPUT)
    'co,de,ka,ta'
    """
    graph = _parse(data)

    def _maxcomplete(used: set[str], remaining: set[str]) -> set[str]:
        return (
            max(
                (
                    _maxcomplete(used | {key}, remaining & graph[key])
                    for key in remaining
                ),
                key=len,
            )
            if remaining
            else used
        )

    return ",".join(sorted(_maxcomplete(set(), set(graph))))


parts = (part1, part2)
