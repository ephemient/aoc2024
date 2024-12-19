"""
Day 19: Linen Layout
"""

SAMPLE_INPUT = """\
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
"""


def _count(keys: list[str], target: str) -> int:
    counts = [1]
    for i in range(1, len(target) + 1):
        counts.append(
            sum(counts[i - len(key)] for key in keys if target[:i].endswith(key))
        )
    return counts[-1]


def solve(data: str) -> str:
    """
    >>> print(solve(SAMPLE_INPUT))
    6
    16
    """
    keys, *targets = data.splitlines()
    keys = keys.split(", ")
    part1, part2 = 0, 0
    for target in targets:
        n = _count(keys, target) if target else None
        if n:
            part1 += 1
            part2 += n
    return f"{part1}\n{part2}"


parts = (solve,)
