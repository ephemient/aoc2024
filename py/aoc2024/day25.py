"""
Day 25: Code Chronicle
"""

SAMPLE_INPUT = """
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
"""


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    3
    """
    keys, locks = [], []
    for group in data.split("\n\n"):
        cols = ["".join(col) for col in zip(*filter(None, group.splitlines()))]
        dst = keys if group.strip().startswith(".") else locks
        dst.append([col.index("#") or col.index(".") for col in cols])
    return sum(all(x >= y for x, y in zip(key, lock)) for key in keys for lock in locks)


parts = (part1,)
