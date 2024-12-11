"""
Day 11: Plutonian Pebbles
"""

from collections import Counter


def part1(data: str) -> int:
    return _solve(data, 25)


def part2(data: str) -> int:
    return _solve(data, 75)


def _solve(data: str, n: int) -> int:
    """
    >>> _solve("0 1 10 99 999", 1)
    7
    >>> _solve("125 17", 6)
    22
    >>> _solve("125 17", 25)
    55312
    """
    counts = Counter(map(int, data.split()))
    for i in range(n):
        next = Counter()
        for num, count in counts.items():
            if num == 0:
                next[1] += count
            else:
                string = str(num)
                if len(string) % 2 == 0:
                    next[int(string[: len(string) // 2])] += count
                    next[int(string[len(string) // 2 :])] += count
                else:
                    next[2024 * num] += count
        counts = next
    return counts.total()


parts = (part1, part2)
