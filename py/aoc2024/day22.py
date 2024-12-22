"""
Day 22: Monkey Market
"""

from collections import defaultdict, deque
from functools import reduce

SAMPLE_INPUT_1 = """\
1
10
100
2024
"""
SAMPLE_INPUT_2 = """\
1
2
3
2024
"""


def _step(num: int) -> int:
    num = num ^ num << 6 & 16777215
    num = num ^ num >> 5 & 16777215
    num = num ^ num << 11 & 16777215
    return num


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_1)
    37327623
    """
    return sum(
        reduce(lambda num, _: _step(num), range(2000), int(line))
        for line in data.splitlines()
        if line
    )


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_2)
    23
    """
    sequences = defaultdict(dict)
    for i, line in enumerate(data.splitlines()):
        if not line:
            continue
        window = deque((int(line),), 5)
        for _ in range(2000):
            if len(window) == window.maxlen:
                window.popleft()
            window.append(_step(window[-1]))
            if len(window) == window.maxlen:
                prices = [value % 10 for value in window]
                values = sequences[tuple(x - y for x, y in zip(prices, prices[1:]))]
                if i not in values:
                    values[i] = prices[-1]
    return max(sum(values.values()) for values in sequences.values())


parts = (part1, part2)
