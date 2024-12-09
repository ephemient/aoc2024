"""
Day 9: Disk Defragmenter
"""

from itertools import accumulate

SAMPLE_INPUT = """
2333133121414131402
"""


def _rangesum(start: int, size: int) -> int:
    return (2 * start + size - 1) * size // 2


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    1928
    """
    chunks = [int(c) for c in data if c.isdigit()]
    total, offset, i, j = 0, 0, 0, len(chunks) - 1
    while i <= j:
        if not i % 2:
            size = chunks[i]
            total += i // 2 * _rangesum(offset, size)
            offset += size
            i += 1
        elif not j % 2:
            size = min(chunks[i], chunks[j])
            total += j // 2 * _rangesum(offset, size)
            offset += size
            chunks[i] -= size
            if chunks[i] <= 0:
                i += 1
            chunks[j] -= size
            if chunks[j] <= 0:
                j -= 1
        else:
            j -= 1
    return total


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    2858
    """
    chunks = [int(c) for c in data if c.isdigit()]
    offsets = list(accumulate(chunks, initial=0))
    total = 0
    for i in range(len(chunks) - 1 & ~1, -1, -2):
        size = chunks[i]
        offset = offsets[i]
        for j in range(1, i, 2):
            if chunks[j] >= size:
                offset = offsets[j]
                offsets[j] += size
                chunks[j] -= size
                break
        total += i // 2 * _rangesum(offset, size)
    return total


parts = (part1, part2)
