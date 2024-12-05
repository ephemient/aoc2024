"""
Day 5: Print Queue
"""

from typing import Callable, Iterable, List, Set, Tuple

SAMPLE_INPUT = """
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"""


def _parse(data: str) -> Tuple[Set[Tuple[int, int]], List[List[int]]]:
    (deps, updates) = data.split("\n\n")
    deps = {
        tuple(int(page) for page in line.split("|"))
        for line in deps.splitlines()
        if line
    }
    updates = [
        [int(page) for page in line.split(",")] for line in updates.splitlines() if line
    ]
    return deps, updates


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    143
    """
    deps, updates = _parse(data)
    return sum(
        pages[len(pages) // 2]
        for pages in updates
        if all((y, x) not in deps for i, x in enumerate(pages) for y in pages[i + 1 :])
    )


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    123
    """
    deps, updates = _parse(data)
    return sum(
        sorted_pages[len(pages) // 2]
        for pages in updates
        if pages
        != (sorted_pages := _partial_sorted(pages, lambda x, y: (y, x) not in deps))
    )


def _partial_sorted[T](iterable: Iterable[T], ok: Callable[[T, T], bool]) -> List[T]:
    output = list(iterable)
    i = 0
    while i < len(output):
        x = output[i]
        for j in range(i + 1, len(output)):
            y = output[j]
            if not ok(x, y):
                output[i], output[j] = y, x
                break
        else:
            i += 1
    return output


parts = (part1, part2)
