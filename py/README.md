# [Advent of Code 2024](https://adventofcode.com/2024)
### my answers in [Python](https://www.python.org/) ![Python CI](https://github.com/ephemient/aoc2024/workflows/Python%20CI/badge.svg)

This project builds with [Poetry](https://python-poetry.org/).

Setup:

```sh
pipx install poetry
poetry install
```

Run the test suite:

```sh
poetry run pytest
```

Run the benchmarks:

```sh
poetry run pytest --benchmark-enable
```

Print solutions for the inputs provided in local data files:

```sh
poetry run aoc2024
```

Lint and format code with [Ruff](https://docs.astral.sh/ruff/):

```sh
poetry run ruff check --fix
poetry run ruff format
```
