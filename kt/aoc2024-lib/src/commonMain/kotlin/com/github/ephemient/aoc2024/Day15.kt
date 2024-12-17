package com.github.ephemient.aoc2024

class Day15(private val input: String) {
    fun part1(): Int {
        val state = State(input.substringBefore("\n\n"))
        for (char in input.substringAfter("\n\n", "")) Direction[char]?.let(state::move)
        return state.toInt()
    }

    fun part2(): Int {
        val state = State(input.substringBefore("\n\n")).doubled()
        for (char in input.substringAfter("\n\n", "")) Direction[char]?.let(state::move)
        return state.toInt()
    }

    private enum class Direction(val dy: Int, val dx: Int) {
        EAST(0, 1), NORTH(-1, 0), WEST(0, -1), SOUTH(1, 0),
        ;

        companion object {
            operator fun get(char: Char) = when (char) {
                '>' -> EAST
                '^' -> NORTH
                '<' -> WEST
                'v' -> SOUTH
                else -> null
            }
        }
    }

    private class State(private val maze: Array<CharArray>) {
        constructor(input: String) : this(input.lines().let { lines -> Array(lines.size) { lines[it].toCharArray() } })

        fun doubled() = State(
            Array(maze.size) {
                val line = maze[it]
                CharArray(line.size * 2) {
                    when (val char = line[it / 2]) {
                        '@' -> if (it % 2 == 0) '@' else '.'
                        'O' -> if (it % 2 == 0) '[' else ']'
                        else -> char
                    }
                }
            }
        )

        private var robot = maze.withIndex().firstNotNullOf { (y, line) ->
            line.withIndex().firstNotNullOfOrNull { (x, c) ->
                if (c == '@') y to x else null
            }
        }

        private operator fun get(pos: IntPair) = maze.getOrNull(pos.first)?.getOrNull(pos.second) ?: '\u0000'
        private operator fun set(pos: IntPair, value: Char) {
            maze[pos.first][pos.second] = value
        }

        fun move(dir: Direction) {
            when (dir) {
                Direction.EAST, Direction.WEST -> {
                    val chain = buildList {
                        var pos = robot
                        while (true) {
                            add(pos)
                            pos += dir
                            when (this@State[pos]) {
                                'O', '[', ']' -> {}
                                '.' -> break
                                else -> return
                            }
                        }
                    }
                    for (pos in chain.asReversed()) this[pos + dir] = this[pos]
                    this[robot] = '.'
                }
                else -> {
                    val chain = buildList {
                        var front = setOf(robot)
                        do {
                            add(front)
                            front = buildSet {
                                for (pos in front) {
                                    val pos = pos + dir
                                    when (this@State[pos]) {
                                        'O' -> add(pos)
                                        '[' -> add(pos) or add(pos + Direction.EAST)
                                        ']' -> add(pos) or add(pos + Direction.WEST)
                                        '.' -> {}
                                        else -> return
                                    }
                                }
                            }
                        } while (front.isNotEmpty())
                    }
                    for (front in chain.asReversed()) {
                        for (pos in front) {
                            this[pos + dir] = this[pos]
                            this[pos] = '.'
                        }
                    }
                }
            }
            robot += dir
            check(this[robot] == '@')
        }

        fun toInt(): Int {
            var total = 0
            for ((y, line) in maze.withIndex()) {
                for ((x, char) in line.withIndex()) {
                    if (char == 'O' || char == '[') total += 100 * y + x
                }
            }
            return total
        }
    }

    companion object {
        private operator fun IntPair.plus(dir: Direction) = first + dir.dy to second + dir.dx
    }
}
