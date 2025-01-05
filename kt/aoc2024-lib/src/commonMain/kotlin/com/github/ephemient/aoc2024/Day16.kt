package com.github.ephemient.aoc2024

class Day16(input: String) {
    private val maze: Set<IntPair>
    private val start: IntPair
    private val end: IntPair
    init {
        var start: IntPair? = null
        var end: IntPair? = null
        maze = buildSet {
            for ((y, line) in input.lineSequence().withIndex()) {
                for ((x, char) in line.withIndex()) {
                    when (char) {
                        '#' -> add(y to x)
                        'S' -> {
                            require(start == null)
                            start = y to x
                        }
                        'E' -> {
                            require(end == null)
                            end = y to x
                        }
                    }
                }
            }
        }
        this.start = requireNotNull(start)
        this.end = requireNotNull(end)
    }

    private val cachedExplore = mutableListOf<Result>()
    private val visited = mutableMapOf<State, Int>()
    private val queue = PriorityQueue(compareBy(Result::points)).apply {
        add(Result(State(start.first, start.second, 0, 1), 0, 0))
    }
    private fun explore() = cachedExplore.asSequence() + sequence {
        while (!queue.isEmpty()) {
            val state = queue.remove()
            val previous = visited[state.state]
            check(previous == null || previous <= state.points)
            if (previous != null && previous < state.points) continue
            yield(state)
            if (previous == null) {
                visited[state.state] = state.points
                state.state.forward().let { if (it.pos() !in maze) queue.add(Result(it, state.points + 1, 1)) }
                state.state.left().let { if (it.pos() !in maze) queue.add(Result(it, state.points + 1001, 2)) }
                state.state.right().let { if (it.pos() !in maze) queue.add(Result(it, state.points + 1001, 4)) }
            }
        }
    }.onEach(cachedExplore::add)

    fun part1(): Int = explore().first { it.state.pos() == end }.points

    fun part2(): Int {
        var best = -1
        val paths = explore()
            .takeWhile { best !in 0..<it.points }
            .onEach { if (it.state.pos() == end) best = it.points }
            .groupingBy { it.state }
            .fold(0) { ways, (_, _, way) -> ways or way }
        val stack = mutableListOf(
            State(end.first, end.second, 0, 1),
            State(end.first, end.second, 1, 0),
            State(end.first, end.second, 0, -1),
            State(end.first, end.second, -1, 0),
        )
        val seen = stack.toMutableSet()
        while (stack.isNotEmpty()) {
            val key = stack.removeLast()
            val ways = paths.getOrElse(key) { 0 }
            val (y, x, dy, dx) = key
            if (ways and 1 != 0) State(y - dy, x - dx, dy, dx).let { if (seen.add(it)) stack.add(it) }
            if (ways and 2 != 0) State(y - dy, x - dx, dx, -dy).let { if (seen.add(it)) stack.add(it) }
            if (ways and 4 != 0) State(y - dy, x - dx, -dx, dy).let { if (seen.add(it)) stack.add(it) }
        }
        return seen.mapTo(mutableSetOf(), State::pos).size
    }

    private data class State(val y: Int, val x: Int, val dy: Int, val dx: Int) {
        fun pos() = y to x
        fun forward() = State(y + dy, x + dx, dy, dx)
        fun left() = State(y - dx, x + dy, -dx, dy)
        fun right() = State(y + dx, x - dy, dx, -dy)
    }

    private data class Result(val state: State, val points: Int, val way: Int)
}
