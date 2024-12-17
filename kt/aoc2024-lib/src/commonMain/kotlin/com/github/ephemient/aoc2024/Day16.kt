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

    fun part1(): Int {
        val visited = mutableSetOf<IntPair>()
        val queue = PriorityQueue(compareBy(IndexedValue<State>::index))
        queue.add(IndexedValue(0, State(start)))
        while (!queue.isEmpty()) {
            val (score, state) = queue.remove()
            val pos = state.pos()
            if (!visited.add(pos)) continue
            if (state.pos() == end) return score
            for (value in arrayOf(
                IndexedValue(score + 1, state.forward()),
                IndexedValue(score + 1001, state.left()),
                IndexedValue(score + 1001, state.right()),
            )) {
                if (value.value.pos() !in maze) queue.add(value)
            }
        }
        error("no path")
    }

    fun part2(): Int {
        val best = part1()
        val acc = mutableSetOf<IntPair>()
        val visited = mutableMapOf<State, Int>()
        val queue = PriorityQueue(compareBy(IndexedValue<Pair<State, Set<IntPair>>>::index))
        queue.add(IndexedValue(0, State(start) to setOf(start)))
        while (!queue.isEmpty()) {
            val (score, value) = queue.remove()
            val (state, path) = value
            val lastScore = visited[state]
            check(lastScore == null || lastScore <= score)
            if (lastScore != null && lastScore < score) continue
            if (lastScore == null) visited[state] = score
            if (state.pos() == end) {
                check(score == best)
                acc.addAll(path)
                continue
            }
            for ((nextScore, nextState) in arrayOf(
                IndexedValue(score + 1, state.forward()),
                IndexedValue(score + 1001, state.left()),
                IndexedValue(score + 1001, state.right())
            )) {
                if (nextScore > best) continue
                val pos = nextState.pos()
                if (pos in maze || pos in path) continue
                queue.add(IndexedValue(nextScore, nextState to path + pos))
            }
        }
        return acc.size
    }

    private data class State(val y: Int, val x: Int, val dy: Int, val dx: Int) {
        constructor(pos: IntPair) : this(pos.first, pos.second, 0, 1)
        fun pos() = y to x
        fun forward() = State(y + dy, x + dx, dy, dx)
        fun left() = State(y - dx, x + dy, -dx, dy)
        fun right() = State(y + dx, x - dy, dx, -dy)
    }
}
