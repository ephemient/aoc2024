package com.github.ephemient.aoc2024

class Day18(input: String, private val size: Int = 70) {
    private val coords = input.lineSequence().mapNotNull { line ->
        val x = line.substringBefore(',').toIntOrNull() ?: return@mapNotNull null
        val y = line.substringAfter(',').toIntOrNull() ?: return@mapNotNull null
        x to y
    }.toList()

    private fun findPath(obstacles: Iterable<IntPair>): Set<IntPair>? {
        val visited = obstacles.toMutableSet()
        val queue = ArrayDeque<Node>()
        queue.add(Node(0, 0))
        while (queue.isNotEmpty()) {
            val node = queue.removeFirst()
            val (x, y) = node
            if (x == size && y == size) return generateSequence(node) { it.next }.map { it.x to it.y }.toSet()
            if (!visited.add(x to y)) continue
            if (x > 0) queue.addLast(Node(x - 1, y, node))
            if (y > 0) queue.addLast(Node(x, y - 1, node))
            if (y < size) queue.addLast(Node(x, y + 1, node))
            if (x < size) queue.addLast(Node(x + 1, y, node))
        }
        return null
    }

    fun part1(limit: Int = 1024): Int? = findPath(coords.subList(0, limit))?.size?.minus(1)

    fun part2(): String? {
        var i = 0
        while (i < coords.size) {
            val path = findPath(coords.subList(0, i + 1)) ?: return coords[i].let { (x, y) -> "$x,$y" }
            do i++ while (i < coords.size && coords[i] !in path)
        }
        return null
    }

    private data class Node(val x: Int, val y: Int, val next: Node? = null)
}
