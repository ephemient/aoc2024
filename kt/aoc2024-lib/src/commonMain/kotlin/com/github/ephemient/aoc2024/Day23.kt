package com.github.ephemient.aoc2024

class Day23(input: String) {
    private val graph: Map<String, Set<String>> = buildMap<String, MutableSet<String>> {
        for (line in input.lineSequence()) {
            if ('-' !in line) continue
            val (a, b) = line.split('-', limit = 2)
            getOrPut(minOf(a, b), ::mutableSetOf).add(maxOf(a, b))
            getOrPut(maxOf(a, b), ::mutableSetOf)
        }
    }

    fun part1() = graph.entries.sumOf { (a, bs) ->
        bs.sumOf { b ->
            graph.getValue(b).count { c ->
                c in bs && (a.startsWith('t') || b.startsWith('t') || c.startsWith('t'))
            }
        }
    }

    fun part2() = part2(emptySet(), graph.keys).joinToString(",")

    private fun part2(used: Set<String>, remaining: Set<String>): Set<String> =
        remaining.maxOfWithOrNull(compareBy(Set<String>::size)) {
            part2(used + it, remaining intersect graph.getValue(it))
        } ?: used
}
