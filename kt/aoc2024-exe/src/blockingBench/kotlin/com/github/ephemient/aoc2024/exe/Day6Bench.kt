package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day6
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking

@State(Scope.Benchmark)
class Day6Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(6)
    }

    @Benchmark
    fun part1() = Day6(input).part1()

    @Benchmark
    fun part2() = runBlocking(Dispatchers.Default) {
        Day6(input).part2()
    }

    @Benchmark
    fun both(bh: Blackhole) = runBlocking(Dispatchers.Default) {
        val day6 = Day6(input)
        bh.consume(day6.part1())
        bh.consume(day6.part2())
    }
}
