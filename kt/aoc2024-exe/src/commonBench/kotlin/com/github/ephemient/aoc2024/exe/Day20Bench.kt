package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day20
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day20Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(20)
    }

    @Benchmark
    fun part1() = runBlockingBenchmark {
        Day20(input).part1()
    }

    @Benchmark
    fun part2() = runBlockingBenchmark {
        Day20(input).part2()
    }

    @Benchmark
    fun solve(bh: Blackhole) = runBlockingBenchmark {
        val day20 = Day20(input)
        bh.consume(day20.part1())
        bh.consume(day20.part2())
    }
}
