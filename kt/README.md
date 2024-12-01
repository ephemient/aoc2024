# [Advent of Code 2024](https://adventofcode.com/2024)
### my answers in [Kotlin](https://www.kotlinlang.org/) ![Kotlin CI](https://github.com/ephemient/aoc2024/workflows/Kotlin%20CI/badge.svg)

This project builds with [Gradle](https://gradle.org/).

Run the test suite:

```sh
./gradlew :aoc2024-lib:allTests
```

Run [kotlinx.benchmark](https://github.com/Kotlin/kotlinx-benchmark) ([JMH](https://openjdk.java.net/projects/code-tools/jmh/)) benchmarks:

```sh
./gradlew :aoc2024-exe:benchmark
```

Print solutions for the inputs provided in local data files:

```sh
./gradlew :aoc2024-exe:jvmRun :aoc2024-exe:runReleaseExecutable{LinuxX64,Macos{X64,Arm64}} :aoc2024-exe:jsNodeProductionRun
```

Run all checks, including [Detekt](https://detekt.github.io/) static code analysis:

```sh
./gradlew check
```
