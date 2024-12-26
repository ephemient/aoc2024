import org.graalvm.buildtools.gradle.internal.agent.AgentConfigurationFactory
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.function.Predicate

plugins {
    application
    alias(libs.plugins.native.image)
}

application {
    mainClass.set("com.github.ephemient.aoc2024.exe.Main")
}

val benchmarkDir = layout.buildDirectory.dir(
    "reports/benchmarks/main/" +
        LocalDateTime.now().format(DateTimeFormatter.ISO_DATE_TIME).replace(':', '.')
)

val benchmark by sourceSets.creating
val benchmarkRun by tasks.registering(JavaExec::class) {
    System.getenv("GRAALVM_HOME")?.ifEmpty { null }?.let { setExecutable("$it/bin/java") }
    mainClass = "org.openjdk.jmh.Main"
    classpath(benchmark.output, benchmark.runtimeClasspath)
    args("-f", 0, "-wi", 1, "-w", "0s", "-r", "0s", "-bm", "avgt", "-tu", "us", "-i", 1)
    args("-rf", "json", "-rff", "/dev/null", "-e", "jvm")
    project.findProperty("benchmarkExclude")?.let { args("-e", it) }
    project.findProperty("benchmarkInclude")?.let { args(it) }
    outputs.dir(AgentConfigurationFactory.getAgentOutputDirectoryForTask(layout, name))
}
val syncBenchmarkRunMetadata by tasks.registering(Sync::class) {
    into(layout.buildDirectory.dir("generated/resources/benchmark"))
    from(benchmarkRun) {
        into("META-INF/native-image/com.github.ephemient.aoc2024/benchmark")
    }
}
graalvmNative {
    binaries {
        getByName("main") {
            imageName = "aoc2024-native"
        }
        create("benchmark") {
            imageName = "aoc2024-native-benchmark"
            mainClass = "org.openjdk.jmh.Main"
            classpath(syncBenchmarkRunMetadata, benchmark.output, benchmark.runtimeClasspath)
            runtimeArgs("-f", 0, "-wi", 1, "-w", "1s", "-r", "1s", "-bm", "avgt", "-tu", "us")
            val benchmarkFile = benchmarkDir.get().file("graalvmBench.json")
            runtimeArgs("-rf", "json", "-rff", benchmarkFile.asFile, "-e", "jvm")
            project.findProperty("benchmarkExclude")?.let { runtimeArgs("-e", it) }
            project.findProperty("benchmarkInclude")?.let { runtimeArgs(it) }
        }
    }
    agent {
        tasksToInstrumentPredicate = Predicate { it.name == "benchmarkRun" }
    }
}
tasks.named("nativeBenchmarkRun") {
    val benchmarkDir = benchmarkDir.get()
    outputs.file(benchmarkDir.file("graalvmBench.json"))
    doFirst { benchmarkDir.asFile.mkdirs() }
}

val externalTestClasses = configurations.dependencyScope("externalTestClasses")
val externalTestClasspath = configurations.resolvable("externalTestClasspath") {
    extendsFrom(externalTestClasses.get())
    isTransitive = false
    attributes {
        attribute(
            LibraryElements.LIBRARY_ELEMENTS_ATTRIBUTE,
            objects.named(LibraryElements.CLASSES),
        )
    }
}
configurations.testImplementation {
    extendsFrom(externalTestClasses.get())
}
tasks.test {
    testClassesDirs = files(testClassesDirs, externalTestClasspath.get())
    useJUnitPlatform()
}

dependencies {
    implementation(projects.aoc2024Exe)
    testImplementation(libs.junit.jupiter.api)
    externalTestClasses(projects.aoc2024Lib) {
        capabilities {
            requireCapability("com.github.ephemient.aoc2024:aoc2024-test:1.0")
        }
    }
    benchmark.implementationConfigurationName(projects.aoc2024Exe) {
        capabilities {
            requireCapability("com.github.ephemient.aoc2024:aoc2024-bench:1.0")
        }
    }
}
