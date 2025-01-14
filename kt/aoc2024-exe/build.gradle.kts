import org.gradle.internal.extensions.stdlib.capitalized
import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSetTree

plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.kotlin.plugin.allopen)
    alias(libs.plugins.detekt)
    alias(libs.plugins.kotlinx.benchmark)
    distribution
}

class UpdateWasmWrapper(private val mainFile: Provider<RegularFile>) : Action<Task> {
    override fun execute(target: Task) {
        val mainFile = mainFile.get().asFile
        mainFile.writeText(
            mainFile.readText().replace(
                " argv, env, ",
                " argv, env, preopens: { '/data': env['AOC2024_DATADIR'] ?? '.' }, "
            )
        )
    }
}

kotlin {
    @Suppress("SpreadOperator")
    listOf(
        jvm {
            mainRun {
                mainClass = "com.github.ephemient.aoc2024.exe.Main"
            }
        },
        wasmJs {
            nodejs()
            binaries.executable()
        },
        wasmWasi {
            nodejs()
            for (binary in binaries.executable()) {
                binary.linkTask.configure {
                    doLast(UpdateWasmWrapper(binary.mainFile))
                }
            }
        },
        js {
            nodejs()
            binaries.executable()
        },
        *arrayOf(linuxArm64(), linuxX64(), macosArm64(), macosX64(), mingwX64()).onEach {
            it.binaries.executable {
                entryPoint("com.github.ephemient.aoc2024.exe.main")
            }
        },
    ).onEach {
        it.compilations {
            create("bench") {
                associateWith(getByName("main"))
            }
        }
    }

    applyDefaultHierarchyTemplate()
    applyHierarchyTemplate {
        withSourceSetTree(KotlinSourceSetTree("bench"))
        common {
            group("blocking") {
                withJvm()
                group("native") {
                    withNative()
                }
            }
            group("nonblocking") {
                withWasmJs()
            }
            group("nonJvm") {
                group("native") {
                    withNative()
                }
                withWasmJs()
            }
        }
    }

    sourceSets {
        commonMain {
            dependencies {
                implementation(projects.aoc2024Lib)
                implementation(libs.kotlinx.coroutines)
            }
        }
        getByName("commonBench") {
            dependencies {
                implementation(libs.kotlinx.benchmark)
            }
        }
        jsMain {
            dependencies {
                implementation(libs.kotlin.wrappers.node)
            }
        }
        nativeMain {
            dependencies {
                implementation(libs.okio)
            }
        }
    }

    val detektTaskNames = targets.flatMap { target ->
        target.compilations.map { compilation ->
            "detekt${target.targetName.capitalized()}${compilation.compilationName.capitalized()}"
        }
    }
    tasks.check {
        dependsOn(detektTaskNames)
    }
}

dependencies {
    detektPlugins(libs.bundles.detekt.plugins)
}

distributions {
    main {
        val jvmRuntimeClasspath = files(
            tasks.named("jvmJar"),
            configurations.getByName("jvmRuntimeClasspath"),
        )

        val startScripts by tasks.registering(CreateStartScripts::class) {
            mainClass = "com.github.ephemient.aoc2024.exe.Main"
            classpath = jvmRuntimeClasspath
            outputDir = File(buildDir, "scripts")
            applicationName = project.name
        }

        contents {
            from(startScripts) {
                into("bin")
            }
            from(jvmRuntimeClasspath) {
                into("lib")
            }
            from("src/dist")
        }
    }
}

allOpen {
    annotation("org.openjdk.jmh.annotations.State")
}

benchmark {
    targets {
        register("jvmBench")
        register("wasmJsBench")
        register("linuxArm64Bench")
        register("linuxX64Bench")
        register("macosArm64Bench")
        register("macosX64Bench")
        register("mingwX64Bench")
    }

    configurations {
        getByName("main") {
            warmups = 1
            iterationTime = 1
            iterationTimeUnit = "s"
            mode = "avgt"
            outputTimeUnit = "us"
            project.findProperty("benchmarkInclude")?.let { include(it.toString()) }
            project.findProperty("benchmarkExclude")?.let { exclude(it.toString()) }
        }
    }
}

configurations.consumable("jvmBenchmark") {
    attributes {
        attribute(Category.CATEGORY_ATTRIBUTE, objects.named(Category.LIBRARY))
        attribute(
            TargetJvmEnvironment.TARGET_JVM_ENVIRONMENT_ATTRIBUTE,
            objects.named(TargetJvmEnvironment.STANDARD_JVM),
        )
        attribute(LibraryElements.LIBRARY_ELEMENTS_ATTRIBUTE, objects.named(LibraryElements.JAR))
        attribute(Usage.USAGE_ATTRIBUTE, objects.named(Usage.JAVA_RUNTIME))
        attribute(Bundling.BUNDLING_ATTRIBUTE, objects.named(Bundling.EMBEDDED))
    }
    outgoing {
        capability("com.github.ephemient.aoc2024:aoc2024-bench:1.0")
        afterEvaluate {
            artifact(tasks.named("jvmBenchBenchmarkJar"))
        }
    }
}
