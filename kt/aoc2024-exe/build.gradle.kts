import org.jetbrains.kotlin.gradle.plugin.KotlinSourceSetTree

plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.kotlin.plugin.allopen)
    alias(libs.plugins.detekt)
    alias(libs.plugins.kotlinx.benchmark)
    distribution
}

kotlin {
    jvm {
        mainRun {
            mainClass = "com.github.ephemient.aoc2024.exe.Main"
        }
        compilations {
            create("bench") {
                associateWith(getByName("main"))
            }
        }
    }
    js {
        nodejs()
        binaries.executable()
    }
    for (target in arrayOf(linuxArm64(), linuxX64(), macosArm64(), macosX64(), mingwX64())) {
        target.binaries.executable {
            entryPoint("com.github.ephemient.aoc2024.exe.main")
        }
        target.compilations {
            create("bench") {
                associateWith(getByName("main"))
            }
        }
    }

    applyDefaultHierarchyTemplate()
    applyHierarchyTemplate {
        withSourceSetTree(KotlinSourceSetTree("bench"))
        common {
            withJvm()
            withNative()
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
