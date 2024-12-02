plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.detekt)
}

kotlin {
    jvm()
    wasmJs {
        browser()
        nodejs()
    }
    wasmWasi {
        nodejs()
    }
    js {
        browser()
        nodejs()
    }
    linuxArm64()
    linuxX64()
    macosArm64()
    macosX64()
    mingwX64()

    sourceSets {
        commonMain {
            dependencies {
                implementation(libs.kotlinx.coroutines)
            }
        }

        commonTest {
            dependencies {
                implementation(kotlin("test"))
                implementation(libs.kotlinx.coroutines.test)
            }
        }

        jvmTest {
            dependencies {
                implementation(kotlin("test-junit5"))
                implementation(libs.junit.jupiter.api)
                runtimeOnly(libs.junit.jupiter.engine)
            }
        }
    }
}

dependencies {
    detektPlugins(libs.bundles.detekt.plugins)
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
}
