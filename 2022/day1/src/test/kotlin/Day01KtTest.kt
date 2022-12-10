import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day01KtTest {
    @Test
    fun testPart1() {
        val testInput =
            """
                1000
                2000
                3000

                4000

                5000
                6000

                7000
                8000
                9000

                10000
            """.trimIndent()

        assertEquals(24000, day01.part1(testInput))
    }

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day01.txt")!!.readText()
        assertEquals(69836, day01.part1(myInput))
    }

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day01.txt")!!.readText()
        assertEquals(207968, day01.part2(myInput))
    }
}