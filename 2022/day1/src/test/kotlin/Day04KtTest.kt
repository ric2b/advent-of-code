import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day04KtTest {
    @Test
    fun testPart1() {
        val testInput =
            """
                2-4,6-8
                2-3,4-5
                5-7,7-9
                2-8,3-7
                6-6,4-6
                2-6,4-8
            """.trimIndent()

        assertEquals(2, day04.part1(testInput))
    }

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day04.txt")!!.readText()
        assertEquals(644, day04.part1(myInput))
    }

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day04.txt")!!.readText()
        assertEquals(926, day04.part2(myInput))
    }
}