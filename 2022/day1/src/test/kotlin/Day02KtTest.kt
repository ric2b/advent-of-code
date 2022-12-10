import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day02KtTest {
    @Test
    fun testPart1() {
        val testInput =
            """
                A Y
                B X
                C Z
            """.trimIndent()

        assertEquals(15, day02.part1(testInput))
    }

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day02.txt")!!.readText()
        assertEquals(15523, day02.part1(myInput))
    }

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day02.txt")!!.readText()
        assertEquals(15702, day02.part2(myInput))
    }
}