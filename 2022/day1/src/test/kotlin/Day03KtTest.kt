import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day03KtTest {
    @Test
    fun testPart1() {
        val testInput =
            """
                vJrwpWtwJgWrhcsFMMfFFhFp
                jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                PmmdzqPrVvPwwTWBwg
                wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                ttgJtRGJQctTZtZT
                CrZsJsPPZsGzwwsLwLmpwMDw
            """.trimIndent()

        assertEquals(157, day03.part1(testInput))
    }

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day03.txt")!!.readText()
        assertEquals(8243, day03.part1(myInput))
    }

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day03.txt")!!.readText()
        assertEquals(2631, day03.part2(myInput))
    }
}