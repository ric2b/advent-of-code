import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day08KtTest {
    private val testInput =
        """
            30373
            25512
            65332
            33549
            35390
        """.trimIndent()


    @Test
    fun testPart1() = assertEquals(21, day08.part1(testInput))

    @Test
    fun testPart1MyInput() {
        val myInput = File("src/main/resources/inputs/day08.txt").readText()
        assertEquals(1805, day08.part1(myInput))
    }

    @Test
    fun testPart2() = assertEquals(8, day08.part2(testInput))

    @Test
    fun testPart2MyInput() {
        val myInput = File("src/main/resources/inputs/day08.txt").readText()
        assertEquals(444528, day08.part2(myInput))
    }
}