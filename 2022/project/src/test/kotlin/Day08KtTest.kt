import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day08KtTest {
    private val testInput =
        """
            30373
            25512
            65332
            33549
            35390
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day08.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(21, day08.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(1805, day08.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(8, day08.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(444528, day08.part2(myInput))
}