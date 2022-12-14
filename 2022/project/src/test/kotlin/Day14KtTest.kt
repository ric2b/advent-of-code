import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day14KtTest {
    private val testInput =
        """
            498,4 -> 498,6 -> 496,6
            503,4 -> 502,4 -> 502,9 -> 494,9
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day14.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(24, day14.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(1406, day14.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(93, day14.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(20870, day14.part2(myInput))
}
