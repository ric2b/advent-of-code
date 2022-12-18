import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day19KtTest {
    private val testInput =
        """

        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day19.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(64, day19.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(4450, day19.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(58, day19.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(2564, day19.part2(myInput))
}
