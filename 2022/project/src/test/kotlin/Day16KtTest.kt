import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day16KtTest {
    private val testInput =
        """

        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day16.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(-1, day16.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(-1, day16.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(-1, day16.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(-1, day16.part2(myInput))
}
