import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test

@Disabled
class Day20KtTest {
    private val testInput =
        """

        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day20.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(33, day20.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(1719, day20.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(3472, day20.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(19530, day20.part2(myInput))
}
