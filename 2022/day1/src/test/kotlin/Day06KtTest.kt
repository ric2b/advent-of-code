import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day06KtTest {
    @Test
    fun testPart1() {
        assertEquals(7, day06.part1("mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
        assertEquals(5, day06.part1("bvwbjplbgvbhsrlpgdmjqwftvncz"))
        assertEquals(6, day06.part1("nppdvjthqldpwncqszvftbrmjlhg"))
        assertEquals(10, day06.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
        assertEquals(11, day06.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))
    }

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day06.txt")!!.readText()
        assertEquals(1282, day06.part1(myInput))
    }

    @Test
    fun testPart2() {
        assertEquals(19, day06.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb"))
        assertEquals(23, day06.part2("bvwbjplbgvbhsrlpgdmjqwftvncz"))
        assertEquals(23, day06.part2("nppdvjthqldpwncqszvftbrmjlhg"))
        assertEquals(29, day06.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))
        assertEquals(26, day06.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))
    }

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day06.txt")!!.readText()
        assertEquals(3513, day06.part2(myInput))
    }
}