import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day07KtTest {
    private val testInput =
        """
            $ cd /
            $ ls
            dir a
            14848514 b.txt
            8504156 c.dat
            dir d
            $ cd a
            $ ls
            dir e
            29116 f
            2557 g
            62596 h.lst
            $ cd e
            $ ls
            584 i
            $ cd ..
            $ cd ..
            $ cd d
            $ ls
            4060174 j
            8033020 d.log
            5626152 d.ext
            7214296 k
        """.trimIndent()


    @Test
    fun testPart1() = assertEquals(95437, day07.part1(testInput))

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day07.txt")!!.readText()
        assertEquals(1182909, day07.part1(myInput))
    }

    @Test
    fun testPart2() = assertEquals(24933642, day07.part2(testInput))

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day07.txt")!!.readText()
        assertEquals(2832508, day07.part2(myInput))
    }
}