<script>
    export let raw_input;

    const p1 = raw_input.split('\n').map(s => {
        const numbers = [...s.match(/\d/g)]
        return Number(`${numbers.at(0)}${numbers.at(-1)}`)
    }).reduce((a, b) => a + b);

    const p2 = raw_input.split('\n').map(s => {
        const replacements = {
            'one': 'o1ne',
            'two': 't2wo',
            'three': 't3hree',
            'four': 'f4our',
            'five': 'f5ive',
            'six': 's6ix',
            'seven': 's7even',
            'eight': 'e8ight',
            'nine': 'n9ine',
        }

        for(const [original, replacement] of Object.entries(replacements)) {
            s = s.replace((new RegExp(original, 'g')), replacement);
        }

        const numbers = [...s.match(/\d/g)]
        return Number(`${numbers.at(0)}${numbers.at(-1)}`)
    }).reduce((a, b) => a + b);
    // const part1 = depths => depths.filter((value, i) => depths[i+1] > value).length;
    // const part2 = depths => depths.filter((value, i) => window(depths, i + 1) > window(depths, i)).length;
    //
    // export let part1_result;
    // export let part2_result;
</script>


Part 1: {p1}

<br>

Part 2: {p2}
