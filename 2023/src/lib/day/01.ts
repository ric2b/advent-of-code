export function part1(raw_input: String): Number {
    return raw_input.split('\n').map(s => {
        const numbers = [...s.match(/\d/g)]
        return Number(`${numbers.at(0)}${numbers.at(-1)}`)
    }).reduce((a, b) => a + b);
}

export function part2(raw_input) {
    return raw_input.split('\n').map(s => {
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
}
