<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    function parse(raw_input) {
        const [template, ...raw_rules] = raw_input.split('\n').filter(l => l !== '');
        return { template, insertion_rules: new Map(raw_rules.map(r => r.split(' -> '))) };
    }

    function part1({ template, insertion_rules}) {
        let polymer = template;

        for (let step = 0; step < 10; step++) {
            const insert_queue = [];
            for (let i = 0; i < polymer.length - 1; i++) {
                const to_insert = insertion_rules.get(polymer.slice(i, i+2));
                if (to_insert) insert_queue.push({ i: i + 1, char: to_insert });
            }

            insert_queue.forEach(({i, char}, j) => {
                polymer = polymer.slice(0, i+j) + char + polymer.substring(i+j);
            });
        }

        const counts = count(polymer);
        return Math.max(...counts.values()) - Math.min(...counts.values());
    }

    function part2({ template, insertion_rules}) {
        let pair_counts = new Map([...insertion_rules.keys()].map(pair => [pair, 0]));

        for (var i = 0; i < template.length - 1; i++) {
            const pair = template.slice(i, i+2);
            pair_counts.set(pair, pair_counts.get(pair) + 1)
        }

        for (let step = 0; step < 40; step++) {
            const new_pair_counts = new Map();

            pair_counts.forEach((count, pair) => {
                const inserted_char = insertion_rules.get(pair);
                const [pair_1, pair_2] = [pair[0] + inserted_char, inserted_char + pair[1]];

                new_pair_counts.set(pair_1, (new_pair_counts.get(pair_1) || 0) + count);
                new_pair_counts.set(pair_2, (new_pair_counts.get(pair_2) || 0) + count);
            });
            pair_counts = new_pair_counts;
        }

        const letter_counts = new Map();
        pair_counts.forEach((count, pair) => {
            [...pair].forEach(letter => letter_counts.set(letter, (letter_counts.get(letter) || 0) + count));
        });

        const [first_letter, last_letter] = [template[0], template.slice(-1)];
        letter_counts.forEach((count, letter) => {
            let adjusted_count = count;
            if (letter === first_letter) adjusted_count++;
            if (letter === last_letter) adjusted_count++;

            letter_counts.set(letter, adjusted_count / 2);
        });

        return Math.max(...letter_counts.values()) - Math.min(...letter_counts.values());
    }

    function count(iterable) {
        const counter = new Map();
        for (const v of iterable) {
            counter.set(v, (counter.get(v) || 0) + 1);
        }
        return counter;
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
