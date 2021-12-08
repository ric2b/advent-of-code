<script>
	import { onMount } from 'svelte';
    import { assets } from '$app/paths';

	let part1_result, part2_result;
	onMount(async () => {
		const raw_input = await fetch(`${assets}/inputs/day08.txt`).then(r => r.text());
        const input = parse(raw_input);
		part1_result = part1(input);
        part2_result = part2(input);
    });

    function parse(raw_input) {
        return raw_input.split('\n').filter(l => l !== '').map(line => {
            const [signals, output] = line.split(' | ').map(s => s.split(' '));
            return { signals, output };
        })
    }

    function part1(input) {
        const decoded_output = input.map(({ _signals, output }) => {
           return output.map(d => {
               switch (d.length) {
                   case 2: return 1;
                   case 4: return 4;
                   case 3: return 7;
                   case 7: return 8;
                   default: return undefined;
               }
           })
        });

        return decoded_output.flat().filter(d => d !== undefined).length;
    }

    function part2(input) {
        return JSON.stringify(input);
    }
</script>

<p>Part 1: {part1_result}</p>
<p>Part 2: {part2_result}</p>
