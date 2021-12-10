<p class="aoc_yellow">Part 1: {part1_result}</p>
<p class="aoc_yellow">Part 2: {part2_result}</p>

<script>
    function parse(raw_input) {
        return raw_input.split('\n').filter(l => l !== '').map(line => {
            const [patterns, output] = line.split(' | ').map(s => s.split(' '));
            return { patterns, output };
        })
    }

    function part1(input) {
        const decoded_output = input.map(({ _patterns, output }) => {
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

    //   0:      1:      2:      3:      4:
    //  aaaa    ....    aaaa    aaaa    ....
    // b    c  .    c  .    c  .    c  b    c
    // b    c  .    c  .    c  .    c  b    c
    //  ....    ....    dddd    dddd    dddd
    // e    f  .    f  e    .  .    f  .    f
    // e    f  .    f  e    .  .    f  .    f
    //  gggg    ....    gggg    gggg    ....
    //
    //   5:      6:      7:      8:      9:
    //  aaaa    aaaa    aaaa    aaaa    aaaa
    // b    .  b    .  .    c  b    c  b    c
    // b    .  b    .  .    c  b    c  b    c
    //  dddd    dddd    ....    dddd    dddd
    // .    f  e    f  .    f  e    f  .    f
    // .    f  e    f  .    f  e    f  .    f
    //  gggg    gggg    ....    gggg    gggg

    // acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab
    // 8       2,3,5 2,3,5 2,3,5 7   0,6,9  0,6,9  4    0,6,9  1
    // 8       5     2     3     7   9      6      4    0      1
    // cdfeb fcadb cdfeb cdbaf

    // acdfg: cf, abcdefg, acf, bcdf, abcdefg

    // a: abcdefg
    // b: abcdefg
    // c: abcdefg
    // d: abcdefg
    // e: abcdefg
    // f: abcdefg
    // g: abcdefg

    // step 1: use basic digits to remove candidates (length 2 -> those wires have to be c or f
    // a: cf
    // b: cf
    // c: abcdefg
    // d: acf
    // e: bcdf
    // f: bcdf
    // g: abcdefg

    // step 1.5: clean up candidates (not in digit 1 -> can't be c or f)
    // a: cf
    // b: cf
    // c: eg
    // d: a
    // e: bd
    // f: bd
    // g: eg

    // step 2: for remaining digits, use digit_segments to figure out which wires it needs
    // 0: abcefg -> [d] + [e, f] + [a, b] + [c, g] + [a, b] + [c, g]
    // 2: acdeg -> [d] + [a,b] + [e,f] + [c, g] + [c, g]
    // 3: acdfg -> [d] + [a,b] + [e,f] + [a,b] + [c,g]
    // 5: abdfg -> [d] + [e, f] + [e, f] + [a, b] + [c, g]
    // 6: abdefg -> [d] + [e, f] + [e,f] + [c, g] + [a, b]
    // 9: abcdfg -> [d] + [e, f] + [a, b] + [e, f] + [a, b]

    // step 3: notice repeats which mean both wires have to be present
    // 0: abcefg -> d + a + b + c + g + [e, f] -> cagedb
    // 2: acdeg -> d + [a,b] + [e,f] + c + g -> gcdfa
    // 3: acdfg -> d + a + b + [e,f] + [c,g] -> fbcad
    // 5: abdfg -> d + e + f + [a, b] + [c, g] -> cdfbe
    // 6: abdefg -> d + e + f + c + g + [a, b] -> cdfgeb
    // 9: abcdfg -> d + e + f + a + b + [c, g] -> cefabd

    function part2(input) {
        const decoded_outputs = input.map(({ patterns, output }) => {
            const wire_seg_candidates = new Map(segments.map(seg => [seg, new Set(segments)]));
            const known_digit_wires = Array(10);
            const digit_segments = make_digit_segments();

            // step 1
            patterns.forEach(pattern => {
                const digit = {2: 1, 4: 4, 3: 7, 7: 8}[pattern.length]; // 1
                if (digit === undefined) return;

                const wires = new Set(pattern.split('')); // {a, b}
                known_digit_wires[digit] = wires;
                wires.forEach(wire_on => {
                    if (digit_segments[digit].size < wire_seg_candidates.get(wire_on).size) {
                        wire_seg_candidates.set(wire_on, digit_segments[digit]);
                    }
                }); // {a: {c, f}, b: {c, f}}
            });

            // step 1.5
            for (const [wire, seg_candidates] of wire_seg_candidates) { // a, {c, f}
                known_digit_wires.forEach((digit_wires, digit) => { // {a, b}
                    if (!digit_wires.has(wire)) {
                        digit_segments[digit].forEach(digit_segment => seg_candidates.delete(digit_segment));
                    }
                });
            }

            // step 2
            const digit_wire_candidates = new Map([0, 2, 3, 5, 6, 9].map(digit => {
                const segments_wire_candidates = [];
                for (const segment of digit_segments[digit]) {
                    const segment_wire_candidates = [];
                    for (const [wire, seg_candidates] of wire_seg_candidates) {
                        if (seg_candidates.has(segment)) segment_wire_candidates.push(wire);
                    }
                    segments_wire_candidates.push(segment_wire_candidates);
                }
                return [digit, segments_wire_candidates];
            }));

            // step 3
            const digit_wire_requirements = new Map();
            for (const [digit, wire_candidates] of digit_wire_candidates) {
                const wire_requirements = [];
                const wire_candidate_count = new Map();
                wire_candidates.forEach(segment_wire_candidates => {
                    if (segment_wire_candidates.length === 1) {
                        wire_requirements.push(Array.from(segment_wire_candidates)[0]);
                    } else {
                        segment_wire_candidates.forEach(c => {
                            wire_candidate_count.set(c, (wire_candidate_count.get(c) || 0) + 1);
                        });
                    }
                });
                for (const [wire_candidate, count] of wire_candidate_count) {
                    if (count > 1) wire_requirements.push(wire_candidate);
                }
                digit_wire_requirements.set(digit, wire_requirements);
            }

            // step 4: identify remaining patterns
            patterns.forEach(pattern => {
                const pattern_wires = new Set(pattern.split(''));
                [0, 2, 3, 5, 6, 9].map(digit => {
                    const wire_requirements = digit_wire_requirements.get(digit);
                    if (pattern_wires.size === digit_segments[digit].size) {
                        if (wire_requirements.every(req_wire => pattern_wires.has(req_wire))) {
                            known_digit_wires[digit] = pattern_wires;
                        }
                    }
                });
            });

            // step 5: decode output
            return Number(output.map(pattern => {
                const trivial_digit = {2: 1, 4: 4, 3: 7, 7: 8}[pattern.length];
                if (trivial_digit) return trivial_digit;

                const pattern_wires = pattern.split('');

                for (const [digit, digit_wires] of known_digit_wires.entries()) {
                    if ([1,4,7,8].includes(digit)) continue;
                    if (digit_wires.size === pattern_wires.length && pattern_wires.every(wire => digit_wires.has(wire))) {
                        return digit;
                    }
                }
            }).join(''));
        });

        return decoded_outputs.reduce((a, b) => a + b);
    }

    const segments = 'abcdefg'.split('');

    const make_digit_segments = () => [
        'abcefg', // 0
        'cf', // 1
        'acdeg', // 2
        'acdfg', // 3
        'bcdf', // 4
        'abdfg', // 5
        'abdefg', // 6
        'acf', // 7
        'abcdefg', // 8
        'abcdfg', // 9
    ].map(s => new Set(s.split('')));

    export let raw_input;

    $: input = parse(raw_input);
	$: part1_result = part1(input);
    $: part2_result = part2(input);

</script>
