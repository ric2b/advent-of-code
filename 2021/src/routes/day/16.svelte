<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '')[0];

    const versions = [];

    function part1(input) {
        const bin = [...input].map(c => hex2bin(c)).join('');
        parse_packet(bin)
        return versions.reduce((a, b) => a + b);
    }

    function part2(input) {
        const bin = [...input].map(c => hex2bin(c)).join('');
        const { result, rest } = parse_packet(bin)
        return result;
    }

    const hex2bin = hex => (parseInt(hex, 16).toString(2)).padStart(4, '0');

    function integer(bin, bit_length) {
        return {result: parseInt(bin.slice(0, bit_length), 2), rest: bin.slice(bit_length)};
    }

    function literal(bin) {
        const chunks = []

        let rest = bin, is_end;
        do {
            is_end = rest[0];
            chunks.push(rest.slice(1, 5));
            rest = rest.slice(5);
        } while (is_end === '1');

        const result = parseInt(chunks.join(''), 2);

        // console.log(`literal value: ${result}`);
        return { result, rest };
    }

    function operator(operation){
        return bin => {
            let result, rest;
            const content = bin.slice(1);
            const results = [];

            if (bin[0] === '0') {
                ({result, rest} = integer(content, 15));
                // console.log(`total length: ${result}`);
                for (let total_length = result; total_length > content.length - 15 - rest.length;) {
                    ({result, rest} = parse_packet(rest));
                    results.push(result);
                }
            } else {
                ({result, rest} = integer(content, 11));
                // console.log(`total packets: ${result}`);
                for (let sub_packet_count = result; sub_packet_count > 0; sub_packet_count--) {
                    ({result, rest} = parse_packet(rest));
                    results.push(result);
                }
            }

            return {result: results.reduce(operation), rest};
        }
    }

    function parse_packet(bin) {
        const version = parseInt(bin.slice(0, 3), 2);
        const type_id = parseInt(bin.slice(3, 6), 2);
        const raw_content = bin.slice(6);

        versions.push(version);
        // console.log({version, type_id, raw_content});

        return packet_parsers[type_id](raw_content);
    }

    const packet_parsers = [
        operator((a, b) => a + b),
        operator((a, b) => a * b),
        operator((a, b) => Math.min(a, b)),
        operator((a, b) => Math.max(a, b)),
        literal,
        operator((a, b) => a > b ? 1 : 0),
        operator((a, b) => a < b ? 1 : 0),
        operator((a, b) => a === b ? 1 : 0),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
        operator((a, b) => a + b),
    ];

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
