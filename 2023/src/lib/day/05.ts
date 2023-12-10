interface range {
	start: number,
	end: number,
	offset: number
}

export function part1(raw_input: string): number {
	const [raw_seeds, ...raw_conversions] = raw_input.trim().split('\n\n');
	const seeds = [...raw_seeds.match(/\d+/g)].map(Number);
	const conversions = raw_conversions.map(parse_conversion);

	const locations = seeds.map((seed) => {
		let current_value = seed;
		for (const conversion of conversions) {
			const matched_conversion = conversion.find((c) => c.start <= current_value && current_value < c.end);

			if (matched_conversion) { current_value = current_value + matched_conversion.offset; }
		}
		return current_value;
	});

	return Math.min(...locations);
}

export function part2(raw_input: string): number {
	const [raw_seeds, ...raw_conversions] = raw_input.trim().split('\n\n');
	const seed_ranges = [...raw_seeds.match(/\d+ \d+/g)].flatMap((s) => {
		const [start, length] = s.split(' ').map(Number);
		return { start, end: start + length - 1, offset: 0 };
	});

	const conversions = raw_conversions.map(parse_conversion);
	let ranges = seed_ranges;
	for (const level of conversions) {
		ranges = new_ranges_after_conversion(ranges, level);
	}

	const locations = ranges.map(range => range.start + range.offset)

	return Math.min(...locations);
}

function new_ranges_after_conversion(ranges: range[], conversions: range[]): range[] {
	const new_ranges = [];

	new_ranges.push(...ranges.flatMap(r => {
		const overlaping_conversions = conversions.filter(c => overlap(r, c));

		if (overlaping_conversions.length == 0) {
			return [r];
		}

		const split_ranges = []

		const min_start: number = Math.min(...overlaping_conversions.map(c => c.start)) - r.offset;
		const max_end: number = Math.max(...overlaping_conversions.map(c => c.end)) - r.offset;

		if (r.start < min_start) {
			split_ranges.push({start: r.start, end: min_start, offset: r.offset});
		}
		if (r.end > max_end) {
			split_ranges.push({start: max_end, end: r.end, offset: r.offset});
		}

		overlaping_conversions.map(c => {
			const overlap_start = Math.max(r.start, c.start - r.offset);
			const overlap_end = Math.min(r.end, c.end - r.offset);
			split_ranges.push({start: overlap_start, end: overlap_end, offset: r.offset + c.offset})
		});

		return split_ranges;
	}));

	return new_ranges;
}

function overlap(range: range,  conversion: range) {
	return (range.start + range.offset) <= conversion.end && (range.end + range.offset) >= conversion.start
}

function parse_conversion(raw_conversion): range[] {
	const [, ...raw_ranges] = raw_conversion.split('\n');
	return raw_ranges.map((raw_range) => {
		const [destination, start, length] = [...raw_range.match(/\d+/g)].map(Number);
		return { start, end: start + length, offset: destination - start };
	});
}
