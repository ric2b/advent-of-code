export function part1(raw_input: string): number {
	// const [seeds, seed_to_soil, soil_to_fertilizer, fertilizer_to_water, water_to_light, light_to_temperature, temperature_to_humidity, humidity_to_location] = raw_input.trim().split('\n\n')
	const [raw_seeds, ...raw_conversions] = raw_input.trim().split('\n\n');

	const seeds = [...raw_seeds.match(/\d+/g)].map(Number);

	const conversions = raw_conversions.map((raw_conversion) => {
		const [, ...raw_ranges] = raw_conversion.split('\n');
		return raw_ranges.map((raw_range) => {
			const [destination, source, length] = [...raw_range.match(/\d+/g)].map(Number);
			return { source, length, destination };
		});
	});

	const locations = seeds.map((seed) => {
		let current_value = seed;
		for (const conversion of conversions) {
			const matched_conversion = conversion.find(
				(c) => c.source <= current_value && current_value < c.source + c.length
			);

			if (matched_conversion) {
				current_value =
					current_value + (matched_conversion.destination - matched_conversion.source);
			}
		}
		return current_value;
	});

	return Math.min(...locations);
}

export function part2(raw_input: string): number {
	const [raw_seeds, ...raw_conversions] = raw_input.trim().split('\n\n');

	const seed_ranges = [...raw_seeds.match(/\d+ \d+/g)].flatMap((s) => {
		const [start, length] = s.split(' ').map(Number);
		return { start, length };
	});

	const conversions = raw_conversions.map((raw_conversion) => {
		const [, ...raw_ranges] = raw_conversion.split('\n');
		return raw_ranges.map((raw_range) => {
			const [destination, source, length] = [...raw_range.match(/\d+/g)].map(Number);
			return { source, length, destination };
		});
	});

	conversions.reverse();

	for (let i = 0; ; i++) {
		let current_value = i;
		for (const conversion of conversions) {
			const matched_conversion = conversion.find(
				(c) => c.destination <= current_value && current_value < c.destination + c.length
			);

			if (matched_conversion) {
				current_value =
					current_value - (matched_conversion.destination - matched_conversion.source);
			}
		}

		for (const seed_range of seed_ranges) {
			if (
				seed_range.start <= current_value &&
				current_value <= seed_range.start + seed_range.length
			) {
				return i;
			}
		}
	}
}
