export function part1(raw_input: string): number {

	// const [seeds, seed_to_soil, soil_to_fertilizer, fertilizer_to_water, water_to_light, light_to_temperature, temperature_to_humidity, humidity_to_location] = raw_input.trim().split('\n\n')
	const [raw_seeds, ...raw_conversions] = raw_input.trim().split('\n\n')

	const seeds = [...raw_seeds.match(/\d+/g)].map(Number)

	const conversions = raw_conversions.map(raw_conversion => {
		const [conversion_type, ...raw_ranges] = raw_conversion.split('\n')
		return raw_ranges
			.map(raw_range => {
				const [destination, source, length] = [...raw_range.match(/\d+/g)].map(Number)
				return { source, length, destination }
			});
	});

	const locations = seeds.map(seed => {
		let current_value = seed;
		for(const conversion of conversions) {
			const matched_conversion = conversion.find(c => c.source <= current_value && current_value < c.source + c.length)

			if (matched_conversion) {
				current_value = current_value + (matched_conversion.destination - matched_conversion.source)
			}
		}
		return current_value;
	})

	return Math.min(...locations);
}

export function part2(raw_input: string): number {
	const [raw_seeds, ...raw_conversions] = raw_input.trim().split('\n\n')

	const seed_ranges = [...raw_seeds.match(/\d+ \d+/g)].flatMap(s => {
		const [start, length] = s.split(' ').map(Number);
		// return Array.from({ length }, (_, i) => start + i);
		return { start, length };
	})

	const conversions = raw_conversions.map(raw_conversion => {
		const [conversion_type, ...raw_ranges] = raw_conversion.split('\n')
		return raw_ranges
			.map(raw_range => {
				const [destination, source, length] = [...raw_range.match(/\d+/g)].map(Number)
				return { source, length, destination }
			});
	});

	const locations = seed_ranges.map((seed_range, i) => {
		console.log(`Range ${i} of ${seed_ranges.length}, length: ${seed_range.length}`);
		let min_location_for_range = Infinity

		for (let seed = seed_range.start; seed < seed_range.start + seed_range.length; seed++) {
			let current_value = seed;
			for(const conversion of conversions) {
				const matched_conversion = conversion.find(c => c.source <= current_value && current_value < c.source + c.length)

				if (matched_conversion) {
					current_value = current_value + (matched_conversion.destination - matched_conversion.source)
				}
			}
			if (current_value < min_location_for_range) {
				min_location_for_range = current_value;
			}
		}

		return min_location_for_range;
	})

	return Math.min(...locations);
}
