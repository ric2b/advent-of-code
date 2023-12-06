export function part1(raw_input: string): number {
	const [raw_times, raw_records] = raw_input.trim().split('\n')

	const times = [...raw_times.match(/\d+/g)].map(Number)
	const records = [...raw_records.match(/\d+/g)].map(Number)

	return times.map((race_time, i) => {
		const record = records[i];

		let ways = 0;
		for(let hold_time= 1; hold_time < race_time; hold_time++) {
			const distance = hold_time * (race_time - hold_time);
			if (distance > record) {
				ways++
			}
		}
		return ways;
	}).reduce((a, b) => a * b);
}

export function part2(raw_input: string): number {
	const [raw_time, raw_record] = raw_input.trim().split('\n')

	const race_time = Number([...raw_time.match(/\d+/g)].join(''))
	const record = Number([...raw_record.match(/\d+/g)].join(''))

	let ways = 0;
	for(let hold_time= 1; hold_time < race_time; hold_time++) {
		const distance = hold_time * (race_time - hold_time);
		if (distance > record) {
			ways++
		}
	}
	return ways;
}
