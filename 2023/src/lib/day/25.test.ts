import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1, part2} from './25';

describe('part 1', () => {
	it('calculates the right value for example', () => {
		const raw_input = `
			jqt: rhn xhk nvd
			rsh: frs pzl lsr
			xhk: hfx
			cmg: qnr nvd lhk bvb
			rhn: xhk bvb hfx
			bvb: xhk hfx
			pzl: lsr hfx nvd
			qnr: nvd
			ntq: jqt hfx bvb xhk
			nvd: lhk
			lsr: lhk
			rzs: qnr cmg lsr rsh
			frs: qnr lhk lsr
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(3);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/25.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(602151);
	});

	it('calculates the right value for the modified input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/25_cut.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(21843);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', async () => {
		const raw_input = `
			19, 13, 30 @ -2,  1, -2
			18, 19, 22 @ -1, -1, -2
			20, 25, 34 @ -2, -2, -4
			12, 31, 28 @ -1, -2, -1
			20, 19, 15 @  1, -5, -3
		`.replace(/^[ \t]+/gm, '');

		expect(await part2(raw_input)).toBe(47);
	});

	it('calculates the right value for the input', async () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '25_cut.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(await part2(raw_input)).toBe(540355811503157);
	}, 30_000);
});
