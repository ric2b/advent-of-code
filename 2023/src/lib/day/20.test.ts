import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1, part2, part2_cycle_lengths, part2_render} from './20';

describe('part 1', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			broadcaster -> a, b, c
			%a -> b
			%b -> c
			%c -> inv
			&inv -> a
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(32000000);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			broadcaster -> a
			%a -> inv, con
			&inv -> b
			%b -> con
			&con -> output
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(11687500);
	});

	it('calculates the right value for example 3', () => {
		const raw_input = `
			broadcaster -> a, b
			%a -> con
			%b -> con
			&con -> output 
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(11250000);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/20.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		// 619965920 is too low
		// 620179588 is too low

		expect(part1(raw_input)).toBe(711650489);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			broadcaster -> a, b, c
			%a -> b
			%b -> c
			%c -> inv
			&inv -> a
		`.replace(/^[ \t]+/gm, '');

		expect(part2_render(raw_input)).toBe(1);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			broadcaster -> a
			%a -> inv, con
			&inv -> b
			%b -> con
			&con -> output
		`.replace(/^[ \t]+/gm, '');

		expect(part2_render(raw_input)).toBe(1);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '20.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		// for rx to go low: ks & pm & dl & vk all have to be high

		// ks: cycle of 3917, first on at 3916
		// pm: cycle of 3833, first on at 3832
		// dl: cycle of 3769 , first on at 3768
		// vk: cycle of 3877, first on at 3876

		// LCM = 219388737656593

		expect(part2(raw_input)).toBe(219388737656593);
	});
});
