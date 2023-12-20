import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './20';

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

		expect(part2(raw_input)).toBe(167409079868000);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			broadcaster -> a
			%a -> inv, con
			&inv -> b
			%b -> con
			&con -> output
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(167409079868000);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '20.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(78242031808225);
	});
});
