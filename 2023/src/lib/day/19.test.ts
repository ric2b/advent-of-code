import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './19';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			px{a<2006:qkq,m>2090:A,rfg}
			pv{a>1716:R,A}
			lnx{m>1548:A,A}
			rfg{s<537:gd,x>2440:R,A}
			qs{s>3448:A,lnx}
			qkq{x<1416:A,crn}
			crn{x>2662:A,R}
			in{s<1351:px,qqz}
			qqz{s>2770:qs,m<1801:hdj,R}
			gd{a>3333:R,R}
			hdj{m>838:A,pv}
			
			{x=787,m=2655,a=1222,s=2876}
			{x=1679,m=44,a=2067,s=496}
			{x=2036,m=264,a=79,s=2244}
			{x=2461,m=1339,a=466,s=291}
			{x=2127,m=1623,a=2188,s=1013}
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(19114);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/19.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(397643);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			px{a<2006:qkq,m>2090:A,rfg}
			pv{a>1716:R,A}
			lnx{m>1548:A,A}
			rfg{s<537:gd,x>2440:R,A}
			qs{s>3448:A,lnx}
			qkq{x<1416:A,crn}
			crn{x>2662:A,R}
			in{s<1351:px,qqz}
			qqz{s>2770:qs,m<1801:hdj,R}
			gd{a>3333:R,R}
			hdj{m>838:A,pv}
			
			{x=787,m=2655,a=1222,s=2876}
			{x=1679,m=44,a=2067,s=496}
			{x=2036,m=264,a=79,s=2244}
			{x=2461,m=1339,a=466,s=291}
			{x=2127,m=1623,a=2188,s=1013}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(167409079868000);
	});

	it('calculates the right value for custom example a1', () => {
		const raw_input = `
			in{x<2000:pa,R}
			pa{x<1000:pb,R}
			pb{x<500:pc,R}
			pc{x<100:pd,R}
			pd{x<2:A,R}

			{x=1,m=2,a=3,s=4}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(1 * 4000 * 4000 * 4000);
	});

	it('calculates the right value for custom example a2', () => {
		const raw_input = `
			in{x<1351:px,R}
			px{m<2006:k,R}
			k{a>2090:qkq,R}
			qkq{s<1416:A,R}
			
			{x=1,m=2,a=3,s=4}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(1350 * 2005 * (4000 - 2090) * 1415);
	});

	it('calculates the right value for custom example 1a', () => {
		const raw_input = `
			in{x}
			x{x<1:m,R}
			m{m<1:a,R}
			a{a<1:s,R}
			s{s<1:A,R}
			
			{x=1,m=2,a=3,s=4}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(0);
	});

	it('calculates the right value for custom example 1', () => {
		const raw_input = `
			in{x}
			x{x<2:m,R}
			m{m<2:a,R}
			a{a<2:s,R}
			s{s<2:A,R}
			
			{x=1,m=2,a=3,s=4}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(1);
	});

	it('calculates the right value for custom example 2', () => {
		const raw_input = `
			in{x}
			x{x<3:m,R}
			m{m<3:a,R}
			a{a<3:s,R}
			s{s<3:A,R}
			
			{x=1,m=2,a=3,s=4}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(2*2*2*2);
	});

	it('calculates the right value for custom example 3', () => {
		const raw_input = `
			in{x}
			x{x>3999:m,R}
			m{m>3999:a,R}
			a{a>3999:s,R}
			s{s>3999:A,R}
			
			{x=1,m=2,a=3,s=4}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(1);
	});

	it('calculates the right value for custom example 4', () => {
		const raw_input = `
			in{x}
			x{x>4000:m,R}
			m{m>4000:a,R}
			a{a>4000:s,R}
			s{s>4000:A,R}
			
			{x=1,m=2,a=3,s=4}
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(0);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '19.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(132392981697081);
	});
});
