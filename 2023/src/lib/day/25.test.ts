import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1} from './25';

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

		// hfx/pzl, bvb/cmg, nvd/jqt
		// jqt: rhn xhk nvd

		expect(part1(raw_input)).toBe(54);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/25.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(602151);
	});
});
