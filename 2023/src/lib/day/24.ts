import { init } from 'z3-solver';

export function part1(raw_input: string, min: number = 200_000_000_000_000, max: number = 400_000_000_000_000): number {
	const hailstones: Hailstone[] = raw_input.trim().split('\n').map((line, i) => {
		const [x, y, z, dx, dy, dz] = line.match(/(-?\d+)/g).map(Number);
		return new Hailstone(new Vector(x, y), new Vector(dx, dy), i);
	});

	const bounding_box = new Box(new Vector(min, min), new Vector(max, max));

	let count = 0;
	for (let i = 0; i < hailstones.length - 1; i++) {
		for (let j = i + 1; j < hailstones.length; j++) {
			if (hailstones[i].intersect(hailstones[j], bounding_box)) {
				count++;
			}
		}
	}

	return count;
}

export async function part2(raw_input: string): Promise<number> {
	const hailstones: Hailstone[] = raw_input.trim().split('\n').map((line, i) => {
		const [x, y, z, dx, dy, dz] = line.match(/(-?\d+)/g).map(Number);
		return new Hailstone(new Vector(x, y, z), new Vector(dx, dy, dz), i);
	});

	// return solve(hailstones);
	return solve(hailstones.slice(0,3)); // seems to be enough and runs much faster
}

async function solve(hailstones: Hailstone[]): Promise<number> {
	const { Context } = await init();
	const { Solver, Int } = new Context('main');
	const solver = new Solver();

	const [x, y, z] = [Int.const('x'), Int.const('y'), Int.const('z')];
	const [dx, dy, dz] = [Int.const('dx'), Int.const('dy'), Int.const('dz')];
	const t = hailstones.map((_, i) => Int.const(`t${i}`));

	hailstones.forEach((h, i) => {
		solver.add(t[i].mul(h.speed.x).add(h.position.x).sub(x).sub(t[i].mul(dx)).eq(0));
		solver.add(t[i].mul(h.speed.y).add(h.position.y).sub(y).sub(t[i].mul(dy)).eq(0));
		solver.add(t[i].mul(h.speed.z).add(h.position.z).sub(z).sub(t[i].mul(dz)).eq(0));
	})

	await solver.check();
	return Number(solver.model().eval(x.add(y).add(z)).value());
}

function close_enough(a: number, b: number): boolean {
	return Math.abs(a - b) < 1e-10;
}

class Hailstone {
	public readonly position: Vector;
	public readonly speed: Vector;
	public readonly id: number;

	constructor(position: Vector, speed: Vector, id: number) {
		this.position = position;
		this.speed = speed;
		this.id = id;
	}

	intersect(other: Hailstone, box: Box): boolean {
		const a1 = this.speed.y / this.speed.x;
		const b1 = this.position.y - a1 * this.position.x;

		const a2 = other.speed.y / other.speed.x;
		const b2 = other.position.y - a2 * other.position.x;

		if (close_enough(a1, a2)) {
			return false;
		}

		const cx = (b2 - b1) / (a1 - a2);
		const cy = cx * a1 + b1;

		const in_future = ((cx > this.position.x) === (this.speed.x > 0)) && ((cx > other.position.x) === (other.speed.x > 0))

		return in_future && box.includes(new Vector(cx, cy));
	}
}

class Box {
	public readonly x_range: Range;
	public readonly y_range: Range;

	constructor(start: Vector, end: Vector) {
		this.x_range = new Range(start.x, end.x);
		this.y_range = new Range(start.y, end.y);
	}

	includes(point: Vector): boolean {
		return this.x_range.includes(point.x) && this.y_range.includes(point.y);
	}
}

class Range {
	public readonly start: number;
	public readonly end: number;

	constructor(start: number, end: number) {
		this.start = Math.min(start, end);
		this.end = Math.max(start, end);
	}

	overlap(other: Range): boolean {
		return this.start <= other.end && this.end >= other.start;
	}

	includes(value: number): boolean {
		return value >= this.start && value <= this.end;
	}

	*iterator() {
		for (let i = this.start; i <= this.end; i++) {
			yield i;
		}
	}

	[Symbol.iterator]() {
		return this.iterator();
	}
}

class Vector {
	public readonly x: number;
	public readonly y: number;
	public readonly z: number;

	constructor(x: number, y: number, z: number = 0) {
		this.x = x;
		this.y = y;
		this.z = z;
	}
}
