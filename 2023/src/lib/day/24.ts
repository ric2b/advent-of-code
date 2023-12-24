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

export function part2(raw_input: string): number {
	return 2;
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
			// if (close_enough(b1, b2)) {
			// 	console.log('identical')
			// 	return false;
			// }
			// console.log('parallel')
			return false;
		}

		const cx = (b2 - b1) / (a1 - a2);
		const cy = cx * a1 + b1;

		const in_future = ((cx > this.position.x) === (this.speed.x > 0)) && ((cx > other.position.x) === (other.speed.x > 0))

		return in_future && box.includes(new Vector(cx, cy));
	}

	// px, py, vx, vy
	// x(t) = t*vx + px
	// y(t) = t*vy + py

	// intercept: x1(t) = x2(t) && y1(t) == y2(t)
	//
	// t*vx1 + px1 = t*vx2 + px2
	// t*(vx1-vx2) = px2 - px1
	// t = (px2 - px1) / (vx1 - vx2)
	//
	// t*vy1 + py1 = t*vy2 + py2
	// t*(vy1-vy2) = py2 - py1
	// t = (py2 - py1) / (vy1 - vy2)

	// m1x+ b1 = m2x + b2
	// vx1 * x + px1 = vx2 * x + px2
	// vx1 * x = vx2 * x + px2 - px1
	// (vx1 - vx2) * x = (px2 - px1)
	// x = (px2 - px1) / (vx1 - vx2)
}

class Box {
	public readonly x_range: Range;
	public readonly y_range: Range;

	constructor(start: Vector, end: Vector) {
		this.x_range = new Range(start.x, end.x);
		this.y_range = new Range(start.y, end.y);
	}

	overlap(other: Box) {
		return this.x_range.overlap(other.x_range) && this.y_range.overlap(other.y_range);
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
	// public readonly key: string;

	constructor(x: number, y: number) {
		this.x = x;
		this.y = y;
		// this.key = Vector.key(x, y);
	}

	add(other: Vector): Vector {
		return new Vector(this.x + other.x, this.y + other.y);
	}

	crossProduct(other: Vector): number {
		return this.x * other.y - other.x * this.y;
	}

	// static key(x: number, y: number): string {
	// 	return `${x},${y}`;
	// }
	//
	// static from_key(key: string): Vector {
	// 	const [x, y] = key.split(',').map(Number);
	// 	return new Vector(x, y);
	// }
}
