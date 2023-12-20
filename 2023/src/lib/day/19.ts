type WorkflowId = string

interface Rule {
	condition?: Condition;
	match_result: Status | WorkflowId;
}

interface Condition {
	type: Type;
	range: Range;
}

type Rating = Map<Type, number>
type Status = 'A'|'R';
type Type = 'x'|'m'|'a'|'s';

export function part1(raw_input: string): number {
	const [raw_workflows, raw_ratings] = raw_input.trim().split('\n\n')

	const workflows = parse_workflows(raw_workflows);

	const ratings: Rating[] = raw_ratings.split('\n').map(line => {
		const rating: Rating = new Map();
		line.slice(1, -1).split(',').forEach(raw_rating => {
			const [type, raw_value]: [Type, string] = raw_rating.split('=')
			rating.set(type, Number(raw_value));
		});
		return rating;
	});

	return ratings.map(rating => apply_workflow(workflows, rating)).reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	// work backwards and find how each A can be reached?
	const [raw_workflows,] = raw_input.trim().split('\n\n')

	const workflows = parse_workflows(raw_workflows);

	const workflow_graph: Map<WorkflowId, Node> = new Map();
	workflows.forEach((rules, workflow_id) => {
		const workflow_node: Node = rules.reverse().reduce((else_node: Node | Status | WorkflowId, rule: Rule) => {
			return new Node(
				rule.condition ? rule.condition : { x: new Range(1, 4001) },
				rule.match_result,
				else_node
			)
		}, 'R')

		workflow_graph.set(workflow_id, workflow_node);
	});

	const root = workflow_graph.get('in');
	const valid_ratings: RatingRanges[] =  combinations(workflow_graph, root);

	// px{a<2006:qkq,m>2090:A,rfg}
	// pv{a>1716:R,A}
	// lnx{m>1548:A,A}
	// rfg{s<537:gd,x>2440:R,A}
	// qs{s>3448:A,lnx}
	// qkq{x<1416:A,crn}
	// crn{x>2662:A,R}
	// in{s<1351:px,qqz}
	// qqz{s>2770:qs,m<1801:hdj,R}
	// gd{a>3333:R,R}
	// hdj{m>838:A,pv}

	return 2;
}

function combinations(workflow_graph: Map<WorkflowId, Node>, root: Node | Status | WorkflowId): RatingRanges[] {
	if (root === 'A') {
		return [new RatingRanges()];
	} else if (root === 'R') {
		return [];
	} else if (typeof root === 'string') {
		root = workflow_graph.get(root);
	}

	const condition_ranges = new RatingRanges(root.condition);
	return [
		combinations(workflow_graph, root.match_node).flatMap(r => condition_ranges.combine(r)),
		combinations(workflow_graph, root.else_node).flatMap(r => condition_ranges.invert().combine(r)),
	].flat();
}

class RatingRanges {
	public readonly x: Range[];
	public readonly m: Range[];
	public readonly a: Range[];
	public readonly s: Range[];

	constructor(ranges = {}) {
		this.x = ranges.x != undefined ? [ranges.x] : [new Range(1, 4001)];
		this.m = ranges.m != undefined ? [ranges.m] : [new Range(1, 4001)];
		this.a = ranges.a != undefined ? [ranges.a] : [new Range(1, 4001)];
		this.s = ranges.s != undefined ? [ranges.s] : [new Range(1, 4001)];
	}

	combine(other: RatingRanges): RatingRanges {
		return new RatingRanges({
			x: this.x.flatMap(r => other.x.map(o => r.intersect(o))),
			m: this.m.flatMap(r => other.m.map(o => r.intersect(o))),
			a: this.a.flatMap(r => other.a.map(o => r.intersect(o))),
			s: this.s.flatMap(r => other.s.map(o => r.intersect(o))),
		})
	}

	invert(): RatingRanges {
		return new RatingRanges({
			x: this.x.map(r => r.invert()),
			m: this.m.map(r => r.invert()),
			a: this.a.map(r => r.invert()),
			s: this.s.map(r => r.invert()),
		})
	}
}

class Node {
	public readonly condition: Condition;
	public readonly match_node: Status | WorkflowId;
	public readonly else_node: Node | Status | WorkflowId;

	constructor(condition: Condition, match_node: Status | WorkflowId, else_node: Node | Status | WorkflowId) {
		this.condition = condition;
		this.match_node = match_node;
		this.else_node = else_node;
	}
}

class Range {
	public readonly start: number;
	public readonly end: number;

	constructor(start: number, end: number) {
		this.start = Math.min(start, end);
		this.end = Math.max(start, end);
	}

	include(x: number): boolean {
		return this.start <= x && x < this.end;
	}

	intersect(other: Range): Range {
		if (this.start <= other.end && other.start <= this.end) {
			return new Range(Math.max(this.start, other.start), Math.min(this.end, other.end));
		} else {
			return new Range(0, 0);
		}
	}
	invert(): Range[] {
		return [
			new Range(1, this.start),
			new Range(this.end, 4001),
		].filter(r => r.length() > 0);
	}

	length(): number {
		return this.end - this.start;
	}
}

function apply_workflow(workflows: Map<string, Rule[]>, rating: Rating, workflow_id = 'in'): number {
	for (const rule: Rule of workflows.get(workflow_id)) {
		let result;

		if (rule.condition == undefined) {
			result = rule.match_result;
		} else {
			if (rule.condition.range.include(rating.get(rule.condition.type))) {
				result = rule.match_result;
			}
		}

		if (result == 'A') {
			return [...rating.values()].reduce((a, b) => a + b);
		} else if (result == 'R') {
			return 0;
		} else if (result != undefined) {
			return apply_workflow(workflows, rating, result);
		}
	}

	throw new Error('bug');
}

function parse_workflows(raw_workflows: string): Map<WorkflowId, Rule[]> {
	const workflows: Map<WorkflowId, Rule[]> = new Map();

	raw_workflows.split('\n').forEach(line => {
		const [, id, raw_rules] = /(?<id>\w+)\{(?<raw_rules>[\w,:<>]+)\}/.exec(line);
		const rules: Rule[] = raw_rules.split(',').map(raw_rule => {
			const rule_regex = /((?<rating_type>[xmas])(?<comparison>[<>])(?<value>\d+):)?(?<match_result>\w+)/;
			const [,, type, comparison, raw_value, match_result] = rule_regex.exec(raw_rule);

			if (type != undefined) {
				const value = Number(raw_value);
				const range = comparison == '<' ? new Range(1, value) : new Range(value + 1, 4000);

				return {
					condition: { type, range },
					match_result,
				};
			} else {
				return { match_result };
			}
		})
		workflows.set(id, rules);
	});

	return workflows;
}
