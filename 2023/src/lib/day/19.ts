type WorkflowId = string

interface Rule {
	condition?: Condition;
	match_result: Status | WorkflowId;
}

type Rating = Map<Type, number>
type Status = 'A'|'R';
type Type = 'x'|'m'|'a'|'s';
type Operator = '<'|'>';

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
			return new Node(rule.condition, rule.match_result, else_node)
		}, 'R')

		workflow_graph.set(workflow_id, workflow_node);
	});

	const root = workflow_graph.get('in');
	const valid_ratings: RatingRanges[] =  combinations(workflow_graph, root);

	return valid_ratings.map(rr => rr.volume()).reduce((a, b) => a + b, 0);
}

function combinations(workflow_graph: Map<WorkflowId, Node>, root: Node | Status | WorkflowId): RatingRanges[] {
	if (root === 'A') {
		return [new RatingRanges()];
	} else if (root === 'R') {
		return [];
	}

	if (typeof root === 'string') {
		root = workflow_graph.get(root);
	}

	if (root.condition == undefined) {
		return combinations(workflow_graph, root.match_node);
	}

	const match_combinations = combinations(workflow_graph, root.match_node).flatMap(rr => rr.section_matching(root.condition));
	const else_combinations = combinations(workflow_graph, root.else_node).flatMap(rr => rr.section_matching(root.condition.invert()));

	return match_combinations.concat(...else_combinations).filter(rr => rr.volume() > 0);
}

class RatingRanges {
	public readonly x: Range;
	public readonly m: Range;
	public readonly a: Range;
	public readonly s: Range;

	constructor(ranges = {}) {
		this.x = ranges.x != undefined ? ranges.x : new Range(1, 4001);
		this.m = ranges.m != undefined ? ranges.m : new Range(1, 4001);
		this.a = ranges.a != undefined ? ranges.a : new Range(1, 4001);
		this.s = ranges.s != undefined ? ranges.s : new Range(1, 4001);
	}

	section_matching(condition: Condition): RatingRanges {
		const split_axis_type = condition.type;
		const split_axis_range = this[split_axis_type];

		const matching_range: Range = split_axis_range.section_matching(condition);

		const props = this.to_object();
		props[split_axis_type] = matching_range;

		return new RatingRanges(props);
	}

	to_object() {
		return { x: this.x, m: this.m, a: this.a, s: this.s };
	}

	volume(): number {
		return this.x.length() * this.m.length() * this.a.length() * this.s.length();
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
		this.start = Math.max(1, Math.min(start, end));
		this.end = Math.min(4001, Math.max(start, end));
	}

	section_matching(condition: Condition): Range {
		let new_range;
		switch (condition.operator) {
			case '<':
				new_range = new Range(Math.min(this.start, condition.value - 1), Math.min(this.end, condition.value));
				return new_range;
			case '>':
				new_range = new Range(Math.max(this.start, condition.value + 1), Math.max(this.end, condition.value));
				return new_range;
			default:
				throw new Error('bug');
		}
	}

	length(): number {
		return this.end - this.start;
	}
}

class Condition {
	public readonly type: Type;
	public readonly operator: Operator;
	public readonly value: number;

	constructor(type: Type, operator: Operator, value: number) {
		this.type = type;
		this.operator = operator;
		this.value = value;
	}

	matches(value: number): boolean {
		switch (this.operator) {
			case "<":
				return value < this.value;
			case ">":
				return value > this.value;
			default:
				throw new Error('bug');
		}
	}

	invert(): Condition {
		switch (this.operator) {
			case '<':
				return new Condition(this.type, '>', this.value - 1);
			case '>':
				return new Condition(this.type, '<', this.value + 1);
			default:
				throw new Error('bug');
		}
	}
}

function apply_workflow(workflows: Map<string, Rule[]>, rating: Rating, workflow_id = 'in'): number {
	for (const rule: Rule of workflows.get(workflow_id)) {
		let result;

		if (rule.condition == undefined) {
			result = rule.match_result;
		} else {
			const rating_value = rating.get(rule.condition.type);
			if (rule.condition.matches(rating_value)) {
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
				return {
					condition: new Condition(type, comparison, Number(raw_value)),
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
