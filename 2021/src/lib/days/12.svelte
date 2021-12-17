<!-- {#each [...part1_result] as [node, edges]}
    <input value="{node}"/>
    {#each [...edges] as edge}
        <input value="{edge}"/>
    {/each}
    <br>
{/each} -->

<!-- 
+ start,A,b,A,c,A,end
+ start,A,b,A,end
+ start,A,b,end
+ start,A,c,A,b,A,end
+ start,A,c,A,b,end
+ start,A,c,A,end
+ start,A,end
+ start,b,A,c,A,end
+ start,b,A,end
+ start,b,end 
-->

<script>
    const parse = raw_input => raw_input.split('\n').filter(l => l !== '').map(e => e.split('-'));

    function part1(edges) {
        const graph = edges.flat().reduce((g, u) => g.set(u, new Set()), new Map());
        edges.reduce((g, [u, v]) => { g.get(u).add(v); g.get(v).add(u); return g; }, graph);

        const visited = new Set(), current_path = [], paths = [];

        const dfs = (u, end) => {
            if (visited.has(u) && u !== u.toUpperCase()) return;

            visited.add(u);
            current_path.push(u);

            if (u === end) {
                paths.push(current_path.slice());
                visited.delete(u);
                current_path.pop();
                return;
            }
            
            graph.get(u)?.forEach(v => dfs(v, end));
            current_path.pop();
            visited.delete(u);
        }

        dfs('start', 'end')

        return paths.length;
    }

    function part2(edges) {
        //return 43;
        const graph = edges.flat().reduce((g, u) => g.set(u, new Set()), new Map());
        edges.reduce((g, [u, v]) => { g.get(u).add(v); g.get(v).add(u); return g; }, graph);

        const visited = new Set(), stack = ['start'], current_path = [], paths = [];
        let double_visit = null;

        const dfs = (u, end) => {
            if (visited.has(u) && u !== u.toUpperCase()) {
                if (u === 'start' || double_visit !== null) return;
                
                double_visit = u;
            } 

            visited.add(u);
            current_path.push(u);

            if (u === end) {
                paths.push(current_path.slice());
                visited.delete(u);
                current_path.pop();
                return;
            }
            
            graph.get(u).forEach(v => dfs(v, end));
            current_path.pop();
            
            if (double_visit === u) {
                double_visit = null;  
            } else {
                visited.delete(u);
            }
        }

        dfs('start', 'end')
        return paths.length;
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
