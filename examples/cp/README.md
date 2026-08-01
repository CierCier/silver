# Competitive Programming Examples

Silver examples written the way competitive-programming solutions are:
a stdin scanner, generic algorithms, and collections.

Compile any example with `cargo run -p agc -- <file> -O2 -o <binary>` and
pipe the sample input. All examples read from stdin and write to stdout.

## A + B

```bash
cargo run -p agc -- examples/cp/a_plus_b.ag -O2 -o /tmp/a_plus_b
printf '2 40\n' | /tmp/a_plus_b
```

Expected output:

```
42
```

## Sort N numbers

```bash
cargo run -p agc -- examples/cp/sort_numbers.ag -O2 -o /tmp/silver_sort_numbers
printf '5\n4 1 5 1 3\n' | /tmp/silver_sort_numbers
```

Expected output:

```
1
1
3
4
5
```

## Frequency count

```bash
cargo run -p agc -- examples/cp/frequency_count.ag -O2 -o /tmp/frequency_count
printf '6\n1 2 2 3 3 3\n3\n1\n2\n4\n' | /tmp/frequency_count
```

Expected output:

```
1
2
0
```

## Grid BFS

`0` is passable, `1` is a wall. Prints the shortest path length from
`(0,0)` to `(rows-1, cols-1)`, or `-1` if unreachable.

```bash
cargo run -p agc -- examples/cp/grid_bfs.ag -O2 -o /tmp/grid_bfs
printf '3 3\n0 0 1\n0 1 0\n0 0 0\n' | /tmp/grid_bfs
```

Expected output:

```
4
```

## Dijkstra

`BinaryHeap` is a max-heap, so the example implements `Lt<Node>` with
reversed distance ordering to get min-heap behavior. Prints the shortest
distance from node 0 to every node (`-1` if unreachable).

```bash
cargo run -p agc -- examples/cp/dijkstra.ag -O2 -o /tmp/dijkstra
printf '4 4\n0 1 10\n0 2 3\n2 1 1\n1 3 2\n' | /tmp/dijkstra
```

Expected output:

```
0
4
3
6
```
