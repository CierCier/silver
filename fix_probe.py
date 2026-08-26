import re

p = "agc/src/bin/lexprobe.rs"
s = open(p).read()
s = s.replace("examples/casting.ag", "examples/array_init.ag")

old = """    for c0 in root.children() {
        println!("DEPTH1 kind={} leaf={} span={:?}", c0.kind(), c0.is_leaf(), c0.span());
    }"""
new = """    let fn_node = root.children().find(|c| c.kind() == 10).unwrap();
    let mut leaves: Vec<(u16, &str)> = Vec::new();
    fn_node.walk_leaves(&mut |k, t| leaves.push((k, t)));
    println!("fn leaves={}", leaves.len());
    for (k, t) in leaves.iter().rev().take(6).rev() {
        println!("  {:?} {:?}", t, k);
    }"""
assert old in s
s = s.replace(old, new)
open(p, "w").write(s)
print("patched")
