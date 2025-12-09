use std::{collections::HashSet, fs, time::SystemTime};

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
struct Junction {
    x: i64,
    y: i64,
    z: i64,
}

fn main() {
    let file =
        fs::read_to_string("/Users/evanoverman/Documents/Repos/AdventOfCode2025/Day8.txt").unwrap();

    for _ in 0..100 {
        let start = SystemTime::now();

        let junctions: Vec<Junction> = file
            .lines()
            .map(|line| {
                let comps: Vec<&str> = line.split(',').collect();
                Junction {
                    x: comps[0].parse().unwrap(),
                    y: comps[1].parse().unwrap(),
                    z: comps[2].parse().unwrap(),
                }
            })
            .collect();

        let mut pairs_set: HashSet<(Junction, Junction, i64)> =
            HashSet::with_capacity(junctions.len().pow(2));

        for pair in pairs(&junctions) {
            pairs_set.insert(pair);
        }

        let mut pairs_sorted: Vec<(Junction, Junction, i64)> = Vec::with_capacity(pairs_set.len());

        for pair in pairs_set {
            pairs_sorted.push(pair);
        }

        pairs_sorted.sort_unstable_by_key(|x| x.2);

        let last = connect(junctions, pairs_sorted);

        println!("{}", last.0.x * last.1.x);

        let end = SystemTime::now();
        println!("Connection Took: {:?}", end.duration_since(start).unwrap());
    }
}

fn connect(
    junctions: Vec<Junction>,
    conns: Vec<(Junction, Junction, i64)>,
) -> (Junction, Junction) {
    let mut circuts: Vec<HashSet<Junction>> = Vec::with_capacity(junctions.len());

    for i in 0..junctions.len() {
        let mut hash = HashSet::new();
        hash.insert(junctions[i]);
        circuts.push(hash);
    }

    let mut last_conn: (Junction, Junction) =
        (Junction { x: 0, y: 0, z: 0 }, Junction { x: 0, y: 0, z: 0 });
    let mut idx = 0;

    while circuts.len() != 1 {
        let (junction_1, junction_2, _) = conns[idx];
        idx += 1;

        if junction_1 == junction_2 {
            continue;
        }

        if let Some(c1) = circuts.iter().position(|c| c.contains(&junction_1))
            && let Some(c2) = circuts.iter().position(|c| c.contains(&junction_2))
        {
            if c1 == c2 {
                continue;
            }

            let (c1, c2) = (c1.min(c2), c1.max(c2));
            let c = circuts.swap_remove(c2);

            for i in c {
                circuts[c1].insert(i);
            }

            last_conn = (junction_1, junction_2);
        }
    }

    last_conn
}

#[inline(always)]
fn pairs(ts: &[Junction]) -> Vec<(Junction, Junction, i64)> {
    let mut pairs: Vec<(Junction, Junction, i64)> = Vec::with_capacity(ts.len().pow(2));

    for i in 0..ts.len() {
        for j in i..ts.len() {
            pairs.push((ts[i], ts[j], distance(ts[i], ts[j])));
        }
    }

    pairs
}

#[inline(always)]
fn distance(
    Junction { x, y, z }: Junction,
    Junction {
        x: x1,
        y: y1,
        z: z1,
    }: Junction,
) -> i64 {
    let x = x1 - x;
    let y = y1 - y;
    let z = z1 - z;
    x * x + y * y + z * z
}
