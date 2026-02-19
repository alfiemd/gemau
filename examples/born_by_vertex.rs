use itertools::Itertools;
use std::time::Instant;

use gemau::DeadEnd;

const DEFAULT_MAX_VERTICES: usize = 10;

fn main() {
    let max_vertices = std::env::args()
        .nth(1)
        .and_then(|arg| arg.parse::<usize>().ok())
        .unwrap_or(DEFAULT_MAX_VERTICES);

    if max_vertices == 0 {
        println!("It's not very interesting with 0 vertices!");
        return;
    }

    let start = Instant::now();
    let games_by_vertex = build_games_by_vertex(max_vertices);
    let elapsed = start.elapsed();

    println!("Dead ends with exactly n vertices");
    let width = max_vertices.to_string().len();
    for (vertex_count, games) in games_by_vertex.iter().enumerate().skip(1) {
        println!("n={vertex_count:>width$}: {}", games.len(), width = width);
    }
    println!();
    println!("Generated up to n = {max_vertices} in {:.3?}", elapsed);
}

/// Returns buckets where index `i` stores all canonical forms with exactly `i` vertices.
fn build_games_by_vertex(max_vertices: usize) -> Vec<Vec<DeadEnd>> {
    let mut buckets = vec![Vec::new(); max_vertices + 1];
    buckets[1] = vec![DeadEnd::ZERO];

    for v in 2..=max_vertices {
        buckets[v] = build_vertex_bucket(v, &buckets);
    }

    buckets
}

/// Builds a vertex bucket by exploring all option sets whose vertices sum to `n-1`.
fn build_vertex_bucket(vertices: usize, buckets: &[Vec<DeadEnd>]) -> Vec<DeadEnd> {
    let rem_vertices = vertices - 1;

    let mut new_bucket = Vec::new();
    let mut chosen_options: Vec<&DeadEnd> = Vec::new();

    explore_option_sets(
        rem_vertices,
        rem_vertices,
        buckets,
        &mut chosen_options,
        &mut new_bucket,
    );

    new_bucket
}

/// Depth-first search over option choices.
///
/// - `current_option_size` is the  current vertex count of options we are considering.
/// - `rem_vertices` is exactly how many vertices are still required.
fn explore_option_sets<'a>(
    current_option_size: usize,
    rem_vertices: usize,
    buckets: &'a [Vec<DeadEnd>],
    chosen_options: &mut Vec<&'a DeadEnd>,
    new_bucket: &mut Vec<DeadEnd>,
) {
    if rem_vertices == 0 {
        // If any chosen option is dominated, then the canonical form would strictly reduce the
        // vertex count.
        if !chosen_options
            .iter()
            .all(|g| !chosen_options.iter().any(|h| h > g))
        {
            return;
        }

        new_bucket.push(DeadEnd::with_options_unchecked(
            chosen_options.iter().map(|&option| option.clone()),
        ));
        return;
    }
    if current_option_size == 0 {
        return;
    }

    // Case 1: choose no options of this size.
    explore_option_sets(
        current_option_size - 1,
        rem_vertices,
        buckets,
        chosen_options,
        new_bucket,
    );

    // Case 2: choose one or more options of this size.
    let size_bucket = &buckets[current_option_size];
    let max_pick_count = (rem_vertices / current_option_size).min(size_bucket.len());

    for pick_count in 1..=max_pick_count {
        for picked in size_bucket.iter().combinations(pick_count) {
            let checkpoint = chosen_options.len();
            chosen_options.extend(picked);

            explore_option_sets(
                current_option_size - 1,
                rem_vertices - pick_count * current_option_size,
                buckets,
                chosen_options,
                new_bucket,
            );

            chosen_options.truncate(checkpoint);
        }
    }
}
