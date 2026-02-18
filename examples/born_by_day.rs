use itertools::Itertools;

use gemau::DeadEnd;

fn main() {
    let mut days: [Vec<DeadEnd>; 5] = std::array::from_fn(|_| Vec::new());
    days[0] = vec![DeadEnd::ZERO];

    for i in 1..days.len() {
        days[i] = next_day(&days[i - 1]);
    }

    for (i, day) in days.iter().enumerate() {
        println!("day {i}: {}", day.len());
    }

    println!();
    println!("forms born by day 3 are:");
    for g in &days[3] {
        println!("{g}");
    }
}

fn next_day(day: &[DeadEnd]) -> Vec<DeadEnd> {
    let mut new_day = Vec::new();

    for subset in subsets(day) {
        let canonical = DeadEnd::with_options_unchecked(subset).canonical();
        if !new_day.contains(&canonical) {
            new_day.push(canonical);
        }
    }

    new_day
}

fn subsets<T: Clone>(slice: &[T]) -> impl Iterator<Item = Vec<T>> + '_ {
    slice.iter().cloned().powerset()
}
