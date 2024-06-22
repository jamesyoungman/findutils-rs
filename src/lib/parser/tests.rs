// Parser tests.
use std::collections::VecDeque;

mod and;
mod comma;
mod or;
mod parens;
mod program;

fn make_vdq<T, I>(items: I) -> VecDeque<T>
where
    I: IntoIterator<Item = T>,
{
    items.into_iter().collect()
}
