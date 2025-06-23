use rustler::{Env, NifResult, Term, Encoder};
use rustler::types::atom::ok;

#[rustler::nif]
fn add<'a>(env: Env<'a>, a: i64, b: i64) -> NifResult<Term<'a>> {
    Ok((ok(), a + b).encode(env))
}

rustler::init!("dev_add_nif", [add]);
