use rustler::{Env, NifResult, Term, Encoder};
use rustler::types::atom::ok;

fn _query(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif]
fn query<'a>(env: Env<'a>, a: i64, b: i64) -> NifResult<Term<'a>> {
    Ok((ok(), _query(a, b)).encode(env))
}

rustler::init!("dev_weavedb_nif", [query]);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = _query(2, 2);
        assert_eq!(result, 4);
    }
}
