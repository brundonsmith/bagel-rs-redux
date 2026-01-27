use nom::error::ErrorKind;

use crate::{
    ast::{container::AST, grammar::Any, slice::Slice},
    parse::utils::{ParseResult, RawParseError, RawParseErrorDetails},
};

use precedence_index::precedence_index;

fn precedence_inner<TKind>(
    levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
    after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
    i: Slice,
) -> ParseResult<AST<TKind>>
where
    TKind: 'static + Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    let start_index = precedence_index(levels, after);
    for level in &levels[start_index..] {
        for f in *level {
            let res = f(i.clone());

            if res.is_ok() {
                return res;
            }
        }
    }

    Err(nom::Err::Error(RawParseError {
        src: i,
        details: RawParseErrorDetails::Kind(ErrorKind::Fail),
    }))
}

std::thread_local! {
  static MEMOIZED_MAPPING_PRECEDENCE: std::cell::RefCell<std::collections::HashMap<(*const u8, Option<*const u8>, Slice), ParseResult<AST<Any>>>>  = std::cell::RefCell::new(std::collections::HashMap::new());
}

#[allow(unused_variables)]
pub fn precedence<TKind>(
    levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
    after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
    i: Slice,
) -> ParseResult<AST<TKind>>
where
    TKind: 'static + Clone + TryFrom<Any>,
    Any: From<TKind>,
{
    let levels_ptr = levels.as_ptr().cast::<u8>();
    let after_ptr = after.map(|after| after as *const u8);

    let r = MEMOIZED_MAPPING_PRECEDENCE.with(|hm| {
        let hm = hm.borrow_mut();
        hm.get(&(levels_ptr, after_ptr, i.clone())).cloned()
    });
    if let Some(r) = r {
        return r.map(|(s, ast)| (s, ast.try_downcast::<TKind>().unwrap()));
    }
    let r = precedence_inner(levels, after, i.clone());
    MEMOIZED_MAPPING_PRECEDENCE.with(|hm| {
        let mut hm: std::cell::RefMut<
            std::collections::HashMap<(*const u8, Option<*const u8>, Slice), ParseResult<AST<Any>>>,
        > = hm.borrow_mut();
        hm.insert(
            (levels_ptr, after_ptr, i),
            r.clone().map(|(s, ast)| (s, ast.upcast::<Any>())),
        );
    });
    r
}

mod precedence_index {

    use crate::ast::{container::AST, grammar::Any, slice::Slice};

    use super::ParseResult;

    fn precedence_index_inner<TKind>(
        levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
        after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
    ) -> usize
    where
        TKind: 'static + Clone + TryFrom<Any>,
        Any: From<TKind>,
    {
        match after {
            Some(after) => {
                levels
                    .iter()
                    .take_while(|level| !level.contains(&after))
                    .count()
                    + 1
            }
            None => 0,
        }
    }

    std::thread_local! {
      static MEMOIZED_MAPPING_PRECEDENCE_INDEX: std::cell::RefCell<std::collections::HashMap<(*const u8, Option<*const u8>), usize>>  = std::cell::RefCell::new(std::collections::HashMap::new());
    }

    #[allow(unused_variables)]
    pub fn precedence_index<TKind>(
        levels: &'static [&'static [fn(Slice) -> ParseResult<AST<TKind>>]],
        after: Option<fn(Slice) -> ParseResult<AST<TKind>>>,
    ) -> usize
    where
        TKind: 'static + Clone + TryFrom<Any>,
        Any: From<TKind>,
    {
        let levels_ptr = levels.as_ptr().cast::<u8>();
        let after_ptr = after.map(|after| after as *const u8);

        let r = MEMOIZED_MAPPING_PRECEDENCE_INDEX.with(|hm| {
            let hm = hm.borrow_mut();
            hm.get(&(levels_ptr, after_ptr)).cloned()
        });
        if let Some(r) = r {
            return r;
        }
        let r = precedence_index_inner(levels, after);
        MEMOIZED_MAPPING_PRECEDENCE_INDEX.with(|hm| {
            let mut hm: std::cell::RefMut<
                std::collections::HashMap<(*const u8, Option<*const u8>), usize>,
            > = hm.borrow_mut();
            hm.insert((levels_ptr, after_ptr), r);
        });
        r
    }
}
