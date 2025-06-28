use std::collections::{BTreeMap, HashMap, HashSet};
use std::iter;

use unsynn::{
    And, BraceGroupContaining, BracketGroupContaining, Cached, Caret, Comma, CommaDelimited, Cons,
    DelimitedVec, Dot, DotDelimited, DotDotEq, Either, Equal, FatArrow, IParse, Ident,
    LiteralString, Nothing, ParenthesisGroupContaining, Parser, PunctAny, Semicolon,
    SemicolonDelimited, Span, ToTokens, TokenIter, TokenStream,
};

use crate::model::{IfGuard, LetExpr, MatchArm, MethodCall};

mod model;

#[derive(Debug)]
struct Key(LiteralString);

impl Key {
    fn as_str(&self) -> &str {
        self.0.as_str()
    }

    fn as_bytes(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for Key {}

impl std::hash::Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl PartialOrd for Key {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Key {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Parser for Key {
    fn parser(tokens: &mut TokenIter) -> unsynn::Result<Self> {
        LiteralString::parser(tokens).map(Self)
    }
}

impl ToTokens for Key {
    fn to_tokens(&self, tokens: &mut unsynn::TokenStream) {
        self.0.to_tokens(tokens);
    }
}

impl ToTokens for &'_ Key {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        Key::to_tokens(*self, tokens);
    }
}

#[derive(Debug)]
struct ValueStream(TokenStream);

impl Parser for ValueStream {
    fn parser(tokens: &mut TokenIter) -> unsynn::Result<Self> {
        let ptokens = tokens
            .clone()
            .take_while(|it| PunctAny::<','>::parser(&mut it.to_token_iter()).is_err());

        let mut mtokens = TokenStream::new();
        mtokens.extend(ptokens);

        // skip that many tokens
        for _ in tokens.take(mtokens.clone().into_iter().count()) {}

        Ok(Self(mtokens))
    }
}

impl ToTokens for ValueStream {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}

unsynn::unsynn! {
    keyword Let = "let";
    keyword If = "if";
    keyword Else = "else";
    keyword Match = "match";

    struct Entry {
        key: Key,
        kv_sep: FatArrow,
        value: ValueStream,
    }

    struct Input {
        key: Ident,
        key_sep: Comma,
        entries: unsynn::CommaDelimitedVec<Entry>,
    }
}

#[proc_macro]
pub fn tiny_map(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: Input = unsynn::TokenStream::from(input).to_token_iter().parse().unwrap();

    build_tiny_map(
        input.key,
        input
            .entries
            .0
            .iter()
            .map(|entry| (&entry.value.key, Value::Some(&entry.value.value.0)))
            .collect::<HashMap<_, _>>(),
        false,
    )
    .into()
}

#[proc_macro]
pub fn tiny_map_ignore_case(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: Input = unsynn::TokenStream::from(input).to_token_iter().parse().unwrap();

    build_tiny_map(
        input.key,
        input
            .entries
            .0
            .iter()
            .map(|entry| (&entry.value.key, Value::Some(&entry.value.value.0)))
            .collect::<HashMap<_, _>>(),
        true,
    )
    .into()
}

#[derive(Clone)]
enum Value<'a> {
    // TokenTree(&'a TokenTree),
    // TokenStream(&'a TokenStream),
    Some(&'a TokenStream),
    Table(Box<Table<'a>>),
}

#[derive(Debug, Clone)]
pub(crate) enum Algorithm {
    Position { idx: usize },
    Xor { idx1: usize, idx2: usize },
}

#[derive(Clone)]
pub(crate) struct Table<'a> {
    pub algorithm: Algorithm,
    pub positions: Vec<(u8, &'a Key, Value<'a>)>,
    pub ignore_case: bool,
}

fn build_tiny_map(key: Ident, options: HashMap<&Key, Value>, ignore_case: bool) -> TokenStream {
    let mut map: BTreeMap<usize, HashMap<&Key, Value>> = BTreeMap::new();
    let mut min_key_size = usize::MAX;
    let mut max_key_size = 0;

    for (key, value) in &options {
        let key_size = key.0.as_str().len();
        min_key_size = min_key_size.min(key_size);
        max_key_size = max_key_size.max(key_size);
        map.entry(key_size).or_default().insert(key, value.clone());
    }

    let key_ident = unsynn::Ident::new("__key", Span::mixed_site());

    let (header, footer) = (
        SemicolonDelimited {
            value: LetExpr::new(key_ident.clone(), key),
            delimiter: Some(Semicolon::new()),
        },
        Nothing,
    );

    // Try building a simple lookup table
    if let Some(table) = try_hash(&options, min_key_size, false, ignore_case) {
        let else_cond = Cons {
            first: Else(Cached::new("else")),
            second: BraceGroupContaining::new(Ident::new("None", Span::call_site())),
            third: Nothing,
            fourth: Nothing,
        };

        BraceGroupContaining::new(Cons {
            first: header,
            second: Cons {
                first: If(Cached::new("if")),
                second: DotDelimited {
                    value: ParenthesisGroupContaining::new(Cons {
                        first: min_key_size,
                        second: DotDotEq::new(),
                        third: max_key_size,
                        fourth: Nothing,
                    }),
                    delimiter: Some(Dot::new()),
                },
                third: MethodCall::new(
                    Ident::new("contains", Span::call_site()),
                    CommaDelimited {
                        value: Cons {
                            first: And::new(),
                            second: key_ident,
                            third: Dot::new(),
                            fourth: MethodCall::new(Ident::new("len", Span::call_site()), Nothing),
                        },
                        delimiter: None,
                    },
                ),
                fourth: BraceGroupContaining::new(table),
            },
            third: else_cond,
            fourth: Nothing,
        })
        .to_token_stream()
    } else {
        let match_default = Ident::new("None", Span::call_site());

        let match_arms = map
            .iter()
            .map(|(size, keys)| {
                if keys.len() == 1 {
                    let (key, value) = keys.iter().next().unwrap();

                    let key = Cons {
                        first: And::new(),
                        second: BracketGroupContaining::new(DelimitedVec(
                            key.as_bytes()
                                .iter()
                                .map(|&value| CommaDelimited {
                                    value,
                                    delimiter: Some(Comma::new()),
                                })
                                .collect(),
                        )),
                        third: Nothing,
                        fourth: Nothing,
                    };
                    let if_guard = IfGuard::new(match ignore_case {
                        true => Either::<_, _>::First(Cons {
                            first: key_ident.clone(),
                            second: Dot::new(),
                            third: MethodCall::new(
                                Ident::new("eq_ignore_ascii_case", Span::call_site()),
                                key,
                            ),
                            fourth: Nothing,
                        }),
                        false => Either::Second(Cons {
                            first: key_ident.clone(),
                            second: Equal::new(),
                            third: key,
                            fourth: Nothing,
                        }),
                    });

                    CommaDelimited {
                        value: MatchArm::new_guarded(
                            *size,
                            if_guard,
                            FatArrow::new(),
                            value.clone(),
                        )
                        .to_token_stream(),
                        delimiter: Some(Comma::new()),
                    }
                } else {
                    let table = try_hash(keys, *size, true, ignore_case).unwrap_or_else(|| {
                        panic!(
                            "Failed to build lookup table for {} keys: {:?}",
                            keys.len(),
                            keys.iter().map(|(k, _)| k).collect::<Vec<_>>()
                        )
                    });

                    CommaDelimited {
                        value: MatchArm::new(
                            *size,
                            FatArrow::new(),
                            BraceGroupContaining::new(table),
                        )
                        .to_token_stream(),
                        delimiter: Some(Comma::new()),
                    }
                }
            })
            .chain(iter::once_with(|| CommaDelimited {
                value: MatchArm::new(
                    Ident::new("_", Span::call_site()),
                    FatArrow::new(),
                    match_default,
                )
                .to_token_stream(),
                delimiter: Some(Comma::new()),
            }));

        let match_arms = DelimitedVec(match_arms.collect());

        BraceGroupContaining::new(Cons {
            first: header,
            second: Cons {
                first: Match(Cached::new("match")),
                second: Cons {
                    first: key_ident,
                    second: Dot::new(),
                    third: MethodCall::new(Ident::new("len", Span::call_site()), Nothing),
                    fourth: Nothing,
                },
                third: BraceGroupContaining::new(match_arms),
                fourth: Nothing,
            },
            third: footer,
            fourth: Nothing,
        })
        .to_token_stream()
    }
}

impl Algorithm {
    pub fn hash(&self, value: &[u8]) -> u8 {
        match self {
            Algorithm::Position { idx } => value[*idx],
            Algorithm::Xor { idx1, idx2 } => value[*idx1] ^ value[*idx2],
        }
    }
}

pub(crate) fn try_hash<'a>(
    keys: &HashMap<&'a Key, Value<'a>>,
    size: usize,
    is_final_pass: bool,
    ignore_case: bool,
) -> Option<Table<'a>> {
    // Use direct mapping
    if size == 1 && is_final_pass {
        return Some(Table {
            algorithm: Algorithm::Position { idx: 0 },
            positions: keys
                .iter()
                .collect::<BTreeMap<_, _>>()
                .iter()
                .map(|(key, value)| (key.as_bytes()[0], **key, (*value).clone()))
                .collect(),
            ignore_case,
        });
    }

    // Try finding a key index that contains a byte unique to all keys
    let mut best_match_count = 0;
    let mut best_match_algo = Algorithm::Position { idx: 0 };
    for idx in 0..size {
        let mut byte_set = HashSet::new();
        let algorithm = Algorithm::Position { idx };
        for key in keys.keys() {
            byte_set.insert(algorithm.hash(key.as_str().as_bytes()));
        }
        if byte_set.len() == keys.len() {
            return Some(Table {
                positions: keys
                    .iter()
                    .collect::<BTreeMap<_, _>>()
                    .iter()
                    .map(|(key, value)| (algorithm.hash(key.as_bytes()), **key, (*value).clone()))
                    .collect(),
                algorithm,
                ignore_case,
            });
        } else if byte_set.len() > best_match_count {
            best_match_count = byte_set.len();
            best_match_algo = Algorithm::Position { idx };
        }
    }

    // Try XORing key positions
    for i in 0..size {
        for j in i + 1..size {
            let mut byte_set = HashSet::new();
            let algorithm = Algorithm::Xor { idx1: i, idx2: j };
            for key in keys.keys() {
                byte_set.insert(algorithm.hash(key.0.as_str().as_bytes()));
            }
            if byte_set.len() == keys.len() {
                return Some(Table {
                    positions: keys
                        .iter()
                        .map(|(key, value)| {
                            (algorithm.hash(key.0.as_str().as_bytes()), (*key, value.clone()))
                        })
                        .collect::<BTreeMap<_, _>>()
                        .into_iter()
                        .map(|(key, (a, b))| (key, a, b))
                        .collect(),
                    algorithm,
                    ignore_case,
                });
            } else if byte_set.len() > best_match_count {
                best_match_count = byte_set.len();
                best_match_algo = algorithm;
            }
        }
    }

    if is_final_pass {
        let mut key_groups = HashMap::new();
        for (key, value) in keys {
            key_groups
                .entry(best_match_algo.hash(key.as_bytes()))
                .or_insert_with(HashMap::new)
                .insert(*key, value.clone());
        }
        let mut table = Table {
            algorithm: best_match_algo,
            positions: Vec::with_capacity(keys.len()),
            ignore_case,
        };

        for (hash, keys) in key_groups {
            if keys.len() > 1 {
                let sub_table = try_hash(&keys, size, true, ignore_case).unwrap();
                table.positions.push((
                    hash,
                    keys.keys().next().unwrap(),
                    Value::Table(Box::new(sub_table)),
                ));
            } else {
                let (key, value) = keys.into_iter().next().unwrap();
                table.positions.push((hash, key, value));
            }
        }

        Some(table)
    } else {
        None
    }
}

impl ToTokens for Table<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let id_hash = Ident::new("hash", Span::mixed_site());

        let index = |idx| Cons {
            first: Ident::new("__key", Span::mixed_site()),
            second: BracketGroupContaining::new(idx),
            third: Nothing,
            fourth: Nothing,
        };

        let index_lowercase = |idx| Cons {
            first: DotDelimited { value: index(idx), delimiter: Some(Dot::new()) },
            second: MethodCall::new(Ident::new("to_ascii_lowercase", Span::call_site()), Nothing),
            third: Nothing,
            fourth: Nothing,
        };

        let algorithm = match &self.algorithm {
            Algorithm::Position { idx } => {
                if self.ignore_case {
                    SemicolonDelimited {
                        value: LetExpr::new(id_hash.clone(), index_lowercase(*idx)),
                        delimiter: Some(Semicolon::new()),
                    }
                    .to_token_stream()
                } else {
                    SemicolonDelimited {
                        value: LetExpr::new(id_hash.clone(), index(*idx)),
                        delimiter: Some(Semicolon::new()),
                    }
                    .to_token_stream()
                }
            }
            Algorithm::Xor { idx1, idx2 } => {
                if self.ignore_case {
                    SemicolonDelimited {
                        value: LetExpr::new(
                            id_hash.clone(),
                            Cons {
                                first: index_lowercase(*idx1),
                                second: Caret::new(),
                                third: index_lowercase(*idx2),
                                fourth: Nothing,
                            },
                        ),
                        delimiter: Some(Semicolon::new()),
                    }
                    .to_token_stream()
                } else {
                    SemicolonDelimited {
                        value: LetExpr::new(
                            id_hash.clone(),
                            Cons {
                                first: index(*idx1),
                                second: Caret::new(),
                                third: index(*idx2),
                                fourth: Nothing,
                            },
                        ),
                        delimiter: Some(Semicolon::new()),
                    }
                    .to_token_stream()
                }
            }
        };

        let match_default = Ident::new("None", Span::call_site());

        let arms: Vec<_> = self
            .positions
            .iter()
            .map(|(hash, key, value)| {
                let guard = if key.as_bytes().len() > 1 && !matches!(value, Value::Table(_)) {
                    let key = Cons {
                        first: And::new(),
                        second: BracketGroupContaining::new(DelimitedVec(
                            key.as_bytes()
                                .iter()
                                .map(|&value| CommaDelimited {
                                    value,
                                    delimiter: Some(Comma::new()),
                                })
                                .collect(),
                        )),
                        third: Nothing,
                        fourth: Nothing,
                    };

                    if self.ignore_case {
                        Either::<_, _, _>::First(IfGuard::new(Cons {
                            first: Ident::new("__key", Span::mixed_site()),
                            second: Dot::new(),
                            third: MethodCall::new(
                                Ident::new("eq_ignore_ascii_case", Span::call_site()),
                                CommaDelimited { value: key, delimiter: None },
                            ),
                            fourth: Nothing,
                        }))
                    } else {
                        Either::Second(IfGuard::new(Cons {
                            first: Ident::new("__key", Span::mixed_site()),
                            second: Equal::new(),
                            third: key,
                            fourth: Nothing,
                        }))
                    }
                } else {
                    Either::Third(Nothing)
                };

                MatchArm::new_guarded(*hash, guard, FatArrow::new(), value.clone())
                    .to_token_stream()
            })
            .chain(iter::once_with(|| {
                MatchArm::new(Ident::new("_", Span::call_site()), FatArrow::new(), match_default)
                    .to_token_stream()
            }))
            .map(|value| CommaDelimited { value, delimiter: Some(Comma::new()) })
            .collect();

        Cons {
            first: algorithm,
            second: Match(Cached::new("match")),
            third: id_hash,
            fourth: BraceGroupContaining::new(DelimitedVec(arms)),
        }
        .to_tokens(tokens);
    }
}

impl ToTokens for Value<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        struct Hack<'a, T>(&'a T);
        impl<T: ToTokens> ToTokens for Hack<'_, T> {
            fn to_tokens(&self, tokens: &mut TokenStream) {
                self.0.to_tokens(tokens);
            }
        }

        match self {
            Value::Some(inner) => {
                Ident::new("Some", Span::call_site()).to_tokens(tokens);
                ParenthesisGroupContaining::new(Hack(*inner)).to_tokens(tokens);
            }
            Value::Table(table) => BraceGroupContaining::new(Hack(table)).to_tokens(tokens),
        }
    }
}
