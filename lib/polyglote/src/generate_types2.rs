use std::collections::HashMap;

use heck::ToUpperCamelCase;
use proc_macro2::Ident;
use quote::{format_ident, quote};

use crate::generate_types::BijectiveFormatedIdentifier as _;
use crate::keywords::{AdditionalKeyword, CppKeyword, JavaKeyword};
use crate::preprocess::{DChildren, Fields, Hidden, MultipleChildren, RequiredChildren};
use crate::preprocess::{Named, SubTypes};

use super::*;

pub fn serialize_types(typesys: &TypeSys) {
    let res = process_types_into_tokens(typesys);
    println!("{}", res);
    let res = syn::parse_file(&res.to_string()).unwrap();
    let res = prettyplease::unparse(&res);
    println!("{}", res);
}

pub(crate) fn process_types_into_tokens(typesys: &TypeSys) -> proc_macro2::TokenStream {
    let mut merged = quote! {};
    let mut from_u16 = quote! {};
    let mut cat_from_u16 = quote! {};
    let mut from_str = quote! {};
    let mut to_str = quote! {};
    let mut to_pair = quote! {};
    let mut as_vec_toks = quote! {};
    let mut hidden_toks = quote! {};
    let mut hidden_toks_pred = quote! {};
    let mut keyword_toks = quote! {};
    let mut concrete_toks = quote! {};
    let mut with_field_toks = quote! {};
    let mut abstract_toks = quote! {};
    let mut supertype_pred = quote! {};
    let mut named_pred = quote! {};

    let mut alias_dedup = HashMap::<hecs::Entity, Ident>::default();
    let mut leafs = generate_types::HM::default();
    <JavaKeyword as strum::IntoEnumIterator>::iter().for_each(|x| {
        leafs.unamed.insert(x.to_string(), format!("{:?}", x));
    });
    <CppKeyword as strum::IntoEnumIterator>::iter().for_each(|x| {
        leafs.unamed.insert(x.to_string(), format!("{:?}", x));
    });
    <AdditionalKeyword as strum::IntoEnumIterator>::iter().for_each(|x| {
        leafs.unamed.insert(x.to_string(), format!("{:?}", x));
    });

    let mut dup_map = HashMap::<hecs::Entity, u8>::default();

    for (i, e) in typesys.list.iter().enumerate() {
        let i = i as u16;
        let v = typesys.types.entity(*e).unwrap();
        let t = v.get::<&preprocess::T>().unwrap().0.to_string();

        let dup_count = {
            let count = dup_map.entry(*e).or_insert(0);
            let dup_count = *count;
            *count += 1;
            dup_count
        };

        if t == "URI" {
            dbg!(
                &t,
                v.get::<&Named>().is_none(),
                v.get::<&SubTypes>().is_some(),
                v.get::<&Fields>().is_some(),
                v.get::<&DChildren>().is_some(),
                dup_count,
            );
        }

        if v.get::<&Named>().is_none() {
            // leaf/token
            let camel_case = t.try_format_ident(dup_count);
            let raw = t.clone();
            let (q, kind) = if let Some(camel_case) = &camel_case {
                assert!(!camel_case.is_empty(), "{},{}", t, t.to_upper_camel_case());
                let kind = if camel_case == "0" {
                    let camel_case = leafs.fmt(&t, |k| format!("TS{}", &k));
                    format_ident!("{}", &camel_case)
                } else {
                    format_ident!("{}", &camel_case)
                };

                (
                    quote! {
                        #kind,
                    },
                    kind,
                )
            } else {
                let mut k = leafs.fmt(&t, |k| format!("TS{}", &k.to_upper_camel_case()));

                if dup_count > 0 {
                    k.push_str(&"_".repeat(dup_count as usize));
                }
                let kind = format_ident!("{}", &k);

                (
                    quote! {
                        // #[strum(serialize = #raw)]
                        #kind(Raw<#raw>),
                    },
                    kind,
                )
            };

            if v.has::<Hidden>() {
                hidden_toks.extend(q);
                hidden_toks_pred.extend(quote! {
                    #kind,
                });
                cat_from_u16.extend(quote! {
                    #i => TypeEnum::Hidden(Hidden::#kind),
                });
                as_vec_toks.extend(quote! {
                    Hidden(#kind),
                });
            } else {
                keyword_toks.extend(q);
                cat_from_u16.extend(quote! {
                    // #i => TypeEnum::Keyword(Keyword::#kind),
                    #i => Type::#kind,
                });
                as_vec_toks.extend(quote! {
                    Keyword(#kind),
                });
            }
            from_u16.extend(quote! {
                #i => Type::#kind,
            });
            merged.extend(quote! {
                #kind,
            });
            to_str.extend(quote! {
                Type::#kind => #raw,
            });
            from_str.extend(quote! {
                #raw => Type::#kind,
            });
            to_pair.extend(quote! {
                #kind = #raw;
            });
            alias_dedup.insert(*e, kind);
        } else if let Some(st) = v.get::<&SubTypes>() {
            let camel_case = t.try_format_ident(dup_count);
            let kind = format_ident!(
                "{}",
                &camel_case
                    .clone()
                    .unwrap_or_else(|| t.to_upper_camel_case())
            );
            let raw = t.clone();
            let mut sub_toks = quote! {};
            for e in &st.0 {
                let v = typesys.types.entity(*e).unwrap();
                let t = &v.get::<&preprocess::T>().unwrap().0;
                let camel_case = t.try_format_ident(dup_count);
                if let Some(camel_case) = camel_case {
                    let kind = format_ident!("{}", &camel_case);
                    sub_toks.extend(quote! {
                        // #[strum(serialize = #raw)]
                        #kind,
                    });
                } else {
                    let kind = if v.get::<&Named>().is_none() {
                        let k = leafs.fmt(t, |k| format!("TS{}", &k.to_upper_camel_case()));
                        format_ident!("{}", &k)
                    } else {
                        format_ident!("{}", &t.to_upper_camel_case())
                    };
                    sub_toks.extend(quote! {
                        // #[strum(serialize = #raw)]
                        #kind,
                    });
                }
            }
            hidden_toks_pred.extend(quote! {
                #kind,
            });
            supertype_pred.extend(quote! {
               #kind,
            });
            named_pred.extend(quote! {
               #kind,
            });
            if camel_case.is_none() {
                abstract_toks.extend(quote! {
                    // #[strum(serialize = #raw)]
                    #kind(Raw<#raw>, #sub_toks),
                });
            } else {
                abstract_toks.extend(quote! {
                    #kind(#sub_toks),
                });
            }
            cat_from_u16.extend(quote! {
                #i => TypeEnum::Abstract(Abstract::#kind),
            });
            as_vec_toks.extend(quote! {
                Abstract(#kind),
            });

            merged.extend(quote! {
                #kind,
            });
            from_u16.extend(quote! {
                #i => Type::#kind,
            });
            to_str.extend(quote! {
                Type::#kind => #raw,
            });
            from_str.extend(quote! {
                #raw => Type::#kind,
            });
            to_pair.extend(quote! {
                #kind = #raw;
            });
            alias_dedup.insert(*e, kind);
        } else if let Some(fields) = v.get::<&Fields>() {
            let camel_case = t.try_format_ident(dup_count);
            let kind = format_ident!(
                "{}",
                &camel_case
                    .clone()
                    .unwrap_or_else(|| t.to_upper_camel_case())
            );
            let raw = t.clone();
            let mut fields_toks = quote! {};
            for e in &fields.0 {
                let v = typesys.types.entity(*e).unwrap();
                let t = &v.get::<&preprocess::Role>().unwrap().0;
                let camel_case = t.try_format_ident(dup_count);
                assert_ne!(camel_case, None);
                let t = if t == "type" { "r#type" } else { t };
                let kind = format_ident!("{}", &t);
                let cs = &v.get::<&preprocess::DChildren>().unwrap().0;
                let mut cs_toks = quote! {};
                for e in cs {
                    let v = typesys.types.entity(*e).unwrap();
                    let t = &v.get::<&preprocess::T>().unwrap().0;
                    let camel_case = t.try_format_ident(dup_count);
                    if let Some(camel_case) = camel_case {
                        let kind = format_ident!("{}", &camel_case);
                        cs_toks.extend(quote! {
                            #kind,
                        });
                    } else {
                        let kind = if v.get::<&Named>().is_none() {
                            let k = leafs.fmt(t, |k| format!("TS{}", &k.to_upper_camel_case()));
                            format_ident!("{}", &k)
                        } else {
                            format_ident!("{}", &t.to_upper_camel_case())
                        };
                        cs_toks.extend(quote! {
                            #kind,
                        });
                    }
                }
                if v.has::<RequiredChildren>() {
                    if v.has::<MultipleChildren>() {
                        fields_toks.extend(quote! {
                            #kind:MultReq<(#cs_toks)>,
                        });
                    } else {
                        fields_toks.extend(quote! {
                            #kind:Req<(#cs_toks)>,
                        });
                    }
                } else if v.has::<MultipleChildren>() {
                    fields_toks.extend(quote! {
                        #kind:Mult<(#cs_toks)>,
                    });
                } else {
                    fields_toks.extend(quote! {
                        #kind: (#cs_toks),
                    });
                }
            }
            if let Some(cs) = v.get::<&preprocess::DChildren>() {
                let mut cs_toks = quote! {};
                for e in &cs.0 {
                    let v = typesys.types.entity(*e).unwrap();
                    let t = &v.get::<&preprocess::T>().unwrap().0;
                    let camel_case = t.try_format_ident(dup_count);
                    if let Some(camel_case) = camel_case {
                        let kind = format_ident!("{}", &camel_case);
                        cs_toks.extend(quote! {
                            #kind,
                        });
                    } else {
                        let kind = if v.get::<&Named>().is_none() {
                            let k = leafs.fmt(t, |k| format!("TS{}", &k.to_upper_camel_case()));
                            format_ident!("{}", &k)
                        } else {
                            format_ident!("{}", &t.to_upper_camel_case())
                        };
                        cs_toks.extend(quote! {
                            #kind,
                        });
                    }
                }
                // fields_toks.extend(quote! {
                //     _cs:(#cs_toks),
                // });

                if v.has::<RequiredChildren>() {
                    if v.has::<MultipleChildren>() {
                        fields_toks.extend(quote! {
                            _cs:MultReq<(#cs_toks)>,
                        });
                    } else {
                        fields_toks.extend(quote! {
                            _cs:Req<(#cs_toks)>,
                        });
                    }
                } else if v.has::<MultipleChildren>() {
                    fields_toks.extend(quote! {
                        _cs:Mult<(#cs_toks)>,
                    });
                } else {
                    fields_toks.extend(quote! {
                        _cs: (#cs_toks),
                    });
                }
            }
            if camel_case.is_none() {
                with_field_toks.extend(quote! {
                    // #[strum(serialize = #raw)]
                    #kind{_ser: Raw<#raw>, #fields_toks},
                });
            } else {
                with_field_toks.extend(quote! {
                    #kind{#fields_toks},
                });
            }
            cat_from_u16.extend(quote! {
                #i => TypeEnum::WithFields(WithFields::#kind),
            });
            as_vec_toks.extend(quote! {
                WithFields(#kind),
            });

            merged.extend(quote! {
                #kind,
            });
            from_u16.extend(quote! {
                #i => Type::#kind,
            });
            to_str.extend(quote! {
                Type::#kind => #raw,
            });
            from_str.extend(quote! {
                #raw => Type::#kind,
            });
            named_pred.extend(quote! {
               #kind,
            });
            to_pair.extend(quote! {
                #kind = #raw;
            });
            alias_dedup.insert(*e, kind);
        } else if let Some(cs) = v.get::<&DChildren>() {
            let camel_case = t.try_format_ident(dup_count);
            let kind = format_ident!(
                "{}",
                &camel_case
                    .clone()
                    .unwrap_or_else(|| t.to_upper_camel_case())
            );
            let raw = t.clone();
            let mut cs_toks = quote! {};
            for e in &cs.0 {
                let v = typesys.types.entity(*e).unwrap();
                let t = &v.get::<&preprocess::T>().unwrap().0;
                let camel_case = t.try_format_ident(dup_count);
                if let Some(camel_case) = camel_case {
                    let kind = format_ident!("{}", &camel_case);
                    cs_toks.extend(quote! {
                        #kind,
                    });
                } else {
                    let kind = if v.get::<&Named>().is_none() {
                        let k = leafs.fmt(t, |k| format!("TS{}", &k.to_upper_camel_case()));
                        format_ident!("{}", &k)
                    } else {
                        format_ident!("{}", &t.to_upper_camel_case())
                    };
                    cs_toks.extend(quote! {
                        #kind,
                    });
                }
            }
            let cs_toks = if v.has::<RequiredChildren>() {
                if v.has::<MultipleChildren>() {
                    quote! {
                        MultReq<(#cs_toks)>,
                    }
                } else {
                    quote! {
                        Req<(#cs_toks)>,
                    }
                }
            } else if v.has::<MultipleChildren>() {
                quote! {
                    Mult<(#cs_toks)>,
                }
            } else {
                quote! {
                    #cs_toks
                }
            };
            if camel_case.is_none() {
                concrete_toks.extend(quote! {
                    // #[strum(serialize = #raw)]
                    #kind(Raw<#raw>,#cs_toks),
                });
            } else {
                concrete_toks.extend(quote! {
                    #kind(#cs_toks),
                });
            }
            cat_from_u16.extend(quote! {
                #i => TypeEnum::Concrete(Concrete::#kind),
            });
            as_vec_toks.extend(quote! {
                Concrete(#kind),
            });

            merged.extend(quote! {
                #kind,
            });
            from_u16.extend(quote! {
                #i => Type::#kind,
            });
            to_str.extend(quote! {
                Type::#kind => #raw,
            });
            from_str.extend(quote! {
                #raw => Type::#kind,
            });
            named_pred.extend(quote! {
               #kind,
            });
            to_pair.extend(quote! {
                #kind = #raw;
            });
            alias_dedup.insert(*e, kind);
        } else {
            let camel_case = t.try_format_ident(dup_count);
            let kind = format_ident!(
                "{}",
                &camel_case.clone().unwrap_or_else(|| {
                    let mut s = t.to_upper_camel_case();
                    if dup_count > 0 {
                        s.push_str(&"_".repeat(dup_count as usize));
                    }
                    s
                })
            );
            let raw = t.clone();

            if t == "URI" {
                dbg!(&t, &raw, &kind, dup_count,);
            }

            if camel_case.is_none() {
                concrete_toks.extend(quote! {
                    // #[strum(serialize = #raw)]
                    #kind(Raw<#raw>),
                });
            } else {
                concrete_toks.extend(quote! {
                    #kind,
                });
            }
            cat_from_u16.extend(quote! {
                #i => TypeEnum::Concrete(Concrete::#kind),
            });
            as_vec_toks.extend(quote! {
                Concrete(#kind),
            });
            if v.has::<Hidden>() {
                panic!();
            }

            merged.extend(quote! {
                #kind,
            });
            from_u16.extend(quote! {
                #i => Type::#kind,
            });
            to_str.extend(quote! {
                Type::#kind => #raw,
            });
            from_str.extend(quote! {
                #raw => Type::#kind,
            });
            named_pred.extend(quote! {
               #kind,
            });
            to_pair.extend(quote! {
                #kind = #raw;
            });
            alias_dedup.insert(*e, kind);
        }
    }

    let res = quote! {
        declare_type!{
            #to_pair
        }
        impl Type {
            pub fn is_hidden(&self) -> bool {
                is!(self,
                    #hidden_toks_pred
                )
            }
            pub fn is_supertype(&self) -> bool {
                is!(self,
                    #supertype_pred
                )
            }
            pub fn is_named(&self) -> bool {
                is!(self,
                    #named_pred
                )
            }

        }
    };
    res
}
