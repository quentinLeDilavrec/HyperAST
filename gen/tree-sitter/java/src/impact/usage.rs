use crate::types::{TIdN, Type};
use core::fmt;
use hyperast::{
    filter::BloomResult,
    nodes::RefContainer,
    position::{
        Scout, SpHandle, StructuralPositionStore, TreePath, TreePathMut, TypedScout, TypedTreePath,
    },
    store::defaults::LabelIdentifier,
    types::{
        Children, HyperAST, HyperASTShared, IterableChildren, LabelStore, Labeled, NodeId, Tree,
        TypeStore, TypeTrait, Typed, TypedHyperAST, TypedNodeStore, TypedTree, WithChildren,
        WithSerialization,
    },
};
use num::{cast, one, zero, ToPrimitive, Zero};
use std::fmt::Debug;
// use hyperast_core::tree::tree::{WithChildren, Tree, Labeled};

use crate::impact::{
    element::{IdentifierFormat, LabelPtr},
    reference::DisplayRef,
};
use hyperast::types::AnyType;

use super::{
    element::ExplorableRef,
    element::{RefPtr, RefsEnum},
    partial_analysis::PartialAnalysis,
};

// TODO use generic node and store

pub struct RefsFinder<'a, IdN, HAST: HyperAST<'a>> {
    stores: &'a HAST,
    ana: &'a mut PartialAnalysis,
    /// result of search
    sp_store: &'a mut StructuralPositionStore<IdN, HAST::Idx>,
    refs: Vec<SpHandle>,
}

impl<'a, IdN, HAST: HyperAST<'a>> Debug for RefsFinder<'a, IdN, HAST> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RefsFinder")
            .field("ana", &self.ana)
            .field("sp_store", &self.sp_store)
            .field("refs", &self.refs.len())
            .finish()
    }
}

impl<'a, IdN, HAST: HyperAST<'a>> RefsFinder<'a, IdN, HAST> {
    pub fn new(
        stores: &'a HAST,
        ana: &'a mut PartialAnalysis,
        sp_store: &'a mut StructuralPositionStore<IdN, HAST::Idx>,
    ) -> Self {
        Self {
            stores,
            ana,
            sp_store,
            refs: Default::default(),
        }
    }
}

macro_rules! missing_rule {
    () => {
        log::error!("missing rule");
    };
    ($($arg:tt)+) => {{
        log::error!($($arg)+);
    }};
}

/// Main traversal of HyperAST
/// Recusive traversal, it goes through declaration without handling them particularly
/// thus is should not search for references to `this` or `super`
impl<'a, IdN, HAST> RefsFinder<'a, IdN, HAST>
where
    HAST: TypedHyperAST<
        'a,
        TIdN<IdN>,
        IdN = IdN,
        // T = HashedNodeRef<'a,Type>,
        Label = LabelIdentifier,
    >,
    IdN: 'a + Copy + Eq + Debug + NodeId<IdN = IdN>,
    HAST::Idx: num::PrimInt + num::traits::NumAssign + Debug,
    HAST: hyperast::types::TypeStore,
    HAST: hyperast::types::LabelStore<str, I = LabelIdentifier>,
    HAST: hyperast::types::NodeStore<IdN, R<'a> = <HAST as hyperast::types::HyperAST<'a>>::T>,
    <HAST as hyperast::types::HyperAST<'a>>::T:
        TypedTree<Type = AnyType, ChildIdx = HAST::Idx> + WithSerialization,
    <HAST as hyperast::types::HyperAST<'a>>::T: RefContainer<Result = BloomResult>,
    <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT:
        TypedTree<Type = Type, ChildIdx = HAST::Idx>,
    <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT:
        RefContainer<Result = BloomResult>,
{
    pub fn eq_root_scoped(
        d: ExplorableRef,
        stores: &'a HAST,
        b: <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT,
    ) -> bool {
        match d.as_ref() {
            RefsEnum::Root => false, // TODO check, not sure
            RefsEnum::MaybeMissing => false,
            RefsEnum::ScopedIdentifier(o, i) => {
                let t = b.get_type();
                if t == Type::ScopedAbsoluteIdentifier {
                    let mut bo = false;
                    assert!(b.has_children());
                    // TODO todo!();
                    let it = b.children().unwrap().iter_children();
                    let it = it.collect::<Vec<_>>();
                    for x in it.iter().rev() {
                        // log::trace!("d:{:?}",d);
                        let b = stores.typed_node_store().try_resolve(x).unwrap().0;
                        let t = b.get_type();
                        if t == Type::ScopedAbsoluteIdentifier {
                            if !Self::eq_root_scoped(d.with(*o), stores, b) {
                                return false;
                            }
                        } else if t == Type::Identifier {
                            if bo {
                                return Self::eq_root_scoped(d.with(*o), stores, b);
                            }
                            if let Some(l) = b.try_get_label() {
                                if l != i.as_ref() {
                                    return false;
                                } else {
                                }
                            } else {
                                panic!()
                            }
                            bo = true;
                        }
                    }
                    true
                } else if t == Type::Identifier {
                    if let Some(l) = b.try_get_label() {
                        if l != i.as_ref() {
                            false
                        } else {
                            if let RefsEnum::Root = d.with(*o).as_ref() {
                                true
                            } else {
                                false
                            }
                        }
                    } else {
                        panic!()
                    }
                } else {
                    todo!("{:?}", t)
                }
            }
            RefsEnum::TypeIdentifier(..) => false,
            x => {
                panic!("{:?}", x)
            }
        }
    }

    /// Find all references to `target` that was declared in `package`
    /// WARN maybe do not search targets that end with unqualified this, use find_all_with_this it it works
    /// returns the indexes that should be used on self.sp_store the `StructuralPositionStore<NodeIdentifier>`
    pub fn find_all(
        mut self,
        package: RefPtr,
        target: RefPtr,
        mut scout: TypedScout<TIdN<HAST::IdN>, HAST::Idx>,
    ) -> Vec<SpHandle> {
        // self.sp_store.check_with(self.stores, &scout).expect("find_all before");
        // self.find_refs::<false>(package, target, &mut scout);
        let current = scout.node_always(&self.sp_store).unwrap();
        let b = self.stores.typed_node_store().resolve(&current);
        self.find_refs2::<false>(package, target, current, b, &mut scout);
        self.refs
    }
    /// Find all references to `target` that was declared in `package`
    /// WARN maybe do not search targets that end with unqualified this, use find_all_with_this it it works
    /// returns the indexes that should be used on self.sp_store the `StructuralPositionStore<NodeIdentifier>`
    pub fn find_all_with<const IM: bool>(
        mut self,
        package: RefPtr,
        target: RefPtr,
        mut scout: TypedScout<TIdN<HAST::IdN>, HAST::Idx>,
    ) -> Vec<SpHandle> {
        // self.sp_store.check_with(self.stores, &scout).expect("find_all_with before");
        let current = scout.node_always(&self.sp_store).unwrap();
        let b = self.stores.typed_node_store().resolve(&current);
        self.find_refs2::<IM>(package, target, current, b, &mut scout);
        self.refs
    }
    /// Find all references to `target` that was declared in `package`
    /// WARN do not search targets that end with unqualified this, use find_ref_this
    /// returns the indexes that should be used on self.sp_store the `StructuralPositionStore<NodeIdentifier>`
    pub fn find_all_is_this(
        self,
        package: RefPtr,
        scout: TypedScout<TIdN<HAST::IdN>, HAST::Idx>,
    ) -> Vec<SpHandle> {
        // let mm = self.ana.solver.intern(RefsEnum::MaybeMissing);
        // let this = self.ana.solver.intern(RefsEnum::This(mm));
        todo!(
            "need a TypedScout for find constructors {:?} {} {:?}",
            self,
            package,
            scout
        );
        // self.find_constructors(scout.clone());
        // self.sp_store.check_with(self.stores, &scout).expect("find_all_is_this before");
        // self.find_refs_with_this(package, this, &mut scout);
        // self.sp_store.check_with(self.stores, &scout).expect("find_all_is_this after");
        // self.refs
    }

    #[allow(unused)] // need to do some ab testing
    /// WARN do not search targets that end with unqualified this, use find_ref_this
    fn find_refs<const IM: bool>(
        &mut self,
        package: RefPtr,
        target: RefPtr,
        scout: &mut Scout<HAST::IdN, HAST::Idx>,
    ) -> Vec<RefPtr> {
        self.sp_store
            .check_with(self.stores, scout)
            .expect("find_refs");
        let current = scout.node_always(&self.sp_store);
        let b = self.stores.typed_node_store().try_resolve(&current);
        let has_children = b.as_ref().map_or(false, |(x, _)| x.has_children());
        // let t = b.as_ref().map_or(Type::ERROR, |(x,_)|x.get_type());
        if let Some((b, current)) = b {
            let t = b.get_type();
            let scout = &mut self.sp_store.type_scout(scout, &current);
            match self.find_refs_pre(t, &b, package, scout, target, &current) {
                Ok(value) => (),
                Err(value) => return value,
            };
            return self.find_refs2::<IM>(package, target, current, b, scout);
        }
        let b = hyperast::types::NodeStore::resolve(self.stores.node_store(), &current);
        let t = self.stores.resolve_type(&current);
        if !IM && !has_children {
            log::debug!("d=1 {:?}", "'Not Java'");
            return vec![];
        } else if !IM && self.check_oracle(&b, target) == BloomResult::DoNotContain {
            log::debug!("d=1 {:?}", "'Not Java'");
            log::debug!("Do not contains");
            return vec![];
        }
        if !b.has_children() {
            log::error!("droped on {:?}", t);
            return vec![];
        }
        let mut v: Vec<usize> = vec![];
        log::debug!("c_count {:?}", b.child_count());
        // scout.down();

        for (i, x) in b.children().unwrap().iter_children().enumerate() {
            // scout.inc(*x);
            assert_eq!(current, scout.node_always(&self.sp_store));
            scout.goto(*x, num::cast(i).unwrap());
            log::trace!(
                "rec {} search ref {}",
                i,
                DisplayRef::from((
                    self.ana.solver.nodes.with(target),
                    self.stores.label_store()
                )),
            );
            let z = self.find_refs::<false>(package, target, scout);
            for w in v.clone() {
                log::trace!(
                    "also rec search ref {}",
                    DisplayRef::from((self.ana.solver.nodes.with(w), self.stores.label_store())),
                );
                let z = self.find_refs::<false>(package, w, scout);
                v.extend(z)
            }
            v.extend(z);
            v.dedup();
            scout.up(&self.sp_store);
        }
        vec![]
    }

    fn find_refs2<const IM: bool>(
        &mut self,
        package: RefPtr,
        target: RefPtr,
        current: TIdN<IdN>,
        b: <HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) -> Vec<RefPtr> {
        // self.sp_store.check_with(self.stores, scout).expect("find_refs");
        // let current = scout.node_typed().unwrap();
        // let b = self.stores.typed_node_store().resolve(current);
        let has_children = b.has_children();
        let Some(t) = b.try_get_type() else {
            return vec![];
        };
        match self.find_refs_pre(t, &b, package, scout, target, &current) {
            Ok(_) => (),
            Err(value) => return value,
        };
        if !IM && !has_children {
            log::debug!("d=1 {:?}", &t);
            return vec![];
        } else if !IM && self.check_oracle(&b, target) == BloomResult::DoNotContain {
            log::debug!("d=1 {:?}", &t);
            log::debug!("Do not contains");
            return vec![];
        } else {
            log::debug!("d=1 {:?}", &t);
            log::debug!("++++++++++++++Maybe contains");

            if t == Type::MethodInvocation // find object
                || t == Type::FormalParameter // find simple type
                || t == Type::SpreadParameter
                || t == Type::TypeBound // find simple type
                || t == Type::ObjectCreationExpression // find simple object
                || t == Type::ArrayCreationExpression // find simple object
                || t == Type::ScopedAbsoluteIdentifier // find identifier
                || t == Type::CatchType // TODO to check
                || t == Type::FieldAccess // TODO to check
                || t == Type::FieldDeclaration // TODO to check
                || t == Type::Superclass // TODO to check for hierachy
                || t == Type::SuperInterfaces // TODO to check for hierachy
                || t == Type::ExtendsInterfaces // TODO to check for hierachy
                || t == Type::InstanceofExpression // TODO to handle
                || t == Type::AnnotatedType // TODO to handle
                || t == Type::ClassLiteral // to handle A.class
                || t == Type::ArrayType // TODO to handle A[]
                || t == Type::MethodReference // TODO to handle A::m
                || t == Type::CastExpression // TODO to handle (A)x
                // || t == Type::ConstructorDeclaration // TODO to handle constructors
                || t == Type::ConstantDeclaration // find simple type
                || t == Type::LocalVariableDeclaration // find simple type
                || t == Type::EnhancedForVariable
                // || t == Type::ForStatement // no need look at LocalVariable and Expressions
                // || t == Type::TryWithResourcesStatement // no need look at resource
                // || t == Type::CatchClause // no need look at catch variable
                || t == Type::EnhancedForStatement // TODO to handle declarative staements
                || t == Type::Resource
                || t == Type::ArgumentList
                || t == Type::TypeArguments
                || t == Type::TernaryExpression
                || t == Type::UpdateExpression
                || t == Type::UnaryExpression
                || t == Type::BinaryExpression
                || t == Type::ParenthesizedExpression
                || t == Type::ReturnStatement
                || t == Type::ThrowStatement
                || t == Type::AssignmentExpression
                || t == Type::AssertStatement
                || t == Type::VariableDeclarator
                || t == Type::ArrayAccess
                || t == Type::Annotation
                || t == Type::MarkerAnnotation
                || t == Type::Throws
                || t == Type::WildcardExtends
                || t == Type::WildcardSuper
            // TODO to check
            // find identifier
            {
                // Here, for now, we try to find Identifiers (not invocations)
                // thus we either search directly for scoped identifiers
                // or we search for simple identifiers because they do not present refs in themself
                log::debug!("!found {:?}", &t);
                // TODO todo!();
                // log::debug!("{}",legion_with_refs::TreeSyntax::new(
                //     self.stores.node_store(),
                //     self.stores.label_store(),
                //     current,
                // ));

                self.exact_match(target, scout.clone());
            } else if t == Type::GenericType // find simple type
                || t == Type::ScopedIdentifier // find identifier
                || t == Type::ScopedTypeIdentifier
            {
                // Here, for now, we try to find Identifiers (not invocations)
                // thus we either search directly for scoped identifiers
                // or we search for simple identifiers because they do not present refs in themself
                // Moreover we try to avoid double matching refs
                let tt = match scout.clone().up(self.sp_store) {
                    Some(Ok(x)) => Some(self.stores.typed_node_store().resolve(&x).get_type()),
                    Some(Err(x)) => self
                        .stores
                        .typed_node_store()
                        .try_resolve(&x)
                        .map(|(x, _)| x.get_type()),
                    None => None,
                };
                if tt != Some(Type::ObjectCreationExpression) {
                    log::debug!("!found {:?}", &t);

                    log::debug!(
                        "{}",
                        hyperast::nodes::SyntaxSerializer::new(self.stores, *current.as_id())
                    );

                    self.exact_match(target, scout.clone());
                }
            } else if t == Type::This {
                log::debug!("!found This");
                log::debug!(
                    "{}",
                    hyperast::nodes::SyntaxSerializer::new(self.stores, *current.as_id())
                );
                self.exact_match(target, scout.clone());
                return vec![];
            } else if t == Type::TypeIdentifier {
                log::debug!("!found TypeIdentifier");
                log::debug!(
                    "{}",
                    hyperast::nodes::SyntaxSerializer::new(self.stores, *current.as_id())
                );
                self.exact_match(target, scout.clone());
                return vec![];
            } else if t == Type::MethodDeclaration {
                // java_tree_gen::print_tree_syntax(
                log::debug!(
                    "{}",
                    hyperast::nodes::SyntaxSerializer::new(self.stores, *current.as_id())
                );
                self.exact_match(target, scout.clone());
            } else if !has_children {
                return vec![];
            }
        }
        if !b.has_children() {
            log::error!("droped on {:?}", t);
            return vec![];
        }
        let mut v: Vec<usize> = vec![];
        log::debug!("c_count {:?}", b.child_count());
        // scout.down();

        for (i, x) in b.children().unwrap().iter_children().enumerate() {
            // scout.inc(*x);
            // TODO assert_eq!(Ok(current),scout.node_always(&self.sp_store));
            let (b, x) = self.stores.typed_node_store().try_resolve(x).unwrap();
            scout.goto_typed(x, num::cast(i).unwrap());
            log::trace!(
                "rec {} search ref {}",
                i,
                DisplayRef::from((
                    self.ana.solver.nodes.with(target),
                    self.stores.label_store()
                )),
            );
            let z = self.find_refs2::<false>(package, target, x, b, scout);
            for w in v.clone() {
                log::trace!(
                    "also rec search ref {}",
                    DisplayRef::from((self.ana.solver.nodes.with(w), self.stores.label_store())),
                );
                let b = self.stores.typed_node_store().resolve(&x);
                let z = self.find_refs2::<false>(package, w, x, b, scout);
                v.extend(z)
            }
            v.extend(z);
            v.dedup();
            scout.up(&self.sp_store);
        }
        vec![]
    }

    fn find_refs_pre(
        &mut self,
        t: Type,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        package: usize,
        _scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
        target: usize,
        current: &TIdN<IdN>,
    ) -> Result<Type, Vec<usize>>
    where
        IdN: Copy + Eq + Debug,
    {
        if t == Type::Spaces {
            return Err(vec![]);
        } else if t.is_comment() {
            return Err(vec![]);
        } else if t == Type::PackageDeclaration {
            let root_ref = self.ana.solver.intern(RefsEnum::Root);
            let d = self.ana.solver.nodes.with(package);
            let (b, x) = {
                let mut i = num::zero();
                let r;
                let x: TIdN<IdN>;
                assert!(b.has_children());
                loop {
                    let y = b.child(&i).unwrap();
                    // let b = self.stores.node_store().resolve(y);
                    let b = self.stores.typed_node_store().try_resolve(&y);
                    if let Some((b, y)) = b {
                        let t = b.get_type();
                        if t == Type::ScopedAbsoluteIdentifier || t == Type::Identifier {
                            r = b;
                            x = y;
                            break;
                        } else {
                            i = i + num::one();
                        }
                    } else {
                        i = i + num::one();
                    }
                }
                (r, x)
            };
            log::debug!(
                "d=1 {} {:?}",
                DisplayRef::from((
                    self.ana.solver.nodes.with(target),
                    self.stores.label_store()
                )),
                &t,
                // scout.make_position(&self.sp_store, self.stores)
            );
            log::debug!(
                "{}",
                hyperast::nodes::SyntaxSerializer::new(self.stores, *x.as_id())
            );

            if Self::eq_root_scoped(d, self.stores, b) {
                if let Some(x) = self.ana.solver.try_unsolve_node_with(target, package) {
                    if let Some(y) = self.ana.solver.try_unsolve_node_with(target, root_ref) {
                        return Err(vec![x, y]);
                    } else {
                        return Err(vec![x]);
                    }
                } else {
                    let d = self.ana.solver.nodes.with(package);
                    let b = self.stores.typed_node_store().resolve(&x);
                    Self::eq_root_scoped(d, self.stores, b);
                    self.ana.solver.try_unsolve_node_with(target, package);
                    // TODO panic!()
                }
            } else {
                if let Some(x) = self.ana.solver.try_unsolve_node_with(target, root_ref) {
                    return Err(vec![x]);
                } else {
                    return Err(vec![]);
                }
            }
        } else if t == Type::Program {
            log::debug!(
                "d=1 {} {:?}",
                DisplayRef::from((
                    self.ana.solver.nodes.with(target),
                    self.stores.label_store()
                )),
                &t,
                // scout.make_position(&self.sp_store, &self.stores)
            );
        // } else if t == Type::MavenDirectory {
        //     log::debug!(
        //         "d=1 {:?} {:?}",
        //         &t,
        //         scout.make_position(&self.sp_store, &self.stores)
        //     );
        //     // idem
        } else if t == Type::Directory {
            log::debug!(
                "d=1 {} {:?}",
                DisplayRef::from((
                    self.ana.solver.nodes.with(target),
                    self.stores.label_store()
                )),
                &t,
                // scout.make_position(&self.sp_store, &self.stores)
            );
            // TODO if package, get top level declarations then localize if ref.
            // in the end we do not need due to the way we do the impact ana.
            // we should only come from parent of package with canonical id.
        } else if t == Type::ImportDeclaration {
            log::debug!("d=1 {:?}", &t);
            // TODO move print to maybe contains branch
            log::debug!(
                "{}",
                hyperast::nodes::SyntaxSerializer::new(self.stores, *current.as_id(),)
            );
            if target == package {
                return Err(vec![]);
            }

            let mut curr = target;

            let parse_import = || {
                let b = self.stores.typed_node_store().resolve(current);
                let mut scop = None;
                let mut sstatic = false;
                let mut asterisk = false;
                assert!(b.has_children());
                for c in b.children().unwrap().iter_children() {
                    let b = self.stores.typed_node_store().try_resolve(c).unwrap();
                    let c = &b.1;
                    match b.0.get_type() {
                        Type::Static => sstatic = true,
                        Type::Asterisk => asterisk = true,
                        Type::Identifier => scop = Some(*c),
                        Type::ScopedAbsoluteIdentifier => scop = Some(*c),
                        _ => (),
                    }
                }
                (sstatic, scop.unwrap(), asterisk)
            };
            let mut parsed_import: Option<(bool, TIdN<IdN>, bool)> = None;
            loop {
                let d = self.ana.solver.nodes.with(curr);
                // let c = b.check(Into::<Box<[u8]>>::into(d.clone()).as_ref());
                let c = b.check(d.clone());
                if let BloomResult::MaybeContain = c {
                    log::debug!("+++import+++++Maybe contains");

                    let (stic, scop, asterisk) = if let Some(x) = &parsed_import {
                        x.clone()
                    } else {
                        parsed_import = Some(parse_import());
                        parsed_import.unwrap().clone()
                    };

                    if Self::eq_root_scoped(
                        d,
                        self.stores,
                        self.stores.typed_node_store().resolve(&scop),
                    ) {
                        if stic {
                            log::debug!("the import is static");
                        }
                        if asterisk {
                            if target != curr {
                                log::debug!("on-demand import matched ref");
                                if let Some(x) = self.ana.solver.try_unsolve_node_with(target, curr)
                                {
                                    return Err(vec![x]);
                                }
                            }
                        } else {
                            let d = self.ana.solver.nodes.with(curr);
                            if let RefsEnum::ScopedIdentifier(o, _) = d.as_ref() {
                                curr = *o;
                            } else {
                                return Err(vec![]);
                            };
                            log::debug!("import matched ref");
                            assert_ne!(target, curr);
                            if let Some(x) = self.ana.solver.try_unsolve_node_with(target, curr) {
                                return Err(vec![x]);
                            }
                        }
                    }

                    if curr == package {
                        return Err(vec![]);
                    }
                } else {
                    log::debug!("Do not contains");
                }

                let d = self.ana.solver.nodes.with(curr);
                if let RefsEnum::ScopedIdentifier(o, _) = d.as_ref() {
                    curr = *o;
                } else {
                    return Err(vec![]);
                };
            }
            // let whole_match;
            // let c = {
            //     let d = self.ana.solver.nodes.with(target);
            //     b.check(Into::<Box<[u8]>>::into(d.clone()).as_ref())
            // };
            // let asterisk_match;
            // let c = if let BloomResult::MaybeContain = c {
            //     whole_match = true;
            //     c
            // } else {
            //     whole_match = false;
            //     let c = {
            //         let d = self.ana.solver.nodes.with(package);
            //         b.check(Into::<Box<[u8]>>::into(d.clone()).as_ref())
            //     };
            //     c
            // };
            // let c = if let BloomResult::MaybeContain = c {
            //     let d = self.ana.solver.nodes.with(target);
            //     if let RefsEnum::ScopedIdentifier(o, _) = d.as_ref() {
            //         if *o == package {
            //             asterisk_match = false;
            //         } else {
            //             asterisk_match = true;
            //         }
            //     } else {
            //         asterisk_match = true;
            //     }
            //     c
            // } else {
            //     asterisk_match = false;
            //     let d = self.ana.solver.nodes.with(target);
            //     if let RefsEnum::ScopedIdentifier(o, _) = d.as_ref() {
            //         let d = self.ana.solver.nodes.with(*o);
            //         b.check(Into::<Box<[u8]>>::into(d.clone()).as_ref())
            //     } else {
            //         c
            //     }
            // };
            // log::debug!("d=1 {:?}", &t);
            // if let BloomResult::MaybeContain = c {
            //     log::debug!("+++import+++++Maybe contains");
            //     let parent_match = !whole_match && !asterisk_match;
            //     java_tree_gen_full_compress_legion_ref::print_tree_syntax(
            //         &self.stores.node_store,
            //         &self.stores.label_store,
            //         &current,
            //     );
            //     log::debug!();
            //     let (stic, scop, asterisk) = {
            //         let b = self.stores.node_store().resolve(current);
            //         let mut scop = None;
            //         let mut sstatic = false;
            //         let mut asterisk = false;
            //         for c in b.get_children() {
            //             let b = self.stores.node_store().resolve(*c);
            //             match b.get_type() {
            //                 Type::TS86 => sstatic = true,
            //                 Type::Asterisk => asterisk = true,
            //                 Type::Identifier => scop = Some((*c, b)),
            //                 Type::ScopedAbsoluteIdentifier => scop = Some((*c, b)),
            //                 _ => (),
            //             }
            //         }
            //         (sstatic, scop.unwrap(), asterisk)
            //     };
            //     if stic {
            //         return vec![]; // TODO
            //     } else if asterisk && (parent_match || asterisk_match) {
            //         let d = self.ana.solver.nodes.with(package);
            //         if Self::eq_root_scoped(d, self.stores, scop.1) {
            //             if let Some(x) = self.ana.solver.try_unsolve_node_with(target, package) {
            //                 return vec![x];
            //             };
            //             log::debug!("on-demand import matched ref");
            //         } else {
            //             return vec![];
            //         }
            //     } else if parent_match {
            //         let d = self.ana.solver.nodes.with(target);
            //         let o = if let RefsEnum::ScopedIdentifier(o, _) = d.as_ref() {
            //             let d = self.ana.solver.nodes.with(*o);
            //             if Self::eq_root_scoped(d, self.stores, scop.1) {
            //                 Some(*o)
            //             } else {
            //                 None
            //             }
            //         } else {
            //             None
            //         };
            //         if let Some(o) = o {
            //             log::debug!("import matched inner type ref");
            //             if let Some(x) = &self.ana.solver.try_unsolve_node_with(target, o) {
            //                 return vec![*x];
            //             } else {
            //                 panic!();
            //             }
            //         } else {
            //             return vec![];
            //         }
            //     } else {
            //         let d = self.ana.solver.nodes.with(target);
            //         if Self::eq_root_scoped(d, self.stores, scop.1) {
            //             // TODO use self.ana.solver.try_unsolve_node_with
            //             let d = self.ana.solver.nodes.with(target);
            //             let i = if let RefsEnum::ScopedIdentifier(_, i) = d.as_ref() {
            //                 *i
            //             } else {
            //                 panic!()
            //             };
            //             let o = self.ana.solver.intern(RefsEnum::MaybeMissing);
            //             let i = self.ana.solver.intern(RefsEnum::ScopedIdentifier(o, i));
            //             // let i = handle_import(
            //             //     java_tree_gen,
            //             //     self.ana,
            //             //     self.stores.node_store().resolve(scop.0),
            //             // );
            //             log::debug!("import matched ref");
            //             return vec![i];
            //         } else {
            //             return vec![];
            //         }
            //     }
            // } else {
            //     log::debug!("Do not contains");
            //     return vec![];
            // }
        }
        Ok(t)
    }
    fn check_oracle<T: RefContainer<Result = BloomResult>>(
        &self,
        b: &T,
        target: RefPtr,
    ) -> BloomResult {
        // b.get_component::<BloomSize>()
        // .map(|_| {
        let d = self.ana.solver.nodes.with(target);
        b.check(d)
        // })
        // .unwrap_or(BloomResult::MaybeContain)
    }
}

/// prerequisite: recusive traversal limited to expressions ie. should not cross declarations
impl<'a, IdN, HAST> RefsFinder<'a, IdN, HAST>
where
    HAST: TypedHyperAST<
        'a,
        TIdN<IdN>,
        IdN = IdN,
        // T = HashedNodeRef<'a,Type>,
        Label = LabelIdentifier,
    >,
    IdN: Copy + Eq + Debug + NodeId<IdN = IdN>,
    HAST::Idx: num::PrimInt + num::traits::NumAssign + Debug,
    HAST: hyperast::types::TypeStore,
    HAST: hyperast::types::LabelStore<str, I = LabelIdentifier>,
    HAST: hyperast::types::NodeStore<IdN, R<'a> = <HAST as hyperast::types::HyperAST<'a>>::T>,
    <HAST as hyperast::types::HyperAST<'a>>::T: Tree<ChildIdx = HAST::Idx> + WithSerialization,
    <HAST as hyperast::types::HyperAST<'a>>::T: RefContainer<Result = BloomResult>,
    <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT:
        TypedTree<Type = Type, ChildIdx = HAST::Idx>,
    <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT:
        RefContainer<Result = BloomResult>,
{
    pub fn exact_match(&mut self, target: RefPtr, scout: TypedScout<TIdN<IdN>, HAST::Idx>) {
        let d = ExplorableRef {
            rf: target,
            nodes: &self.ana.solver.nodes,
        };

        match d.as_ref().clone() {
            RefsEnum::Root => panic!(),
            RefsEnum::MaybeMissing => panic!(),
            RefsEnum::ScopedIdentifier(o, i) => {
                self.exact_match_scoped_references(o, i.as_ref(), scout)
            }
            RefsEnum::TypeIdentifier(o, i) => {
                self.exact_match_scoped_references(o, i.as_ref(), scout)
            }
            RefsEnum::MethodReference(_, _) => todo!(),
            RefsEnum::ConstructorReference(_) => todo!(),
            RefsEnum::Invocation(_, _, _) => todo!(),
            RefsEnum::ConstructorInvocation(_, _) => todo!(),
            RefsEnum::Primitive(_) => panic!(),
            RefsEnum::Array(_) => todo!(),
            RefsEnum::This(_) => {
                let b = self
                    .stores
                    .typed_node_store()
                    .resolve(&scout.node_always(self.sp_store).unwrap());
                if b.get_type() != Type::This {
                    // log::debug!("not matched");
                } else {
                    assert!(!b.has_children()); // TODO
                    self.successful_match(&mut scout.into());
                }
            }
            RefsEnum::Super(_) => todo!(),
            RefsEnum::ArrayAccess(_) => todo!(),
            RefsEnum::Mask(_, _) => panic!(),
            RefsEnum::Or(_) => todo!(),
        }
    }

    pub fn exact_match_scoped_references(
        &mut self,
        o: RefPtr,
        i: &LabelIdentifier,
        mut scout: TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        let b = self
            .stores
            .typed_node_store()
            .resolve(&scout.node_always(&self.sp_store).unwrap());
        let t = b.get_type();
        if t == Type::MethodInvocation {
            self.exact_match_method_invocation(&b, o, i, &mut scout);
        } else if t == Type::ObjectCreationExpression {
            self.exact_match_object_creation_expression(&b, o, i, &mut scout);
        } else if t == Type::MethodDeclaration {
            self.exact_match_method_declaration(&b, o, i, &mut scout);
        } else if t == Type::FormalParameter
            || t == Type::LocalVariableDeclaration
            || t == Type::EnhancedForVariable
            || t == Type::ConstantDeclaration
        {
            self.exact_match_variable_declaration(&b, o, i, &mut scout);
        } else if t == Type::SuperInterfaces
            || t == Type::Superclass
            || t == Type::ExtendsInterfaces
            || t == Type::TypeBound
        {
            self.exact_match_extend_impl_things(&b, o, i, &mut scout);
        } else if t == Type::EnhancedForStatement {
            self.exact_match_enhanced_for_statement(&b, o, i, &mut scout);
        } else if t == Type::ArrayAccess {
            self.exact_match_array_access(&b, o, i, &mut scout);
        } else if t == Type::GenericType {
            self.exact_match_generic_type(&b, o, i, &mut scout);
        } else if t == Type::ArrayType {
            self.exact_match_array_type(&b, o, i, &mut scout);
        } else if t == Type::InstanceofExpression {
            self.exact_match_instanceof_expression(&b, o, i, &mut scout);
        } else if t == Type::CastExpression {
            self.exact_match_cast_expression(&b, o, i, &mut scout);
        } else if t == Type::MethodReference {
            self.exact_match_method_reference(&b, o, i, &mut scout);
        } else if t == Type::FieldAccess {
            self.exact_match_field_access(&b, o, i, &mut scout);
        } else if t == Type::ScopedTypeIdentifier {
            if let Some(scout) = self.is_scoped_type_identifier_exact_match(&b, o, i, scout) {
                self.successful_match(&mut scout.into());
            }
        } else if t == Type::ClassLiteral {
            self.exact_match_class_literal(&b, o, i, &mut scout);
        } else if t == Type::AnnotatedType {
            self.exact_match_annotated_type(&b, o, i, &mut scout);
        } else if t == Type::FieldDeclaration {
            self.exact_match_field_declaration(&b, o, i, &mut scout);
        } else if t == Type::ConstructorDeclaration {
            // todo!()
            // get ?.identifier from contructor then compare to ref
        } else if t == Type::CatchType {
            // self.exact_match_catch_type(&b, o, i, &mut scout);
            self.exact_match_identifier_in_expr_like(&b, o, i, &mut scout)
        } else if t == Type::Resource {
            self.exact_match_resource(&b, o, i, &mut scout)
        } else if t == Type::VariableDeclarator {
            self.exact_match_var_declarator(&b, o, i, &mut scout)
        } else if t == Type::ArgumentList
            || t == Type::ElementValueArrayInitializer
            || t == Type::TypeArguments
        {
            self.exact_match_identifier_in_expr_like(&b, o, i, &mut scout)
        } else if t == Type::This || t == Type::Super {
            // self.exact_match_this_super(&b, o, i, &mut scout)
        } else if t == Type::TernaryExpression
            || t == Type::AssignmentExpression
            || t == Type::UpdateExpression
            || t == Type::UnaryExpression
            || t == Type::BinaryExpression
            || t == Type::ParenthesizedExpression
            || t == Type::ThrowStatement
            || t == Type::ReturnStatement
            || t == Type::AssertStatement
            || t == Type::SpreadParameter
            || t == Type::WildcardExtends
            || t == Type::WildcardSuper
            || t == Type::ArrayCreationExpression
            || (t == Type::Throws && b.has_children())
        {
            self.exact_match_identifier_in_expr_like(&b, o, i, &mut scout)
        } else if t == Type::Annotation || t == Type::MarkerAnnotation {
            self.exact_match_identifier_in_expr_like(&b, o, i, &mut scout)
        } else if t == Type::Identifier || t == Type::TypeIdentifier {
            if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            } else {
                let l = b.try_get_label().unwrap();
                if l == i {
                    log::debug!("success 101");
                    self.successful_match(&mut scout.into()); // TODO
                }
            }
        } else {
            missing_rule!("exact_match_scoped_references do not handle {:?}", t)
        }
    }

    /// WIP
    /// TODO evaluate validity
    pub fn exact_match_this_super(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let x = b.child(&zero()).unwrap();
        let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = b.get_type();
        if t == Type::TypeIdentifier {
            if let Some(l) = b.try_get_label() {
                if l == i {
                    log::debug!("success this_super");
                    self.successful_match(scout); // TODO

                    let mut s = scout.clone();
                    s.goto_typed(x, zero());
                    if let Some(_) = self.is_scoped_type_identifier_exact_match(&b, o, i, s) {
                        self.successful_match(scout);
                    }
                }
            } else {
                missing_rule!("exact_match_this_super where typeIdentifier do not have a label")
            }
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_this_super missing {:?}", t)
        }
    }
    fn exact_match_var_declarator(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        assert!(b.has_children());
        let len = b.child_count();
        let x = b.child(&(len - one())).unwrap();
        let (bb, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();
        if t == Type::Identifier {
            let l = bb.try_get_label().unwrap();
            if l == i {
                scout.goto_typed(x, len - one());
                scout.check(self.stores).unwrap();
                log::debug!("success var_declarator");
                self.successful_match(scout); // TODO
            }
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_var_declarator missing {:?}", t)
        }
    }
    fn exact_match_identifier_in_expr_like(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        assert!(b.has_children());
        for (j, x) in b.children().unwrap().iter_children().enumerate() {
            let (r, x) = self.stores.typed_node_store().try_resolve(x).unwrap();
            let t = r.get_type();
            if t == Type::TypeIdentifier {
                let l = r.try_get_label().unwrap();
                if l == i {
                    scout.goto_typed(x, cast(j).unwrap());
                    log::debug!("success identifier_in_expr_like 1");
                    self.successful_match(scout);
                    scout.up(&self.sp_store);
                }
            } else if t == Type::Identifier {
                let l = r.try_get_label().unwrap();
                if l == i {
                    scout.goto_typed(x, cast(j).unwrap());
                    log::debug!("success identifier_in_expr_like 2");
                    self.successful_match(scout);
                    scout.up(&self.sp_store);
                }
            }
        }
    }

    fn exact_match_resource(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        _o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let mut j = zero();
        loop {
            let x = b.child(&j).unwrap();
            let (r, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
            let t: Type = r.get_type();
            if t == Type::TypeIdentifier {
                let l = r.try_get_label().unwrap();
                if l == i {
                    scout.goto_typed(x, j);
                    log::debug!("success 8");
                    self.successful_match(scout);
                }
                return;
            } else if t == Type::Identifier {
                let l = r.try_get_label().unwrap();
                if l == i {
                    scout.goto_typed(x, j);
                    log::debug!("success 8.1");
                    self.successful_match(scout);
                    return;
                }
            } else if t == Type::Modifiers {
            } else {
                return;
            }
            j += one();
        }
    }

    fn exact_match_field_declaration(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        assert!(b.has_children());
        let (r, t, j, x) = {
            let mut i = zero();
            let r;
            let t;
            let mut x;
            loop {
                let (b, xx) = self
                    .stores
                    .typed_node_store()
                    .try_resolve(&b.child(&i).unwrap())
                    .unwrap();
                x = xx;
                let tt = b.get_type();
                if tt == Type::Modifiers {
                    i += one();
                } else if tt == Type::Annotation {
                    i += one();
                } else if tt == Type::Spaces {
                    i += one();
                } else {
                    r = b;
                    t = tt;
                    break;
                }
            }
            (r, t, i, x)
        };
        if t == Type::TypeIdentifier {
            let l = r.try_get_label().unwrap();
            if l == i {
                scout.goto_typed(x, j);
                self.successful_match(scout);
            }
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_field_declaration missing {:?}", t)
        }
    }

    fn exact_match_annotated_type(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        _o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let x = b.child_rev(&zero()).unwrap();
        let len = b.child_count();
        let (bb, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();
        if t == Type::TypeIdentifier {
            if let Some(l) = bb.try_get_label() {
                if l == i {
                    scout.goto_typed(x, len - one());
                    scout.check(self.stores).unwrap();
                    log::debug!("success 9");
                    self.successful_match(scout); // TODO
                }
            } else {
                todo!()
            }
        } else if t == Type::ScopedTypeIdentifier {
            // log::debug!("not matched"); // TODO should check the fully qual name
        } else if t == Type::GenericType {
            // log::debug!("not matched"); // TODO should check the fully qual name
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_annotated_type missing {:?}", t)
        }
    }

    fn exact_match_class_literal(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        _o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let x = b.child(&zero()).unwrap();
        let (bb, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();
        if t == Type::TypeIdentifier || t == Type::Identifier {
            if let Some(l) = b.try_get_label() {
                if l == i {
                    scout.goto_typed(x, zero());
                    scout.check(self.stores).unwrap();
                    log::debug!("success 8");
                    self.successful_match(scout); // TODO
                }
            } else {
                // todo!() // TODO should not append
            }
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_class_literal missing {:?}", t)
        }
    }

    fn exact_match_method_reference(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        let x = b.child(&zero()).unwrap();
        let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = b.get_type();
        if t == Type::TypeIdentifier {
            let l = b.try_get_label().unwrap();
            // TODO should also check if is fully qual then equal to target
            if l != i {
            } else if &RefsEnum::MaybeMissing == self.ana.solver.nodes.with(o).as_ref() {
                scout.goto_typed(x, zero());
                log::debug!("success 6.1");
                self.successful_match(scout); // TODO
            }
        } else if t == Type::Identifier {
            if let Some(l) = b.try_get_label() {
                // TODO should also check if is fully qual then equal to target
                if l != i {
                    // log::debug!("not matched"); // TODO
                } else if &RefsEnum::MaybeMissing == self.ana.solver.nodes.with(o).as_ref() {
                    scout.goto_typed(x, zero());
                    log::debug!("success 6.1");
                    self.successful_match(scout); // TODO
                }
            } else {
                todo!()
            }
        } else if t == Type::Break { // TODO contructor
        } else if t == Type::This {
        } else if t == Type::Super {
        } else if t == Type::ScopedTypeIdentifier {
            // log::debug!("not matched"); // TODO should check the fully qual name
        } else if t == Type::GenericType {
            // log::debug!("not matched"); // TODO should check the fully qual name
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_method_reference missing {:?}", t)
        }
    }

    fn exact_match_cast_expression(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &LabelIdentifier,
        mut scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        let mut ok = false;
        let mut ok2 = false;
        for (j, x) in b.children().unwrap().iter_children().enumerate() {
            let (r, x) = self.stores.typed_node_store().try_resolve(x).unwrap();
            let t = r.get_type();
            if t == Type::LParen {
                // (
                ok = true;
            } else if t == Type::RParen {
                // )
                ok = false;
                ok2 = true;
            } else if t == Type::Spaces {
            } else if ok {
                if t == Type::TypeIdentifier {
                    let l = r.try_get_label().unwrap();
                    if l == i {
                        log::debug!("success 15");
                        self.successful_match(&mut scout);
                        scout.goto_typed(x, cast(j).unwrap());
                        // WARN cast is not trivial to relax
                        // TODO see how to handle it
                        self.successful_match(&mut scout);
                    }
                } else if t == Type::Identifier {
                } else if t == Type::_ConstructorDeclarator {
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_cast_expression missing {:?}", t)
                }
            } else if ok2 {
                if t == Type::Identifier {
                    let l = r.try_get_label().unwrap();
                    if l == i {
                        log::debug!("success 15.1");
                        scout.goto_typed(x, cast(j).unwrap());
                        self.successful_match(scout);
                        scout.up(self.sp_store);
                    }
                } else if t == Type::This {
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_cast_expression missing' {:?}", t)
                    // TODO aaa not yet implemented: True
                }
            }
        }
    }

    fn exact_match_instanceof_expression(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        let x = b.child(&zero()).unwrap();
        let (bb, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();
        if t == Type::Identifier {
            let l = bb.try_get_label().unwrap();
            // TODO should also check if is fully qual then equal to target
            if l == i {
                // if o == MaybeMissing
                log::debug!("success 6");
                scout.goto_typed(x, zero());
                self.successful_match(scout); // TODO
                scout.up(self.sp_store);
            }
        } else if t == Type::This { // TODO
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            todo!("{:?}", t)
        }
        let x = b.child_rev(&zero()).unwrap();
        let len = b.child_count();
        let (bb, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();
        if t == Type::TypeIdentifier || t == Type::Identifier {
            let l = bb.try_get_label().unwrap();
            // TODO should also check if is fully qual then equal to target
            if l == i {
                // if o == MaybeMissing
                log::debug!("success 6.1");
                scout.goto_typed(x, len - one());
                self.successful_match(scout); // TODO
                scout.up(self.sp_store);
            }
        } else if t == Type::This {
            panic!(); // sure ?
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_instanceof_expression missing {:?}", t)
        }
    }

    fn exact_match_array_type(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        let x = b.child(&zero()).unwrap();
        let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = b.get_type();
        if t == Type::TypeIdentifier {
            scout.goto_typed(x, zero());
            let l = b.try_get_label().unwrap();
            if l == i {
                log::debug!("success 5.1");
                self.successful_match(scout); // TODO
            }
        } else if t == Type::ScopedTypeIdentifier {
            // log::debug!("not matched"); // should be handled after
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_array_type missing {:?}", t)
        }
    }

    fn exact_match_generic_type(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        let x = b.child(&zero()).unwrap();
        let (b, _) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = b.get_type();
        if t == Type::TypeIdentifier {
            // scout.goto(x, 0);
            let l = b.try_get_label().unwrap();
            if l == i {
                log::debug!("success 5");
                self.successful_match(scout); // TODO
            }
        } else if t == Type::This { // TODO
        } else if t == Type::ScopedTypeIdentifier {
            // log::debug!("not matched"); // should be handled after
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_generic_type missing {:?}", t)
        }
    }

    fn exact_match_extend_impl_things(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        _o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let mut j = zero();
        let mut r;
        let mut t;
        let mut ok = false;
        let l = b.child_count();
        while j < l {
            let x = b.child(&j).unwrap();
            // r = self.stores.node_store().resolve(x);
            let x = {
                let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
                r = b;
                x
            };
            t = r.get_type();
            if t == Type::Extends {
                // ext
                ok = true;
            } else if t == Type::Implements {
                // impl
                ok = true;
            } else if t == Type::Comma { // comma
            } else if t == Type::Spaces {
            } else if ok {
                if t == Type::TypeIdentifier {
                    if let Some(l) = r.try_get_label() {
                        if l != i {
                            // log::debug!("not matched"); // TODO
                        } else {
                            log::debug!("success 4 2");
                            // scout.up(self.sp_store);
                            scout.goto_typed(x, j);
                            self.successful_match(scout);
                            break;
                        }
                    } else {
                        todo!()
                    }
                } else if t == Type::AnnotatedType {
                    // let x = r.get_child_rev(&0);
                    // let b = self.stores.node_store().resolve(x);
                    // let t = b.get_type();
                    // if t == Type::TypeIdentifier {
                    //     let l = b.try_get_label().unwrap();
                    //     if l == i {
                    //         log::debug!("success 6");
                    //         scout.goto(x, j as usize);
                    //         self.successful_match(scout);
                    //     }
                    // } else if is_individually_matched(t) || is_never_reference(t) {
                    // } else {
                    //     todo!("{:?}", t)
                    // }
                } else if t == Type::_ConstructorDeclarator {
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_extend_impl_things missing {:?}", t)
                }
            }
            j += one();
        }
    }

    fn exact_match_variable_declaration(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        assert!(b.has_children());
        let (r, t, j, x) = {
            let x = b.child(&zero()).unwrap();
            let (r, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
            let t = r.get_type();
            if t == Type::Modifiers {
                let two: <HAST as HyperASTShared>::Idx =
                    <<HAST as HyperASTShared>::Idx as num::One>::one() + one();
                let x = b.child(&(two)).unwrap();
                let (r, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
                let t = r.get_type();
                // scout.goto(x, 2);
                (r, t, two, x)
            } else {
                // scout.goto(x, 0);
                (r, t, zero(), x)
            }
        };
        if t == Type::TypeIdentifier {
            let l = r.try_get_label().unwrap();
            if l == i {
                log::debug!("success 4");
                scout.goto_typed(x, j);
                self.successful_match(scout);
            }
        } else if t == Type::ArrayType {
            // log::debug!("not matched"); // TODO not sure
        } else if is_individually_matched(t) || is_never_reference(t) {
            // log::debug!("not matched");
        } else {
            missing_rule!("exact_match_variable_declaration missing {:?}", t)
        }
    }

    fn exact_match_method_declaration(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        _o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let mut j = num::zero();
        let r;
        let t;
        let mut x;
        loop {
            let xx = b.child(&j).unwrap();
            let (b, xx) = self.stores.typed_node_store().try_resolve(&xx).unwrap();
            x = xx;
            let tt = b.get_type();
            if tt == Type::Modifiers {
                j = j + num::one();
            } else if tt == Type::TypeParameters {
                j = j + num::one();
            } else if tt == Type::Annotation {
                j = j + num::one();
            } else if tt == Type::Spaces {
                j = j + num::one();
            } else {
                r = b;
                t = tt;
                break;
            }
        }
        if t == Type::TypeIdentifier {
            let l = r.try_get_label().unwrap();
            if l == i {
                scout.goto_typed(x, j);
                self.successful_match(scout);
            }
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_method_declaration missing {:?}", t)
        }
    }

    fn exact_match_method_invocation(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        _o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let x = b.child(&num::zero()).unwrap();
        let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        scout.goto_typed(x, zero());
        let t = b.get_type();
        if t == Type::TypeIdentifier {
            let l = b.try_get_label().unwrap();
            if l == i {
                log::debug!("success 1");
                self.successful_match(scout); // TODO
            }
        } else if t == Type::Identifier {
            let l = b.try_get_label().unwrap();
            if l == i {
                log::debug!("success 2");
                self.successful_match(scout); // TODO
            }
        // } else if t == Type::FieldAccess {
        //     // if b.has_label() {
        //     //     let l = b.get_label();
        //     //     if l != i {
        //     //         // log::debug!("not matched"); // TODO
        //     //     } else {log::debug!("success 2.1");
        //     //         self.successful_match(&mut scout); // TODO
        //     //     }
        //     // } else {
        //     //     todo!()
        //     // }
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else if t == Type::This {
            // TODO if scoped might be handled after
        } else if t == Type::Super {
            // TODO if scoped might be handled after
        } else {
            missing_rule!("exact_match_method_invocation missing {:?}", t)
        }
    }

    fn exact_match_object_creation_expression(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let j = b.child_count() - num::one();
        let cs = b.children().unwrap();
        let has_body = self
            .stores
            .typed_node_store()
            .try_resolve(&cs[j])
            .unwrap()
            .0
            .get_type()
            == Type::ClassBody;
        let mut j = j.into();
        if has_body {
            log::debug!("object creation expression has body");
            j -= one();
        }

        let mut matched = false;
        let mut o = o;
        let mut matched_scout = None;

        loop {
            let x = cs[j];
            let (r, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
            let t = r.get_type();

            if t == Type::TypeIdentifier {
                let l = r.try_get_label().unwrap();
                if i == l {
                    let mut scout2 = scout.clone();
                    scout2.goto_typed(x, j);
                    matched_scout = Some(scout2);
                    matched = true;
                }
            } else if t == Type::New {
                // new token
                if j.is_zero() {
                    if matched {
                        match self.ana.solver.nodes.with(o).as_ref() {
                            RefsEnum::MaybeMissing => {
                                self.successful_match(scout);
                                if let Some(mut scout) = matched_scout {
                                    self.successful_match(&mut scout);
                                }
                                // if has_body {
                                //     let x = cs[cs.len() - 1];
                                //     scout.goto(x, cs.len() - 1);
                                //     self.successful_match(scout);
                                // }
                            }
                            _ => {}
                        }
                    }
                    return;
                }
                j -= one();
                break;
            } else if t == Type::GenericType {
                // TODO need full check if creating anonymous class
                log::debug!("need to handle new C<T>() "); // used for  type generics

                let mut scout2 = scout.clone();
                scout2.goto_typed(x, j);
                let x = r.child(&zero()).unwrap();
                let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
                let t = b.get_type();
                if t == Type::TypeIdentifier {
                    // scout.goto(x, 0);
                    let l = b.try_get_label().unwrap();
                    if l == i {
                        scout2.goto_typed(x, zero());
                        matched_scout = Some(scout2);
                        matched = true;
                    }
                } else if t == Type::This { // TODO
                } else if t == Type::ScopedTypeIdentifier {
                    // log::debug!("not matched"); // should be handled after
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_object_creation_expression missing {:?}", t)
                }
            } else if t == Type::ScopedTypeIdentifier {
                // TODO need full check if creating anonymous class
                log::debug!("need to handle new a.b.C() ");
                // NOTE need to uptdate o with what remains

                let mut scout2 = scout.clone();
                scout2.goto_typed(x, j);
                if let Some(oo) =
                    self.exact_match_object_creation_expression_aux(&r, o, i, scout2.clone())
                {
                    matched_scout = Some(scout2);
                    o = oo;
                    matched = true;
                } else {
                    let x = r.child(&zero()).unwrap();
                    let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
                    scout2.goto_typed(x, zero());
                    let t = b.get_type();
                    if t == Type::TypeIdentifier || t == Type::Identifier {
                        let l = b.try_get_label().unwrap();
                        if i == l {
                            self.successful_match(&mut scout2);
                        }
                    }
                }
            } else if t == Type::TypeArguments {
                // used for constructor generics
            } else if t == Type::ArgumentList {
            } else if t == Type::Spaces {
            } else if t == Type::Annotation {
            } else if t == Type::MarkerAnnotation {
            } else if is_individually_matched(t) || is_never_reference(t) {
            } else {
                missing_rule!("exact_match_object_creation_expression missing' {:?}", t)
            }
            if j.is_zero() {
                return;
            }
            j -= one();
        }
        loop {
            let (r, xx) = self.stores.typed_node_store().try_resolve(&cs[j]).unwrap();
            let t = r.get_type();
            if t == Type::TypeIdentifier || t == Type::Identifier {
                if let Some(l) = r.try_get_label() {
                    if matched {
                        match self.ana.solver.nodes.with(o).as_ref() {
                            RefsEnum::ScopedIdentifier(oo, ii) => {
                                match self.ana.solver.nodes.with(*oo).as_ref() {
                                    RefsEnum::MaybeMissing => {
                                        if l == ii.as_ref() {
                                            log::debug!("success 3");
                                            self.successful_match(scout);
                                            // if has_body {
                                            //     let x = cs[cs.len() - 1];
                                            //     scout.goto(x, cs.len() - 1);
                                            //     self.successful_match(scout);
                                            // }
                                        }
                                    }
                                    _ => {
                                        return;
                                    }
                                }
                            }
                            _ => {}
                        }
                    } else {
                        match self.ana.solver.nodes.with(o).as_ref() {
                            RefsEnum::ScopedIdentifier(oo, ii) => {
                                match self.ana.solver.nodes.with(*oo).as_ref() {
                                    RefsEnum::MaybeMissing => {
                                        if l == ii.as_ref() {
                                            log::debug!("success 3");
                                            let mut scout = scout.clone();
                                            scout.goto_typed(xx, j);
                                            self.successful_match(&mut scout)
                                        }
                                    }
                                    _ => {
                                        return;
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                } else {
                    todo!()
                }
                return;
            } else if t == Type::Annotation {
            } else if t == Type::MarkerAnnotation {
            } else if t == Type::Spaces {
            } else if t == Type::Dot {
            } else if t == Type::FieldAccess {
                log::debug!(
                    "need to handle a.new b.C() {}",
                    hyperast::nodes::SyntaxSerializer::new(
                        self.stores,
                        scout.node_always(&self.sp_store).unwrap().as_id().clone()
                    )
                );
            } else if t == Type::GenericType {
                // TODO need full check if creating anonymous class
                log::debug!(
                    "need to handle a<T>.new b.C() {}",
                    hyperast::nodes::SyntaxSerializer::new(
                        self.stores,
                        scout.node_always(&self.sp_store).unwrap().as_id().clone()
                    )
                );
            } else if t == Type::ScopedTypeIdentifier {
                // TODO need a unit test
                // TODO need full check if creating anonymous class
                log::debug!("need to handle matching A in A.new B.C() ");
                if matched {
                    let (oo, ii) = match self.ana.solver.nodes.with(o).as_ref() {
                        RefsEnum::ScopedIdentifier(o, i) => (*o, *i.as_ref()),
                        RefsEnum::TypeIdentifier(o, i) => (*o, *i.as_ref()),
                        _ => return,
                    };

                    let mut scout2 = scout.clone();
                    let xx = cs[j];
                    let (bb, xx) = self.stores.typed_node_store().try_resolve(&xx).unwrap();
                    scout2.goto_typed(xx, j);
                    if let Some(_) =
                        self.is_scoped_type_identifier_exact_match(&bb, oo, &ii, scout2)
                    {
                        self.successful_match(scout);
                        // if has_body {
                        //     let x = cs[cs.len() - 1];
                        //     scout.goto(x, cs.len() - 1);
                        //     self.successful_match(scout);
                        // }
                    }
                }
                return;
            } else if is_individually_matched(t) || is_never_reference(t) {
            } else {
                missing_rule!("exact_match_object_creation_expression missing'' {:?}", t)
            }
            if j.is_zero() {
                if matched {
                    match self.ana.solver.nodes.with(o).as_ref() {
                        RefsEnum::MaybeMissing => {
                            self.successful_match(scout);
                            if let Some(mut scout) = matched_scout {
                                self.successful_match(&mut scout);
                            }
                            // if has_body {
                            //     let x = cs[cs.len() - 1];
                            //     scout.goto(x, cs.len() - 1);
                            //     self.successful_match(scout);
                            // }
                        }
                        _ => {}
                    }
                }
                return;
            }
            j -= one();
        }
    }

    fn exact_match_object_creation_expression_aux(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &LabelIdentifier,
        scout: TypedScout<TIdN<IdN>, HAST::Idx>,
    ) -> Option<usize> {
        assert!(b.has_children());
        let x = b.child_rev(&zero()).unwrap();
        let (bb, _) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();

        if t == Type::TypeIdentifier {
            if let Some(l) = bb.try_get_label() {
                if l != i {
                    return None;
                }
            } else {
                todo!()
            }
        } else {
            panic!("{:?}", t)
        }

        let x = b.child(&zero()).unwrap();
        let (bb, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();
        if t == Type::TypeIdentifier {
            if let Some(l) = bb.try_get_label() {
                match self.ana.solver.nodes.with(o).as_ref() {
                    RefsEnum::MaybeMissing => {
                        if l == i {
                            return Some(o);
                        }
                    }
                    RefsEnum::ScopedIdentifier(oo, ii) => {
                        if l == ii.as_ref() {
                            return Some(*oo);
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            } else {
                todo!()
            }
        } else if t == Type::ScopedTypeIdentifier {
            let (o, i) = match self.ana.solver.nodes.with(o).as_ref() {
                RefsEnum::ScopedIdentifier(oo, ii) => (*oo, *ii.as_ref()),
                RefsEnum::TypeIdentifier(_, _) => {
                    todo!()
                }
                RefsEnum::MaybeMissing | RefsEnum::Root => {
                    return None;
                }
                x => {
                    todo!("{:?}", x); // return None; // TODO handle other cases
                }
            };
            let mut scout = scout.clone();
            scout.goto_typed(x, zero());

            log::trace!(
                "recursive call to exact_match_object_creation_expression2_aux {:?}",
                self.ana.solver.nodes.with(o)
            );
            return self.exact_match_object_creation_expression_aux(&bb, o, &i, scout);
        } else if t == Type::GenericType {
            // log::debug!("not matched"); // TODO should check the fully qual name
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            missing_rule!("exact_match_object_creation_expression_aux missing {:?}", t)
        }
        None
    }

    fn exact_match_enhanced_for_statement(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        let mut ok = false;
        let mut ok2 = false;
        for (j, x) in b.children().unwrap().iter_children().enumerate() {
            let (r, x) = self.stores.typed_node_store().try_resolve(x).unwrap();
            let t = r.get_type();
            if t == Type::LParen {
                // (
                ok = true;
            } else if t == Type::RParen {
                // )
                break;
            } else if t == Type::Spaces {
            } else if t == Type::Colon {
                // :
                ok = false;
                ok2 = true;
            } else if ok {
                if t == Type::TypeIdentifier {
                    let l = r.try_get_label().unwrap();
                    if l == i {
                        log::debug!("success 10");
                        scout.goto_typed(x, cast(j).unwrap());
                        self.successful_match(scout);
                        scout.up(self.sp_store);
                    }
                } else if t == Type::Identifier {
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_enhanced_for_statement missing {:?}", t)
                }
            } else if ok2 {
                if t == Type::Identifier {
                    let l = r.try_get_label().unwrap();
                    if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
                    } else if l == i {
                        log::debug!("success 11");
                        scout.goto_typed(x, cast(j).unwrap());
                        self.successful_match(scout);
                        scout.up(self.sp_store);
                    }
                } else if t == Type::This {
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_enhanced_for_statement missing' {:?}", t)
                }
            }
        }
    }

    fn exact_match_array_access(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        if &RefsEnum::MaybeMissing != self.ana.solver.nodes.with(o).as_ref() {
            return;
        }
        let mut ok = true;
        let mut ok2 = false;
        for (j, x) in b.children().unwrap().iter_children().enumerate() {
            let (r, x) = self.stores.typed_node_store().try_resolve(x).unwrap();
            let t = r.get_type();
            if t == Type::LBracket {
                // [
                ok = false;
                ok2 = true;
            } else if t == Type::RBracket {
                // ]
            } else if t == Type::Spaces {
            } else if ok {
                if t == Type::Identifier {
                    let l = r.try_get_label().unwrap();
                    if l == i {
                        log::debug!("success array access 1");
                        scout.goto_typed(x, cast(j).unwrap());
                        self.successful_match(scout);
                        scout.up(self.sp_store);
                    }
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_array_access missing {:?}", t)
                }
            } else if ok2 {
                if t == Type::Identifier {
                    let l = r.try_get_label().unwrap();
                    if l == i {
                        log::debug!("success array access 2");
                        scout.goto_typed(x, cast(j).unwrap());
                        self.successful_match(scout);
                        scout.up(self.sp_store);
                    }
                } else if is_individually_matched(t) || is_never_reference(t) {
                } else {
                    missing_rule!("exact_match_array_access missing' {:?}", t)
                }
            }
        }
    }

    fn exact_match_field_access(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: RefPtr,
        i: &LabelIdentifier,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        assert!(b.has_children());
        let (o_id, _sup, _i_id) = {
            let len = b.child_count();
            let cs = b.children().unwrap();
            (cs[zero()], len.to_usize().unwrap() > 3, cs[len - one()])
        };
        let (o_b, o_id) = self.stores.typed_node_store().try_resolve(&o_id).unwrap();
        let o_t = o_b.get_type();

        let eref = self.ana.solver.nodes.with(o);

        if let RefsEnum::MaybeMissing = eref.as_ref() {
            if o_t == Type::Identifier {
                let l = o_b.try_get_label().unwrap();
                if l == i {
                    // match ?.a on a.b
                    scout.goto_typed(o_id, zero());
                    scout.check(self.stores).unwrap();
                    log::debug!("success exact_match_field_access 1.1");
                    self.successful_match(scout);
                }
            }
        } else {
            if let Some(_s) = self.is_field_access_exact_match(&b, o, i, scout.clone()) {
                // *scout = s;
                self.successful_match(scout);
            }
        }
    }

    /// returns the last well matching scout.
    /// so that we can do the match in 2 times like with `A.new B.C()` (nit a field access here ;))
    fn is_field_access_exact_match(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &string_interner::symbol::SymbolU32,
        mut scout: TypedScout<TIdN<IdN>, HAST::Idx>,
    ) -> Option<TypedScout<TIdN<IdN>, HAST::Idx>> {
        assert!(b.has_children());
        // TODO should handle and do a test case for explicit access to the member of a parent instance eg. A.super.b
        // TODO also look at how to handle precisely this.a or super.a
        let (o_id, _sup, i_id) = {
            let len = b.child_count();
            let cs = b.children().unwrap();
            (cs[zero()], len.to_usize().unwrap() > 3, cs[len - one()])
        };
        let (o_b, o_id) = self.stores.typed_node_store().try_resolve(&o_id).unwrap();
        let o_t = o_b.get_type();
        let (i_b, _) = self.stores.typed_node_store().try_resolve(&i_id).unwrap();
        let i_t = i_b.get_type();

        let eref = self.ana.solver.nodes.with(o);

        if let RefsEnum::MaybeMissing = eref.as_ref() {
            return None;
        }
        if i_t == Type::This && o_t == Type::FieldAccess {
            // TODO make sure it is possible
            scout.goto_typed(o_id, zero());
            return self.is_field_access_exact_match(&o_b, o, &i, scout);
        }

        if i_t == Type::Identifier {
            let l = i_b.try_get_label().unwrap();
            if l != i {
                return None;
            }
        }

        let oi = match eref.as_ref().clone() {
            RefsEnum::ScopedIdentifier(oo, ii) => (oo, *ii.as_ref()),
            RefsEnum::TypeIdentifier(oo, ii) => (oo, *ii.as_ref()),
            RefsEnum::Root => {
                panic!();
            }
            RefsEnum::This(o) => {
                match self.ana.solver.nodes.with(o).as_ref().clone() {
                    RefsEnum::MaybeMissing => {
                        return None; // TODO
                    }
                    RefsEnum::TypeIdentifier(oo, ii) => {
                        return self.is_scoped_type_identifier_exact_match(
                            &o_b,
                            oo,
                            ii.as_ref(),
                            scout,
                        )
                    }
                    x => todo!("{:?}", x),
                }

                // if i_t == Type::This && o_t == Type::FieldAccess {
                //     scout.goto(o_id, 0);
                //     return self.is_field_access_exact_match(&o_b, o, &i, scout);
                // }
            }
            x => {
                todo!("{:?}", x);
            }
        };

        if o_t == Type::Identifier {
            let l = o_b.try_get_label().unwrap();
            match self.ana.solver.nodes.with(o).as_ref() {
                RefsEnum::MaybeMissing => {
                    if l == i {
                        // match ?.a on a.b
                        scout.goto_typed(o_id, zero());
                        scout.check(self.stores).unwrap();
                        log::debug!("success is_field_access_exact_match 1.1");
                        return Some(scout);
                    }
                    return None;
                }
                RefsEnum::ScopedIdentifier(oo, ii) => {
                    match self.ana.solver.nodes.with(*oo).as_ref() {
                        // match (?.a).b on (a).b
                        RefsEnum::MaybeMissing if l == ii.as_ref() => {
                            scout.goto_typed(o_id, zero());
                            scout.check(self.stores).unwrap();
                            log::debug!("success is_field_access_exact_match 1.1");
                            return Some(scout);
                        }
                        _ => return None,
                    }
                }
                _ => return None,
            }
        } else if o_t == Type::This {
            panic!();
            // [ ] There should be a ref finder (the recusive part that go though decls) specialized for this,
            // as it needs to handle going through type declarations (because it changes how `this` should be match).
            // NOTE but just the exact match should be ok matching this if it is what ze want to match,
            // only the recursive search needs to be specialized,
            // [ ] actually for this reason the exact match should not traverse type declarations.
            // NOTE I think there should also be more logic used when searching through hierarchy.
            // return Some(scout);
        } else if o_t == Type::FieldAccess {
            let (o, i) = oi;
            scout.goto_typed(o_id, zero());

            log::trace!(
                "recursive call to is_field_access_exact_match {:?}",
                self.ana.solver.nodes.with(o)
            );
            return self.is_field_access_exact_match(&o_b, o, &i, scout);
        // } else if o_t == Type::ScopedTypeIdentifier {
        // } else if o_t == Type::GenericType {
        } else {
            log::debug!(
                "{}",
                hyperast::nodes::SyntaxSerializer::new(
                    self.stores,
                    scout.node_always(&self.sp_store).unwrap().as_id().clone()
                )
            );
            todo!("{:?}", o_t)
        }
    }

    fn is_scoped_type_identifier_exact_match(
        &mut self,
        b: &<HAST as TypedHyperAST<'a, TIdN<IdN>>>::TT,
        o: usize,
        i: &string_interner::symbol::SymbolU32,
        mut scout: TypedScout<TIdN<IdN>, HAST::Idx>,
    ) -> Option<TypedScout<TIdN<IdN>, HAST::Idx>> {
        assert!(b.has_children());
        // TODO should handle and do a test case for explicit access to the member of a parent instance eg. A.super.b
        let x = b.child(&zero()).unwrap();
        let (bb, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
        let t = bb.get_type();
        let mut matching_o = false;
        if t == Type::TypeIdentifier {
            if let Some(l) = bb.try_get_label() {
                match self.ana.solver.nodes.with(o).as_ref() {
                    RefsEnum::MaybeMissing => {
                        if l == i {
                            scout.goto_typed(x, zero());
                            scout.check(self.stores).unwrap();
                            log::debug!("success is_scoped_type_identifier_exact_match 1");
                            return Some(scout);
                        }
                    }
                    RefsEnum::ScopedIdentifier(oo, ii) => {
                        match self.ana.solver.nodes.with(*oo).as_ref() {
                            RefsEnum::MaybeMissing => {
                                if l == ii.as_ref() {
                                    matching_o = true;
                                }
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            } else {
                todo!()
            }
        } else if t == Type::This {
            // return Some(scout);
        } else if t == Type::Identifier {
            if let Some(l) = bb.try_get_label() {
                match self.ana.solver.nodes.with(o).as_ref() {
                    RefsEnum::MaybeMissing => {
                        if l == i {
                            scout.goto_typed(x, zero());
                            scout.check(self.stores).unwrap();
                            log::debug!("success is_scoped_type_identifier_exact_match 1");
                            return Some(scout);
                        }
                    }
                    RefsEnum::ScopedIdentifier(oo, ii) => {
                        match self.ana.solver.nodes.with(*oo).as_ref() {
                            RefsEnum::MaybeMissing => {
                                if l == ii.as_ref() {
                                    matching_o = true;
                                }
                            }
                            _ => {
                                return None;
                            }
                        }
                    }
                    _ => {
                        return None;
                    }
                }
            } else {
                todo!()
            }
        } else if t == Type::ScopedTypeIdentifier {
            let oi = match self.ana.solver.nodes.with(o).as_ref() {
                RefsEnum::ScopedIdentifier(oo, ii) => Some((*oo, *ii.as_ref())),
                RefsEnum::TypeIdentifier(_, _) => {
                    todo!()
                }
                RefsEnum::MaybeMissing | RefsEnum::Root => {
                    return None;
                }
                RefsEnum::This(_) => {
                    // TODO use remaining refs
                    None
                }
                x => {
                    todo!("{:?}", x); // return None; // TODO handle other cases
                }
            };
            if let Some((o, i)) = oi {
                let mut scout = scout.clone();
                scout.goto_typed(x, zero());

                log::trace!(
                    "recursive call to is_scoped_type_identifier_exact_match {:?}",
                    self.ana.solver.nodes.with(o)
                );
                if let Some(_) = self.is_scoped_type_identifier_exact_match(&bb, o, &i, scout) {
                    matching_o = true;
                }
            } else {
                matching_o = true;
            }
        } else if t == Type::GenericType {
            // log::debug!("not matched"); // TODO should check the fully qual name
        } else if is_individually_matched(t) || is_never_reference(t) {
        } else {
            todo!("{:?}", t)
        }
        if matching_o {
            let x = b.child_rev(&zero()).unwrap();
            let (bb, _) = self.stores.typed_node_store().try_resolve(&x).unwrap();
            let t = bb.get_type();

            if t == Type::TypeIdentifier || t == Type::Identifier {
                if let Some(l) = bb.try_get_label() {
                    if l == i {
                        // scout.goto(x, 2);
                        scout.check(self.stores).unwrap();
                        log::debug!("success is_scoped_type_identifier_exact_match 3");
                        return Some(scout);
                    }
                } else {
                    todo!()
                }
            } else {
                panic!("{:?}", t)
            }
        }
        None
    }

    pub fn successful_match(&mut self, scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>) {
        self.sp_store.check(self.stores).expect("aa");
        scout.check(self.stores).expect("bb");
        let _r = self.sp_store.push_typed(scout);
        if let Err(e) = self.sp_store.check(self.stores) {
            log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
            log::error!("corrupted scout: {}", e)
        }
        // let x = scout.node_always(&self.sp_store);
        // let b = self.stores.node_store().resolve(x);
        // let t = b.get_type();

        // TODO log::debug!("zszz {:?}", scout.make_position(&self.sp_store, self.stores));
        // if t == Type::ScopedTypeIdentifier || t == Type::GenericType || t == Type::TypeIdentifier {
        let mut scout = if let Some(x) = self.relax_to_type(scout.clone()) {
            x
        } else if let Some(x) = self.relax_to_typed(scout.clone()) {
            x
        } else {
            log::debug!("abort match because of relax");
            return;
        };
        if let Err(_) = scout.check(self.stores) {
            log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
            log::error!("corrupted scout'")
        }
        // handle body of ObjectCreationExpression
        {
            // TODO log::debug!("zzz {:?}", scout.make_position(&self.sp_store, self.stores));
            let x = scout.node_always(&self.sp_store).unwrap();
            let b = self.stores.typed_node_store().resolve(&x);
            let t = b.get_type();
            if t == Type::ObjectCreationExpression {
                assert!(b.has_children());
                let len = b.child_count();
                let cs = b.children().unwrap();
                let x = cs[len - one()];
                let (b, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
                let t = b.get_type();
                if t == Type::ClassBody {
                    let mut scout = scout.clone();
                    scout.goto_typed(x, len - one());
                    let r = self.sp_store.push_typed(&mut scout);
                    if let Err(e) = self.sp_store.check(self.stores) {
                        log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
                        log::error!("corrupted scout class body: {}", e)
                    } else {
                        self.refs.push(r);
                    }
                    // let it = self.sp_store.get(r);
                    // TODO log::debug!("really found {:?}", it.make_position(self.stores));
                }
                let mut s = scout.clone();
                let Some(x) = s.up(&self.sp_store) else {
                    dbg!("expected up to work");
                    return;
                };
                let x = x.unwrap();
                // let x = s.up(&self.sp_store).expect("up").unwrap();
                let b = self.stores.typed_node_store().resolve(&x);
                let t = b.get_type();
                if t == Type::ExpressionStatement
                    || t == Type::CastExpression
                    || t == Type::ParenthesizedExpression
                {
                    let _ = scout.up(&self.sp_store).expect("up");
                    scout.check(self.stores).expect("dd");
                    if let Err(e) = self.sp_store.check(self.stores) {
                        log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
                        log::error!("corrupted scout after class body: {}", e);
                        return;
                    } else {
                        scout = self.relax_to_typed(scout.clone()).expect("relax");
                        scout.check(self.stores).expect("ee");
                    }
                }
            }
        }

        scout.check(self.stores).expect("dd");
        let r = self.sp_store.push_typed(&mut scout);
        if let Err(e) = self.sp_store.check(self.stores) {
            log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
            log::error!("corrupted scout'': {}", e)
        } else {
            self.refs.push(r);
        }
        // let it = self.sp_store.get(r);
        // TODO log::debug!("really found {:?}", it.make_position(&self.stores));
    }

    /// relax to type eg. generic
    /// ObjectCreationExpression do not go up to statement expression because we need tp handle class body if any
    fn relax_to_type(
        &mut self,
        scout: TypedScout<TIdN<IdN>, HAST::Idx>,
    ) -> Option<TypedScout<TIdN<IdN>, HAST::Idx>> {
        let x = scout.node_always(&self.sp_store).unwrap();
        let o = scout.offset_always(&self.sp_store);
        let b = self.stores.typed_node_store().resolve(&x);
        let t = b.get_type();

        if t == Type::ScopedIdentifier
            || t == Type::ScopedTypeIdentifier
            // || t == Type::ArrayCreationExpression
            // || t == Type::ParenthesizedExpression
            // || t == Type::CastExpression
            || t == Type::GenericType
        // || t == Type::SuperInterfaces
        // || t == Type::Superclass
        // || t == Type::ExtendsInterfaces
        // || t == Type::AnnotatedType
        // || t == Type::This
        // || t == Type::CatchType
        // || t == Type::ArrayType
        // || t == Type::TypeBound
        {
        } else if t == Type::Identifier || t == Type::TypeIdentifier {
        } else {
            return None;
        }
        let mut parent_scout = scout.clone();
        if let Some(xx) = parent_scout.up(&self.sp_store) {
            let bb = self.stores.typed_node_store().resolve(&xx.unwrap());
            let tt = bb.get_type();
            if tt == Type::ScopedIdentifier
                || tt == Type::ScopedTypeIdentifier
                || tt == Type::FieldAccess
                || tt == Type::MethodInvocation
            {
                Some(scout)
            } else if tt == Type::ObjectCreationExpression {
                if t == Type::ScopedIdentifier
                    || t == Type::ScopedTypeIdentifier
                    || t == Type::GenericType
                {
                    self.relax_to_type(parent_scout)
                } else {
                    Some(scout)
                }
            } else if tt == Type::ArrayCreationExpression {
                self.relax_to_type(parent_scout)
            } else if tt == Type::GenericType {
                self.relax_to_type(parent_scout)
            } else if tt == Type::CastExpression {
                // WARN for spoon
                assert!(bb.has_children());
                let cs = bb.children().unwrap();
                for x in cs.after(o).iter_children() {
                    let t = self
                        .stores
                        .typed_node_store()
                        .try_resolve(x)
                        .unwrap()
                        .0
                        .get_type();
                    if t == Type::LParen {
                        return Some(scout);
                    }
                }
                self.relax_to_typed(parent_scout) // TODO check it
            } else {
                Some(scout)
            }
        } else {
            Some(scout)
        }
    }

    /// relax type to the element it types
    /// ObjectCreationExpression do not go up to statement expression because we need tp handle class body if any
    fn relax_to_typed(
        &mut self,
        scout: TypedScout<TIdN<IdN>, HAST::Idx>,
    ) -> Option<TypedScout<TIdN<IdN>, HAST::Idx>> {
        let x = scout.node_always(&self.sp_store).unwrap();
        let b = self.stores.typed_node_store().resolve(&x);
        let t = b.get_type();

        if t.is_type_body()
            // || t.is_directory()
            || t.is_block_related()
            || t.is_structural_statement()
            || t.is_argument_list()
            || t.is_parameter_list()
        {
            log::error!("should not relax {:?}", t);
            log::error!("backtrace: {}", std::backtrace::Backtrace::force_capture());
            return None;
        } else if t == Type::ScopedIdentifier
            || t == Type::ScopedTypeIdentifier
            || t == Type::ArrayCreationExpression
            || t == Type::ParenthesizedExpression
            || t == Type::CastExpression
            || t == Type::GenericType
            || t == Type::SuperInterfaces
            || t == Type::Superclass
            || t == Type::ExtendsInterfaces
            || t == Type::AnnotatedType
            || t == Type::This
            || t == Type::CatchType
            || t == Type::ArrayType
            || t == Type::TypeBound
        {
        } else if t == Type::Identifier || t == Type::TypeIdentifier {
        } else if t == Type::FieldAccess {
            return Some(scout);
        } else if t == Type::VariableDeclarator {
            return Some(scout);
        } else if t == Type::ObjectCreationExpression {
            return Some(scout);
        } else if t == Type::Annotation || t == Type::MarkerAnnotation {
            return Some(scout);
        } else if t.is_parameter()
            || t.is_type_declaration()
            || t.is_value_member()
            || t.is_executable_member()
            || t == Type::LocalVariableDeclaration//|| t.is_declarative_statement()
            || t == Type::ExpressionStatement//|| t.is_simple_statement()
            || t == Type::EnhancedForVariable
            || t == Type::Resource
            || t == Type::CatchFormalParameter
        {
            return Some(scout);
        } else if t == Type::ArgumentList || t == Type::TypeArguments {
            return Some(scout);
        } else if t == Type::TernaryExpression
            || t == Type::UpdateExpression
            || t == Type::UnaryExpression
            || t == Type::BinaryExpression
            || t == Type::ReturnStatement
            || t == Type::ThrowStatement
            || t == Type::AssertStatement
            || t == Type::AssignmentExpression
            || t == Type::Wildcard
        {
            return Some(scout);
        } else {
            log::warn!("what to do with {:?}", t);
            return Some(scout);
        }
        let mut parent_scout = scout.clone();
        if let Some(xx) = parent_scout.up(&self.sp_store) {
            let Ok(xx) = xx else { return Some(scout) };
            let bb = self.stores.typed_node_store().resolve(&xx);
            let tt = bb.get_type();
            if tt == Type::ScopedIdentifier
                || tt == Type::ScopedTypeIdentifier
                || tt == Type::FieldAccess
                || tt == Type::MethodInvocation
            {
                Some(scout)
            } else if tt == Type::ParenthesizedExpression {
                let mut parent_parent_scout = parent_scout.clone();
                if let Some(xxx) = parent_parent_scout.up(&self.sp_store) {
                    let bbb = self.stores.typed_node_store().resolve(&xxx.unwrap());
                    let ttt = bbb.get_type();
                    if ttt == Type::SwitchExpression
                        || ttt == Type::IfStatement
                        || ttt == Type::WhileStatement
                        || ttt == Type::DoStatement
                        || ttt == Type::SynchronizedStatement
                    {
                        Some(scout)
                    } else {
                        self.relax_to_typed(parent_scout)
                    }
                } else {
                    None
                }
            } else if tt == Type::CatchType
                || tt == Type::Annotation
                || tt == Type::MarkerAnnotation
            {
                self.relax_to_typed(parent_scout)
            } else if tt == Type::CastExpression {
                assert!(bb.has_children());
                let cs = bb.children().unwrap();
                let o = scout.offset_always(&self.sp_store);
                for x in cs.after(o).iter_children() {
                    let t = self
                        .stores
                        .typed_node_store()
                        .try_resolve(x)
                        .unwrap()
                        .0
                        .get_type();
                    if t == Type::_MethodHeader {
                        return Some(scout);
                    }
                }
                self.relax_to_typed(parent_scout)
            } else if tt == Type::ArrayCreationExpression || tt == Type::ObjectCreationExpression {
                self.relax_to_typed(parent_scout) // TODO check
            } else if tt == Type::GenericType || tt == Type::ArrayType {
                self.relax_to_typed(parent_scout)
            } else if tt == Type::AnnotatedType {
                self.relax_to_typed(parent_scout)
            } else if tt == Type::TypeBound
                || tt == Type::WildcardExtends
                || tt == Type::WildcardSuper
            {
                parent_scout.up(&self.sp_store);
                self.relax_to_typed(parent_scout)
            } else if tt == Type::SuperInterfaces
                || tt == Type::Superclass
                || tt == Type::ExtendsInterfaces
            {
                // parent_scout.up(&self.sp_store);
                // Some(parent_scout)
                Some(scout)
            } else if tt.is_parameter()
                || tt.is_type_declaration()
                || tt.is_value_member()
                || tt.is_executable_member()
                || tt == Type::LocalVariableDeclaration//|| tt.is_declarative_statement()
                || tt == Type::ExpressionStatement//|| tt.is_simple_statement()
                || tt == Type::EnhancedForVariable
                || tt == Type::Resource
                || tt == Type::CatchFormalParameter
            {
                Some(parent_scout)
            } else if tt.is_type_body()
                // || tt.is_directory()
                || tt.is_block_related()
                || tt.is_structural_statement()
                || tt == Type::ForStatement
                || tt == Type::EnhancedForStatement
            {
                Some(scout)
            } else if tt.is_argument_list() {
                Some(scout)
            } else if tt.is_parameter_list()
            // || tt.is_directory()
            // || tt.is_block_related()
            // || tt.is_structural_statement()
            {
                Some(scout)
            } else if tt == Type::LambdaExpression
                || tt == Type::InstanceofExpression
                || tt == Type::MethodReference
            {
                return Some(scout);
            } else if tt == Type::ArgumentList || t == Type::TypeArguments {
                return Some(scout);
            } else if tt == Type::AssignmentExpression
                || tt == Type::TernaryExpression
                || tt == Type::UpdateExpression
                || tt == Type::UnaryExpression
                || tt == Type::BinaryExpression
                || tt == Type::ReturnStatement
                || t == Type::ThrowStatement
                || tt == Type::AssertStatement
            {
                return Some(scout);
            } else if tt == Type::ArrayType {
                // TODO check
                return Some(scout);
            } else if tt == Type::VariableDeclarator {
                return Some(scout);
            } else {
                log::debug!("what to do with parent node {:?} of {:?}", tt, t);
                Some(scout)
            }
        } else {
            Some(scout)
        }
    }

    // wait for stable inherent_associated_types feature
    // type S = Scout<HAST::IdN>;

    // /// relax type to the expression it types
    // pub fn relax_to_next_typed(&mut self, scout: Scout<HAST::IdN,HAST::Idx>) -> Option<Scout<HAST::IdN,HAST::Idx>> {
    //     let x = scout.node_always(&self.sp_store);
    //     let b = self.stores.typed_node_store().resolve(&x);
    //     let t = b.get_type();

    //     if t.is_type_body()
    //         || t.is_directory()
    //         || t.is_block_related()
    //         || t.is_structural_statement()
    //     {
    //         return None;
    //         // } else if t.is_declarative_statement()
    //         //     || t.is_simple_statement()
    //         //     || t.is_parameter()
    //         //     || t.is_type_declaration()
    //         // {
    //     }
    //     let mut parent_scout = scout.clone();
    //     if let Some(xx) = parent_scout.up(&self.sp_store) {
    //         let bb = self.stores.node_store().resolve(&xx);
    //         let tt = bb.get_type();
    //         if tt == Type::ScopedIdentifier || tt == Type::ScopedTypeIdentifier {
    //             Some(scout)
    //         } else if tt == Type::ParenthesizedExpression {
    //             Some(parent_scout)
    //         } else if tt == Type::AssignmentExpression || tt == Type::CastExpression {
    //             Some(parent_scout)
    //         } else if tt == Type::ArrayCreationExpression || tt == Type::ObjectCreationExpression {
    //             Some(parent_scout) // TODO check
    //         } else if tt == Type::GenericType {
    //             Some(parent_scout)
    //         } else if tt == Type::AnnotatedType || tt == Type::AnnotatedType {
    //             Some(parent_scout)
    //         } else if tt == Type::SuperInterfaces
    //             || tt == Type::Superclass
    //             || tt == Type::ExtendsInterfaces
    //         {
    //             parent_scout.up(&self.sp_store);
    //             Some(parent_scout)
    //         } else if tt.is_declarative_statement()
    //             || tt.is_simple_statement()
    //             || tt.is_parameter()
    //             || tt.is_type_declaration()
    //             || tt.is_value_member()
    //             || tt.is_executable_member()
    //         {
    //             Some(parent_scout)
    //         } else if tt.is_type_body()
    //             || tt.is_directory()
    //             || tt.is_block_related()
    //             || tt.is_structural_statement()
    //         {
    //             return None;
    //         } else if tt.is_argument_list() {
    //             return None;
    //         } else if tt.is_parameter_list()
    //         // || tt.is_directory()
    //         // || tt.is_block_related()
    //         // || tt.is_structural_statement()
    //         {
    //             return None;
    //         } else {
    //             Some(scout)
    //         }
    //     } else {
    //         Some(scout)
    //     }
    // }
}

/// WARN not exaustive set
fn is_individually_matched(t: Type) -> bool {
    t == Type::ScopedTypeIdentifier
        || t == Type::FieldAccess
        || t == Type::ScopedIdentifier
        || t == Type::MethodInvocation
        || t == Type::ArrayAccess
        || t == Type::ObjectCreationExpression
        || t == Type::ParenthesizedExpression
        || t == Type::TernaryExpression
        || t == Type::GenericType
        || t == Type::AnnotatedType
        || t == Type::ArrayType
        || t == Type::CastExpression
        || t == Type::Modifiers
        || t == Type::ArrayCreationExpression
        || t == Type::BinaryExpression
        || t == Type::UnaryExpression
        || t == Type::UpdateExpression
        || t == Type::SwitchExpression
        || t == Type::AssignmentExpression
        || t == Type::EnhancedForVariable
        || t == Type::InstanceofExpression
        || t == Type::FieldAccess
        || t == Type::ArrayInitializer
        || t == Type::LambdaExpression
        || t == Type::MethodReference
}
/// WARN not exaustive set
fn is_never_reference(t: Type) -> bool {
    t.is_comment()
    || t == Type::ClassLiteral // out of scope for tool ie. reflexivity
    || t == Type::StringLiteral
    || t == Type::CharacterLiteral
    || t == Type::NullLiteral
    || t == Type::VoidType
    || t == Type::DecimalIntegerLiteral
    || t == Type::OctalIntegerLiteral
    || t == Type::BinaryIntegerLiteral
    || t == Type::HexIntegerLiteral
    || t == Type::IntegralType
    || t == Type::FloatingPointType
    || t == Type::HexFloatingPointLiteral
    || t == Type::DecimalFloatingPointLiteral
    || t == Type::BooleanType
    // log::debug!("not matched"); // TODO not sure
}

pub fn remake_pkg_ref<'a, IdN: Copy + Eq + NodeId<IdN = IdN> + Debug, HAST>(
    stores: &'a HAST,
    ana: &mut PartialAnalysis,
    x: TIdN<HAST::IdN>,
) -> Option<RefPtr>
where
    HAST: TypedHyperAST<
        'a,
        TIdN<IdN>,
        IdN = IdN,
        // T = HashedNodeRef<'a,Type>,
        Label = LabelIdentifier,
    >,
    <HAST as hyperast::types::HyperAST<'a>>::T: Tree<ChildIdx = HAST::Idx>,
    <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT:
        TypedTree<Type = Type, ChildIdx = HAST::Idx>,
{
    // log::debug!("{}",hyperast::nodes::SyntaxSerializer::new(
    //     stores,
    //     x
    // ));
    let b = stores.typed_node_store().resolve(&x);
    let t = b.get_type();
    let two = <HAST::Idx as num::One>::one() + one();
    if t == Type::ScopedAbsoluteIdentifier {
        assert!(b.has_children());
        let x = b.child(&zero()).unwrap();
        let (_, x) = stores.typed_node_store().try_resolve(&x).unwrap();
        let o = remake_pkg_ref(stores, ana, x)?;
        let x = b.child(&(two)).unwrap();
        let (b, _) = stores.typed_node_store().try_resolve(&x).unwrap();
        if let Some(i) = b.try_get_label() {
            let f = IdentifierFormat::from(stores.label_store().resolve(i));
            let l = LabelPtr::new(*i, f);
            let i = ana.solver.intern(RefsEnum::ScopedIdentifier(o, l));
            Some(i)
        } else {
            None
        }
    } else if t == Type::Identifier {
        let i = b.get_label_unchecked();
        let o = ana.solver.intern(RefsEnum::Root);
        let f = IdentifierFormat::from(stores.label_store().resolve(i));
        let l = LabelPtr::new(*i, f);
        let i = ana.solver.intern(RefsEnum::ScopedIdentifier(o, l));
        Some(i)
    } else if t == Type::PackageDeclaration {
        assert!(b.has_children());
        let x = b.child(&two).unwrap();
        let (_, x) = stores.typed_node_store().try_resolve(&x).unwrap();
        remake_pkg_ref(stores, ana, x)
    } else if t == Type::Spaces {
        log::error!("remake_pkg_ref space");
        None
    } else {
        log::error!("remake_pkg_ref {:?}", t);
        None
    }
}

impl<'a, IdN: Copy + Eq + NodeId + Debug, HAST> RefsFinder<'a, IdN, HAST>
where
    HAST: TypedHyperAST<
        'a,
        TIdN<IdN>,
        IdN = IdN,
        // T = HashedNodeRef<'a,Type>,
        Label = LabelIdentifier,
    >,
    HAST: hyperast::types::TypeStore,
    HAST: hyperast::types::LabelStore<str, I = LabelIdentifier>,
    HAST: hyperast::types::NodeStore<IdN, R<'a> = <HAST as hyperast::types::HyperAST<'a>>::T>,
    HAST::Idx: num::PrimInt + num::traits::NumAssign + Debug,
    IdN: Copy + Eq + Debug + NodeId<IdN = IdN>,
    <HAST as hyperast::types::HyperAST<'a>>::T: Tree<ChildIdx = HAST::Idx> + WithSerialization,
    <HAST as hyperast::types::HyperAST<'a>>::T: RefContainer<Result = BloomResult>,
    <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT:
        TypedTree<Type = Type, ChildIdx = HAST::Idx>,
    <HAST as hyperast::types::TypedHyperAST<'a, TIdN<IdN>>>::TT:
        RefContainer<Result = BloomResult>,
{
    /// Structurally find constructors in class
    pub fn find_constructors(&mut self, mut scout: TypedScout<TIdN<IdN>, HAST::Idx>) {
        let Ok(x) = scout.node_always(&self.sp_store) else {
            return;
        };
        let b = self.stores.typed_node_store().resolve(&x);
        let t = b.get_type();
        if t == Type::ClassBody {
        } else if t == Type::InterfaceBody {
        } else if t == Type::EnumBodyDeclarations {
        } else {
            return;
        }
        assert!(b.has_children());
        for (j, x) in b.children().unwrap().iter_children().enumerate() {
            let (r, x) = self.stores.typed_node_store().try_resolve(x).unwrap();
            let t = r.get_type();
            if t == Type::ConstructorDeclaration {
                scout.goto_typed(x, cast(j).unwrap());
                self.successful_match(&mut scout);
                scout.up(&self.sp_store);
            }
        }
    }

    #[allow(unused)] // TODO use it for ab testing
    /// WARN intended to be used starting from searched class
    /// and to find simple this
    fn find_refs_with_this(
        &mut self,
        package: RefPtr,
        target: RefPtr,
        scout: &mut TypedScout<TIdN<IdN>, HAST::Idx>,
    ) {
        let current = scout.node_always(&self.sp_store).unwrap();
        let b = self.stores.typed_node_store().resolve(&current);
        let t = b.get_type();
        if t == Type::Spaces {
            return;
        } else if t.is_comment() {
            return;
        } else if t == Type::ImportDeclaration
            // || t == Type::MavenDirectory
            // || t == Type::Directory
            || t == Type::Program
            || t == Type::PackageDeclaration
        {
            panic!()
        } else if t == Type::ClassDeclaration
            || t == Type::InterfaceDeclaration
            || t == Type::EnumDeclaration
            || t == Type::AnnotationTypeDeclaration
        {
            return;
        } else if t == Type::FieldAccess {
            if !b.has_children() {
                return;
            }
            let (_, xx) = self
                .stores
                .typed_node_store()
                .try_resolve(&b.child(&zero()).unwrap())
                .unwrap();
            scout.goto_typed(xx, zero());
            log::trace!(
                "rec search 'this' ref {}",
                DisplayRef::from((
                    self.ana.solver.nodes.with(target),
                    self.stores.label_store()
                )),
            );
            self.find_refs_with_this(package, target, scout);
            scout.up(&self.sp_store);
            return;
        } else if t == Type::This {
            log::debug!("!found 'this' {:?}", &t);
            log::debug!(
                "{}",
                hyperast::nodes::SyntaxSerializer::new(self.stores, current.as_id().clone(),)
            );
            self.successful_match(scout);
            // scout.check(&self.stores).expect("a");
            // let r = self.sp_store.push(scout);
            // self.sp_store.check(&self.stores).expect("b");
            // self.refs.push(r);
            // let it = ExploreStructuralPositions::from((&*self.sp_store, r));
            // log::debug!("really found 'this' {:?}", it.make_position(&self.stores));
        }

        if !b.has_children() {
            return;
        }
        log::debug!("d=1 {:?}", &t);
        let d = self.ana.solver.nodes.with(target);
        let c = b.check(d);
        // let c = if b.get_component::<BloomSize>().is_ok() {
        //     let d = self.ana.solver.nodes.with(target);
        //     b.check(d)
        // } else {
        //     BloomResult::MaybeContain
        // };

        struct IoOut<W: std::io::Write> {
            stream: W,
        }

        impl<W: std::io::Write> std::fmt::Write for IoOut<W> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                self.stream
                    .write_all(s.as_bytes())
                    .map_err(|_| std::fmt::Error)
            }
        }
        if let BloomResult::MaybeContain = c {
            log::debug!("++++++++++++++Maybe contains");
        } else {
            log::debug!("Do not contains");
            return;
        }

        assert!(b.has_children());
        log::debug!("c_count {:?}", b.child_count());
        let mut i = zero();
        for x in b.children().unwrap().iter_children() {
            let (_, x) = self.stores.typed_node_store().try_resolve(&x).unwrap();
            scout.goto_typed(x, i);
            i += one();
            log::trace!(
                "rec search 'this' ref {}",
                DisplayRef::from((
                    self.ana.solver.nodes.with(target),
                    self.stores.label_store()
                )),
            );
            self.find_refs_with_this(package, target, scout);
            scout.up(&self.sp_store);
        }
    }
}
