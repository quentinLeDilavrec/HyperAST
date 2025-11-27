//! Log the grammar of corresponding language to rerun.

use core::f32;
use std::{ops::Mul, sync::Arc};

use rerun::external::{arrow, re_types};
pub fn log_languages() -> Result<(), Box<dyn std::error::Error>> {
    let rec = rerun::RecordingStream::global(rerun::StoreKind::Recording).unwrap();

    let lang = polyglote::Lang {
        language: hyperast_gen_ts_java::language(),
        name: "java",
        node_types: hyperast_gen_ts_java::node_types(),
        highlights: "",
        tags: "",
        injects: "",
    };
    let types = polyglote::preprocess_aux(&lang)?;

    eprintln!("{}", types);

    log_3dgraph_language(&rec, &lang, &types)?;
    log_2dgraph_language(&rec, &lang, &types)?;

    let lang = polyglote::Lang {
        language: hyperast_gen_ts_cpp::language(),
        name: "cpp",
        node_types: hyperast_gen_ts_cpp::node_types(),
        highlights: "",
        tags: "",
        injects: "",
    };
    let types = polyglote::preprocess_aux(&lang)?;

    eprintln!("{}", types);

    log_3dgraph_language(&rec, &lang, &types)?;
    log_2dgraph_language(&rec, &lang, &types)?;

    let lang = polyglote::Lang {
        language: hyperast_gen_ts_tsquery::language(),
        name: "tsquery",
        node_types: hyperast_gen_ts_tsquery::node_types(),
        highlights: "",
        tags: "",
        injects: "",
    };
    let types = polyglote::preprocess_aux(&lang)?;

    eprintln!("{}", types);

    log_3dgraph_language(&rec, &lang, &types)?;
    log_2dgraph_language(&rec, &lang, &types)?;

    Ok(())
}

fn log_2dgraph_language(
    rec: &rerun::RecordingStream,
    lang: &polyglote::Lang,
    types: &polyglote::preprocess::TypeSys,
) -> rerun::RecordingStreamResult<()> {
    let path = &["language", lang.name].map(|x| x.into())[..];
    fn split_f(
        mut acc: (Vec<String>, Vec<String>),
        x: (String, bool),
    ) -> (Vec<String>, Vec<String>) {
        if x.1 { &mut acc.0 } else { &mut acc.1 }.push(x.0);
        acc
    }
    let leafs = types.leafs().fold((vec![], vec![]), split_f);
    let concrete = types.concrete().fold((vec![], vec![]), split_f);
    let r#abstract = types.r#abstract().collect();

    let triplet = make_triplet(&leafs.1, [80, 140, 255]); // unnamed
    let triplet = chaining(triplet, make_triplet(&leafs.0, [140, 80, 255])); //named
    let triplet = chaining(triplet, make_triplet(&concrete.0, [80, 255, 140])); //with_fields
    let triplet = chaining(triplet, make_triplet(&concrete.1, [140, 255, 80])); //named
    let triplet = chaining(triplet, make_triplet(&r#abstract, [255, 80, 80]));

    let (node_ids, labels, colors) = triplet;
    let graph_nodes = rerun::GraphNodes::new(node_ids)
        .with_labels(labels)
        .with_colors(colors);
    rec.log(path, &graph_nodes)?;

    let subtypes = types.r#abstract_subtypes();
    let concrete_children = types.concrete_children();
    let concrete_fields = types
        .concrete_fields()
        .map(|(x, v)| (x.clone(), v.iter().map(|x| x.1.clone()).collect::<Vec<_>>()));
    let arr = subtypes.chain(concrete_children).chain(concrete_fields);
    let graph_edges = rerun::GraphEdges::new(
        arr.into_iter()
            .map(|(t, cs)| cs.into_iter().map(|y| (t.clone(), y)).collect::<Vec<_>>())
            .flatten()
            .collect::<Vec<_>>(),
    )
    .with_directed_edges();
    rec.log(path, &graph_edges)
}

fn log_3dgraph_language(
    rec: &rerun::RecordingStream,
    lang: &polyglote::Lang,
    types: &polyglote::preprocess::TypeSys,
) -> rerun::RecordingStreamResult<()> {
    let mut map = Map {
        map: std::collections::HashMap::default(),
        rec: rec.clone(),
        name: lang.name,
    };
    let r#abstract = types.r#abstract().collect();
    dbg!(&r#abstract);
    map.log(r#abstract, "abstract", "abstract", 20.0)?;
    let leafs = types.leafs().fold((vec![], vec![]), split_f);
    map.log(leafs.1, "leaf", "unnamed", 0.0)?;
    map.log(leafs.0, "leaf", "named", 5.0)?;
    let concrete = types.concrete().fold((vec![], vec![]), split_f);
    map.log(concrete.0, "concrete", "with_fields", 10.0)?;
    map.log(concrete.1, "concrete", "named", 15.0)?;
    let subtypes = types.r#abstract_subtypes();
    rec.clone().log(
        format!("language/{}/subtyping", lang.name),
        &map.arrows(subtypes),
    )?;
    let concrete_children = types.concrete_children();
    rec.clone().log(
        format!("language/{}/children", lang.name),
        &map.arrows(concrete_children),
    )?;
    let concrete_fields: Vec<_> = types.concrete_fields().collect();
    let labels = concrete_fields
        .iter()
        .flat_map(|x| x.1.iter().map(|x| x.0.clone()));
    let concrete_fields = concrete_fields
        .iter()
        .map(|(x, v)| (x.clone(), v.iter().map(|x| x.1.clone()).collect::<Vec<_>>()));
    rec.clone().log(
        format!("language/{}/fields", lang.name),
        &map.arrows(concrete_fields).with_labels(labels),
    )?;
    Ok(())
}

struct Map {
    map: std::collections::HashMap<String, [f32; 3]>,
    rec: rerun::RecordingStream,
    name: &'static str,
}
impl Map {
    fn points(&mut self, v: Vec<String>, o: f32) -> rerun::Points3D {
        let l = (v.len() as f32) / f32::consts::TAU;
        rerun::Points3D::new((0..v.len()).map(|i| {
            let rad = (i as f32 / v.len() as f32).mul(f32::consts::TAU);
            let p = [rad.sin() * l, o, rad.cos() * l];
            self.map.insert(v[i].clone(), p.clone());
            p
        }))
        .with_labels(v.into_iter().map(|x| x))
    }
    fn arrows(&self, v: impl Iterator<Item = (String, Vec<String>)>) -> rerun::Arrows3D {
        let translate = |x: &[f32; 3], y: &[f32; 3]| [y[0] - x[0], y[1] - x[1], y[2] - x[2]];
        let (ori, dest): (Vec<&[f32; 3]>, Vec<[f32; 3]>) = v
            .map(|(t, cs)| {
                let x = self.map.get(&t).unwrap();
                cs.into_iter()
                    .filter_map(|t| self.map.get(&t))
                    .map(|y| (x, translate(x, y)))
                    .collect::<Vec<_>>()
            })
            .flatten()
            .unzip();
        rerun::Arrows3D::from_vectors(dest).with_origins(ori)
    }

    fn log(
        &mut self,
        v: Vec<String>,
        t: &str,
        adj: &str,
        o: f32,
    ) -> rerun::RecordingStreamResult<()> {
        let path = ["language", self.name, t, adj].map(|x| x.into());
        let path = if adj.is_empty() {
            &path[..path.len() - 1]
        } else {
            &path[..]
        };
        let points = self.points(v, o);
        self.rec.log(path, &points)
    }
}

fn split_f(mut acc: (Vec<String>, Vec<String>), x: (String, bool)) -> (Vec<String>, Vec<String>) {
    if x.1 { &mut acc.0 } else { &mut acc.1 }.push(x.0);
    acc
}

fn make_triplet(
    v: &Vec<String>,
    color: [u8; 3],
) -> (
    impl Iterator<Item = String> + '_,
    impl Iterator<Item = String> + '_,
    impl Iterator<Item = [u8; 3]> + '_,
) {
    let colors = (0..v.len()).map(move |_| color.clone());
    let node_ids = v.iter().cloned();
    let labels = v.iter().cloned();
    (node_ids, labels, colors)
}
fn chaining<
    It0: Iterator,
    It1: Iterator,
    It2: Iterator,
    Itb0: Iterator<Item = It0::Item>,
    Itb1: Iterator<Item = It1::Item>,
    Itb2: Iterator<Item = It2::Item>,
>(
    ta: (It0, It1, It2),
    tb: (Itb0, Itb1, Itb2),
) -> (
    impl Iterator<Item = It0::Item>,
    impl Iterator<Item = It1::Item>,
    impl Iterator<Item = It2::Item>,
) {
    (ta.0.chain(tb.0), ta.1.chain(tb.1), ta.2.chain(tb.2))
}
