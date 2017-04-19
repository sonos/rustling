#[macro_use]
extern crate error_chain;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::fmt::Debug;

use errors::*;

pub mod errors {
    error_chain! {
        types {
            ClassifyError, ClassifyErrorKind, ClassifyResultExt, ClassifyResult;
        }
    }
}

trait ClassId: Eq + Hash + Clone + Debug {}
trait Feature: Eq + Hash + Clone + Debug {}

struct Input<Feat: Feature> {
    rule_id: &'static str,
    features: Vec<Feat>,
    children: Vec<Input<Feat>>,
}

// #[derive(PartialEq,Debug,Clone)]
// struct Model<Id:ClassId, Feat:Feature> {
// pub classifiers: HashMap<RuleId, Classifier<Id,Feat>>,
// }
//

#[derive(PartialEq,Debug,Clone)]
struct Classifier<Id: ClassId, Feat: Feature> {
    pub classes: HashMap<Id, ClassInfo<Feat>>,
}

#[derive(PartialEq,Debug,Clone)]
struct ClassInfo<Feat: Feature> {
    pub example_count: usize,
    pub unk_probalog: f32,
    pub class_probalog: f32,
    pub feat_probalog: HashMap<Feat, f32>,
}

// impl<Id:ClassId, Feat:Feature> Model<Id, Feat> {
// pub fn classify(&self, input: &Input<Feat>) -> ClassifyResult<f32> {
// let classifier = if let Some(classifier) = self.classifiers.get(input.rule_id) {
// classifier
// } else {
// return Ok(0.0);
// };
//
// let mut bag_of_features: HashMap<Feat, usize> = HashMap::new();
// for feat in &input.features {
// let counter = bag_of_features.entry(feat.clone()).or_insert(0);
// counter += 1;
// }
//
// FIXME: we do nothing with the false
// let mut best = classifier.classify(&bag_of_features)?;
// let mut probalog = best.1;
// for child in &input.children {
// probalog += self.classify(&child)?;
// }
// Ok(probalog)
// }
// }
//

impl<Id: ClassId, Feat: Feature> Classifier<Id, Feat> {

    // max(log(π(Prob(feat|class)^count)*Prob(class))) =
    // max(sum(logprob(feat|class)*count + logprob(class))

    pub fn scores(&self, bag_of_features: &HashMap<Feat, usize>) -> Vec<(Id,f32)> {
        let mut scores:Vec<_> = self.classes
            .iter()
            .map(|(cid, cinfo)| {
                let probalog: f32 = bag_of_features.iter()
                    .map(|(feat, count)| {
                        *count as f32 * cinfo.feat_probalog.get(feat).unwrap_or(&cinfo.unk_probalog)
                    })
                    .sum();
                (cid.clone(), probalog + cinfo.class_probalog)
            }).collect();
        let normlog = f32::ln(scores.iter().map(|p| f32::exp(p.1)).sum::<f32>());
        for s in scores.iter_mut() {
            s.1 -= normlog
        }
        scores
    }

    pub fn classify(&self, bag_of_features: &HashMap<Feat, usize>) -> ClassifyResult<(Id, f32)> {
        Ok(self.scores(bag_of_features).into_iter()
            .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(::std::cmp::Ordering::Equal))
            .ok_or("no classes in classifier")?)
    }

    pub fn train(examples: &Vec<(HashMap<Feat, usize>, Id)>) -> Classifier<Id, Feat> {
        let mut classes: HashMap<Id, (usize, HashMap<Feat, usize>)> = HashMap::new();
        let total_examples = examples.len();
        let mut all_features = HashSet::new();
        for &(ref features, ref class) in examples {
            let mut data = classes.entry(class.clone()).or_insert_with(|| (0, HashMap::new()));
            data.0 += 1;
            for (feat, count) in features {
                all_features.insert(feat.clone());
                *data.1.entry(feat.clone()).or_insert(0) += *count;
            }
        }
        let total_features = all_features.len();
        let class_infos = classes.into_iter()
            .map(|(k, v)| {
                let smooth_denom: f32 = (total_features + v.1.values().sum::<usize>()) as f32;
                let feat_probalog = v.1
                    .into_iter()
                    .map(|(k, v)| (k, f32::ln((v as f32 + 1 as f32) / smooth_denom)))
                    .collect();
                (k,
                 ClassInfo {
                     example_count: v.0,
                     class_probalog: f32::ln(v.0 as f32 / total_examples as f32),
                     unk_probalog: f32::ln(1.0 / smooth_denom),
                     feat_probalog: feat_probalog,
                 })
            })
            .collect();
        Classifier { classes: class_infos }
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! hmap(
        { $($key:expr => $value:expr),*} => {
            {
                let mut m = ::std::collections::HashMap::new();
                $( m.insert($key, $value); )*
                m
            }
        };
        ($($k:expr => $v:expr),+,) => { hmap!($($k => $v),+) }
    );

    #[derive(Eq,PartialEq,Debug,Hash,Clone)]
    enum Species {
        Cat,
        Dog,
        Human,
    }
    impl ClassId for Species {}

    #[derive(Eq,PartialEq,Debug,Hash,Clone)]
    enum Friend {
        Cat,
        Dog,
        Human,
        Fish,
    }
    impl Feature for Friend {}

    fn trained_classifier() -> Classifier<Species, Friend> {
        Classifier {
            classes: hmap!(
                Species::Cat => ClassInfo {
                    class_probalog: -1.0986123,
                    unk_probalog: -2.3978953,
                    example_count: 4,
                    feat_probalog: hmap!(
                        Friend::Cat => -1.0116009,
                        Friend::Human => -1.704748,
                        Friend::Fish => -1.0116009,
                    )
                },
                Species::Dog => ClassInfo {
                    class_probalog: -1.0986123,
                    unk_probalog: -2.3978953,
                    example_count: 4,
                    feat_probalog: hmap!(
                        Friend::Cat => -1.704748,
                        Friend::Dog => -1.0116009,
                        Friend::Human => -1.0116009,
                    )
                },
                Species::Human => ClassInfo {
                    class_probalog: -1.0986123,
                    unk_probalog: -2.7725887,
                    example_count: 4,
                    feat_probalog: hmap!(
                        Friend::Cat => -1.3862944,
                        Friend::Dog => -1.3862944,
                        Friend::Human => -1.3862944,
                        Friend::Fish => -1.3862944,
                    )
                }
            ),
        }
    }

    #[test]
    fn test_train() {
        let examples = vec! {
            (hmap!(Friend::Dog => 1, Friend::Human => 1, Friend::Cat => 1), Species::Dog),
            (hmap!(Friend::Dog => 1), Species::Dog),
            (hmap!(Friend::Dog => 1, Friend::Human => 1), Species::Dog),
            (hmap!(Friend::Human => 1), Species::Dog),
            (hmap!(Friend::Fish => 1, Friend::Cat => 1), Species::Cat),
            (hmap!(Friend::Cat => 1), Species::Cat),
            (hmap!(Friend::Fish => 1), Species::Cat),
            (hmap!(Friend::Human => 1, Friend::Fish => 1, Friend::Cat => 1), Species::Cat),
            (hmap!(Friend::Human => 1, Friend::Fish => 1, Friend::Cat => 1, Friend::Dog => 1), Species::Human),
            (hmap!(Friend::Fish => 1, Friend::Cat => 1, Friend::Dog => 1), Species::Human),
            (hmap!(Friend::Human => 1, Friend::Fish => 1, Friend::Dog => 1), Species::Human),
            (hmap!(Friend::Human => 1, Friend::Cat => 1), Species::Human),
        };
        let classifier = Classifier::train(&examples);
        assert_eq!(trained_classifier(), classifier);
    }

    #[test]
    fn test_classify_norm() {
        let classifier = trained_classifier();
        let probable_cat = hmap!(Friend::Fish => 1, Friend::Cat => 1);
        let norm = classifier.scores(&probable_cat).iter().map(|pair| pair.1).map(f32::exp).sum::<f32>();
        assert!(norm > 0.9999 && norm < 1.0001);
    }

    #[test]
    fn test_classify() {
        let classifier = trained_classifier();
        let probable_cat = hmap!(Friend::Fish => 1, Friend::Cat => 1);
        assert_eq!(Species::Cat, classifier.classify(&probable_cat).unwrap().0);

        let probable_dog = hmap!(Friend::Human => 1, Friend::Dog => 1);
        assert_eq!(Species::Dog, classifier.classify(&probable_dog).unwrap().0);

        let probable_human = hmap!(Friend::Dog => 1, Friend::Cat => 1, Friend::Human => 1, Friend::Fish => 1);
        assert_eq!(Species::Human, classifier.classify(&probable_human).unwrap().0);

    }
}
