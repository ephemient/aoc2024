use std::collections::BTreeSet;
use std::collections::{btree_map::Entry, BTreeMap};
use std::mem::swap;
use std::num::ParseIntError;

use itertools::{FoldWhile, Itertools};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Operator {
    And,
    Or,
    Xor,
}

#[derive(Debug, Eq, PartialEq, thiserror::Error)]
pub enum Error {
    #[error("Format")]
    Format,
    #[error("UnknownOperator({0})")]
    UnknownOperator(String),
    #[error("Parse({0})")]
    Parse(ParseIntError),
    #[error("Solver")]
    Solver,
}

impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        Self::Parse(value)
    }
}

type ParsedInput<'a> = (
    BTreeMap<&'a str, bool>,
    BTreeMap<&'a str, (&'a str, Operator, &'a str)>,
);

fn parse(data: &str) -> Result<ParsedInput, Error> {
    let (values, wires) = data.split_once("\n\n").ok_or(Error::Format)?;
    Ok((
        values
            .lines()
            .map(|line| {
                let (key, value) = line.split_once(": ").ok_or(Error::Format)?;
                let value = match value {
                    "0" => false,
                    "1" => true,
                    _ => return Err(Error::Format),
                };
                Ok((key, value))
            })
            .collect::<Result<_, _>>()?,
        wires
            .lines()
            .map(|line| {
                let (src, dst) = line.split_once(" -> ").ok_or(Error::Format)?;
                let mut iter = src.splitn(3, ' ');
                let lhs = iter.next().ok_or(Error::Format)?;
                let operator = match iter.next().ok_or(Error::Format)? {
                    "AND" => Operator::And,
                    "OR" => Operator::Or,
                    "XOR" => Operator::Xor,
                    op => return Err(Error::UnknownOperator(op.to_string())),
                };
                let rhs = iter.next().ok_or(Error::Format)?;
                Ok((dst, (lhs.min(rhs), operator, lhs.max(rhs))))
            })
            .collect::<Result<_, _>>()?,
    ))
}

pub fn part1(data: &str) -> Result<usize, Error> {
    let (mut values, wires) = parse(data)?;
    fn eval<K: Copy + Ord>(
        values: &mut BTreeMap<K, bool>,
        wires: &BTreeMap<K, (K, Operator, K)>,
        key: K,
    ) -> Option<bool> {
        if let Entry::Occupied(entry) = values.entry(key) {
            return Some(*entry.get());
        }
        let (lhs, op, rhs) = wires.get(&key)?;
        let lhs = eval(values, wires, *lhs)?;
        let rhs = eval(values, wires, *rhs)?;
        match op {
            Operator::And => Some(lhs & rhs),
            Operator::Or => Some(lhs | rhs),
            Operator::Xor => Some(lhs ^ rhs),
        }
    }
    wires
        .clone()
        .split_off("z")
        .keys()
        .fold_while(Ok(0), |acc, key| {
            let Some(n) = key.strip_prefix('z') else {
                return FoldWhile::Done(acc);
            };
            let n = match n.parse::<usize>() {
                Ok(n) => n,
                Err(err) => return FoldWhile::Done(Err(Error::Parse(err))),
            };
            match eval(&mut values, &wires, key) {
                Some(false) => FoldWhile::Continue(Ok(acc.unwrap())),
                Some(true) => FoldWhile::Continue(Ok(acc.unwrap() | 1 << n)),
                None => FoldWhile::Done(Err(Error::Format)),
            }
        })
        .into_inner()
}

fn exchange<'a, T: Clone + Eq + Ord>(
    a: &'a mut T,
    b: &'a mut T,
    acc: &mut BTreeSet<T>,
    values: impl Iterator<Item = &'a mut T>,
) {
    if *a == *b {
        return;
    }
    acc.insert(a.clone());
    acc.insert(b.clone());
    for value in values {
        if *value == *a {
            *value = b.clone();
        } else if *value == *b {
            *value = a.clone();
        }
    }
    swap(a, b);
}

pub fn part2(data: &str) -> Result<String, Error> {
    let (_, mut wires) = parse(data)?;
    let mut connections = wires
        .iter()
        .map(|(k, v)| (*v, k.to_string()))
        .collect::<BTreeMap<_, _>>();
    let mut acc = BTreeSet::new();
    let mut carry: Option<String> = None;
    let mut first = true;
    for key in wires.split_off("z").into_keys() {
        let Some(suffix) = key.strip_prefix('z') else {
            break;
        };
        let x = format!("x{}", suffix);
        let y = format!("y{}", suffix);
        let mut z = key.to_string();
        if let Some(mut carry1) = carry {
            if let Some(mut half_add) = connections.get(&(&x, Operator::Xor, &y)).cloned() {
                let Some(mut full_add) = connections
                    .get(&(
                        (&half_add).min(&carry1),
                        Operator::Xor,
                        (&half_add).max(&carry1),
                    ))
                    .cloned()
                    .or_else(|| {
                        let mut alternative = connections.get(&(&x, Operator::And, &y))?.clone();
                        exchange(
                            &mut half_add,
                            &mut alternative,
                            &mut acc,
                            connections.values_mut(),
                        );
                        connections
                            .get(&(
                                (&half_add).min(&carry1),
                                Operator::Xor,
                                (&half_add).max(&carry1),
                            ))
                            .cloned()
                    })
                else {
                    return Err(Error::Solver);
                };
                exchange::<String>(&mut full_add, &mut z, &mut acc, connections.values_mut());
                carry = if let (Some(overflow1), Some(overflow2)) = (
                    connections.get(&(&x, Operator::And, &y)),
                    connections.get(&(
                        (&half_add).min(&carry1),
                        Operator::And,
                        (&half_add).max(&carry1),
                    )),
                ) {
                    connections
                        .get(&(
                            overflow1.min(overflow2),
                            Operator::Or,
                            overflow1.max(overflow2),
                        ))
                        .cloned()
                } else {
                    None
                };
            } else {
                exchange(&mut carry1, &mut z, &mut acc, connections.values_mut());
                carry = None;
            }
        } else if first {
            let mut add = connections
                .get(&(&x, Operator::Xor, &y))
                .ok_or(Error::Solver)?
                .clone();
            exchange::<String>(&mut add, &mut z.clone(), &mut acc, connections.values_mut());
            carry = connections.get(&(&x, Operator::And, &y)).cloned();
        } else {
            return Err(Error::Solver);
        }
        first = false;
    }
    Ok(acc.into_iter().join(","))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        x00: 1
        x01: 1
        x02: 1
        y00: 0
        y01: 1
        y02: 0

        x00 AND y00 -> z00
        x01 XOR y01 -> z01
        x02 OR y02 -> z02
    "};

    static EXAMPLE_2: &str = indoc! {"
        x00: 1
        x01: 0
        x02: 1
        x03: 1
        x04: 0
        y00: 1
        y01: 1
        y02: 1
        y03: 1
        y04: 1

        ntg XOR fgs -> mjb
        y02 OR x01 -> tnw
        kwq OR kpj -> z05
        x00 OR x03 -> fst
        tgd XOR rvg -> z01
        vdt OR tnw -> bfw
        bfw AND frj -> z10
        ffh OR nrd -> bqk
        y00 AND y03 -> djm
        y03 OR y00 -> psh
        bqk OR frj -> z08
        tnw OR fst -> frj
        gnj AND tgd -> z11
        bfw XOR mjb -> z00
        x03 OR x00 -> vdt
        gnj AND wpb -> z02
        x04 AND y00 -> kjc
        djm OR pbm -> qhw
        nrd AND vdt -> hwm
        kjc AND fst -> rvg
        y04 OR y02 -> fgs
        y01 AND x02 -> pbm
        ntg OR kjc -> kwq
        psh XOR fgs -> tgd
        qhw XOR tgd -> z09
        pbm OR djm -> kpj
        x03 XOR y03 -> ffh
        x00 XOR y04 -> ntg
        bfw OR bqk -> z06
        nrd XOR fgs -> wpb
        frj XOR qhw -> z04
        bqk OR frj -> z07
        y03 OR x01 -> nrd
        hwm AND bqk -> z03
        tgd XOR rvg -> z12
        tnw OR pbm -> gnj
    "};

    static EXAMPLE_3: &str = indoc! {"
        x00: 0
        x01: 1
        x02: 0
        x03: 1
        x04: 0
        x05: 1
        y00: 0
        y01: 0
        y02: 1
        y03: 1
        y04: 0
        y05: 1

        x00 AND y00 -> z05
        x01 AND y01 -> z02
        x02 AND y02 -> z01
        x03 AND y03 -> z03
        x04 AND y04 -> z04
        x05 AND y05 -> z00
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Ok(4), part1(EXAMPLE_1));
        assert_eq!(Ok(2024), part1(EXAMPLE_2));
    }

    #[ignore]
    #[test]
    fn part2_examples() {
        assert_eq!(Ok("z00,z01,z02,z05".to_string()), part2(EXAMPLE_3));
    }
}
