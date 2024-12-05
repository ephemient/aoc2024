use std::collections::BTreeSet;
use std::mem::swap;
use std::num::ParseIntError;

#[derive(Debug, Eq, PartialEq, thiserror::Error)]
pub enum Error {
    #[error("Format")]
    Format,
    #[error("Parse({0})")]
    Parse(ParseIntError),
}

impl From<ParseIntError> for Error {
    fn from(value: ParseIntError) -> Self {
        Self::Parse(value)
    }
}

type ParsedInput = (BTreeSet<(u32, u32)>, Vec<Vec<u32>>);

fn parse(data: &str) -> Result<ParsedInput, Error> {
    let (deps, updates) = data.split_once("\n\n").ok_or(Error::Format)?;
    let deps = deps
        .lines()
        .map(|line| {
            let (x, y) = line.split_once('|').ok_or(Error::Format)?;
            Ok((x.parse()?, y.parse()?))
        })
        .collect::<Result<_, Error>>()?;
    let updates = updates
        .lines()
        .map(|line| line.split(',').map(|page| page.parse::<u32>()).collect())
        .collect::<Result<Vec<_>, _>>()?;
    Ok((deps, updates))
}

pub fn part1(data: &str) -> Result<u32, Error> {
    let (deps, updates) = parse(data)?;
    Ok(updates
        .into_iter()
        .filter_map(|pages| {
            if pages
                .iter()
                .enumerate()
                .all(|(i, x)| pages[i + 1..].iter().all(|y| !deps.contains(&(*y, *x))))
            {
                pages.get(pages.len() / 2).copied()
            } else {
                None
            }
        })
        .sum())
}

pub fn part2(data: &str) -> Result<u32, Error> {
    let (deps, updates) = parse(data)?;
    Ok(updates
        .into_iter()
        .filter_map(|mut pages| {
            let mut deranged = false;
            for i in 0..pages.len() {
                let pages = &mut pages[i..];
                'scan: loop {
                    let (x, pages) = pages.split_first_mut().unwrap();
                    for y in pages.iter_mut() {
                        if deps.contains(&(*y, *x)) {
                            deranged = true;
                            swap(x, y);
                            continue 'scan;
                        }
                    }
                    break;
                }
            }
            if deranged {
                pages.get(pages.len() / 2).copied()
            } else {
                None
            }
        })
        .sum())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        47|53
        97|13
        97|61
        97|47
        75|29
        61|13
        75|53
        29|13
        97|29
        53|29
        61|53
        97|53
        61|29
        47|13
        75|47
        97|75
        47|61
        75|61
        47|29
        75|13
        53|13

        75,47,61,53,29
        97,61,53,29,13
        75,29,13
        75,97,47,61,53
        61,13,29
        97,13,75,29,47
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Ok(143), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Ok(123), part2(EXAMPLE));
    }
}
