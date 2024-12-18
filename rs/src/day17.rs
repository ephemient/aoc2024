use itertools::Itertools;

fn parse(data: &str) -> Option<(usize, usize, usize, Vec<usize>)> {
    let mut iter = data.lines();
    let a = iter.next()?.strip_prefix("Register A: ")?.parse().ok()?;
    let b = iter.next()?.strip_prefix("Register B: ")?.parse().ok()?;
    let c = iter.next()?.strip_prefix("Register C: ")?.parse().ok()?;
    let program = iter
        .find(|line| !line.is_empty())?
        .strip_prefix("Program: ")?
        .split(',')
        .map(|s| s.parse())
        .collect::<Result<_, _>>()
        .ok()?;
    Some((a, b, c, program))
}

fn run(mut a: usize, mut b: usize, mut c: usize, program: &[usize]) -> Option<Vec<usize>> {
    let mut outputs = vec![];
    let mut ip = 0;
    while let Some((instruction, operand)) = program.get(ip).zip(program.get(ip + 1)) {
        let combo = match operand {
            0..=3 => *operand,
            4 => a,
            5 => b,
            6 => c,
            _ => return None,
        };
        match instruction {
            0 => a >>= combo,
            1 => b ^= operand,
            2 => b = combo & 7,
            3 => {
                if a != 0 {
                    ip = *operand;
                    continue;
                }
            }
            4 => b ^= c,
            5 => outputs.push(combo & 7),
            6 => b = a >> combo,
            7 => c = a >> combo,
            _ => return None,
        }
        ip += 2;
    }
    Some(outputs)
}

pub fn part1(data: &str) -> Option<String> {
    let (a, b, c, program) = parse(data)?;
    Some(run(a, b, c, &program)?.into_iter().join(","))
}

pub fn part2(data: &str) -> Option<usize> {
    let (_, b, c, program) = parse(data)?;
    let mut candidates = vec![0];
    while !candidates.is_empty() {
        let mut next = vec![];
        for base in candidates {
            for i in 0..=7 {
                let a = 8 * base + i;
                let output = run(a, b, c, &program)?;
                if output == program {
                    return Some(a);
                }
                if program.ends_with(&output) {
                    next.push(a);
                }
            }
        }
        candidates = next;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &str = indoc! {"
        Register A: 729
        Register B: 0
        Register C: 0

        Program: 0,1,5,4,3,0
    "};

    static EXAMPLE_2: &str = indoc! {"
        Register A: 2024
        Register B: 0
        Register C: 0

        Program: 0,3,5,4,3,0
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(Some("4,6,3,5,6,3,5,2,1,0".to_string()), part1(EXAMPLE_1));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(117440), part2(EXAMPLE_2));
    }
}
