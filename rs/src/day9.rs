use std::iter;

fn parse(data: &str) -> Vec<(usize, (u8, u8))> {
    let mut iter = data
        .bytes()
        .filter_map(|b| {
            if b.is_ascii_digit() {
                Some(b - b'0')
            } else {
                None
            }
        })
        .chain([0])
        .fuse();
    iter::from_fn(move || iter.next().zip(iter.next()))
        .scan(0, |acc, (used, free)| {
            let offset = *acc;
            *acc += (used + free) as usize;
            Some((offset, (used, free)))
        })
        .collect()
}

fn tri_range(offset: u64, size: u64) -> u64 {
    (2 * offset + size - 1) * size / 2
}

pub fn part1(data: &str) -> u64 {
    let mut data = parse(data);
    (0..data.len())
        .scan(data.len(), |end, id| {
            if id >= *end {
                return None;
            }
            let (mut offset, (len, mut free)) = data[id];
            let mut sum = id as u64 * tri_range(offset as u64, len as u64);
            offset += len as usize;
            while free > 0 && id + 1 < *end {
                let id = *end - 1;
                let (_, (used2, _)) = data.get_mut(id).unwrap();
                let moved = free.min(*used2);
                sum += id as u64 * tri_range(offset as u64, moved as u64);
                offset += moved as usize;
                free -= moved;
                *used2 -= moved;
                if *used2 > 0 {
                    break;
                }
                *end = id;
            }
            Some(sum)
        })
        .sum()
}

pub fn part2(data: &str) -> u64 {
    let mut data = parse(data);
    (0..data.len())
        .rev()
        .scan([0; 10], |last_i, id| {
            let (mut offset, (used, _)) = data[id];
            let mut i = last_i[used as usize];
            while i < id {
                let (_, (_, free)) = data.get_mut(i).unwrap();
                if used <= *free {
                    let free2 = *free;
                    *free -= used;
                    offset = data[i + 1].0 - free2 as usize;
                    last_i[used as usize..].fill(i);
                    break;
                }
                i += 1;
            }
            if i != last_i[used as usize] {
                last_i[used as usize..].fill(i);
            }
            Some(id as u64 * tri_range(offset as u64, used as u64))
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &str = indoc! {"
        2333133121414131402
    "};

    #[test]
    fn part1_examples() {
        assert_eq!(1928, part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(2858, part2(EXAMPLE));
    }
}
