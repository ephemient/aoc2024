fn tri_range(offset: u64, size: u64) -> u64 {
    (2 * offset + size - 1) * size / 2
}

pub fn part1(data: &str) -> u64 {
    let mut sizes = data
        .chars()
        .filter_map(|c| c.to_digit(10))
        .collect::<Vec<_>>();
    let (mut total, mut offset) = (0, 0);
    for i in 0..sizes.len() {
        let size = sizes[i];
        if i % 2 == 0 {
            total += i as u64 / 2 * tri_range(offset, size.into());
            offset += size as u64;
        } else {
            let mut free_size = size;
            for (j, size) in sizes.iter_mut().enumerate().skip(i + 1).step_by(2).rev() {
                if free_size == 0 {
                    break;
                }
                let used_size = free_size.min(*size);
                total += j as u64 / 2 * tri_range(offset, used_size.into());
                offset += used_size as u64;
                free_size -= used_size;
                *size -= used_size;
            }
        }
    }
    total
}

pub fn part2(data: &str) -> u64 {
    let mut chunks = data
        .chars()
        .filter_map(|c| c.to_digit(10))
        .scan(0, |acc, size| {
            let offset = *acc;
            *acc = offset + size;
            Some((offset, size))
        })
        .collect::<Vec<_>>();
    (0..chunks.len())
        .step_by(2)
        .rev()
        .map(|i| {
            let (mut offset, size) = chunks[i];
            if let Some((free_offset, free_size)) = chunks
                .iter_mut()
                .take(i)
                .skip(1)
                .step_by(2)
                .find(|(_, free_size)| size <= *free_size)
            {
                offset = *free_offset;
                *free_offset += size;
                *free_size -= size;
            }
            i as u64 / 2 * tri_range(offset.into(), size.into())
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
