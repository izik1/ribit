use std::ops::Range;

pub fn lookup_block<'a, Block>(
    ranges: &[Range<u32>],
    blocks: &'a [Block],
    start_address: u32,
) -> Option<&'a Block> {
    ranges.binary_search_by_key(&start_address, |range| range.start).ok().map(|it| &blocks[it])
}
