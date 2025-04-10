/// Compute the output value `res` >= `val` such that `res` % `alignment` == 0
/// This function requires that `alignement` is a power of 2.
pub fn offset_align_p2(val: usize, alignment: usize) -> usize {
    assert!(alignment.is_power_of_two());
    (val + alignment - 1) & !(alignment - 1)
}
