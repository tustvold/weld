use crate::ast::{ScalarKind, Type};
use cranelift::prelude::*;
use std::cmp::max;

pub fn convert_scalar(s: ScalarKind) -> types::Type {
    match s {
        ScalarKind::Bool => types::I8,
        ScalarKind::I8 => types::I8,
        ScalarKind::I16 => types::I16,
        ScalarKind::I32 => types::I32,
        ScalarKind::I64 => types::I64,
        ScalarKind::U8 => types::I8,
        ScalarKind::U16 => types::I16,
        ScalarKind::U32 => types::I32,
        ScalarKind::U64 => types::I64,
        ScalarKind::F32 => types::F32,
        ScalarKind::F64 => types::F64,
    }
}

pub fn convert_type(t: &Type) -> types::Type {
    match t {
        Type::Scalar(x) => convert_scalar(*x),
        _ => unimplemented!(),
    }
}

pub fn size_of(t: &Type) -> usize {
    match t {
        Type::Scalar(s) => match s {
            ScalarKind::Bool => 1,
            ScalarKind::I8 => 1,
            ScalarKind::I16 => 2,
            ScalarKind::I32 => 4,
            ScalarKind::I64 => 8,
            ScalarKind::U8 => 1,
            ScalarKind::U16 => 2,
            ScalarKind::U32 => 4,
            ScalarKind::U64 => 8,
            ScalarKind::F32 => 4,
            ScalarKind::F64 => 8,
        },
        Type::Struct(ts) => LayoutIterator::new(ts.as_slice()).last().unwrap_or(0),
        _ => unimplemented!(),
    }
}

pub fn alignment(t: &Type) -> usize {
    match t {
        Type::Scalar(s) => match s {
            ScalarKind::Bool => 1,
            ScalarKind::I8 => 1,
            ScalarKind::I16 => 2,
            ScalarKind::I32 => 4,
            ScalarKind::I64 => 8,
            ScalarKind::U8 => 1,
            ScalarKind::U16 => 2,
            ScalarKind::U32 => 4,
            ScalarKind::U64 => 8,
            ScalarKind::F32 => 4,
            ScalarKind::F64 => 8,
        },
        Type::Struct(ts) => ts.iter().map(|x| alignment(x)).max().unwrap_or(0),
        _ => unimplemented!(),
    }
}

pub fn padding_needed(offset: usize, align: usize) -> usize {
    // NB: This assumed align is a power of two
    let rounded_up = offset.wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1);
    rounded_up.wrapping_sub(offset)
}

pub struct LayoutIterator<'a> {
    t: &'a [Type],
    struct_align: usize,
    idx: usize,
    current_offset: usize,
}

impl<'a> LayoutIterator<'a> {
    pub fn new(t: &'a [Type]) -> LayoutIterator<'a> {
        LayoutIterator {
            t,
            struct_align: 0,
            idx: 0,
            current_offset: 0,
        }
    }
}

impl<'a> Iterator for LayoutIterator<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx == self.t.len() + 1 {
            return None;
        } else if self.idx == self.t.len() {
            let pad = padding_needed(self.current_offset, self.struct_align);

            self.current_offset = self.current_offset.checked_add(pad).unwrap();
            self.idx += 1;

            return Some(self.current_offset);
        }

        let child = &self.t[self.idx];
        let child_size = size_of(child);
        let child_align = alignment(child);

        let pad = padding_needed(self.current_offset, child_align);
        let field_start = self.current_offset.checked_add(pad).unwrap();

        self.idx += 1;
        self.struct_align = max(self.struct_align, child_align);
        self.current_offset = field_start.checked_add(child_size).unwrap();

        Some(field_start)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_padding() {
        assert_eq!(padding_needed(3, 8), 5);
        assert_eq!(padding_needed(0, 2), 0);
        assert_eq!(padding_needed(6, 2), 0);
        assert_eq!(padding_needed(23, 1), 0);
        assert_eq!(padding_needed(17, 4), 3)
    }

    #[test]
    fn test_layout() {
        let l1 = LayoutIterator::new(&[Type::Scalar(ScalarKind::F32)]).collect::<Vec<_>>();
        let l2 =
            LayoutIterator::new(&[Type::Scalar(ScalarKind::F32), Type::Scalar(ScalarKind::F64)])
                .collect::<Vec<_>>();

        let l3 = LayoutIterator::new(&[
            Type::Scalar(ScalarKind::F32),
            Type::Scalar(ScalarKind::F64),
            Type::Scalar(ScalarKind::F32),
        ])
        .collect::<Vec<_>>();

        let l4 = LayoutIterator::new(&[
            Type::Scalar(ScalarKind::F32),
            Type::Scalar(ScalarKind::F64),
            Type::Scalar(ScalarKind::F32),
            Type::Scalar(ScalarKind::F32),
        ])
        .collect::<Vec<_>>();

        let l5 = LayoutIterator::new(&[
            Type::Scalar(ScalarKind::Bool),
            Type::Scalar(ScalarKind::Bool),
            Type::Scalar(ScalarKind::I16),
            Type::Scalar(ScalarKind::I32),
        ])
        .collect::<Vec<_>>();

        let l6 = LayoutIterator::new(&[
            Type::Struct(vec![
                Type::Scalar(ScalarKind::Bool),
                Type::Scalar(ScalarKind::F32),
            ]),
            Type::Scalar(ScalarKind::I8),
        ])
        .collect::<Vec<_>>();

        assert_eq!(l1, vec![0, 4]);
        assert_eq!(l2, vec![0, 8, 16]);
        assert_eq!(l3, vec![0, 8, 16, 24]);
        assert_eq!(l4, vec![0, 8, 16, 20, 24]);
        assert_eq!(l5, vec![0, 1, 2, 4, 8]);
        assert_eq!(l6, vec![0, 8, 12]);
    }
}
