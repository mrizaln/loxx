use loxi::util::Loc;

use crate::bytecode::{Address, Offset};

#[derive(Clone, Copy, Debug)]
pub struct StrId(u32);

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct ClassId(u32);

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct FuncId(u32);

#[derive(Clone, Copy, Debug)]
pub struct VarId(u32);

#[derive(Clone, Copy, Debug)]
pub struct ExprId(u32);

#[derive(Default)]
pub struct Metadata {
    str_table: Vec<String>,
    class_table: Vec<ClassInfo>,
    func_table: Vec<FuncInfo>,
    var_table: Vec<VarInfo>,
    expr_table: Vec<ExprInfo>,
}

pub struct ClassInfo {
    pub loc: Loc,
    pub name: StrId,
    pub ctor: FuncId,
    pub methods: Vec<FuncId>,
    pub parent: Option<ClassId>,
}

pub struct FuncInfo {
    pub loc: Loc,
    pub name: StrId,
    pub address: Address,
    pub len: u32,
    pub arity: u32,
    pub captures: Vec<VarId>,
}

pub struct VarInfo {
    pub loc: Loc,
    pub name: StrId,
    pub offset: Offset,
    pub start: Address,
    pub dur: u32,
}

pub struct ExprInfo {
    pub loc: Loc,
    pub label: &'static str,
    pub address: Address,
}

impl Metadata {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_str(&mut self, str: &str) -> StrId {
        match self.str_table.iter().position(|s| s == str) {
            Some(id) => StrId(id as u32),
            None => {
                let index = self.str_table.len();
                self.str_table.push(str.to_owned());
                StrId(index as u32)
            }
        }
    }

    pub fn get_str(&self, id: StrId) -> &str {
        &self.str_table[id.0 as usize]
    }

    pub fn push_class(
        &mut self,
        loc: &Loc,
        name: &str,
        ctor: FuncId,
        methods: Vec<FuncId>,
        parent: Option<ClassId>,
    ) -> ClassId {
        let class = ClassInfo {
            loc: *loc,
            name: self.add_str(name),
            ctor,
            methods,
            parent,
        };
        let index = self.class_table.len();
        self.class_table.push(class);
        ClassId(index as u32)
    }

    pub fn push_func(
        &mut self,
        loc: &Loc,
        name: &str,
        address: Address,
        len: u32,
        arity: u32,
        captures: Vec<VarId>,
    ) -> FuncId {
        let func = FuncInfo {
            loc: *loc,
            name: self.add_str(name),
            address,
            len,
            arity,
            captures,
        };
        let index = self.func_table.len();
        self.func_table.push(func);
        FuncId(index as u32)
    }

    pub fn push_var(
        &mut self,
        loc: &Loc,
        name: &str,
        offset: Offset,
        start: Address,
        dur: u32,
    ) -> VarId {
        let var = VarInfo {
            loc: *loc,
            name: self.add_str(name),
            offset,
            start,
            dur,
        };
        let index = self.var_table.len();
        self.var_table.push(var);
        VarId(index as u32)
    }

    pub fn push_expr(&mut self, loc: &Loc, label: &'static str, address: Address) -> ExprId {
        let expr = ExprInfo {
            loc: *loc,
            label,
            address,
        };
        let index = self.expr_table.len();
        self.expr_table.push(expr);
        ExprId(index as u32)
    }

    pub fn get_func(&self, id: FuncId) -> &FuncInfo {
        &self.func_table[id.0 as usize]
    }

    pub fn get_func_mut(&mut self, id: FuncId) -> &mut FuncInfo {
        &mut self.func_table[id.0 as usize]
    }

    pub fn get_var(&self, id: VarId) -> &VarInfo {
        &self.var_table[id.0 as usize]
    }

    pub fn get_var_mut(&mut self, id: VarId) -> &mut VarInfo {
        &mut self.var_table[id.0 as usize]
    }

    pub fn get_expr(&self, id: ExprId) -> &ExprInfo {
        &self.expr_table[id.0 as usize]
    }

    pub fn get_expr_mut(&mut self, id: ExprId) -> &mut ExprInfo {
        &mut self.expr_table[id.0 as usize]
    }

    pub fn iter_func_info(&self) -> std::slice::Iter<'_, FuncInfo> {
        self.func_table.iter()
    }

    pub fn iter_var_info(&self) -> std::slice::Iter<'_, VarInfo> {
        self.var_table.iter()
    }

    pub fn iter_expr_info(&self) -> std::slice::Iter<'_, ExprInfo> {
        self.expr_table.iter()
    }
}
