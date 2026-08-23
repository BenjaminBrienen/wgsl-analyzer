use std::num::{ParseFloatError, ParseIntError};

use la_arena::Idx;
pub use syntax::ast::operators::*;
use syntax::{
    Parse,
    ast::{self, IncrementDecrement},
};
use wgsl_types::inst::LiteralInstance;

use crate::{
    body::BindingId,
    item_tree::Name,
    type_specifier::{IdentExpression, TypeSpecifierId},
};

pub type ExpressionId = Idx<Expression>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinFloat {
    F16,
    F32,
    Abstract,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinInt {
    I32,
    U32,
    // SHADER_INT64
    I64,
    U64,
    Abstract,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Missing,
    BinaryOperation {
        left_side: ExpressionId,
        right_side: ExpressionId,
        operation: BinaryOperation,
    },
    UnaryOperator {
        expression: ExpressionId,
        operator: UnaryOperator,
    },
    Field {
        expression: ExpressionId,
        name: Name,
    },
    Call {
        ident_expression: IdentExpression,
        arguments: Vec<ExpressionId>,
    },
    Index {
        left_side: ExpressionId,
        index: ExpressionId,
    },
    Literal(LiteralInstance),
    IdentExpression(IdentExpression),
}

pub type StatementId = Idx<Statement>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Missing,
    Compound {
        statements: Vec<StatementId>,
    },
    ConditionalCompound {
        statements: Vec<StatementId>,
    },
    Let {
        binding_id: BindingId,
        type_ref: Option<TypeSpecifierId>,
        initializer: Option<ExpressionId>,
    },
    Const {
        binding_id: BindingId,
        type_ref: Option<TypeSpecifierId>,
        initializer: Option<ExpressionId>,
    },
    Variable {
        binding_id: BindingId,
        type_ref: Option<TypeSpecifierId>,
        initializer: Option<ExpressionId>,
        template_parameters: Vec<ExpressionId>,
    },
    Return {
        expression: Option<ExpressionId>,
    },
    Assignment {
        left_side: ExpressionId,
        right_side: ExpressionId,
    },
    CompoundAssignment {
        left_side: ExpressionId,
        right_side: ExpressionId,
        operator: AssignmentOperator,
    },
    PhonyAssignment {
        right_side: ExpressionId,
    },
    IncrDecr {
        expression: ExpressionId,
        operator: IncrementDecrement,
    },
    If {
        condition: ExpressionId,
        block: StatementId,
        else_if_blocks: Vec<StatementId>,
        else_block: Option<StatementId>,
    },
    For {
        initializer: Option<StatementId>,
        condition: Option<ExpressionId>,
        continuing_part: Option<StatementId>,
        block: StatementId,
    },
    While {
        condition: ExpressionId,
        block: StatementId,
    },
    Switch {
        expression: ExpressionId,
        case_blocks: Vec<(Vec<SwitchCaseSelector>, StatementId)>,
    },
    Loop {
        body: StatementId,
    },
    Discard,
    Break,
    Continue,
    Continuing {
        block: StatementId,
    },
    BreakIf {
        condition: ExpressionId,
    },
    Assert {
        expression: ExpressionId,
    },
    FunctionCall {
        expression: ExpressionId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SwitchCaseSelector {
    Expression(ExpressionId),
    Default,
}
