use std::{iter, ops::Index};

use base_db::SourceDatabase;
use either::Either;
use la_arena::{Arena, ArenaMap, Idx, IdxRange, RawIdx};
use rustc_hash::FxHashMap;
use syntax::ast;
use triomphe::Arc;

use super::{BindingId, Body};
use crate::{
    FileAstId,
    body::Binding,
    database::{CompoundStatementId, DefDatabase, DefinitionWithBodyId},
    expression::{ExpressionId, Statement, StatementId, SwitchCaseSelector},
    expression_store::ExpressionStore,
    item_tree::Name,
};

pub type ScopeId = Idx<ScopeData>;

#[derive(Debug, PartialEq, Eq)]
pub struct ExprScopes {
    scopes: Arena<ScopeData>,
    scope_by_expression: ArenaMap<ExpressionId, ScopeId>,
    scope_entries: Arena<ScopeEntry>,
}

/// All scopes are block scopes in WGSL.
#[derive(Debug, PartialEq, Eq)]
pub struct ScopeData {
    parent: Option<ScopeId>,
    kind: ScopeKind,
    entries: IdxRange<ScopeEntry>,
}

#[derive(Debug, PartialEq, Eq)]
enum ScopeKind {
    None,
    CompoundStatement(StatementId), // Statement should be CompoundStatement
}

/// `AstId` points to an AST node in any file.
///
/// It is stable across reparses, and can be used as salsa key/value.
pub type AstId<N> = crate::InFile<FileAstId<N>>;

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeEntry {
    pub name: Name,
    pub binding: BindingId,
}

impl Index<ScopeId> for ExprScopes {
    type Output = ScopeData;

    fn index(
        &self,
        index: ScopeId,
    ) -> &Self::Output {
        &self.scopes[index]
    }
}

fn empty_entries(idx: usize) -> IdxRange<ScopeEntry> {
    IdxRange::new(Idx::from_raw(RawIdx::from(idx as u32))..Idx::from_raw(RawIdx::from(idx as u32)))
}

impl ExprScopes {
    pub fn expression_scopes_query(
        database: &dyn DefDatabase,
        definition: DefinitionWithBodyId,
    ) -> Arc<Self> {
        let body = database.body(definition);
        Arc::new(Self::new(&body))
    }

    #[must_use]
    pub fn new(body: &Body) -> Self {
        let mut scopes = Self {
            scopes: Arena::default(),
            scope_by_expression: ArenaMap::with_capacity(body.expressions.len()),
            scope_entries: Arena::default(),
        };

        let root = scopes.root_scope();

        scopes.add_parameter_bindings(body, root, &body.parameters);

        if let Some(statement) = body.root {
            match statement {
                Either::Left(statement) => {
                    compute_statement_scopes(statement, body, &mut scopes, root);
                },
                Either::Right(expression) => {
                    compute_expression_scopes(expression, body, &mut scopes, root);
                },
            }
        }

        scopes
    }

    #[must_use]
    pub fn scope_for_expression(
        &self,
        expression: ExpressionId,
    ) -> Option<ScopeId> {
        self.scope_by_expression.get(expression).copied()
    }

    /// Returns the scopes in ascending order.
    pub fn scope_chain(
        &self,
        scope: Option<ScopeId>,
    ) -> impl Iterator<Item = ScopeId> + '_ {
        iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    #[must_use]
    pub fn entries(
        &self,
        scope: ScopeId,
    ) -> &[ScopeEntry] {
        &self.scope_entries[self.scopes[scope].entries.clone()]
    }

    #[must_use]
    pub fn resolve_name_in_scope(
        &self,
        scope: ScopeId,
        name: &Name,
    ) -> Option<&ScopeEntry> {
        self.scope_chain(Some(scope))
            .find_map(|scope| self.entries(scope).iter().find(|entry| entry.name == *name))
    }

    fn root_scope(&mut self) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: None,
            kind: ScopeKind::None,
            entries: empty_entries(self.scope_entries.len()),
        })
    }

    fn set_scope_expression(
        &mut self,
        expression: ExpressionId,
        scope: ScopeId,
    ) {
        _ = self.scope_by_expression.insert(expression, scope);
    }

    fn add_parameter_bindings(
        &mut self,
        body: &Body,
        root: ScopeId,
        parameters: &[BindingId],
    ) {
        for parameter in parameters {
            self.add_binding(&body.store, root, *parameter);
        }
    }

    fn add_binding(
        &mut self,
        store: &ExpressionStore,
        scope: ScopeId,
        binding: BindingId,
    ) {
        let Binding { name, .. } = &store[binding];
        let entry = self.scope_entries.alloc(ScopeEntry {
            name: name.clone(),
            binding,
        });
        self.scopes[scope].entries =
            IdxRange::new_inclusive(self.scopes[scope].entries.start()..=entry);
    }

    fn new_compound_scope(
        &mut self,
        compound_statement: Option<StatementId>,
        parent: ScopeId,
    ) -> ScopeId {
        let kind = match compound_statement {
            Some(id) => ScopeKind::CompoundStatement(id),
            None => ScopeKind::None,
        };
        self.scopes.alloc(ScopeData {
            parent: Some(parent),
            kind,
            entries: empty_entries(self.scope_entries.len()),
        })
    }

    fn new_scope(
        &mut self,
        parent: ScopeId,
    ) -> ScopeId {
        self.scopes.alloc(ScopeData {
            parent: Some(parent),
            kind: ScopeKind::None,
            entries: empty_entries(self.scope_entries.len()),
        })
    }

    pub fn scope_for(
        &self,
        expression: ExpressionId,
    ) -> Option<ScopeId> {
        self.scope_by_expression.get(expression).copied()
    }

    pub fn scope_by_expression(&self) -> &ArenaMap<ExpressionId, ScopeId> {
        &self.scope_by_expression
    }

    fn shrink_to_fit(&mut self) {
        let ExprScopes {
            scopes,
            scope_entries,
            scope_by_expression,
        } = self;
        scopes.shrink_to_fit();
        scope_entries.shrink_to_fit();
        scope_by_expression.shrink_to_fit();
    }
}

fn compute_compound_statement_scopes(
    statements: &[StatementId],
    body: &Body,
    scopes: &mut ExprScopes,
    mut scope: ScopeId,
) {
    for statement in statements {
        scope = compute_statement_scopes(*statement, body, scopes, scope);
    }
}

#[expect(clippy::too_many_lines, reason = "Long but simple match")]
#[must_use]
fn compute_statement_scopes(
    statement_id: StatementId,
    body: &Body,
    scopes: &mut ExprScopes,
    scope: ScopeId,
) -> ScopeId {
    let statement = &body.statements[statement_id];
    match statement {
        Statement::Compound { id, statements } => {
            let new_scope = scopes.new_compound_scope(*id, scope);
            compute_compound_statement_scopes(statements, body, scopes, new_scope);
        },
        Statement::ConditionalCompound { statements } => {
            compute_compound_statement_scopes(statements, body, scopes, scope);
        },
        Statement::Variable {
            binding_id,
            initializer,
            ..
        }
        | Statement::Const {
            binding_id,
            initializer,
            ..
        }
        | Statement::Let {
            binding_id,
            initializer,
            ..
        } => {
            if let Some(init) = initializer {
                compute_expression_scopes(*init, body, scopes, scope);
            }
            let scope = scopes.new_scope(scope);
            scopes.add_binding(body, scope, *binding_id);
            return scope;
        },
        Statement::Assignment {
            left_side,
            right_side,
        }
        | Statement::CompoundAssignment {
            left_side,
            right_side,
            ..
        } => {
            compute_expression_scopes(*left_side, body, scopes, scope);
            compute_expression_scopes(*right_side, body, scopes, scope);
        },
        Statement::PhonyAssignment { right_side } => {
            compute_expression_scopes(*right_side, body, scopes, scope);
        },
        Statement::IncrDecr { expression, .. }
        | Statement::Expression { expression }
        | Statement::Assert { expression } => {
            compute_expression_scopes(*expression, body, scopes, scope);
        },
        Statement::If {
            condition,
            block,
            else_if_blocks,
            else_block,
        } => {
            compute_expression_scopes(*condition, body, scopes, scope);
            compute_statement_scopes(*block, body, scopes, scope);
            for else_if_block in else_if_blocks {
                compute_statement_scopes(*else_if_block, body, scopes, scope);
            }
            if let Some(else_block) = else_block {
                compute_statement_scopes(*else_block, body, scopes, scope);
            }
        },
        Statement::Switch {
            expression,
            case_blocks,
        } => {
            compute_expression_scopes(*expression, body, scopes, scope);

            for (selectors, case) in case_blocks {
                for selector in selectors {
                    if let SwitchCaseSelector::Expression(selector) = selector {
                        compute_expression_scopes(*selector, body, scopes, scope);
                    }
                }

                let case_scope = scopes.new_compound_scope(case, scope);
                compute_statement_scopes(*case, body, scopes, case_scope);
            }
        },
        Statement::For {
            initializer,
            condition,
            continuing_part,
            block,
        } => {
            let mut new_scope = scope;
            if let Some(init) = initializer {
                new_scope = compute_statement_scopes(*init, body, scopes, new_scope);
            }
            if let Some(condition) = condition {
                compute_expression_scopes(*condition, body, scopes, new_scope);
            }
            if let Some(cont) = continuing_part {
                // Variables produced in the continuing block are not used
                compute_statement_scopes(*cont, body, scopes, new_scope);
            }
            compute_statement_scopes(*block, body, scopes, new_scope);
        },
        Statement::While { condition, block } => {
            compute_expression_scopes(*condition, body, scopes, scope);
            compute_statement_scopes(*block, body, scopes, scope);
        },
        Statement::Return { expression } => {
            if let Some(expression) = expression {
                compute_expression_scopes(*expression, body, scopes, scope);
            }
        },
        Statement::BreakIf { condition } => {
            compute_expression_scopes(*condition, body, scopes, scope);
        },
        Statement::Missing | Statement::Discard | Statement::Break | Statement::Continue => {},
        Statement::Continuing { block } | Statement::Loop { body: block } => {
            compute_statement_scopes(*block, body, scopes, scope);
        },
    }
    scope
}

fn compute_expression_scopes(
    expression: ExpressionId,
    body: &Body,
    scopes: &mut ExprScopes,
    scope: ScopeId,
) {
    scopes.set_scope_expression(expression, scope);
    body.store[expression].walk_child_expressions(|child| {
        compute_expression_scopes(child, body, scopes, scope);
    });
}
