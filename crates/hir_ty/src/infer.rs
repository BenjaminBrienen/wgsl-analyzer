mod unify;

use std::{fmt, ops::Index};

use base_db::{Lookup as _, TextRange, TextSize};
use either::Either;
use hir_def::{
    HasSource as _,
    body::{BindingId, Body},
    database::{
        DefinitionWithBodyId, GlobalConstantId, GlobalVariableId, ModuleDefinitionId, OverrideId,
        StructId,
    },
    expression::{
        ArithmeticOperation, BinaryOperation, ComparisonOperation, Expression, ExpressionId,
        Statement, StatementId, SwitchCaseSelector, UnaryOperator,
    },
    expression_store::{ExpressionStore, ExpressionStoreSource, path::Path},
    item_tree::Name,
    mod_path::PathKind,
    name_resolution::ModuleData,
    resolver::{ResolveKind, Resolver},
    signature::{
        ConstantSignature, FieldId, FunctionSignature, OverrideSignature, VariableSignature,
    },
    type_ref::{self, VecDimensionality},
    type_specifier::{IdentExpression, TypeSpecifierId},
};
use la_arena::ArenaMap;
use rustc_hash::FxHashMap;
use wgsl_types::syntax::{AccessMode, AddressSpace, Enumerant};

use crate::{
    builtins::{Builtin, BuiltinId, BuiltinOverload, BuiltinOverloadId},
    database::HirDatabase,
    diagnostics::{InferenceDiagnostic, InferenceDiagnosticKind},
    function::{FunctionDetails, ResolvedFunctionId},
    infer::unify::{UnificationTable, unify},
    lower::{
        Lowered, LoweredKind, ResolvedCall, TemplateParameter, TemplateParameters, TypeContainer,
        TypeLoweringContext, TypeLoweringError, WgslTypeConverter, to_wgsl_binary_operator,
        to_wgsl_unary_operator,
    },
    ty::{
        ArraySize, ArrayType, AtomicType, BuiltinStruct, MatrixType, Pointer, Reference,
        ScalarType, TextureDimensionality, TextureKind, TextureType, Type, TypeKind, VecSize,
        VectorType,
    },
};

#[salsa::tracked]
impl InferenceResult {
    /// Infers the type of a global item.
    /// For `const`s and co, it first uses the specified type,
    /// and then uses the body (expression) to infer the return type.
    #[salsa::tracked(returns(ref), cycle_result = infer_cycle_result)]
    pub fn of(
        db: &dyn HirDatabase,
        definition: DefinitionWithBodyId,
    ) -> Self {
        infer_query(db, definition)
    }
}

// TODO load rule somewhere in here
fn infer_query(
    database: &dyn HirDatabase,
    definition: DefinitionWithBodyId,
) -> InferenceResult {
    let resolver = definition.resolver(database);
    let body = database.body(definition);
    let mut context = InferenceContext::new(database, definition.into(), resolver);
    match definition {
        DefinitionWithBodyId::Function(function) => {
            let data = database.function_data(function).0;
            let return_type = context.collect_fn(&data, &body);
            context.infer_body(&body, return_type, AbstractHandling::Concretize);
        },
        DefinitionWithBodyId::GlobalVariable(variable) => {
            let data = database.global_var_data(variable).0;
            let return_type = context.collect_global_variable(&data, &body);
            context.infer_body(&body, return_type, AbstractHandling::Concretize);
            context.infer_global_variable(&data, &body);
        },
        DefinitionWithBodyId::GlobalConstant(constant) => {
            let data = database.global_constant_data(constant).0;
            let return_type = context.collect_global_constant(&data, &body);
            context.infer_body(&body, return_type, AbstractHandling::Abstract);
        },
        DefinitionWithBodyId::Override(override_declaration) => {
            let data = database.override_data(override_declaration).0;
            let return_type = context.collect_override(&data, &body);
            context.infer_body(&body, return_type, AbstractHandling::Concretize);
        },
        DefinitionWithBodyId::GlobalAssertStatement(_global_assert_statement) => {
            let expression = body.root.and_then(Either::right);
            if let Some(expression) = expression {
                let expected_type = context.types.bool;
                context.infer_expression_expect(expression, expected_type.into(), &body.store);
            }
        },
    }
    context.resolve_all()
}

fn infer_cycle_result(
    database: &dyn HirDatabase,
    _: salsa::Id,
    definition: DefinitionWithBodyId,
) -> InferenceResult {
    let types = InternedStandardTypes::new(database);
    let mut inference_result = InferenceResult::new(types.error);
    let (name, range) = get_name_and_range(database, ModuleDefinitionId::from(definition));

    inference_result.diagnostics.push(InferenceDiagnostic {
        source: ExpressionStoreSource::Body,
        kind: InferenceDiagnosticKind::CyclicType { name, range },
    });

    inference_result
}

fn get_name_and_range(
    database: &dyn HirDatabase,
    definition: ModuleDefinitionId,
) -> (Name, base_db::TextRange) {
    match definition {
        ModuleDefinitionId::Module(file_id) => {
            let module_data = ModuleData::of(database, file_id);
            let full_range = TextRange::empty(TextSize::new(0));

            let name = module_data.as_ref().map_or_else(Name::missing, |module| {
                module.name.clone().unwrap_or_else(|| Name::from("package"))
            });
            (name, full_range)
        },
        ModuleDefinitionId::Function(id) => (
            database.function_data(id).0.name.clone(),
            id.lookup(database)
                .source(database)
                .original_file_range(database)
                .range,
        ),
        ModuleDefinitionId::GlobalVariable(id) => (
            database.global_var_data(id).0.name.clone(),
            id.lookup(database)
                .source(database)
                .original_file_range(database)
                .range,
        ),
        ModuleDefinitionId::GlobalConstant(id) => (
            database.global_constant_data(id).0.name.clone(),
            id.lookup(database)
                .source(database)
                .original_file_range(database)
                .range,
        ),
        ModuleDefinitionId::Override(id) => (
            database.override_data(id).0.name.clone(),
            id.lookup(database)
                .source(database)
                .original_file_range(database)
                .range,
        ),
        ModuleDefinitionId::Struct(id) => (
            database.struct_data(id).0.name.clone(),
            id.lookup(database)
                .source(database)
                .original_file_range(database)
                .range,
        ),
        ModuleDefinitionId::TypeAlias(id) => (
            database.type_alias_data(id).0.name.clone(),
            id.lookup(database)
                .source(database)
                .original_file_range(database)
                .range,
        ),
        ModuleDefinitionId::GlobalAssertStatement(id) => (
            Name::from("const_assert"),
            id.lookup(database)
                .source(database)
                .original_file_range(database)
                .range,
        ),
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct InferenceResult {
    pub(crate) type_of_expression: ArenaMap<ExpressionId, Type>,
    pub(crate) type_of_binding: ArenaMap<BindingId, Type>,
    diagnostics: Vec<InferenceDiagnostic>,
    /// The return type of the function being inferred.
    return_type: Option<Type>,
    call_resolutions: FxHashMap<ExpressionId, ResolvedCall>,
    field_resolutions: FxHashMap<ExpressionId, FieldId>,
    error_type: Type,
}

impl InferenceResult {
    fn new(error_type: Type) -> Self {
        Self {
            type_of_expression: ArenaMap::default(),
            type_of_binding: ArenaMap::default(),
            diagnostics: Vec::default(),
            return_type: Some(error_type), // set in collect_* calls
            call_resolutions: FxHashMap::default(),
            field_resolutions: FxHashMap::default(),
            error_type,
        }
    }

    #[must_use]
    pub fn field_resolution(
        &self,
        expression: ExpressionId,
    ) -> Option<FieldId> {
        self.field_resolutions.get(&expression).copied()
    }

    #[must_use]
    pub fn call_resolution(
        &self,
        expression: ExpressionId,
    ) -> Option<ResolvedCall> {
        self.call_resolutions.get(&expression).copied()
    }

    #[must_use]
    pub fn diagnostics(&self) -> &[InferenceDiagnostic] {
        &self.diagnostics
    }

    #[must_use]
    pub const fn return_type(&self) -> Option<Type> {
        self.return_type
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.type_of_expression.values().next().is_none()
            && self.type_of_binding.values().next().is_none()
            && self.diagnostics.is_empty()
            && self.call_resolutions.is_empty()
            && self.field_resolutions.is_empty()
    }

    #[must_use]
    pub fn expression_type(
        &self,
        id: ExpressionId,
    ) -> Type {
        self.type_of_expression.get(id).map_or_else(
            || {
                debug_assert!(!self.diagnostics().is_empty());
                self.error_type
            },
            |r#type| *r#type,
        )
    }

    #[must_use]
    pub fn binding_type(
        &self,
        id: BindingId,
    ) -> Type {
        self.type_of_binding.get(id).map_or_else(
            || {
                debug_assert!(!self.diagnostics().is_empty());
                self.error_type
            },
            |r#type| *r#type,
        )
    }
}

/// These types are referred to "hardcoded" and are available here for brevity and avoid re-interning.
#[expect(non_snake_case, reason = "matches WGSL spec")]
#[derive(Debug, Clone)]
struct InternedStandardTypes {
    pub abstract_int: Type,
    pub abstract_float: Type,
    pub u32: Type,
    pub u64: Type,
    pub i32: Type,
    pub i64: Type,
    pub f16: Type,
    pub f32: Type,
    // pub f64: Type,
    pub bool: Type,
    // frexp
    pub __frexp_result_abstract: Type,
    pub __frexp_result_f32: Type,
    pub __frexp_result_f16: Type,
    pub __frexp_result_vecN_abstract: Type,
    pub __frexp_result_vecN_f32: Type,
    pub __frexp_result_vecN_f16: Type,
    // modf
    pub __modf_result_abstract: Type,
    pub __modf_result_f32: Type,
    pub __modf_result_f16: Type,
    pub __modf_result_vecN_abstract: Type,
    pub __modf_result_vecN_f32: Type,
    pub __modf_result_vecN_f16: Type,
    // atomicCompareExchangeWeak
    pub __atomic_compare_exchange_result_i32: Type,
    pub __atomic_compare_exchange_result_u32: Type,
    pub error: Type,
}

impl InternedStandardTypes {
    fn new(database: &dyn HirDatabase) -> Self {
        let create = |kind| Type::new(database, kind);
        #[rustfmt::skip]
        return Self {
            abstract_int: create(TypeKind::Scalar(ScalarType::AbstractInt)),
            abstract_float: create(TypeKind::Scalar(ScalarType::AbstractFloat)),
            u32: create(TypeKind::Scalar(ScalarType::U32)),
            u64: create(TypeKind::Scalar(ScalarType::U64)),
            i32: create(TypeKind::Scalar(ScalarType::I32)),
            i64: create(TypeKind::Scalar(ScalarType::I64)),
            f16: create(TypeKind::Scalar(ScalarType::F16)),
            f32: create(TypeKind::Scalar(ScalarType::F32)),
            // f64: create_ty(TypeKind::Scalar(ScalarType::F64)),
            bool: create(TypeKind::Scalar(ScalarType::Bool)),
            __frexp_result_abstract: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__frexp_result_abstract".to_owned(), fields: vec![] })),
            __frexp_result_f32: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__frexp_result_f32".to_owned(), fields: vec![] })),
            __frexp_result_f16: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__frexp_result_f16".to_owned(), fields: vec![] })),
            __frexp_result_vecN_abstract: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__frexp_result_vecN_abstract".to_owned(), fields: vec![] })),
            __frexp_result_vecN_f32: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__frexp_result_vecN_f32".to_owned(), fields: vec![] })),
            __frexp_result_vecN_f16: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__frexp_result_vecN_f16".to_owned(), fields: vec![] })),
            __modf_result_abstract: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__modf_result_abstract".to_owned(), fields: vec![] })),
            __modf_result_f32: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__modf_result_f32".to_owned(), fields: vec![] })),
            __modf_result_f16: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__modf_result_f16".to_owned(), fields: vec![] })),
            __modf_result_vecN_abstract: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__modf_result_vecN_abstract".to_owned(), fields: vec![] })),
            __modf_result_vecN_f32: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__modf_result_vecN_f32".to_owned(), fields: vec![] })),
            __modf_result_vecN_f16: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__modf_result_vecN_f16".to_owned(), fields: vec![] })),
            __atomic_compare_exchange_result_i32: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__atomic_compare_exchange_result<i32>".to_owned(), fields: vec![] })),
            __atomic_compare_exchange_result_u32: create(TypeKind::BuiltinStruct(BuiltinStruct { name: "__atomic_compare_exchange_result<u32>".to_owned(), fields: vec![] })),
            error: create(TypeKind::Error),
        };
    }
}

/// Runs inference for items that have a body, such as functions.
pub struct InferenceContext<'database> {
    database: &'database dyn HirDatabase,
    owner: ModuleDefinitionId,
    /// Root resolver for the entire module.
    resolver: Resolver,
    result: InferenceResult, // set in collect_* calls
    return_type: Option<Type>,
    types: InternedStandardTypes,
    converter: WgslTypeConverter<'database>,
}

impl<'database> InferenceContext<'database> {
    pub fn new(
        database: &'database dyn HirDatabase,
        owner: ModuleDefinitionId,
        resolver: Resolver,
    ) -> Self {
        let types = InternedStandardTypes::new(database);
        Self {
            database,
            owner,
            resolver,
            result: InferenceResult::new(types.error),
            return_type: Some(types.error),
            types,
            converter: WgslTypeConverter::new(database),
        }
    }

    // pub fn with_store<T>(
    //     &mut self,
    //     store: &'database ExpressionStore,
    //     f: impl FnOnce(&mut InferenceContext<'_>) -> T,
    // ) -> T {
    //     let old_store = std::mem::replace(&mut self.store, store);
    //     let result = f(self);
    //     self.store = old_store;
    //     result
    // }

    fn set_expression_type(
        &mut self,
        expression: ExpressionId,
        r#type: Type,
    ) {
        self.result.type_of_expression.insert(expression, r#type);
    }

    fn set_binding_type(
        &mut self,
        binding: BindingId,
        r#type: Type,
    ) {
        self.result.type_of_binding.insert(binding, r#type);
    }

    fn bind_return_type(
        &mut self,
        r#type: Option<Type>,
        body: &Body,
    ) {
        if let Some(r#type) = r#type
            && let Some(binding) = body.main_binding
        {
            self.set_binding_type(binding, r#type);
        }
        self.return_type = r#type;
    }

    fn set_field_resolution(
        &mut self,
        expression: ExpressionId,
        field: FieldId,
    ) {
        self.result.field_resolutions.insert(expression, field);
    }

    fn push_diagnostic(
        &mut self,
        source: ExpressionStoreSource,
        kind: InferenceDiagnosticKind,
    ) {
        self.result
            .diagnostics
            .push(InferenceDiagnostic { source, kind });
    }

    fn push_lowering_diagnostics(
        &mut self,
        mut diagnostics: Vec<TypeLoweringError>,
        store: &ExpressionStore,
    ) {
        for diagnostic in diagnostics {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::InvalidType { error: diagnostic },
            );
        }
    }

    fn resolve_all(mut self) -> InferenceResult {
        self.result.return_type = self.return_type;
        self.result
    }

    fn collect_global_variable(
        &mut self,
        variable: &VariableSignature,
        body: &Body,
    ) -> Option<Type> {
        let r#type = variable
            .r#type
            .map(|r#type| self.lower_type(r#type, &self.resolver.clone(), &variable.store));

        self.bind_return_type(r#type, body);
        r#type
    }

    fn infer_global_variable(
        &mut self,
        variable: &VariableSignature,
        body: &Body,
    ) {
        let (address_space, access_mode) =
            self.infer_variable_template(&variable.template_parameters, &variable.store);
        if address_space == AddressSpace::Function {
            // Function address space is not allowed at the module level
            self.push_diagnostic(
                variable.store.store_source,
                InferenceDiagnosticKind::UnexpectedTemplateArgument {
                    expression: variable.template_parameters[0],
                },
            );
        }
        self.bind_return_type(
            Some(self.make_ref(
                address_space,
                self.return_type.unwrap_or_else(|| {
                    //debug_assert!(!self.result.diagnostics().is_empty());
                    self.types.error
                }),
                access_mode,
            )),
            body,
        );
    }

    fn infer_variable_template(
        &mut self,
        template: &[ExpressionId],
        store: &ExpressionStore,
    ) -> (AddressSpace, AccessMode) {
        let mut context = TypeLoweringContext::new(self.database, &self.resolver, store);
        let template_args: Vec<_> = template
            .iter()
            .map(|argument| context.evaluate_template_argument(*argument))
            .collect();
        self.push_lowering_diagnostics(context.diagnostics, store);

        let default_address_space = match store.store_source {
            ExpressionStoreSource::Body => AddressSpace::Function,
            ExpressionStoreSource::Signature => AddressSpace::Handle,
        };

        let address_space = match template_args.first() {
            Some(TemplateParameter::Enumerant(Enumerant::AddressSpace(address_space))) => {
                *address_space
            },
            None => default_address_space,
            _ => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::UnexpectedTemplateArgument {
                        expression: template[0],
                    },
                );
                default_address_space
            },
        };
        let access_mode = match template_args.get(1) {
            Some(TemplateParameter::Enumerant(Enumerant::AccessMode(access_mode))) => {
                if address_space == AddressSpace::Storage {
                    *access_mode
                } else {
                    // Only the storage address space allows for an access mode
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::UnexpectedTemplateArgument {
                            expression: template[0],
                        },
                    );
                    address_space.default_access_mode()
                }
            },
            None => address_space.default_access_mode(),
            _ => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::UnexpectedTemplateArgument {
                        expression: template[0],
                    },
                );
                address_space.default_access_mode()
            },
        };

        // Mark extra template arguments as errors
        if template.len() > 2 {
            for expression in &template[2..] {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::UnexpectedTemplateArgument {
                        expression: *expression,
                    },
                );
            }
        }
        (address_space, access_mode)
    }

    fn collect_global_constant(
        &mut self,
        constant: &ConstantSignature,
        body: &Body,
    ) -> Option<Type> {
        let r#type = constant
            .r#type
            .map(|r#type| self.lower_type(r#type, &self.resolver.clone(), &constant.store));
        self.bind_return_type(r#type, body);
        r#type
    }

    fn collect_override(
        &mut self,
        override_data: &OverrideSignature,
        body: &Body,
    ) -> Option<Type> {
        let r#type = override_data
            .r#type
            .map(|r#type| self.lower_type(r#type, &self.resolver.clone(), &override_data.store));
        self.bind_return_type(r#type, body);
        r#type
    }

    fn collect_fn(
        &mut self,
        function_data: &FunctionSignature,
        body: &Body,
    ) -> Option<Type> {
        for ((_, parameter), &binding_id) in function_data.parameters.iter().zip(&body.parameters) {
            let parameter_type = self.lower_type(
                parameter.r#type,
                &self.resolver.clone(),
                &function_data.store,
            );
            self.set_binding_type(binding_id, parameter_type);
        }
        self.return_type = function_data.return_type.map(|type_ref| {
            self.lower_type(type_ref, &self.resolver.clone(), &function_data.store)
        });
        self.return_type
    }

    /// Runs type inference on the body and infer the type for `const`s, `var`s and `override`s.
    fn infer_body(
        &mut self,
        body: &Body,
        return_type: Option<Type>,
        abstract_handling: AbstractHandling,
    ) {
        match body.root {
            Some(Either::Left(statement)) => {
                self.infer_statement(statement, body, return_type);
            },
            Some(Either::Right(expression)) => {
                let r#type =
                    self.infer_initializer(body, Some(expression), return_type, abstract_handling);

                if return_type.is_none() {
                    self.bind_return_type(Some(r#type), body);
                }
            },
            None => (),
        }
    }

    fn resolver_for_expression(
        &self,
        expression: ExpressionId,
    ) -> Option<Resolver> {
        let ModuleDefinitionId::Function(function) = self.owner else {
            return None;
        };
        let expression_scopes = self
            .database
            .expression_scopes(DefinitionWithBodyId::Function(function));

        let scope_id = expression_scopes.scope_for_expression(expression)?;

        Some(
            self.resolver
                .clone()
                .push_expression_scope(function, expression_scopes, scope_id),
        )
    }

    fn resolver_for_statement(
        &self,
        statement: StatementId,
    ) -> Resolver {
        let ModuleDefinitionId::Function(function) = self.owner else {
            return self.resolver.clone();
        };

        let expression_scopes = self
            .database
            .expression_scopes(DefinitionWithBodyId::Function(function));

        if let Some(scope_id) = expression_scopes.scope_for_statement(statement) {
            self.resolver
                .clone()
                .push_expression_scope(function, expression_scopes, scope_id)
        } else {
            self.resolver.clone()
        }
    }

    #[expect(clippy::too_many_lines, reason = "match with many small cases")]
    fn infer_statement(
        &mut self,
        statement: StatementId,
        body: &Body,
        return_type: Option<Type>,
    ) {
        let resolver = self.resolver_for_statement(statement);

        match &body.statements[statement] {
            Statement::Compound { statements } => {
                for statement in statements {
                    self.infer_statement(*statement, body, return_type);
                }
            },
            Statement::Variable {
                binding_id,
                type_ref,
                initializer,
                template_parameters,
            } => {
                // The store type is the effective-value-type of the variable’s declaration.
                let mut r#type =
                    self.get_effective_value_type(body, &resolver, *type_ref, *initializer);
                if let Some(initializer_expression) = initializer
                    && !r#type.kind(self.database).is_storable()
                    && !r#type.kind(self.database).is_error()
                {
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::StoreTypeMustBeStorable {
                            actual: r#type,
                            expression: *initializer_expression,
                        },
                    );
                    // this ensures that make_ref has a valid input and analysis can continue
                    r#type = self.types.error;
                }

                let (address_space, access_mode) =
                    self.infer_variable_template(template_parameters, body);
                if address_space != AddressSpace::Function {
                    // Only function address space is allowed
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::UnexpectedTemplateArgument {
                            expression: template_parameters[0],
                        },
                    );
                }
                let reference_type = self.make_ref(address_space, r#type, access_mode);
                self.set_binding_type(*binding_id, reference_type);
            },
            Statement::Const {
                binding_id,
                type_ref,
                initializer,
                ..
            } => {
                let r#type = type_ref.map(|r#type| self.lower_type(r#type, &resolver, body));
                let r#type =
                    self.infer_initializer(body, *initializer, r#type, AbstractHandling::Abstract);
                self.set_binding_type(*binding_id, r#type);
            },
            Statement::Let {
                binding_id,
                type_ref,
                initializer,
            } => {
                let r#type = type_ref.map(|r#type| self.lower_type(r#type, &resolver, body));
                let r#type = self.infer_initializer(
                    body,
                    *initializer,
                    r#type,
                    AbstractHandling::Concretize,
                );
                self.set_binding_type(*binding_id, r#type);
            },

            Statement::Return { expression } => match (expression, return_type) {
                (Some(expression), Some(return_type)) => {
                    self.infer_expression_expect(*expression, self.return_type.into(), body);
                },
                (Some(expression), None) => {
                    let actual =
                        self.infer_expression_expect(*expression, self.return_type.into(), body);
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::UnexpectedReturnValue {
                            expression: *expression,
                            actual,
                        },
                    );
                },
                _ => (),
            },
            Statement::Assignment {
                left_side,
                right_side,
            } => {
                let left_type = self.infer_expression(*left_side, body);

                let kind = left_type.kind(self.database);
                let left_inner = if let TypeKind::Reference(reference) = kind {
                    reference.inner
                } else {
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::AssignmentNotAReference {
                            left_side: *left_side,
                            actual: left_type,
                        },
                    );
                    self.types.error
                };

                self.infer_expression_expect(*right_side, left_inner.into(), body);
            },
            Statement::CompoundAssignment {
                left_side,
                right_side,
                operator,
            } => {
                let left_type = self.infer_expression(*left_side, body);

                let left_kind = left_type.kind(self.database);
                let left_inner = if let TypeKind::Reference(reference) = left_kind {
                    reference.inner
                } else {
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::AssignmentNotAReference {
                            left_side: *left_side,
                            actual: left_type,
                        },
                    );
                    self.types.error
                };

                let r#type = self.infer_binary_op(
                    *right_side,
                    *left_side,
                    *right_side,
                    (*operator).into(),
                    body,
                );

                if !r#type.is_convertible_to(left_inner, self.database) {
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::TypeMismatch {
                            expression: *right_side,
                            actual: r#type,
                            expected: TypeExpectation::Type(TypeExpectationInner::Exact(
                                left_inner,
                            )),
                        },
                    );
                }
            },
            Statement::PhonyAssignment { right_side } => {
                self.infer_expression(*right_side, body);
            },
            Statement::IncrDecr { expression, .. } => {
                let left_type = self.infer_expression(*expression, body);

                let left_kind = left_type.kind(self.database);
                let left_inner = if let TypeKind::Reference(reference) = left_kind {
                    reference.inner
                } else {
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::AssignmentNotAReference {
                            left_side: *expression,
                            actual: left_type,
                        },
                    );
                    self.types.error
                };

                if self
                    .expect_type_inner(left_inner, TypeExpectationInner::IntegerScalar)
                    .is_err()
                {
                    self.push_diagnostic(
                        body.store_source,
                        InferenceDiagnosticKind::TypeMismatch {
                            expression: *expression,
                            actual: left_inner,
                            expected: TypeExpectation::Type(TypeExpectationInner::IntegerScalar),
                        },
                    );
                }
            },
            Statement::If {
                condition,
                block,
                else_if_blocks,
                else_block,
            } => {
                self.infer_statement(*block, body, return_type);
                for else_if_block in else_if_blocks {
                    self.infer_statement(*else_if_block, body, return_type);
                }
                if let Some(else_block) = else_block {
                    self.infer_statement(*else_block, body, return_type);
                }
                self.infer_expression_expect(
                    *condition,
                    TypeKind::Scalar(ScalarType::Bool)
                        .intern(self.database)
                        .into(),
                    body,
                );
            },
            Statement::While { condition, block } => {
                self.infer_statement(*block, body, return_type);
                self.infer_expression_expect(
                    *condition,
                    TypeKind::Scalar(ScalarType::Bool)
                        .intern(self.database)
                        .into(),
                    body,
                );
            },
            Statement::Switch {
                expression,
                case_blocks,
            } => {
                let r#type = self
                    .infer_expression(*expression, body)
                    .loaded(self.database);

                for (selectors, case) in case_blocks {
                    for selector in selectors {
                        if let SwitchCaseSelector::Expression(selector) = selector {
                            self.infer_expression_expect(*selector, r#type.into(), body);
                        }
                    }
                    self.infer_statement(*case, body, return_type);
                }
            },
            Statement::For {
                initializer,
                condition,
                continuing_part,
                block,
            } => {
                if let Some(init) = initializer {
                    self.infer_statement(*init, body, return_type);
                }
                if let Some(cont) = continuing_part {
                    self.infer_statement(*cont, body, return_type);
                }

                if let Some(condition) = condition {
                    self.infer_expression_expect(
                        *condition,
                        TypeKind::Scalar(ScalarType::Bool)
                            .intern(self.database)
                            .into(),
                        body,
                    );
                }

                self.infer_statement(*block, body, return_type);
            },
            Statement::Loop { body: loop_body } => {
                self.infer_statement(*loop_body, body, return_type);
            },
            Statement::Assert { expression } => {
                self.infer_expression_expect(
                    *expression,
                    TypeKind::Scalar(ScalarType::Bool)
                        .intern(self.database)
                        .into(),
                    body,
                );
            },
            Statement::Discard | Statement::Break | Statement::Continue | Statement::Missing => {},
            Statement::Continuing { block } => self.infer_statement(*block, body, return_type),
            Statement::BreakIf { condition } => {
                self.infer_expression_expect(
                    *condition,
                    TypeKind::Scalar(ScalarType::Bool)
                        .intern(self.database)
                        .into(),
                    body,
                );
            },
            Statement::Expression { expression } => {
                self.infer_expression(*expression, body);
            },
        }
    }

    /// Each such declaration must have an explicitly specified type or an initializer.
    /// Both a type and an initializer may be specified.
    /// Each such declaration determines the type for the associated data value, known as the effective-value-type for the declaration.
    /// The effective-value-type of the declaration is:
    /// - The declared type, if explicitly specified.
    /// - Otherwise, if the initializer expression has type T:
    ///   - For a const declaration, the effective-value-type is T itself.
    ///   - For a override, let, or var declaration, the effective-value-type is the concretization of T.
    ///
    /// Each kind of value or variable declaration may place additional constraints on the form of the initializer expression, if present, and on the effective-value-type.
    fn get_effective_value_type(
        &mut self,
        body: &Body,
        resolver: &Resolver,
        type_ref: Option<la_arena::Idx<hir_def::type_specifier::TypeSpecifier>>,
        initializer: Option<ExpressionId>,
    ) -> Type {
        let r#type = type_ref.map(|r#type| self.lower_type(r#type, resolver, body));
        let r#type =
            self.infer_initializer(body, initializer, r#type, AbstractHandling::Concretize);
        r#type.loaded(self.database).concretize(self.database)
    }

    fn infer_initializer(
        &mut self,
        store: &ExpressionStore,
        initializer: Option<ExpressionId>,
        r#type: Option<Type>,
        abstract_handling: AbstractHandling,
    ) -> Type {
        match (r#type, initializer) {
            (Some(r#type), Some(initializer)) => {
                self.infer_expression_expect(initializer, r#type.into(), store);
                r#type
            },
            (Some(r#type), None) => r#type,
            (None, Some(initializer)) => {
                let r#type = self
                    .infer_expression(initializer, store)
                    .loaded(self.database);
                if abstract_handling == AbstractHandling::Concretize {
                    r#type.concretize(self.database)
                } else {
                    r#type
                }
            },
            (None, None) => {
                // dbg!()
                // self.push_diagnostic(store.store_source, InferenceDiagnosticKind::)
                //debug_assert!(!self.result.diagnostics().is_empty());
                self.types.error
            },
        }
    }

    fn expect_type_inner(
        &self,
        r#type: Type,
        expectation: TypeExpectationInner,
    ) -> Result<(), ()> {
        let type_kind = r#type.kind(self.database);
        if type_kind == TypeKind::Error {
            return Ok(());
        }

        match expectation {
            TypeExpectationInner::Exact(expected_type) => {
                if expected_type.kind(self.database) == TypeKind::Error
                    || r#type.is_convertible_to(expected_type, self.database)
                {
                    Ok(())
                } else {
                    Err(())
                }
            },
            TypeExpectationInner::IntegerScalar => {
                if let TypeKind::Scalar(
                    ScalarType::I32 | ScalarType::U32 | ScalarType::I64 | ScalarType::U64,
                ) = r#type.kind(self.database).unref(self.database).as_ref()
                {
                    Ok(())
                } else {
                    Err(())
                }
            },
            TypeExpectationInner::IntegerIndex => {
                if let TypeKind::Scalar(
                    ScalarType::I32 | ScalarType::U32 | ScalarType::AbstractInt,
                ) = r#type.kind(self.database).unref(self.database).as_ref()
                {
                    Ok(())
                } else {
                    Err(())
                }
            },
        }
    }

    fn infer_expression_expect(
        &mut self,
        expression: ExpressionId,
        expected: TypeExpectation,
        store: &ExpressionStore,
    ) -> Type {
        let r#type = self.infer_expression(expression, store);
        match expected {
            TypeExpectation::Type(expected_type) => {
                if self.expect_type_inner(r#type, expected_type) != Ok(()) {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::TypeMismatch {
                            expression,
                            actual: r#type,
                            expected,
                        },
                    );
                }
            },
            TypeExpectation::Any => {},
            TypeExpectation::None => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::UnexpectedReturnValue {
                        expression,
                        actual: r#type,
                    },
                );
            },
        }
        r#type
    }

    #[expect(clippy::too_many_lines, reason = "match with many small cases")]
    fn infer_expression(
        &mut self,
        expression: ExpressionId,
        store: &ExpressionStore,
    ) -> Type {
        let r#type = match &store[expression] {
            Expression::Missing => {
                debug_assert!(!self.result.diagnostics().is_empty());
                self.types.error
            }, // this would be a parser error
            Expression::BinaryOperation {
                left_side,
                right_side,
                operation,
            } => self.infer_binary_op(expression, *left_side, *right_side, *operation, store),
            Expression::UnaryOperator {
                expression,
                operator,
            } => self.infer_unary_op(*expression, *operator, store),
            Expression::Field {
                expression: field_expression,
                name,
            } => self.infer_field_expression(expression, store, *field_expression, name),
            Expression::Call {
                ident_expression,
                arguments,
            } => {
                let arguments: Vec<_> = arguments
                    .iter()
                    .map(|&argument| {
                        (
                            argument,
                            self.infer_expression(argument, store).loaded(self.database),
                        )
                    })
                    .collect();
                // TODO only emit this if this is not a "bare" call statement
                self.infer_call(expression, ident_expression, arguments, store)
                    .unwrap_or(self.types.error)
                // .unwrap_or_else(|| {
                //     self.push_diagnostic(
                //         store.store_source,
                //         InferenceDiagnosticKind::ExpectedReturnValue { expression },
                //     );
                //     self.types.error
                // })
            },
            Expression::Index { left_side, index } => {
                let left_side = self.infer_expression(*left_side, store);
                let left_kind = left_side.kind(self.database);
                let index_type = self.infer_expression(*index, store).loaded(self.database);
                let index_kind = index_type.kind(self.database);
                let index_inner = index_kind.unref(self.database);
                if !index_inner.is_index() {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::TypeMismatch {
                            expression: *index,
                            expected: TypeExpectation::Type(TypeExpectationInner::IntegerIndex),
                            actual: index_type,
                        },
                    );
                }
                match left_kind {
                    TypeKind::Reference(Reference {
                        address_space,
                        inner,
                        access_mode,
                    })
                    | TypeKind::Pointer(Pointer {
                        address_space,
                        inner,
                        access_mode,
                    }) if let TypeKind::Vector(vec) = inner.kind(self.database) => {
                        self.make_ref(address_space, vec.component_type, access_mode)
                    },
                    TypeKind::Vector(vec) => vec.component_type,
                    TypeKind::Reference(Reference {
                        address_space,
                        inner,
                        access_mode,
                    })
                    | TypeKind::Pointer(Pointer {
                        address_space,
                        inner,
                        access_mode,
                    }) if let TypeKind::Matrix(matrix_type) = inner.kind(self.database) => self
                        .make_ref(
                            address_space,
                            TypeKind::Vector(VectorType {
                                size: matrix_type.rows,
                                component_type: matrix_type.inner,
                            })
                            .intern(self.database),
                            access_mode,
                        ),
                    TypeKind::Matrix(matrix_type) => TypeKind::Vector(VectorType {
                        size: matrix_type.rows,
                        component_type: matrix_type.inner,
                    })
                    .intern(self.database),
                    TypeKind::Reference(Reference {
                        address_space,
                        inner,
                        access_mode,
                    })
                    | TypeKind::Pointer(Pointer {
                        address_space,
                        inner,
                        access_mode,
                    }) if let TypeKind::Array(array) = inner.kind(self.database) => {
                        self.make_ref(address_space, array.inner, access_mode)
                    },
                    TypeKind::Array(array) => array.inner,
                    TypeKind::Scalar(_)
                    | TypeKind::Atomic(_)
                    | TypeKind::Struct(_)
                    | TypeKind::BuiltinStruct(_)
                    | TypeKind::Texture(_)
                    | TypeKind::Sampler(_)
                    | TypeKind::Reference(_)
                    | TypeKind::Pointer(_)
                    | TypeKind::BoundVariable(_)
                    | TypeKind::StorageTypeOfTexelFormat(_) => {
                        self.push_diagnostic(
                            store.store_source,
                            InferenceDiagnosticKind::ArrayAccessInvalidType {
                                expression,
                                r#type: left_side,
                            },
                        );
                        self.types.error
                    },
                    // No need to create extra diagnostics for problems upstream
                    TypeKind::Error => {
                        debug_assert!(!self.result.diagnostics.is_empty());
                        self.types.error
                    },
                }
            },
            Expression::Literal(literal) => {
                use hir_def::expression::{BuiltinFloat, BuiltinInt, Literal};
                let type_kind = match literal {
                    Literal::Int(_, BuiltinInt::I32) => TypeKind::Scalar(ScalarType::I32),
                    Literal::Int(_, BuiltinInt::U32) => TypeKind::Scalar(ScalarType::U32),
                    Literal::Int(_, BuiltinInt::I64) => TypeKind::Scalar(ScalarType::I64),
                    Literal::Int(_, BuiltinInt::U64) => TypeKind::Scalar(ScalarType::U64),
                    Literal::Int(_, BuiltinInt::Abstract) => {
                        TypeKind::Scalar(ScalarType::AbstractInt)
                    },
                    Literal::Float(_, BuiltinFloat::F16) => TypeKind::Scalar(ScalarType::F16),
                    Literal::Float(_, BuiltinFloat::F32) => TypeKind::Scalar(ScalarType::F32),
                    Literal::Float(_, BuiltinFloat::Abstract) => {
                        TypeKind::Scalar(ScalarType::AbstractFloat)
                    },
                    Literal::Bool(_) => TypeKind::Scalar(ScalarType::Bool),
                };
                type_kind.intern(self.database)
            },
            Expression::IdentExpression(ident_expression) => {
                self.infer_ident_expression(expression, ident_expression, store)
            },
        };
        self.set_expression_type(expression, r#type);
        r#type
    }

    fn infer_field_expression(
        &mut self,
        expression: ExpressionId,
        store: &ExpressionStore,
        field_expression: ExpressionId,
        name: &Name,
    ) -> Type {
        let expression_type = self.infer_expression(field_expression, store);
        if expression_type.is_err(self.database) {
            debug_assert!(!self.result.diagnostics().is_empty());
            return self.types.error;
        }
        let (kind, ref_info) = match expression_type.kind(self.database) {
            TypeKind::Reference(Reference {
                address_space,
                inner,
                access_mode,
            })
            | TypeKind::Pointer(Pointer {
                address_space,
                inner,
                access_mode,
            }) => (
                inner.kind(self.database),
                Some((address_space, access_mode)),
            ),
            kind @ (TypeKind::Error
            | TypeKind::Scalar(_)
            | TypeKind::Atomic(_)
            | TypeKind::Vector(_)
            | TypeKind::Matrix(_)
            | TypeKind::Struct(_)
            | TypeKind::BuiltinStruct(_)
            | TypeKind::Array(_)
            | TypeKind::Texture(_)
            | TypeKind::Sampler(_)
            | TypeKind::BoundVariable(_)
            | TypeKind::StorageTypeOfTexelFormat(_)) => (kind, None),
        };

        let r#type = match kind {
            TypeKind::Struct(r#struct) => self.infer_struct_field_expression(
                expression,
                store,
                field_expression,
                name,
                expression_type,
                r#struct,
            ),
            TypeKind::BuiltinStruct(builtin_struct) => self.infer_builtin_struct_field_expression(
                store,
                field_expression,
                name,
                expression_type,
                builtin_struct,
            ),
            TypeKind::Vector(vector_type) => {
                return self.infer_vec_swizzle_expression(
                    store,
                    field_expression,
                    name,
                    expression_type,
                    &vector_type,
                    ref_info,
                );
            },
            TypeKind::Error
            | TypeKind::Scalar(_)
            | TypeKind::Atomic(_)
            | TypeKind::Matrix(_)
            | TypeKind::Array(_)
            | TypeKind::Texture(_)
            | TypeKind::Sampler(_)
            | TypeKind::Reference(_)
            | TypeKind::Pointer(_)
            | TypeKind::BoundVariable(_)
            | TypeKind::StorageTypeOfTexelFormat(_) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::NoSuchField {
                        expression: field_expression,
                        name: name.clone(),
                        r#type: expression_type,
                    },
                );
                self.types.error
            },
        };

        match ref_info {
            Some((address_space, access_mode)) => self.make_ref(address_space, r#type, access_mode),
            None => r#type,
        }
    }

    fn infer_function_call(
        &mut self,
        function: &FunctionDetails,
        arguments: &[(ExpressionId, Type)],
        store: &ExpressionStore,
        callee: ExpressionId,
        expression: ExpressionId,
    ) -> Option<Type> {
        if function.parameters.len() == arguments.len() {
            for (expected, (actual_expression, actual_type)) in
                function.parameters().zip(arguments.iter().copied())
            {
                if !actual_type.is_convertible_to(expected, self.database) {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::TypeMismatch {
                            expression: actual_expression,
                            actual: actual_type,
                            expected: TypeExpectation::Type(TypeExpectationInner::Exact(expected)),
                        },
                    );
                }
            }

            function.return_type
        } else {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::FunctionCallArgCountMismatch {
                    expression: callee,
                    n_expected: function.parameters.len(),
                    n_actual: arguments.len(),
                },
            );
            Some(self.types.error)
        }
    }

    fn infer_unary_op(
        &mut self,
        expression: ExpressionId,
        operator: UnaryOperator,
        store: &ExpressionStore,
    ) -> Type {
        let expression_type = self.infer_expression(expression, store);
        if expression_type.is_err(self.database) {
            debug_assert!(!self.result.diagnostics().is_empty());
            return self.types.error;
        }
        match wgsl_types::builtin::type_unary_op(
            to_wgsl_unary_operator(operator),
            &self.converter.to_wgsl_types(expression_type),
        ) {
            Ok(r#type) => self.converter.from_wgsl_types(r#type),
            Err(error) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::WgslError {
                        expression,
                        message: error.to_string(),
                    },
                );
                self.types.error
            },
        }
    }

    fn infer_binary_op(
        &mut self,
        expression: ExpressionId,
        left_side: ExpressionId,
        right_side: ExpressionId,
        operation: BinaryOperation,
        store: &ExpressionStore,
    ) -> Type {
        let left_type = self
            .infer_expression(left_side, store)
            .loaded(self.database);
        let right_type = self
            .infer_expression(right_side, store)
            .loaded(self.database);

        if left_type.is_err(self.database) || right_type.is_err(self.database) {
            debug_assert!(!self.result.diagnostics().is_empty());
            return self.types.error;
        }
        match wgsl_types::builtin::type_binary_op(
            to_wgsl_binary_operator(operation),
            &self.converter.to_wgsl_types(left_type),
            &self.converter.to_wgsl_types(right_type),
        ) {
            Ok(r#type) => self.converter.from_wgsl_types(r#type),
            Err(error) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::WgslError {
                        expression,
                        message: error.to_string(),
                    },
                );
                self.types.error
            },
        }
    }

    fn infer_ident_expression(
        &mut self,
        expression: ExpressionId,
        ident_expression: &IdentExpression,
        store: &ExpressionStore,
    ) -> Type {
        let resolver = self.resolver_for_expression(expression);
        let mut context = TypeLoweringContext::new(
            self.database,
            resolver.as_ref().unwrap_or(&self.resolver),
            store,
        );
        let lowered = context.lower(
            expression,
            &ident_expression.path,
            &ident_expression.template_parameters,
        );
        self.push_lowering_diagnostics(context.diagnostics, store);

        match lowered {
            Lowered::GlobalConstant(id) => {
                InferenceResult::of(self.database, DefinitionWithBodyId::GlobalConstant(id))
                    .return_type
                    .unwrap_or_else(|| {
                        debug_assert!(!self.result.diagnostics().is_empty());
                        self.types.error
                    })
            },
            Lowered::GlobalVariable(id) => {
                InferenceResult::of(self.database, DefinitionWithBodyId::GlobalVariable(id))
                    .return_type
                    .unwrap_or_else(|| {
                        debug_assert!(!self.result.diagnostics().is_empty());
                        self.types.error
                    })
            },
            Lowered::Override(id) => {
                InferenceResult::of(self.database, DefinitionWithBodyId::Override(id))
                    .return_type
                    .unwrap_or_else(|| {
                        debug_assert!(!self.result.diagnostics().is_empty());
                        self.types.error
                    })
            },
            Lowered::Local(id) => self.result.type_of_binding[id],
            Lowered::Type(_)
            | Lowered::TypeWithoutTemplate(_)
            | Lowered::Function(_)
            | Lowered::BuiltinFunction
            | Lowered::Enumerant(_) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::UnexpectedLoweredKind {
                        expression,
                        expected: LoweredKind::Variable,
                        actual: lowered.kind(),
                        path: ident_expression.path.clone(),
                    },
                );
                self.types.error
            },
        }
    }

    fn builtin_vector_inferred_constructor(
        &self,
        size: VecDimensionality,
    ) -> BuiltinId {
        match size {
            VecDimensionality::Two => Builtin::builtin_op_vec2_constructor(self.database),
            VecDimensionality::Three => Builtin::builtin_op_vec3_constructor(self.database),
            VecDimensionality::Four => Builtin::builtin_op_vec4_constructor(self.database),
        }
        .intern(self.database)
    }

    // TODO: should we use the more specific overloads such as `builtin_op_mat2x2_constructor_T`?
    fn builtin_matrix_inferred_constructor(
        &self,
        columns: VecDimensionality,
        rows: VecDimensionality,
    ) -> BuiltinId {
        use type_ref::VecDimensionality::{Four, Three, Two};
        match (columns, rows) {
            (Two, Two) => Builtin::builtin_op_mat2x2_constructor(self.database),
            (Two, Three) => Builtin::builtin_op_mat2x3_constructor(self.database),
            (Two, Four) => Builtin::builtin_op_mat2x4_constructor(self.database),
            (Three, Two) => Builtin::builtin_op_mat3x2_constructor(self.database),
            (Three, Three) => Builtin::builtin_op_mat3x3_constructor(self.database),
            (Three, Four) => Builtin::builtin_op_mat3x4_constructor(self.database),
            (Four, Two) => Builtin::builtin_op_mat4x2_constructor(self.database),
            (Four, Three) => Builtin::builtin_op_mat4x3_constructor(self.database),
            (Four, Four) => Builtin::builtin_op_mat4x4_constructor(self.database),
        }
        .intern(self.database)
    }

    fn type_from_vec_size(
        &self,
        inner: Type,
        vec_size: u8,
    ) -> Type {
        if vec_size == 1 {
            inner
        } else {
            let kind = vec_size.try_into().map_or(TypeKind::Error, |size| {
                TypeKind::Vector(VectorType {
                    size,
                    component_type: inner,
                })
            });
            kind.intern(self.database)
        }
    }

    fn infer_vec_swizzle_expression(
        &mut self,
        store: &ExpressionStore,
        field_expression: ExpressionId,
        name: &Name,
        expression_type: Type,
        vector_type: &VectorType,
        is_ref: Option<(AddressSpace, AccessMode)>,
    ) -> Type {
        const SWIZZLES: [[char; 4]; 2] = [['x', 'y', 'z', 'w'], ['r', 'g', 'b', 'a']];
        let max_size = 4;
        let max_swizzle_index = vector_type.size.as_u8();

        if name.as_str().len() > max_size {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::NoSuchField {
                    expression: field_expression,
                    name: name.clone(),
                    r#type: expression_type,
                },
            );
            return self.types.error;
        }

        for swizzle in &SWIZZLES {
            let allowed_chars = &swizzle[..(usize::from(max_swizzle_index))];
            if name
                .as_str()
                .chars()
                .all(|character| allowed_chars.contains(&character))
            {
                let r#type = self.type_from_vec_size(
                    vector_type.component_type,
                    u8::try_from(name.as_str().len()).unwrap(),
                );
                if let Some((address_space, access_mode)) = is_ref
                // proposal to remove this length check: https://github.com/gpuweb/gpuweb/pull/5268
                    && name.as_str().len() == 1
                {
                    return self.make_ref(address_space, r#type, access_mode);
                }
                return r#type;
            }
        }
        self.push_diagnostic(
            store.store_source,
            InferenceDiagnosticKind::NoSuchField {
                expression: field_expression,
                name: name.clone(),
                r#type: expression_type,
            },
        );
        self.types.error
    }

    fn infer_struct_field_expression(
        &mut self,
        expression: ExpressionId,
        store: &ExpressionStore,
        field_expression: ExpressionId,
        name: &Name,
        expression_type: Type,
        r#struct: StructId,
    ) -> Type {
        let struct_data = self.database.struct_data(r#struct).0;
        let field_types = &self.database.field_types(r#struct).0;
        if let Some(field) = struct_data.field(name) {
            self.set_field_resolution(expression, FieldId { r#struct, field });
            field_types[field]
        } else {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::NoSuchField {
                    expression: field_expression,
                    name: name.clone(),
                    r#type: expression_type,
                },
            );
            self.types.error
        }
    }

    fn infer_builtin_struct_field_expression(
        &mut self,
        store: &ExpressionStore,
        field_expression: ExpressionId,
        name: &Name,
        expression_type: Type,
        builtin_struct: BuiltinStruct,
    ) -> Type {
        if let Some((_, field_type)) = builtin_struct
            .fields
            .into_iter()
            .find(|(field_name, _)| field_name.as_str() == name.as_str())
        {
            field_type
        } else {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::NoSuchField {
                    expression: field_expression,
                    name: name.clone(),
                    r#type: expression_type,
                },
            );
            self.types.error
        }
    }

    fn infer_builtin(
        &mut self,
        store: &ExpressionStore,
        expression: ExpressionId,
        builtin_id: BuiltinId,
        arguments: &[(ExpressionId, Type)],
        name: Option<&'static str>,
    ) -> Option<Type> {
        if let Ok((return_type, overload_id)) = self.try_infer_builtin(builtin_id, arguments) {
            let builtin = builtin_id.lookup(self.database);
            let resolved = builtin.overload(overload_id).r#type;
            self.result
                .call_resolutions
                .insert(expression, ResolvedCall::Function(resolved));
            return_type
        } else {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::NoBuiltinOverload {
                    expression,
                    builtin: builtin_id,
                    name,
                    parameters: arguments
                        .iter()
                        .copied()
                        .map(|(_, r#type)| r#type)
                        .collect(),
                },
            );
            Some(self.types.error)
        }
    }

    fn try_infer_builtin(
        &self,
        builtin_id: BuiltinId,
        arguments: &[(ExpressionId, Type)],
    ) -> Result<(Option<Type>, BuiltinOverloadId), ()> {
        let builtin = builtin_id.lookup(self.database);
        for (overload_id, overload) in builtin.overloads() {
            // Hack: overload resolution algorithm is not implemented here or used
            // here because it is the same as just picking the first valid overload.
            if let Ok((r#type, _conversion_rank)) = self.infer_builtin_overload(overload, arguments)
            {
                return Ok((r#type, overload_id));
            }
        }
        Err(())
    }

    fn infer_builtin_overload(
        &self,
        signature: &BuiltinOverload,
        arguments: &[(ExpressionId, Type)],
    ) -> Result<(Option<Type>, u32), ()> {
        let function_type = signature.r#type.lookup(self.database);
        if function_type.parameters.len() != arguments.len() {
            return Err(());
        }
        let conversion_rank = 0;
        let mut unification_table = UnificationTable::default();
        for (expected, &found) in function_type.parameters().zip(arguments.iter()) {
            unify(self.database, &mut unification_table, expected, found.1)?;
        }
        let return_type = function_type
            .return_type
            .map(|r#type| unification_table.resolve(self.database, r#type));
        Ok((return_type, conversion_rank))
    }

    fn infer_call(
        &mut self,
        expression: ExpressionId,
        callee: &IdentExpression,
        arguments: Vec<(ExpressionId, Type)>,
        store: &ExpressionStore,
    ) -> Option<Type> {
        let resolver = self
            .resolver_for_expression(expression)
            .unwrap_or_else(|| self.resolver.clone());
        let mut context = TypeLoweringContext::new(self.database, &resolver, store);
        let lowered = context.lower(expression, &callee.path, &callee.template_parameters);
        let inferred = match lowered {
            Lowered::Type(r#type) => {
                Some(self.infer_templated_type_constructor(store, expression, r#type, arguments))
            },
            Lowered::TypeWithoutTemplate(r#type) => Some(
                self.infer_type_without_template_constructor(store, expression, r#type, arguments),
            ),
            Lowered::Function(id) => {
                let details = id.lookup(self.database);
                self.result
                    .call_resolutions
                    .insert(expression, ResolvedCall::Function(id));
                self.infer_function_call(&details, &arguments, store, expression, expression)
            },
            Lowered::BuiltinFunction => {
                let template_args = context.eval_template_args(
                    TypeContainer::Expression(expression),
                    &callee.template_parameters,
                );
                self.infer_builtin_function(store, expression, callee, template_args, &arguments)
            },
            Lowered::Enumerant(_)
            | Lowered::GlobalConstant(_)
            | Lowered::GlobalVariable(_)
            | Lowered::Override(_)
            | Lowered::Local(_) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::UnexpectedLoweredKind {
                        expression,
                        expected: LoweredKind::Function,
                        actual: lowered.kind(),
                        path: callee.path.clone(),
                    },
                );
                Some(self.types.error)
            },
        };
        self.push_lowering_diagnostics(context.diagnostics, store);
        inferred
    }

    fn infer_builtin_function(
        &mut self,
        store: &ExpressionStore,
        expression: ExpressionId,
        callee: &IdentExpression,
        mut template_parameters: TemplateParameters,
        arguments: &[(ExpressionId, Type)],
    ) -> Option<Type> {
        let Some(name) = callee.path.mod_path().as_ident() else {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::WgslError {
                    expression,
                    message: format!("invalid builtin {}", callee.path.mod_path()),
                },
            );
            return Some(self.types.error);
        };

        let Some((template_args, converted_arguments)) = self
            .converter
            .to_wgsl_template_parameters(store, expression, template_parameters, arguments)
        else {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::WgslError {
                    expression,
                    message:
                        "internal error: wgsl-types did not align with wgsl-analyzer's type system"
                            .to_owned(),
                },
            );
            return Some(self.types.error);
        };

        if converted_arguments
            .iter()
            .any(|argument| matches!(argument, wgsl_types::Type::Unknown))
        {
            // One of the arguments had an error type
            debug_assert!(
                !self.result.diagnostics().is_empty(),
                "there should already be a diagnostic"
            );
            return Some(self.types.error);
        }

        let return_type = wgsl_types::builtin::type_builtin_fn(
            name.as_str(),
            non_empty(template_args.as_slice()),
            &converted_arguments,
        );

        match return_type {
            Ok(Some(r#type)) => Some(self.converter.from_wgsl_types(r#type)),
            Ok(None) => None,
            Err(error) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::WgslError {
                        expression,
                        message: error.to_string(),
                    },
                );
                Some(self.types.error)
            },
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "large bug not complex match expression"
    )]
    /// Constructor for a type with a fully specified template.
    fn infer_templated_type_constructor(
        &mut self,
        store: &ExpressionStore,
        expression: ExpressionId,
        r#type: Type,
        arguments: Vec<(ExpressionId, Type)>,
    ) -> Type {
        // wgsl_types::builtin::type_ctor(name, tplt, args)
        fn size_to_dimension(size: VecSize) -> VecDimensionality {
            match size {
                VecSize::Two => VecDimensionality::Two,
                VecSize::Three => VecDimensionality::Three,
                VecSize::Four => VecDimensionality::Four,
                #[expect(
                    clippy::unreachable,
                    reason = "this is by far the easiest way to handle it, at least for now"
                )]
                VecSize::BoundVariable(_) => {
                    unreachable!("Can never have unbound type at this point")
                },
            }
        }

        // https://www.w3.org/TR/WGSL/#zero-value-builtin-function
        if (arguments.is_empty() && !r#type.is_constructible(self.database)) {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::NotConstructible { expression, r#type },
            );
        }
        match r#type.kind(self.database) {
            TypeKind::Scalar(scalar_type) => {
                self.call_scalar_constructor(store, scalar_type, expression, r#type, arguments)
            },
            TypeKind::Array(array_type) => {
                if arguments.is_empty() {
                    return r#type;
                }
                for (argument_expression, argument_type) in &arguments {
                    if !argument_type.is_convertible_to(array_type.inner, self.database) {
                        self.push_diagnostic(
                            store.store_source,
                            InferenceDiagnosticKind::TypeMismatch {
                                expression: *argument_expression,
                                expected: TypeExpectation::Type(TypeExpectationInner::Exact(
                                    array_type.inner,
                                )),
                                actual: *argument_type,
                            },
                        );
                    }
                }
                #[expect(
                    clippy::as_conversions,
                    reason = "constructing an array with too many parameters is an error anyway"
                )]
                if let ArraySize::Constant(size) = array_type.size
                    && arguments.len() != size as usize
                {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::FunctionCallArgCountMismatch {
                            expression,
                            n_expected: size as usize,
                            n_actual: arguments.len(),
                        },
                    );
                }
                r#type
            },
            TypeKind::Vector(vec) => {
                if arguments.is_empty() {
                    return r#type;
                }
                let construction_builtin_id =
                    self.builtin_vector_inferred_constructor(size_to_dimension(vec.size));
                let construction_result =
                    self.try_infer_builtin(construction_builtin_id, &arguments);

                if construction_result.is_ok() {
                    r#type
                } else {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::NoConstructor {
                            expression,
                            builtins: construction_builtin_id,
                            r#type,
                            parameters: arguments.into_iter().map(|(_, r#type)| r#type).collect(),
                        },
                    );
                    self.types.error
                }
            },
            TypeKind::Matrix(matrix) => {
                // https://www.w3.org/TR/WGSL/#zero-value-builtin-function
                if arguments.is_empty() {
                    return r#type;
                }
                let construction_builtin_id = self.builtin_matrix_inferred_constructor(
                    size_to_dimension(matrix.columns),
                    size_to_dimension(matrix.rows),
                );
                let construction_result =
                    self.try_infer_builtin(construction_builtin_id, &arguments);
                if construction_result.is_ok() {
                    r#type
                } else {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::NoConstructor {
                            expression,
                            builtins: construction_builtin_id,
                            r#type,
                            parameters: arguments.into_iter().map(|(_, r#type)| r#type).collect(),
                        },
                    );
                    self.types.error
                }
            },
            TypeKind::Struct(struct_id) => {
                self.validate_struct_constructor(store, struct_id, expression, r#type, &arguments)
            },

            // Never constructible
            TypeKind::Texture(_)
            | TypeKind::Sampler(_)
            | TypeKind::Pointer(_)
            | TypeKind::Atomic(_)
            | TypeKind::BuiltinStruct(_)
            | TypeKind::StorageTypeOfTexelFormat(_)
            | TypeKind::BoundVariable(_)
            | TypeKind::Reference(_) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::NotConstructible { expression, r#type },
                );
                self.types.error
            },
            TypeKind::Error => r#type,
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "large bug not complex match expression"
    )]
    /// Constructor for just a type name.
    fn infer_type_without_template_constructor(
        &mut self,
        store: &ExpressionStore,
        expression: ExpressionId,
        r#type: Type,
        arguments: Vec<(ExpressionId, Type)>,
    ) -> Type {
        fn size_to_dimension(size: VecSize) -> VecDimensionality {
            #[expect(
                clippy::unreachable,
                reason = "this is by far the easiest way to handle it, at least for now"
            )]
            match size {
                VecSize::Two => VecDimensionality::Two,
                VecSize::Three => VecDimensionality::Three,
                VecSize::Four => VecDimensionality::Four,
                VecSize::BoundVariable(_) => {
                    unreachable!("Can never have unbound type at this point")
                },
            }
        }

        // https://www.w3.org/TR/WGSL/#zero-value-builtin-function
        if (arguments.is_empty() && !r#type.is_constructible(self.database)) {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::NotConstructible { expression, r#type },
            );
        }

        match r#type.kind(self.database) {
            TypeKind::Scalar(scalar_type) => {
                self.call_scalar_constructor(store, scalar_type, expression, r#type, arguments)
            },
            TypeKind::Array(array_type) => {
                if arguments.is_empty() {
                    return r#type;
                }
                let Some((_, mut first_argument_type)) = arguments.first().copied() else {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::FunctionCallArgCountMismatch {
                            expression,
                            n_expected: 1,
                            n_actual: arguments.len(),
                        },
                    );
                    return self.types.error;
                };

                // all of the following arguments must be the same type as the first argument
                for (argument_expression, argument_type) in &arguments[1..] {
                    if argument_type.is_convertible_to(first_argument_type, self.database) {
                        // Everything is as intended
                    } else if first_argument_type.is_convertible_to(*argument_type, self.database) {
                        // Narrowing the expected type
                        first_argument_type = *argument_type;
                    } else {
                        self.push_diagnostic(
                            store.store_source,
                            InferenceDiagnosticKind::TypeMismatch {
                                expression: *argument_expression,
                                expected: TypeExpectation::Type(TypeExpectationInner::Exact(
                                    first_argument_type,
                                )),
                                actual: *argument_type,
                            },
                        );
                    }
                }
                if let Ok(validated_length) = u32::try_from(arguments.len()) {
                    TypeKind::Array(ArrayType {
                        inner: first_argument_type,
                        binding_array: array_type.binding_array,
                        size: ArraySize::Constant(validated_length),
                    })
                    .intern(self.database)
                } else {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::FunctionCallArgCountMismatch {
                            expression,
                            #[expect(clippy::as_conversions, reason = "usize always holds a u32")]
                            n_expected: ArraySize::MAX as usize,
                            n_actual: arguments.len(),
                        },
                    );
                    TypeKind::Array(ArrayType {
                        inner: first_argument_type,
                        binding_array: array_type.binding_array,
                        size: ArraySize::Constant(ArraySize::MAX),
                    })
                    .intern(self.database)
                }
            },
            TypeKind::Vector(vec) => {
                // See note in WGSL reference:
                // Note: Zero-filled vectors of AbstractInt can be written as vec2(), vec3(), and vec4().
                // https://www.w3.org/TR/WGSL/#zero-value-builtin-function
                if arguments.is_empty() {
                    return TypeKind::Vector(VectorType {
                        size: vec.size,
                        component_type: TypeKind::Scalar(ScalarType::AbstractInt)
                            .intern(self.database),
                    })
                    .intern(self.database);
                }
                let construction_builtin_id =
                    self.builtin_vector_inferred_constructor(size_to_dimension(vec.size));
                let construction_result =
                    self.try_infer_builtin(construction_builtin_id, &arguments);

                if let Ok((r#type, _)) = construction_result {
                    r#type.expect("constructors always have a return type")
                } else {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::NoConstructor {
                            expression,
                            builtins: construction_builtin_id,
                            r#type,
                            parameters: arguments.into_iter().map(|(_, r#type)| r#type).collect(),
                        },
                    );
                    self.types.error
                }
            },
            TypeKind::Matrix(matrix) => {
                // https://www.w3.org/TR/WGSL/#zero-value-builtin-function
                if arguments.is_empty() {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::FunctionCallArgCountMismatch {
                            expression,
                            n_expected: 1,
                            n_actual: arguments.len(),
                        },
                    );
                    return self.types.error;
                }
                let construction_builtin_id = self.builtin_matrix_inferred_constructor(
                    size_to_dimension(matrix.columns),
                    size_to_dimension(matrix.rows),
                );
                let construction_result =
                    self.try_infer_builtin(construction_builtin_id, &arguments);
                if let Ok((r#type, _)) = construction_result {
                    r#type.expect("constructors always have a return type")
                } else {
                    self.push_diagnostic(
                        store.store_source,
                        InferenceDiagnosticKind::NoConstructor {
                            expression,
                            builtins: construction_builtin_id,
                            r#type,
                            parameters: arguments.into_iter().map(|(_, r#type)| r#type).collect(),
                        },
                    );
                    self.types.error
                }
            },
            TypeKind::Struct(struct_id) => {
                self.validate_struct_constructor(store, struct_id, expression, r#type, &arguments)
            },
            // Never constructible
            TypeKind::Texture(_)
            | TypeKind::Sampler(_)
            | TypeKind::Pointer(_)
            | TypeKind::Atomic(_)
            | TypeKind::BuiltinStruct(_)
            | TypeKind::StorageTypeOfTexelFormat(_)
            | TypeKind::BoundVariable(_)
            | TypeKind::Reference(_) => {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::NotConstructible { expression, r#type },
                );
                self.types.error
            },
            TypeKind::Error => r#type,
        }
    }

    fn call_scalar_constructor(
        &mut self,
        store: &ExpressionStore,
        scalar_type: ScalarType,
        expression: ExpressionId,
        r#type: Type,
        arguments: Vec<(ExpressionId, Type)>,
    ) -> Type {
        // https://www.w3.org/TR/WGSL/#zero-value-builtin-function
        if arguments.is_empty() {
            return r#type;
        }
        let construction_builtin_id = match scalar_type {
            ScalarType::Bool => {
                Builtin::builtin_op_bool_constructor(self.database).intern(self.database)
            },
            ScalarType::I32 => {
                Builtin::builtin_op_i32_constructor(self.database).intern(self.database)
            },
            ScalarType::U32 => {
                Builtin::builtin_op_u32_constructor(self.database).intern(self.database)
            },
            ScalarType::F32 => {
                Builtin::builtin_op_f32_constructor(self.database).intern(self.database)
            },
            ScalarType::F16 => {
                Builtin::builtin_op_f16_constructor(self.database).intern(self.database)
            },
            ScalarType::AbstractInt | ScalarType::AbstractFloat => {
                // Panic is correct here, since it should be impossible to enter this branch
                #[expect(
                    clippy::unreachable,
                    reason = "TODO: Refactor to make this not representable"
                )]
                {
                    unreachable!("cannot construct abstract types")
                }
            },
            ScalarType::I64 => {
                Builtin::builtin_op_i64_constructor(self.database).intern(self.database)
            },
            ScalarType::U64 => {
                Builtin::builtin_op_u64_constructor(self.database).intern(self.database)
            },
        };

        let construction_result = self.try_infer_builtin(construction_builtin_id, &arguments);
        if let Ok((r#type, _)) = construction_result {
            r#type.expect("constructors always have a return type")
        } else {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::NoConstructor {
                    expression,
                    builtins: construction_builtin_id,
                    r#type,
                    parameters: arguments.into_iter().map(|(_, r#type)| r#type).collect(),
                },
            );
            self.types.error
        }
    }

    fn validate_struct_constructor(
        &mut self,
        store: &ExpressionStore,
        struct_id: StructId,
        expression: ExpressionId,
        r#type: Type,
        arguments: &[(ExpressionId, Type)],
    ) -> Type {
        // https://www.w3.org/TR/WGSL/#zero-value-builtin-function
        if arguments.is_empty() {
            return r#type;
        }
        let signature = self.database.struct_data(struct_id).0;
        if arguments.len() != signature.fields.len() {
            self.push_diagnostic(
                store.store_source,
                InferenceDiagnosticKind::FunctionCallArgCountMismatch {
                    expression,
                    n_expected: signature.fields.len(),
                    n_actual: arguments.len(),
                },
            );
            return self.types.error;
        }
        let field_types = &self.database.field_types(struct_id).0;
        if let Err(error) = self.typecheck_arguments(store, arguments, field_types) {
            error
        } else {
            r#type
        }
    }

    fn typecheck_arguments(
        &mut self,
        store: &ExpressionStore,
        arguments: &[(la_arena::Idx<Expression>, Type)],
        field_types: &ArenaMap<la_arena::Idx<hir_def::signature::FieldData>, Type>,
    ) -> Result<(), Type> {
        let mut error = None;
        for ((_, field_type), (argument_expression, argument_type)) in
            field_types.iter().zip(arguments.iter())
        {
            if !argument_type.is_convertible_to(*field_type, self.database) {
                self.push_diagnostic(
                    store.store_source,
                    InferenceDiagnosticKind::TypeMismatch {
                        expression: *argument_expression,
                        expected: (*field_type).into(),
                        actual: *argument_type,
                    },
                );
                error = Some(self.types.error);
            }
        }
        match error {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    fn lower_type(
        &mut self,
        type_ref: TypeSpecifierId,
        resolver: &Resolver,
        store: &ExpressionStore,
    ) -> Type {
        let mut context = TypeLoweringContext::new(self.database, resolver, store);
        let r#type = context.lower_type(type_ref);
        self.push_lowering_diagnostics(context.diagnostics, store);
        r#type
    }
}

#[inline]
const fn non_empty<T>(slice: &[T]) -> Option<&[T]> {
    if slice.is_empty() { None } else { Some(slice) }
}

#[derive(PartialEq, Eq, Copy, Clone)]
enum AbstractHandling {
    Concretize,
    Abstract,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TypeExpectationInner {
    Exact(Type),
    IntegerScalar,
    IntegerIndex,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TypeExpectation {
    Type(TypeExpectationInner),
    Any,
    None,
}

impl From<Type> for TypeExpectation {
    fn from(value: Type) -> Self {
        Self::Type(TypeExpectationInner::Exact(value))
    }
}

impl From<Option<Type>> for TypeExpectation {
    fn from(value: Option<Type>) -> Self {
        match value {
            Some(r#type) => Self::Type(TypeExpectationInner::Exact(r#type)),
            None => Self::None,
        }
    }
}

impl From<Option<Option<Type>>> for TypeExpectation {
    fn from(value: Option<Option<Type>>) -> Self {
        match value {
            Some(inner) => inner.into(),
            None => Self::Any,
        }
    }
}

impl InferenceContext<'_> {
    fn make_ref(
        &self,
        address_space: AddressSpace,
        r#type: Type,
        access_mode: AccessMode,
    ) -> Type {
        debug_assert!(!matches!(
            r#type.kind(self.database),
            TypeKind::Reference(_) | TypeKind::Pointer(_)
        ));
        TypeKind::Reference(Reference {
            address_space,
            inner: r#type,
            access_mode,
        })
        .intern(self.database)
    }

    fn ref_to_pointer(
        &self,
        reference: &Reference,
    ) -> Type {
        TypeKind::Pointer(Pointer {
            address_space: reference.address_space,
            inner: reference.inner,
            access_mode: reference.access_mode,
        })
        .intern(self.database)
    }

    fn ptr_to_ref(
        &self,
        pointer: &Pointer,
    ) -> Type {
        TypeKind::Reference(Reference {
            address_space: pointer.address_space,
            inner: pointer.inner,
            access_mode: pointer.access_mode,
        })
        .intern(self.database)
    }
}
