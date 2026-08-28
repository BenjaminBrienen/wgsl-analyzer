use crate::{
    db::{DefinitionWithBodyId, ModuleDefinitionId},
    expression_store::pretty::{print_function, print_struct},
    item_scope::ItemScope,
    name_resolution::ModulesMap,
    signature::{FunctionSignature, StructSignature},
    test_db::TestDatabase,
};
use expect_test::{Expect, expect};
use syntax::Edition;
use test_fixture::WithFixture as _;
use crate::name_resolution::package_modules_map;

#[expect(clippy::needless_pass_by_value, reason = "matches expect! macro")]
fn lower_and_print(
    wa_fixture: &str,
    expect: Expect,
) {
    let db = TestDatabase::with_files(wa_fixture);
    let package = db.fetch_test_package();
    let map = package_modules_map(&db, package);
    let mut definitions = map
        .inner
        .values()
        .map(|module| module.scope.declarations.clone());
    let mut out = String::new();
    for definition in definitions.flatten() {
        match definition {
            ModuleDefinitionId::Struct(struct_id) => {
                out += &print_struct(
                    &db,
                    struct_id,
                    StructSignature::of(&db, struct_id),
                    Edition::CURRENT,
                );
            },
            ModuleDefinitionId::Function(function_id) => {
                out += &print_function(
                    &db,
                    function_id,
                    FunctionSignature::of(&db, function_id),
                    Edition::CURRENT,
                );
            },
            ModuleDefinitionId::GlobalAssertStatement(_id) => (),
            ModuleDefinitionId::GlobalConstant(_id) => (),
            ModuleDefinitionId::GlobalVariable(_id) => (),
            ModuleDefinitionId::Override(_id) => (),
            ModuleDefinitionId::TypeAlias(_id) => (),
        }
    }

    expect.assert_eq(&out);
}

#[test]
fn structs() {
    lower_and_print(
        r"
@if(true)
struct Foo { field: u32 }
@elif(false)
struct Foo { field: i32 }
",
        expect![""],
    );
}
