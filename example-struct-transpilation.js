/*
trait A

struct Test implements A {
  name = 'Test'
  p: B
  component C
}
*/

const lore_schema_Test = Lore.types.schema.struct(
  "Test",           // name
  [lore_type_A],    // supertraits
  Lore.types.any,   // ownedBy
  true,             // isEntity
);

function lore_newtype_Test(componentTypes, isArchetype) {
  return Lore.types.struct(lore_schema_Test, componentTypes, isArchetype);
}

const lore_type_Test = lore_newtype_Test([Lore.types.component(lore_types_C)], true);

function lore_instantiate_Test(members) {
  const tpe = lore_newtype_Test([members.C.lore$type], false);
  return Lore.values.object.create(members, tpe);
}

function lore_type_Test__default_name() {
  return 'Test';
}
