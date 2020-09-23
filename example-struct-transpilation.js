/*
trait A

struct Test implements A {
  p: B
  component C
}
*/

// TODO: Think twice about the name...
function lore_newtype_Test(componentTypes) {
  return Lore.types.struct(
    "Test",             // name
    [lore_type_A],      // declared supertypes
    componentTypes,
    Lore.types.any,     // ownedBy
    true                // isEntity
  );
}

// The archetypal type instance with compile-time component types.
const lore_type_Test = lore_type_Test$create([Lore.types.component(lore_types_C)]);

function lore_instantiate_Test(p, C) {
  // TODO: We really ought to standardize all the internal naming...
  const tpe = lore_type_Test$create([C.lore$type]);
  return Lore.values.object.create(
    { p: p, C: C },
    tpe
  );
}




const lore_schema_Test = Lore.types.schema.struct(
  "Test",           // name
  [lore_type_A],    // declared supertypes
  Lore.types.any,   // ownedBy
  true,             // isEntity
);

function lore_newtype_Test(componentTypes) {
  return Lore.types.struct(lore_schema_Test, componentTypes);
}

const lore_type_Test = lore_newtype_Test([Lore.types.component(lore_types_C)]);

function lore_instantiate_Test(p, C) {
  // Note that we can use the archetype if the struct has no components...
  const tpe = lore_newtype_Test([C.lore$type]);
  return Lore.values.object.create(
    { p: p, C: C },
    tpe
  );
}
