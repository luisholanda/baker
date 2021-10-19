use std::collections::HashMap;

use baker_ir_pb::{type_def::Definition, FunctionCall, IrFile, Namespace, TypeDef};

// A type that can merge [`IrFile`] definitions of multiple layers
// into a single definition.
#[derive(Debug, Default)]
pub struct IrMerger {
    // All IR files defined till now.
    files: HashMap<u32, IrFile>,
    // Map from type name to IR file definition position.
    type_def_maps: HashMap<String, (u32, usize)>,
}

impl IrMerger {
    /// Add a [`IrFile`] definition given by a layer, merging with previous ones
    /// if present.
    pub fn add_ir_file_def(&mut self, ir_file: IrFile) {
        if self.files.contains_key(&ir_file.file_id) {
            self.merge(ir_file);
        } else {
            self.files.insert(ir_file.file_id, ir_file);
        }
    }

    fn merge(&mut self, ir_file: IrFile) {
        let prev_def = self
            .files
            .get_mut(&ir_file.file_id)
            .expect("called merge without a previous definition");
        let prev_root = prev_def
            .root
            .as_mut()
            .expect("no root namespace in previous definition");

        if let Some(root) = ir_file.root {
            merge_namespaces(prev_root, root, ir_file.file_id, &mut self.type_def_maps);
        }
    }

    pub fn into_files(self) -> Vec<IrFile> {
        self.files.into_values().collect()
    }
}

fn merge_namespaces(
    prev: &mut Namespace,
    new: Namespace,
    file_id: u32,
    type_def_map: &mut HashMap<String, (u32, usize)>,
) {
    prev.aliases.extend(new.aliases);
    prev.interfaces.extend(new.interfaces);
    prev.functions.extend(new.functions);

    for type_def in new.types {
        let name = &type_def.header.as_ref().unwrap().name;
        if let Some(&(def_file_id, def_idx)) = type_def_map.get(name) {
            // TODO: return an error here instead of panicking.
            assert_eq!(file_id, def_file_id, "definitions in different files");

            merge_type_def(&mut prev.types[def_idx], type_def);
        } else {
            type_def_map.insert(name.to_string(), (file_id, prev.types.len()));
            prev.types.push(type_def);
        }
    }
}

fn merge_type_def(prev: &mut TypeDef, new: TypeDef) {
    prev.set_visibility(new.visibility().max(prev.visibility()));

    prev.blocks.extend(new.blocks);
    merge_attributes(&mut prev.attributes, new.attributes);

    match prev.definition.as_mut().zip(new.definition) {
        Some((Definition::Record(prev), Definition::Record(new))) => {
            for (name, prop) in new.properties {
                if let Some(prev_prop) = prev.properties.get_mut(&name) {
                    prev_prop.set_visibility(prop.visibility().max(prev_prop.visibility()));
                    merge_attributes(&mut prev_prop.attributes, prop.attributes);
                } else {
                    prev.properties.insert(name, prop);
                }
            }
        }
        Some((Definition::Sum(_), Definition::Sum(_))) => todo!(),
        Some((prev, new)) => panic!("conflicting definitions: {:?} vs {:?}", prev, new),
        None => {}
    }
}

fn merge_attributes(prev: &mut Vec<FunctionCall>, new: Vec<FunctionCall>) {
    // TODO: think better on how to merge attributes.
    prev.extend(new);
}
