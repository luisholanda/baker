use std::collections::HashMap;

use baker_ir_pb::{type_def::Definition, Attribute, IrFile, Namespace, TypeDef};

// A type that can merge [`IrFile`] definitions of multiple layers
// into a single definition.
#[derive(Debug, Default)]
pub struct IrMerger {
    // All IR files defined till now.
    files: HashMap<u32, IrFile>,
    // Map from type name to IR file definition position.
    type_def_maps: HashMap<String, (u32, Vec<String>, usize)>,
}

impl IrMerger {
    /// Add a [`IrFile`] definition given by a layer, merging with previous ones
    /// if present.
    pub fn add_ir_file_def(&mut self, ir_file: IrFile) {
        if self.files.contains_key(&ir_file.file_id) {
            self.merge(ir_file);
        } else {
            self.insert_file(ir_file);
        }

        dbg!(&self.type_def_maps);
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
            merge_namespaces(
                prev_root,
                root,
                ir_file.file_id,
                &mut self.type_def_maps,
                &[],
            );
        }
    }

    fn insert_file(&mut self, ir_file: IrFile) {
        if let Some(root) = &ir_file.root {
            visit_namespaces(root, ir_file.file_id, &mut self.type_def_maps, &[]);
        }
        self.files.insert(ir_file.file_id, ir_file);
    }

    pub fn into_files(self) -> Vec<IrFile> {
        self.files.into_values().collect()
    }
}

fn visit_namespaces(
    ns: &Namespace,
    file_id: u32,
    type_def_map: &mut HashMap<String, (u32, Vec<String>, usize)>,
    curr_ns_path: &[String],
) {
    for (idx, type_def) in ns.types.iter().enumerate() {
        let name = &type_def.header.as_ref().unwrap().name;
        type_def_map.insert(name.to_string(), (file_id, curr_ns_path.to_vec(), idx));
    }

    if !ns.nested_namespaces.is_empty() {
        let mut nested_path = curr_ns_path.to_vec();
        for (name, nested) in &ns.nested_namespaces {
            nested_path.push(name.clone());

            visit_namespaces(nested, file_id, type_def_map, &nested_path);

            nested_path.pop();
        }
    }
}

fn merge_namespaces(
    prev: &mut Namespace,
    new: Namespace,
    file_id: u32,
    type_def_map: &mut HashMap<String, (u32, Vec<String>, usize)>,
    curr_ns_path: &[String],
) {
    prev.aliases.extend(new.aliases);
    prev.interfaces.extend(new.interfaces);
    prev.functions.extend(new.functions);

    for type_def in new.types {
        let name = &type_def.header.as_ref().unwrap().name;
        if let Some((def_file_id, ns_path, def_idx)) = &type_def_map.get(name) {
            // TODO: return an error here instead of panicking.
            assert_eq!(file_id, *def_file_id, "definitions in different files");

            let mut curr_ns = &mut *prev;
            for ns in ns_path {
                curr_ns = curr_ns.nested_namespaces.get_mut(ns).unwrap();
            }

            merge_type_def(&mut curr_ns.types[*def_idx], type_def);
        } else {
            type_def_map.insert(
                name.to_string(),
                (file_id, curr_ns_path.to_vec(), prev.types.len()),
            );
            prev.types.push(type_def);
        }
    }

    if !new.nested_namespaces.is_empty() {
        let mut nested_path = curr_ns_path.to_vec();
        for (name, nested) in new.nested_namespaces {
            nested_path.push(name.clone());

            if let Some(prev) = prev.nested_namespaces.get_mut(&name) {
                merge_namespaces(prev, nested, file_id, type_def_map, &nested_path);
            } else {
                prev.nested_namespaces.insert(name, nested);
            }
            nested_path.pop();
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
        Some((Definition::Sum(prev), Definition::Sum(new))) => {
            for (name, mem) in new.members {
                if let Some(prev_mem) = prev.members.get_mut(&name) {
                    merge_attributes(&mut prev_mem.attributes, mem.attributes);
                } else {
                    // TODO: Can we permit adding new members?
                    panic!(
                        "Cannot add new members to sum type after it is defined: {} - {:?}",
                        name, mem
                    );
                }
            }
        }
        Some((prev, new)) => panic!("conflicting definitions: {:?} vs {:?}", prev, new),
        None => {}
    }
}

fn merge_attributes(prev: &mut Vec<Attribute>, new: Vec<Attribute>) {
    // TODO: think better on how to merge attributes.
    prev.extend(new);
}
