use std::{collections::HashMap, io};

use baker_codegen_pb::{CodegenRequest, CodegenResponse};
use baker_ir_pb::{
    interface::Bounds,
    r#type::{Fundamental, Name},
    type_def::{
        record::Property,
        sum::{member::Value, Member},
        Constraint, Definition, ImplBlock,
    },
    Attribute, Block, Constant, Function, IdentifierPath, Import, Namespace, Pattern, Statement,
    Type, TypeAlias, TypeDef, Visibility,
};
use baker_pkg_pb::Package;
use heck::SnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::*;

fn codegen_impl(req: CodegenRequest) -> io::Result<CodegenResponse> {
    let pkg_graph = req.packages.unwrap();
    let file_to_pkg: HashMap<_, _> = pkg_graph
        .packages
        .iter()
        .flat_map(|(_, pkg)| pkg.files.iter().map(move |f| (f, pkg)))
        .collect();

    let mut pkg_to_token_stream =
        HashMap::<_, TokenStream>::with_capacity(pkg_graph.packages.len());

    for file in req.ir_files {
        let pkg = file_to_pkg[&file.file_id];

        if let Some(root_ns) = file.root {
            let codegen = Codegen::new(pkg.name.clone());
            let stream = codegen.codegen_namespace(root_ns);
            pkg_to_token_stream
                .entry(pkg.id)
                .or_default()
                .extend(stream);
        }
    }

    for (pkg, stream) in pkg_to_token_stream {
        let pkg = &pkg_graph.packages[&pkg];
        write_pkg_file(pkg, stream, &req.output_folder)?;
    }

    Ok(Default::default())
}

fn write_pkg_file(pkg: &Package, stream: TokenStream, output_folder: &str) -> io::Result<()> {
    use std::io::Write;
    let path = std::path::Path::new(output_folder);
    let file_path = path.join(&format!("{}.rs", pkg.name));

    std::fs::create_dir_all(file_path.parent().unwrap())?;

    let mut file = std::fs::File::create(file_path.clone())?;
    write!(&mut file, "{}", stream)?;

    std::process::Command::new("cargo")
        .arg("fmt")
        .arg("--")
        .arg("--config")
        .arg("normalize_doc_attributes=true")
        .arg(file_path)
        .status()?;

    Ok(())
}

struct Codegen {
    namespace: String,
}

impl Codegen {
    fn new(namespace: String) -> Self {
        Self { namespace }
    }

    fn nested(&self, name: &str) -> Self {
        Self {
            namespace: format!("{}.{}", self.namespace, name),
        }
    }
}

impl Codegen {
    fn codegen_namespace(&self, ns: Namespace) -> TokenStream {
        let types = ns.types.into_iter().map(|ty| self.codegen_typedef(ty));

        let ns_tokens = ns.nested_namespaces.into_iter().map(|(ns, nested)| {
            let content = self.nested(&ns).codegen_namespace(nested);
            let ident = make_ident(&ns.to_snake_case());

            quote! { pub mod #ident { #content } }
        });

        let imports = ns.imports.into_iter().map(|imp| self.codegen_import(imp));

        let aliases = ns
            .aliases
            .into_iter()
            .map(|a| self.codegen_type_alias(a, true));

        let constants = ns
            .constants
            .into_iter()
            .map(|c| self.codegen_constant(c, true));

        let traits = ns.interfaces.into_iter().map(|i| self.codegen_trait(i));

        let functions = ns.functions.into_iter().map(|f| self.codegen_function(f));

        quote! {
            #(#imports)*
            #(#aliases)*
            #(#constants)*
            #(#traits)*
            #(#types)*
            #(#functions)*

            #(#ns_tokens)*
        }
    }

    fn codegen_import(&self, imp: Import) -> TokenStream {
        if let Some(module) = imp.module {
            let path = self.identifier_path_to_path(&module, false);

            if imp.glob {
                quote! { use #path::*; }
            } else if imp.things.is_empty() {
                let alias = if let Some(alias) = imp.alias {
                    let alias = make_ident(&alias);
                    quote! { as #alias }
                } else {
                    quote! {}
                };

                quote! { use #path #alias; }
            } else {
                let things = imp
                    .things
                    .into_iter()
                    .map(|t| self.identifier_path_to_path(&t, false));

                quote! { use #path::{ #(#things,)* }; }
            }
        } else {
            quote! {}
        }
    }

    fn codegen_trait(&self, i: baker_ir_pb::Interface) -> TokenStream {
        let header = self.codegen_type(i.header.unwrap());

        let attributes = self.codegen_attributes(i.attributes);

        let methods = i.methods.into_iter().map(|m| self.codegen_function(m));
        let assoc_types = i.assoc_types.into_iter().map(|at| {
            let header = self.codegen_type(at.header.unwrap());

            let bounds_token = if let Some(b) = at.bounds {
                self.codegen_bounds(b)
            } else {
                quote! {}
            };

            quote! { type #header #bounds_token; }
        });

        let bounds = if let Some(b) = i.bounds {
            self.codegen_bounds(b)
        } else {
            quote! {}
        };

        quote! {
            #attributes
            pub trait #header #bounds {
                #(#assoc_types)*

                #(#methods)*
            }
        }
    }

    fn codegen_bounds(&self, b: Bounds) -> TokenStream {
        let traits = b.interfaces.into_iter().map(|i| self.codegen_type(i));

        let mut tokens = quote! { : #(#traits)+* };

        if !b.lifetimes.is_empty() {
            let lfs = self.codegen_lifetimes(b.lifetimes);

            tokens = quote! { #tokens + #(#lfs)+* };
        }

        tokens
    }

    fn codegen_typedef(&self, td: TypeDef) -> TokenStream {
        let vis = codegen_visibility(td.visibility());
        let ty = td.header.unwrap();
        let definition = if let Some(def) = td.definition {
            let definition = self.codegen_definition(def, &ty);
            let attributes = self.codegen_attributes(td.attributes);
            let doc = self.codegen_doc_attributes(&td.documentation);

            quote! {
                #doc
                #attributes
                #vis #definition
            }
        } else {
            quote! {}
        };

        let ty_header = self.codegen_type(ty);
        let impls = td
            .blocks
            .into_iter()
            .map(|block| self.codegen_impl_block(block, &ty_header));

        quote! {
            #definition

            #(#impls)*
        }
    }

    fn codegen_definition(&self, def: Definition, ty: &Type) -> TokenStream {
        let header_tokens = self.codegen_definition_header(ty);

        match def {
            Definition::Record(rec) => {
                let properties = rec
                    .properties
                    .into_iter()
                    .map(|(k, v)| self.codegen_property(k, v));

                quote! { struct #header_tokens { #(#properties)* } }
            }
            Definition::Sum(sum) => {
                let members = sum
                    .members
                    .into_iter()
                    .map(|(n, m)| self.codegen_member(n, m));

                quote! { enum #header_tokens { #(#members)* } }
            }
        }
    }

    fn codegen_definition_header(&self, ty: &Type) -> syn::Type {
        let has_generics_or_lifetimes = !(ty.generics.is_empty() || ty.lifetimes.is_empty());
        let generics = self.codegen_generics(ty.generics.iter().cloned());
        let lifetimes = self.codegen_lifetimes(ty.lifetimes.iter().cloned());

        let mut name = self.type_name_to_path(ty.name.clone().unwrap()).unwrap();
        if has_generics_or_lifetimes {
            let last = name.path.segments.last_mut().unwrap();
            last.arguments =
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    colon2_token: None,
                    lt_token: syn::token::Lt::default(),
                    args: lifetimes.chain(generics).collect(),
                    gt_token: syn::token::Gt::default(),
                });
        }

        syn::Type::Path(name)
    }

    fn codegen_property(&self, name: String, prop: Property) -> TokenStream {
        let name = make_ident(&name);
        let vis = codegen_visibility(prop.visibility());
        let prop_type = self.codegen_type(prop.r#type.unwrap());
        let attributes = self.codegen_attributes(prop.attributes);
        let doc = self.codegen_doc_attributes(&prop.documentation);

        quote! {
            #doc
            #attributes
            #vis #name: #prop_type,
        }
    }

    fn codegen_member(&self, name: String, member: Member) -> TokenStream {
        let name = make_ident(&name);
        let attributes = self.codegen_attributes(member.attributes);
        let doc = self.codegen_doc_attributes(&member.documentation);

        let value = match member.value {
            Some(Value::Fixed(val)) => {
                let lit = syn::Lit::Verbatim(proc_macro2::Literal::i32_unsuffixed(val));
                quote! { = #lit }
            }
            Some(Value::Record(rec)) => {
                let properties = rec
                    .properties
                    .into_iter()
                    .map(|(k, v)| self.codegen_property(k, v));

                quote! { { #(#properties)* } }
            }
            Some(Value::Type(t)) => {
                let ty = self.codegen_type(t);
                quote! { (#ty) }
            }
            None => quote! {},
        };

        quote! { #doc #attributes #name #value, }
    }

    fn codegen_type(&self, ty: Type) -> TokenStream {
        let syn_type = self.type_to_syn_type(ty);

        quote! { #syn_type }
    }

    fn codegen_attribute(&self, attr: Attribute) -> TokenStream {
        use baker_ir_pb::attribute::Value;
        match attr.value {
            Some(Value::Call(call)) => {
                let attribute = self.identifier_path_to_path(&call.function.unwrap(), true);
                let args = call.args.into_iter().map(|a| self.codegen_value(a));
                let kwargs = call
                    .kwargs
                    .into_iter()
                    .map(|(k, v)| (make_ident(&k.to_snake_case()), self.codegen_value(v)))
                    .map(|(k, v)| quote! { #k = #v });

                quote! {
                    #[#attribute(#(#args,)* #(#kwargs),*)]
                }
            }
            Some(Value::Assignment(assign)) => {
                let ident = self.identifier_path_to_path(&assign.ident.unwrap(), false);
                let value = self.codegen_value(assign.value.unwrap());

                quote! {
                    #[#ident = #value]
                }
            }
            Some(Value::Identifier(ident)) => {
                let ident = self.identifier_path_to_path(&ident, false);
                quote! { #[#ident] }
            }
            None => quote! {},
        }
    }

    fn codegen_attributes(&self, attrs: impl IntoIterator<Item = Attribute>) -> TokenStream {
        let attributes = attrs.into_iter().map(|attr| self.codegen_attribute(attr));

        quote! { #(#attributes)* }
    }

    fn codegen_doc_attributes(&self, doc: &str) -> TokenStream {
        let attributes = doc.lines().map(|doc| {
            quote! { #[doc = #doc] }
        });

        quote! { #(#attributes)* }
    }

    fn codegen_generics<'a>(
        &'a self,
        generics: impl IntoIterator<Item = Type> + 'a,
    ) -> impl Iterator<Item = syn::GenericArgument> + 'a {
        generics
            .into_iter()
            .map(move |ty| self.type_to_syn_type(ty))
            .map(syn::GenericArgument::Type)
    }

    fn codegen_lifetimes<'a>(
        &'a self,
        generics: impl IntoIterator<Item = String> + 'a,
    ) -> impl Iterator<Item = syn::GenericArgument> + 'a {
        generics
            .into_iter()
            .map(make_lifetime)
            .map(syn::GenericArgument::Lifetime)
    }

    fn codegen_type_alias(&self, alias: TypeAlias, gen_vis: bool) -> TokenStream {
        let vis = if gen_vis {
            codegen_visibility(alias.visibility())
        } else {
            quote! {}
        };
        if let Some((al, aliased)) = alias.alias.zip(alias.aliased) {
            let doc = self.codegen_doc_attributes(&alias.documentation);
            let attrs = self.codegen_attributes(alias.attributes);
            let alias = self.codegen_type(al);
            let aliased = self.codegen_type(aliased);

            quote! { #doc #attrs #vis type #alias = #aliased; }
        } else {
            quote! {}
        }
    }

    fn type_to_syn_type(&self, mut ty: Type) -> syn::Type {
        match ty.fundamental() {
            Fundamental::Unknown => {
                let has_generics_or_lifetimes =
                    !(ty.generics.is_empty() && ty.lifetimes.is_empty());
                let generics = self.codegen_generics(ty.generics);
                let lifetimes = self.codegen_lifetimes(ty.lifetimes);

                if let Some(Name::Identifier(name)) = &ty.name {
                    let mut name = self.identifier_path_to_path(name, false);
                    if has_generics_or_lifetimes {
                        let last = name.path.segments.last_mut().unwrap();
                        last.arguments = syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments {
                                colon2_token: None,
                                lt_token: syn::token::Lt::default(),
                                args: lifetimes.chain(generics).collect(),
                                gt_token: syn::token::Gt::default(),
                            },
                        );
                    }

                    syn::Type::Path(name)
                } else {
                    unreachable!()
                }
            }
            Fundamental::Tuple => {
                let elements = ty.generics.into_iter().map(|ty| self.type_to_syn_type(ty));

                syn::Type::Tuple(syn::TypeTuple {
                    paren_token: syn::token::Paren::default(),
                    elems: elements.collect(),
                })
            }
            fr @ (Fundamental::Vec | Fundamental::Map | Fundamental::Optional) => {
                let generics = self.codegen_generics(ty.generics);

                let name = match fr {
                    Fundamental::Vec => quote! { ::std::vec::Vec },
                    Fundamental::Map => quote! { ::std::collections::HashMap },
                    Fundamental::Optional => quote! { ::std::option::Option },
                    _ => unreachable!(),
                };

                syn::Type::Verbatim(quote! { #name<#(#generics),*> })
            }
            fr @ (Fundamental::ShrdRef | Fundamental::UniqRef) => {
                let mutability = (fr == Fundamental::UniqRef).then(syn::token::Mut::default);
                let base = syn::Type::Reference(syn::TypeReference {
                    and_token: syn::token::And::default(),
                    lifetime: ty.lifetimes.pop().map(|lf| make_lifetime(lf)),
                    mutability,
                    elem: Box::new(self.type_to_syn_type(ty.generics.remove(0))),
                });

                ty.lifetimes.into_iter().rev().fold(base, |ty, lf| {
                    syn::Type::Reference(syn::TypeReference {
                        and_token: syn::token::And::default(),
                        lifetime: Some(make_lifetime(lf)),
                        mutability,
                        elem: Box::new(ty),
                    })
                })
            }
            fr @ (Fundamental::ConstPtr | Fundamental::Ptr) => syn::Type::Ptr(syn::TypePtr {
                star_token: syn::token::Star::default(),
                const_token: (fr == Fundamental::ConstPtr).then(syn::token::Const::default),
                mutability: (fr == Fundamental::Ptr).then(syn::token::Mut::default),
                elem: Box::new(self.type_to_syn_type(ty.generics.remove(0))),
            }),
            Fundamental::Double => syn::Type::Verbatim(quote! { f64 }),
            Fundamental::Float => syn::Type::Verbatim(quote! { f32 }),
            Fundamental::SInt => syn::Type::Verbatim(quote! { i32 }),
            Fundamental::SLong => syn::Type::Verbatim(quote! { i64 }),
            Fundamental::UInt => syn::Type::Verbatim(quote! { u32 }),
            Fundamental::ULong => syn::Type::Verbatim(quote! { u64 }),
            Fundamental::Bool => syn::Type::Verbatim(quote! { bool }),
            Fundamental::String => syn::Type::Verbatim(quote! { ::std::string::String }),
            Fundamental::Bytes => syn::Type::Verbatim(quote! { ::bytes::Bytes }),
            Fundamental::Self_ => syn::Type::Verbatim(quote! { Self }),
            Fundamental::Slice => {
                let generic = self.type_to_syn_type(ty.generics.remove(0));

                syn::Type::Slice(syn::TypeSlice {
                    bracket_token: syn::token::Bracket::default(),
                    elem: Box::new(generic),
                })
            }
            Fundamental::Dynamic => {
                let int_trait = self.type_to_syn_type(ty.generics.remove(0));

                syn::Type::Verbatim(quote! { dyn #int_trait })
            }
        }
    }

    fn identifier_path_to_path(
        &self,
        path: &IdentifierPath,
        generics_separated: bool,
    ) -> syn::TypePath {
        let mut module = path.scope_as_dotted_path();
        if module.is_empty() {
            // Anything can go here, given that no identifier can start with it.
            module.push_str("&&~123");
        }
        let name_segm = self.identifier_path_segment(path.last(), generics_separated);
        let name_segm = std::iter::once(name_segm);
        let is_global = path.scope() == baker_ir_pb::identifier_path::Scope::Global;
        let is_package = path.scope() == baker_ir_pb::identifier_path::Scope::Package;

        let qual_position = path.segments.len() - 1;

        syn::TypePath {
            qself: path.qualifier.as_deref().map(|q| syn::QSelf {
                lt_token: syn::token::Lt::default(),
                ty: Box::new(self.type_to_syn_type(q.clone())),
                position: qual_position,
                as_token: Some(syn::token::As::default()),
                gt_token: syn::token::Gt::default(),
            }),
            path: syn::Path {
                leading_colon: is_global.then(syn::token::Colon2::default),
                segments: if let Some(submodule) = self.namespace.strip_prefix(&module) {
                    // Example:
                    //     name = Foo
                    //     module = blog.api
                    //     namespace = blog.api.v1
                    //     submodule = .v1
                    //     result = super::Foo
                    let path = submodule
                        .chars()
                        .filter(|c| *c == '.')
                        .flat_map(|_| single_path_segment("super"));

                    path.chain(name_segm).collect()
                } else if let Some(submodule) = module.strip_prefix(&self.namespace) {
                    // Example:
                    //     name = Comment
                    //     module = blog.api.v1.Post
                    //     namespace = blog.api.v1
                    //     submodule = .Post
                    //     result = self::post::Comment
                    if submodule.is_empty() {
                        name_segm.collect()
                    } else {
                        let path = module_path_segments(&submodule[1..]);

                        single_path_segment("self")
                            .chain(path)
                            .chain(name_segm)
                            .collect()
                    }
                } else {
                    let mut common_segs = 0;
                    let mut curr_namespace = self.namespace.as_str();

                    let use_super = if self.namespace.starts_with(&path.segments[0].name) {
                        for seg in &path.segments {
                            if let Some(new_ns) = curr_namespace.strip_prefix(&seg.name) {
                                curr_namespace = new_ns.trim_start_matches('.');
                                common_segs += 1;
                            }
                        }

                        true
                    } else {
                        false
                    };

                    if use_super {
                        let supers = self
                            .namespace
                            .split('.')
                            .skip(common_segs)
                            .flat_map(|_| single_path_segment("super"));

                        let mut module: String = path.segments
                            [common_segs..path.segments.len() - 1]
                            .iter()
                            .map(|s| s.name.clone() + ".")
                            .collect();

                        module.pop();

                        let name = module_path_segments(&module).chain(name_segm);

                        supers.chain(name).collect()
                    } else {
                        let path_segments = path.segments[common_segs..path.segments.len() - 1]
                            .iter()
                            .map(|s| self.identifier_path_segment(s, true));

                        let name = path_segments.chain(name_segm);

                        if is_package {
                            single_path_segment("crate").chain(name).collect()
                        } else {
                            name.collect()
                        }
                    }
                },
            },
        }
    }

    fn identifier_path_to_field<'a>(
        &self,
        path: &'a IdentifierPath,
    ) -> impl Iterator<Item = syn::Member> + 'a {
        path.segments.iter().map(|s| {
            if s.name.chars().all(|c| c.is_numeric()) {
                let idx = syn::Index {
                    index: s.name.parse().expect("bad field segment"),
                    span: Span::call_site(),
                };
                syn::Member::Unnamed(idx)
            } else {
                syn::Member::Named(make_ident(&s.name))
            }
        })
    }

    fn identifier_path_segment(
        &self,
        seg: &baker_ir_pb::identifier_path::Segment,
        generics_separated: bool,
    ) -> syn::PathSegment {
        syn::PathSegment {
            ident: make_ident(&seg.name),
            arguments: {
                if seg.generics.is_empty() && seg.lifetimes.is_empty() {
                    syn::PathArguments::None
                } else {
                    let gens = self.codegen_generics(seg.generics.clone());
                    let lif = self.codegen_lifetimes(seg.lifetimes.clone());

                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        colon2_token: generics_separated.then(syn::token::Colon2::default),
                        lt_token: syn::token::Lt::default(),
                        args: lif.chain(gens).collect(),
                        gt_token: syn::token::Gt::default(),
                    })
                }
            },
        }
    }

    fn type_name_to_path(&self, typ_name: Name) -> Option<syn::TypePath> {
        if let Name::Identifier(ident) = typ_name {
            Some(self.identifier_path_to_path(&ident, false))
        } else {
            None
        }
    }

    fn codegen_impl_block(&self, block: ImplBlock, ty_header: &TokenStream) -> TokenStream {
        let is_trait = block.interface.is_some();
        let trait_prefix = if let Some(ty) = block.interface {
            let ty = self.codegen_type(ty);
            quote! { #ty for }
        } else {
            quote! {}
        };

        let methods = block.methods.into_iter().map(|mut meth| {
            // Remove visibility from trait functions.
            if is_trait {
                meth.set_visibility(Visibility::Private);
            }

            self.codegen_function(meth)
        });

        let gen_prefix = if block.generics.is_empty() && block.lifetimes.is_empty() {
            quote! {}
        } else {
            let generics = self.codegen_generics(block.generics);
            let lifetimes = self.codegen_lifetimes(block.lifetimes);

            quote! { <#(#lifetimes,)* #(#generics,)*> }
        };

        let assoc_types = block
            .assoc_types
            .into_iter()
            .map(|at| self.codegen_type_alias(at, false));

        let constants = block
            .constants
            .into_iter()
            .map(|c| self.codegen_constant(c, false));

        let where_clause = self.codegen_where_clause(block.constraints);

        quote! {
            impl #gen_prefix #trait_prefix #ty_header #where_clause {
                #(#assoc_types)*
                #(#constants)*
                #(#methods)*
            }
        }
    }

    fn codegen_where_clause(&self, constraints: Vec<Constraint>) -> TokenStream {
        if constraints.is_empty() {
            quote! {}
        } else {
            let constraints = constraints.into_iter().map(|c| self.codegen_constraint(c));

            quote! { where #(#constraints)* }
        }
    }

    fn codegen_constraint(&self, constraint: Constraint) -> TokenStream {
        if let Some(constrained) = constraint.constrained {
            if constraint.interfaces.is_empty() && constraint.lifetimes.is_empty() {
                quote! {}
            } else {
                let constrained = self.codegen_type(constrained);

                let infix = if constraint.interfaces.is_empty() || constraint.interfaces.is_empty()
                {
                    quote! {}
                } else {
                    quote! { + }
                };

                let lifetimes = self.codegen_lifetimes(constraint.lifetimes);
                let traits = constraint
                    .interfaces
                    .into_iter()
                    .map(|iface| self.codegen_type(iface));

                quote! { #constrained: #(#traits)+* #infix #(#lifetimes)+*, }
            }
        } else {
            quote! {}
        }
    }

    fn codegen_function(&self, func: Function) -> TokenStream {
        let vis = codegen_visibility(func.visibility());
        let doc = self.codegen_doc_attributes(&func.documentation);
        let header = self.codegen_function_header(func.header.unwrap());

        let ret_ty = if let Some(ty) = func.r#return {
            let ty = self.codegen_type(ty);
            quote! { -> #ty }
        } else {
            quote! {}
        };

        let arguments = func.arguments.into_iter().map(|arg| {
            let name = make_ident(&arg.name);
            let ty = self.codegen_type(arg.r#type.unwrap());

            quote! { #name: #ty }
        });

        let block = if let Some(block) = func.implementation {
            self.codegen_block(block)
        } else {
            quote! { ; }
        };
        let attributes = self.codegen_attributes(func.attributes);

        let asyncness = if func.asyncness {
            quote! { async }
        } else {
            quote! {}
        };

        let receiver = if let Some(recv) = func.receiver {
            match recv.fundamental() {
                Fundamental::ShrdRef => quote! { &self, },
                Fundamental::UniqRef => quote! { &mut self, },
                Fundamental::Self_ => quote! { self, },
                _ => {
                    let typ = self.codegen_type(recv);
                    quote! { self: #typ, }
                }
            }
        } else {
            quote! {}
        };

        let where_clause = self.codegen_where_clause(func.constraints);

        quote! {
            #doc
            #attributes
            #vis #asyncness fn #header (#receiver #(#arguments),*) #ret_ty #where_clause #block
        }
    }

    fn codegen_function_header(&self, header: Type) -> TokenStream {
        let typ = self.type_to_syn_type(header.clone());

        if let syn::Type::Path(tp) = typ {
            quote! { #tp }
        } else {
            unreachable!("invalid return from type_to_syn_type with a function header")
        }
    }

    fn codegen_block(&self, block: Block) -> TokenStream {
        let statements = block
            .statements
            .into_iter()
            .map(|stmt| self.codegen_statement(stmt));

        if let Some(val) = block.return_value {
            let value = self.codegen_value(val);

            quote! {
                {
                    #(#statements)*
                    #value
                }
            }
        } else {
            quote! {
                {
                    #(#statements)*
                }
            }
        }
    }

    fn codegen_statement(&self, statement: Statement) -> TokenStream {
        use baker_ir_pb::statement::Statement;

        match statement.statement {
            Some(Statement::Return(v)) => {
                let val = self.codegen_value(v);
                quote! { return #val; }
            }
            Some(Statement::Assignment(assign)) => {
                use baker_ir_pb::statement::assignment::AssignmentType;
                let assign_type = assign.assignment_type();
                let name = if let Some(ident) = assign.ident {
                    let name = self.identifier_path_to_path(&ident, true);
                    quote! { #name }
                } else {
                    self.codegen_pattern(assign.pattern_decl.unwrap())
                };

                let value = if let Some(val) = assign.value {
                    let val = self.codegen_value(val);
                    quote! { = #val }
                } else {
                    quote! {}
                };

                let typ = if let Some(tp) = assign.r#type {
                    let tp = self.codegen_type(tp);
                    quote! { : #tp }
                } else {
                    quote! {}
                };

                match assign_type {
                    AssignmentType::Reassignment => {
                        let field = if let Some(field) = assign.field {
                            let members = self.identifier_path_to_field(&field);
                            quote! { . #(#members).* }
                        } else {
                            quote! {}
                        };

                        quote! { #name #field #value; }
                    }
                    AssignmentType::DefConstant => quote! { let #name #typ #value; },
                    AssignmentType::DefMutable => quote! { let mut #name #typ #value; },
                }
            }
            Some(Statement::Switch(switch)) => {
                let value = self.codegen_value(switch.value.unwrap());
                let arms = switch.arms.into_iter().map(|arm| {
                    let patterns = arm.pattern.into_iter().map(|p| self.codegen_pattern(p));
                    let block = self.codegen_block(arm.block.unwrap());

                    quote! { #(#patterns)|* => #block }
                });

                quote! { match #value { #(#arms)* } }
            }
            Some(Statement::Expression(val)) => {
                let val = self.codegen_value(val);

                quote! { #val; }
            }
            None => quote! {},
        }
    }

    fn codegen_constant(&self, constant: Constant, gen_vis: bool) -> TokenStream {
        let name = make_ident(&constant.name);
        let vis = if gen_vis {
            codegen_visibility(constant.visibility())
        } else {
            quote! {}
        };

        let doc = self.codegen_doc_attributes(constant.documentation());

        let typ = if let Some(typ) = constant.r#type {
            self.codegen_type(typ)
        } else {
            quote! {}
        };

        let value = if let Some(val) = constant.value {
            self.codegen_value(val)
        } else {
            quote! {}
        };

        quote! { #doc #vis const #name: #typ = #value; }
    }

    fn codegen_value(&self, value: baker_ir_pb::Value) -> TokenStream {
        use baker_ir_pb::value::{ByRef, Value};

        let prefix = match value.by_ref() {
            ByRef::ByValue => quote! {},
            ByRef::ConstRef => quote! { & },
            ByRef::MutRef => quote! { &mut  },
        };

        let value_tokens = match value.value {
            Some(Value::Call(call)) => {
                let func = self.identifier_path_to_path(&call.function.unwrap(), true);
                let args = call.args.into_iter().map(|a| self.codegen_value(a));

                let suffix = if call.is_macro {
                    quote! { ! }
                } else {
                    quote! {}
                };

                quote! { #func#suffix(#(#args),*) }
            }
            Some(Value::Identifier(ident)) => {
                let ident = self.identifier_path_to_path(&ident, true);

                quote! { #ident }
            }
            Some(Value::StringValue(value)) => quote! { #value },
            Some(Value::Tuple(baker_ir_pb::value::Tuple { values })) => {
                let values = values.into_iter().map(|v| self.codegen_value(v));

                quote! { (#(#values,)*) }
            }
            Some(Value::Method(meth)) => {
                let receiver = self.codegen_value(*meth.receiver.unwrap());
                let method =
                    self.codegen_value(baker_ir_pb::Value::func_call(meth.method.unwrap()));

                quote! { #receiver.#method }
            }
            Some(Value::BinOp(mut op)) => {
                use baker_ir_pb::value::bin_op::Op as BinOp;
                let left = self.codegen_value(*op.left.take().unwrap());
                let right = self.codegen_value(*op.right.take().unwrap());

                match op.operator() {
                    BinOp::Unknown => quote! {},
                    BinOp::Eq => quote! { #left == #right },
                    BinOp::Ne => quote! { #left != #right },
                    BinOp::Lt => quote! { #left < #right },
                    BinOp::Le => quote! { #left <= #right },
                    BinOp::Gt => quote! { #left > #right },
                    BinOp::Ge => quote! { #left >= #right },
                    BinOp::And => quote! { #left && #right },
                    BinOp::Or => quote! { #left || #right },
                }
            }
            Some(Value::BoolValue(b)) => quote! { #b },
            Some(Value::Await(val)) => {
                let value = self.codegen_value(*val);

                quote! { #value.await }
            }
            Some(Value::Cast(cast)) => {
                let value = self.codegen_value(*cast.value.unwrap());
                let typ = self.codegen_type(cast.cast_as.unwrap());

                quote! { ( #value as #typ ) }
            }
            Some(Value::UnOp(mut op)) => {
                use baker_ir_pb::value::unary_op::Op;
                let value = self.codegen_value(*op.value.take().unwrap());

                match op.operator() {
                    Op::Unknown => value,
                    Op::Deref => quote! { *#value },
                    Op::Try => quote! { #value? },
                }
            }
            Some(Value::BytesValue(b)) => {
                let lit = syn::LitByteStr::new(&b, Span::mixed_site());

                quote! { #lit }
            }
            None => quote! {},
        };

        quote! { #prefix #value_tokens }
    }

    fn codegen_pattern(&self, pattern: Pattern) -> TokenStream {
        use baker_ir_pb::pattern::Value;

        match pattern.value {
            None => quote! {},
            Some(Value::Constant(val)) => self.codegen_value(val),
            Some(Value::Identifier(ident)) => {
                self.identifier_path_to_path(&ident, true).to_token_stream()
            }
            Some(Value::Sum(sum)) => self.codegen_value(baker_ir_pb::Value::func_call(sum)),
        }
    }
}

fn codegen_visibility(vis: Visibility) -> TokenStream {
    match vis {
        Visibility::Private | Visibility::Protected => quote! {},
        Visibility::Package => quote! { pub(crate) },
        Visibility::Unknown | Visibility::Public => quote! { pub },
    }
}

fn make_lifetime(mut l: String) -> syn::Lifetime {
    l.insert(0, '\'');
    syn::Lifetime::new(&l, Span::mixed_site())
}

fn make_ident(ident: &str) -> syn::Ident {
    syn::Ident::new(ident, Span::mixed_site())
}

fn single_path_segment(ident: &str) -> std::iter::Once<syn::PathSegment> {
    std::iter::once(syn::PathSegment {
        ident: make_ident(ident),
        arguments: syn::PathArguments::None,
    })
}

fn module_path_segments(module: &str) -> impl Iterator<Item = syn::PathSegment> + '_ {
    module
        .split('.')
        .filter(|s| !s.is_empty())
        .map(|segm| syn::PathSegment {
            ident: make_ident(&segm.to_snake_case()),
            arguments: syn::PathArguments::None,
        })
}

fn main() -> io::Result<()> {
    baker_codegen_pb::execute_flow(codegen_impl)
}
