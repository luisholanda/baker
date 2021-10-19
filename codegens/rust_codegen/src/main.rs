use std::{collections::HashMap, io};

use baker_codegen_pb::{CodegenRequest, CodegenResponse};
use baker_ir_pb::{
    r#type::Fundamental,
    type_def::{
        record::Property,
        sum::{member::Value, Member},
        Definition, ImplBlock,
    },
    Block, Function, FunctionCall, Namespace, Statement, Type, TypeDef, Visibility,
};
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

    for file in req.ir_files {
        let pkg = file_to_pkg[&file.file_id];

        if let Some(root_ns) = file.root {
            let codegen = Codegen::new(pkg.name.clone());
            let stream = codegen.codegen_namespace(root_ns);
            eprintln!("{}", stream);
        }
    }

    Ok(Default::default())
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

        quote! {
            #(#types)*
            #(#ns_tokens)*
        }
    }

    fn codegen_typedef(&self, td: TypeDef) -> TokenStream {
        let vis = codegen_visibility(td.visibility());
        let ty = td.header.unwrap();
        let defition = self.codegen_definition(td.definition.unwrap(), &ty);
        let attributes = self.codegen_attributes(td.attributes);
        let doc = self.codegen_doc_attributes(&td.documentation);

        let ty_header = self.codegen_type(ty);
        let impls = td
            .blocks
            .into_iter()
            .map(|block| self.codegen_impl_block(block, &ty_header));

        quote! {
            #doc
            #attributes
            #vis #defition

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
                let members = sum.members.into_iter().map(|m| self.codegen_member(m));

                let enum_name = ty.name.rsplit_once('.').unwrap().1;
                let mod_name = module_path_segments(enum_name);
                let enum_name = make_ident(enum_name);

                quote! {
                    enum #header_tokens { #(#members)* }

                    pub mod #(#mod_name)* {
                        //! Module used to export enum members for use in generated code.
                        //! This can be removed when you stop using the generator.
                        use super::#enum_name::*;
                    }
                }
            }
        }
    }

    fn codegen_definition_header(&self, ty: &Type) -> syn::Type {
        let has_generics_or_lifetimes = !(ty.generics.is_empty() || ty.lifetimes.is_empty());
        let generics = self.codegen_generics(ty.generics.iter().cloned());
        let lifetimes = self.codegen_lifetimes(ty.lifetimes.iter().cloned());

        let mut name = self.name_to_ident_path(&ty.name);
        if has_generics_or_lifetimes {
            let last = name.segments.last_mut().unwrap();
            last.arguments =
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    colon2_token: None,
                    lt_token: syn::token::Lt::default(),
                    args: lifetimes.chain(generics).collect(),
                    gt_token: syn::token::Gt::default(),
                });
        }

        syn::Type::Path(syn::TypePath {
            qself: None,
            path: name,
        })
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

    fn codegen_member(&self, member: Member) -> TokenStream {
        let name = make_ident(&member.name);
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

    fn codegen_attribute(&self, call: FunctionCall) -> TokenStream {
        let attribute = make_ident(&call.function);
        let args = call.args.into_iter().map(|a| self.codegen_value(a));
        let kwargs = call
            .kwargs
            .into_iter()
            .map(|(k, v)| (make_ident(&k), self.codegen_value(v)))
            .map(|(k, v)| quote! { #k = #v });

        quote! {
            #[#attribute(#(#args),*, #(#kwargs),*)]
        }
    }

    fn codegen_attributes(&self, attrs: impl IntoIterator<Item = FunctionCall>) -> TokenStream {
        let attributes = attrs.into_iter().map(|attr| self.codegen_attribute(attr));

        quote! { #(#attributes)* }
    }

    fn codegen_doc_attributes(&self, doc: &str) -> TokenStream {
        let attributes = doc.lines().map(|doc| {
            let lit = syn::Lit::Str(syn::LitStr::new(doc, Span::mixed_site()));
            quote! { #[doc = #lit] }
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
            .map(|l| syn::Lifetime::new(&l, Span::mixed_site()))
            .map(syn::GenericArgument::Lifetime)
    }

    fn type_to_syn_type(&self, mut ty: Type) -> syn::Type {
        match ty.fundamental() {
            Fundamental::Unknown => {
                let has_generics_or_lifetimes =
                    !(ty.generics.is_empty() || ty.lifetimes.is_empty());
                let generics = self.codegen_generics(ty.generics);
                let lifetimes = self.codegen_lifetimes(ty.lifetimes);

                let mut name = self.name_to_path(&ty.name);
                if has_generics_or_lifetimes {
                    let last = name.segments.last_mut().unwrap();
                    last.arguments =
                        syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                            colon2_token: None,
                            lt_token: syn::token::Lt::default(),
                            args: lifetimes.chain(generics).collect(),
                            gt_token: syn::token::Gt::default(),
                        });
                }

                syn::Type::Path(syn::TypePath {
                    qself: None,
                    path: name,
                })
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
                syn::Type::Reference(syn::TypeReference {
                    and_token: syn::token::And::default(),
                    lifetime: ty
                        .lifetimes
                        .first()
                        .map(|lf| syn::Lifetime::new(&lf, Span::mixed_site())),
                    mutability: (fr == Fundamental::UniqRef).then(syn::token::Mut::default),
                    elem: Box::new(self.type_to_syn_type(ty.generics.remove(0))),
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
        }
    }

    fn name_to_ident_path(&self, name: &str) -> syn::Path {
        let segm = name.rsplit_once('.').unwrap().1;

        syn::Path {
            leading_colon: None,
            segments: std::iter::once(syn::PathSegment {
                ident: make_ident(&segm),
                arguments: syn::PathArguments::None,
            })
            .collect(),
        }
    }

    fn name_to_path(&self, name: &str) -> syn::Path {
        syn::Path {
            leading_colon: None,
            segments: if let Some((module, name)) = name.rsplit_once('.') {
                let name_segm = single_path_segment(name);

                if let Some(submodule) = self.namespace.strip_prefix(module) {
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
                    let path_segments = module_path_segments(module);

                    single_path_segment("crate")
                        .chain(path_segments)
                        .chain(name_segm)
                        .collect()
                }
            } else {
                single_path_segment(name).collect()
            },
        }
    }

    fn codegen_impl_block(&self, block: ImplBlock, ty_header: &TokenStream) -> TokenStream {
        let trait_prefix = if let Some(ty) = block.interface {
            let ty = self.codegen_type(ty);
            quote! { #ty for }
        } else {
            quote! {}
        };

        let methods = block
            .methods
            .into_iter()
            .map(|meth| self.codegen_function(meth));

        quote! {
            impl #trait_prefix #ty_header {
                #(#methods)*
            }
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

        let block = self.codegen_block(func.implementation.unwrap());

        quote! {
            #doc
            #vis fn #header () #ret_ty #block
        }
    }

    fn codegen_function_header(&self, header: Type) -> TokenStream {
        let typ = self.type_to_syn_type(header.clone());

        if let syn::Type::Path(mut tp) = typ {
            let last = tp.path.segments.last_mut().unwrap();
            last.ident = make_ident(&header.name.to_snake_case());

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

        quote! {
            {
                #(#statements)*
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
            None => quote! {},
        }
    }

    fn codegen_value(&self, value: baker_ir_pb::Value) -> TokenStream {
        use baker_ir_pb::value::Value;

        match value.value {
            Some(Value::Call(call)) => {
                let func = make_ident(&call.function);
                let args = call.args.into_iter().map(|a| self.codegen_value(a));
                quote! { #func(#(#args),*) }
            }
            Some(Value::Identifier(ident)) => {
                let ident = self.name_to_path(&ident);

                quote! { #ident }
            }
            None => quote! {},
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
    module.split('.').map(|segm| syn::PathSegment {
        ident: make_ident(&segm.to_snake_case()),
        arguments: syn::PathArguments::None,
    })
}

fn main() -> io::Result<()> {
    baker_codegen_pb::execute_flow(codegen_impl)
}
