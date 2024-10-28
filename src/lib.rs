mod constants;
mod generate;
mod globals;
mod structs;
pub mod type_map;
mod utils;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    path::{Path, PathBuf},
};

use dependency_graph::{DependencyGraph, Node, Step};
use generate::build_entry_tokens;
use naga::{
    valid::{Capabilities, ValidationError},
    Module,
};
use naga_oil::compose::{
    get_preprocessor_data, ComposableModuleDescriptor, Composer, ComposerError, ImportDefinition,
    NagaModuleDescriptor, ShaderDefValue, ShaderLanguage, ShaderType,
};
use proc_macro2::TokenStream;

pub mod wgpu_types {
    pub use wgpu_types::*;
}

#[derive(Debug)]
pub enum Error {
    IoError(std::io::Error),
    MissingField(&'static str),
    ComposerError(ComposerError),
    UnknownImport(String),

    NagaValidationError(ValidationError),
    NagaWriterError(naga::back::wgsl::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IoError(e) => e.fmt(f),
            Self::UnknownImport(name) => {
                write!(f, "unknown import `{name}` found in file")
            }
            Self::ComposerError(e) => e.fmt(f),
            Self::MissingField(s) => write!(f, "builder field `{s}` not provided"),
            Self::NagaValidationError(e) => e.fmt(f),
            Self::NagaWriterError(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug, Clone)]
struct WgslFile {
    name: String,
    path: PathBuf,
    dependencies: Vec<Dependency>,
}

#[derive(Debug, Clone)]
struct Dependency {
    name: String,
    items: HashSet<String>,
}

impl Node for WgslFile {
    type DependencyType = Dependency;

    fn dependencies(&self) -> &[Self::DependencyType] {
        &self.dependencies[..]
    }

    fn matches(&self, dependency: &Self::DependencyType) -> bool {
        self.name == dependency.name
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum DefinitionValue {
    Bool(bool),
    Int(i32),
    UInt(u32),
}

impl From<bool> for DefinitionValue {
    fn from(val: bool) -> Self {
        DefinitionValue::Bool(val)
    }
}
impl From<u32> for DefinitionValue {
    fn from(val: u32) -> Self {
        DefinitionValue::UInt(val)
    }
}
impl From<i32> for DefinitionValue {
    fn from(val: i32) -> Self {
        DefinitionValue::Int(val)
    }
}

impl From<DefinitionValue> for ShaderDefValue {
    fn from(value: DefinitionValue) -> Self {
        match value {
            DefinitionValue::Bool(b) => ShaderDefValue::Bool(b),
            DefinitionValue::UInt(u) => ShaderDefValue::UInt(u),
            DefinitionValue::Int(i) => ShaderDefValue::Int(i),
        }
    }
}

#[derive(Default, Debug)]
struct Preprocess {
    files: Vec<WgslFile>,
    // shader_defined_values: HashMap<String, ShaderDefValue>,
}

#[derive(Debug)]
struct PreprocessedFiles {
    resolved_files: Vec<WgslFile>,
    // shader_defined_values: HashMap<String, ShaderDefValue>,
}

// .clone() central in here but whatever
impl Preprocess {
    pub fn preprocess(&mut self, source: &str, path: PathBuf) {
        let (name, imports, ..) = get_preprocessor_data(source);

        let dep_graph_name =
            name.unwrap_or(path.file_stem().unwrap().to_str().unwrap().to_string());

        self.files.push(WgslFile {
            name: dep_graph_name.clone(),
            path,
            dependencies: imports
                .iter()
                .map(|def| Dependency {
                    name: def.import.clone(),
                    items: def.items.iter().cloned().collect::<HashSet<_>>(),
                })
                .collect::<Vec<_>>(),
        });

        // self.shader_defined_values.extend(effective_defs);
    }

    pub fn finish(self) -> Result<PreprocessedFiles, Error> {
        let graph = DependencyGraph::from(&self.files[..]);

        let mut resolved = vec![];

        for file in graph {
            match file {
                Step::Resolved(file) => resolved.push(file.clone()),
                Step::Unresolved(dep) => return Err(Error::UnknownImport(dep.name.clone())),
            }
        }

        Ok(PreprocessedFiles {
            resolved_files: resolved,
            // shader_defined_values: self.shader_defined_values,
        })
    }
}

fn shader_source(file: &Path) -> Result<String, Error> {
    std::fs::read_to_string(file).map_err(Error::IoError)
}

pub type BindGroupLayoutEntryFunc =
    dyn Fn(u32, wgpu_types::ShaderStages) -> wgpu_types::BindGroupLayoutEntry + 'static;

pub(crate) struct Config {
    bg_layout_entry_overrides: HashMap<String, Box<BindGroupLayoutEntryFunc>>,
    separate_files: bool,
    file_name: String,

    // maps the name of a type to the path used to resolve that type
    // 2 different modules may have the same type name but that should be handled by naga_oil
    // this assumes there will never be 2 types of the same name from 2 different files
    resolve_type_map: HashMap<String, TokenStream>,

    defined_constants: HashSet<String>,

    // maps bind group index to name of globals in that bind group
    defined_globals: HashMap<usize, Vec<String>>,

    // keeps track of what structs have vertex buffer layout impls
    // we dont care what file impls it since its public, just if it has one already
    vertex_descriptor_impl: HashSet<String>,
}

pub struct WgslBindgen {
    preprocessed_files: PreprocessedFiles,
    modules: HashMap<String, Module>,
    output: PathBuf,
    separate_files: bool,

    bg_layout_entry_overrides: HashMap<String, Box<BindGroupLayoutEntryFunc>>,
}

impl WgslBindgen {
    pub fn override_bind_group_layout_entry(
        mut self,
        name: impl Into<String>,
        entry: Box<BindGroupLayoutEntryFunc>,
    ) -> Self {
        self.bg_layout_entry_overrides.insert(name.into(), entry);
        self
    }

    pub fn generate(self) -> Result<(), Error> {
        let mut config = Config {
            bg_layout_entry_overrides: self.bg_layout_entry_overrides,
            separate_files: self.separate_files,
            file_name: String::new(),

            resolve_type_map: HashMap::new(),
            defined_constants: HashSet::new(),
            defined_globals: HashMap::new(),
            vertex_descriptor_impl: HashSet::new(),
        };

        let mut all_tokens = TokenStream::new();

        for file in &self.preprocessed_files.resolved_files {
            config.file_name = file.name.clone();

            let module = self
                .modules
                .get(&file.name)
                .expect("missing module for file");

            let tokens = generate::generate(module, &mut config)?;

            if self.separate_files {
                std::fs::write(
                    self.output
                        .join(file.name.to_lowercase())
                        .with_extension("rs"),
                    tokens.to_string(),
                )
                .map_err(Error::IoError)?
            } else {
                all_tokens.extend(tokens);
            }
        }

        if self.separate_files {
            // we only ever want 1 instance of these structs so in multiple files they go in the mod.rs
            let entry_structs = build_entry_tokens();

            let mod_contents = self
                .preprocessed_files
                .resolved_files
                .iter()
                .map(|f| format!("pub mod {};", f.name.to_lowercase()))
                .collect::<Vec<_>>()
                .join("\n");

            std::fs::write(
                self.output.join("mod.rs"),
                format!("{mod_contents}\n{entry_structs}"),
            )
            .map_err(Error::IoError)?;
        } else {
            std::fs::write(self.output, all_tokens.to_string()).map_err(Error::IoError)?;
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct WgslBindgenBuilder {
    shader_root: Option<PathBuf>,
    entries: Vec<PathBuf>,
    user_shader_defs: HashMap<String, ShaderDefValue>,
    output: Option<PathBuf>,
    separate_files: bool,
}

impl WgslBindgenBuilder {
    pub fn shader_root<P: Into<PathBuf>>(self, p: P) -> Self {
        Self {
            shader_root: Some(p.into()),
            ..self
        }
    }

    pub fn output<P: Into<PathBuf>>(self, p: P) -> Self {
        Self {
            output: Some(p.into()),
            ..self
        }
    }

    pub fn add_shader<P: Into<PathBuf>>(mut self, path: P) -> Self {
        self.entries.push(path.into());
        self
    }

    pub fn add_shaders<P: Into<PathBuf>, I: IntoIterator<Item = P>>(mut self, items: I) -> Self {
        self.entries.extend(items.into_iter().map(|f| f.into()));
        self
    }

    pub fn define_preprocessor_value<N: Into<String>, V: Into<DefinitionValue>>(
        mut self,
        name: N,
        value: V,
    ) -> Self {
        let def_value: DefinitionValue = value.into();
        self.user_shader_defs.insert(name.into(), def_value.into());
        self
    }

    pub fn separate_files(self, sep: bool) -> Self {
        Self {
            separate_files: sep,
            ..self
        }
    }

    pub fn build(self) -> Result<WgslBindgen, Error> {
        let mut prep = Preprocess::default();

        let root = self
            .shader_root
            .as_deref()
            .ok_or(Error::MissingField("shader_root"))?;

        for file in self.entries {
            let path = root.join(file);
            prep.preprocess(&shader_source(&path)?, path);
        }

        let preprocessed = prep.finish()?;

        let mut composer = Composer::default();
        composer.capabilities = Capabilities::all();
        composer.validate = true;

        let mut definitions = HashMap::new();

        for file in &preprocessed.resolved_files {
            composer
                .add_composable_module(ComposableModuleDescriptor {
                    source: &shader_source(&file.path)?,
                    file_path: file.path.to_str().unwrap(),
                    language: ShaderLanguage::Wgsl,
                    as_name: Some(file.name.clone()),
                    additional_imports: &[],
                    shader_defs: self.user_shader_defs.clone(),
                })
                .map_err(Error::ComposerError)?;

            let module = composer
                .make_naga_module(NagaModuleDescriptor {
                    source: &shader_source(&file.path)?,
                    file_path: file.path.to_str().unwrap(),
                    shader_type: ShaderType::Wgsl,
                    additional_imports: &[],
                    shader_defs: self.user_shader_defs.clone(),
                })
                .map_err(Error::ComposerError)?;
            definitions.insert(file.name.clone(), module);
        }

        Ok(WgslBindgen {
            preprocessed_files: preprocessed,
            modules: definitions,
            output: self.output.ok_or(Error::MissingField("output"))?,
            separate_files: self.separate_files,

            bg_layout_entry_overrides: HashMap::default(),
        })
    }
}
