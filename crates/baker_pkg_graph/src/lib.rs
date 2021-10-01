use baker_pkg_pb::{Enum, File, Message, Package, PackageGraph};
use idgen::{Id, IdGenerator};
use petgraph::graphmap::DiGraphMap;

/// A graph that represents if a node references another node.
pub type ReferenceGraph<E = ()> = DiGraphMap<Id, E>;

#[derive(Debug)]
pub struct PkgGraph<IdGen> {
    /// The raw package graph.
    ///
    /// Contains all the stored data coming from protobuf files.
    pb: PackageGraph,
    /// Generator used to create all the ids.
    id_generator: IdGen,
    /// A graph storing references between packages.
    pkg_import_graph: ReferenceGraph,
    /// A graph storing imports relation between files.
    files_import_graph: ReferenceGraph,
    /// A graph storing use relation between messages types.
    ///
    /// The edges contains the field that realizes the relation.
    message_use_graph: ReferenceGraph<String>,
}

impl<IG> PkgGraph<IG> {
    /// Construct a new [`PkgGraph`] using `generator` as the [`IdGenerator`].
    pub fn new(generator: IG) -> Self {
        Self {
            pb: PackageGraph::default(),
            id_generator: generator,
            pkg_import_graph: ReferenceGraph::new(),
            files_import_graph: ReferenceGraph::default(),
            message_use_graph: ReferenceGraph::default(),
        }
    }

    /// Get a [`Message`] with the given [`Id`].
    pub fn message(&self, msg_id: Id) -> Option<&Message> {
        self.pb.messages.get(&msg_id)
    }

    /// Get an [`Enum`] with the given [`Id`].
    pub fn enum_(&self, msg_id: Id) -> Option<&Enum> {
        self.pb.enums.get(&msg_id)
    }

    /// Get a [`Message`] or an [`Enum`] with the given [`Id`].
    ///
    /// # Panics
    ///
    /// Panics if `msg_id` isn't declared.
    pub fn type_(&self, msg_id: Id) -> Result<&Message, &Enum> {
        match self.pb.messages.get(&msg_id) {
            Some(msg) => Ok(msg),
            None => match self.pb.enums.get(&msg_id) {
                Some(en) => Err(en),
                None => panic!("unknown id {} used", msg_id),
            },
        }
    }

    /// Get a [`Message`] with the given [`Id`].
    pub fn message_mut(&mut self, msg_id: Id) -> Option<&mut Message> {
        self.pb.messages.get_mut(&msg_id)
    }

    /// Get an [`Enum`] with the given [`Id`].
    pub fn enum_mut(&mut self, msg_id: Id) -> Option<&mut Enum> {
        self.pb.enums.get_mut(&msg_id)
    }

    /// Get an [`File`] with the given [`Id`].
    pub fn file_mut(&mut self, file_id: Id) -> &mut File {
        self.pb.files.get_mut(&file_id).unwrap()
    }

    /// Get an [`File`] with the given [`Id`].
    pub fn pkg_mut(&mut self, pkg_id: Id) -> &mut Package {
        self.pb.packages.get_mut(&pkg_id).unwrap()
    }

    /// Get a [`Message`] or an [`Enum`] with the given [`Id`].
    ///
    /// # Panics
    ///
    /// Panics if `msg_id` isn't declared.
    pub fn type_mut(&mut self, msg_id: Id) -> Result<&mut Message, &mut Enum> {
        match self.pb.messages.get_mut(&msg_id) {
            Some(msg) => Ok(msg),
            None => match self.pb.enums.get_mut(&msg_id) {
                Some(en) => Err(en),
                None => panic!("unknown id {} used", msg_id),
            },
        }
    }
}

impl<IG: IdGenerator> PkgGraph<IG> {
    /// Define a new message with the given _full name_.
    pub fn define_message(&mut self, full_name: String) -> &mut Message {
        let msg = Message {
            id: self.id_generator.generate(),
            name: full_name,
            ..Default::default()
        };

        self.message_use_graph.add_node(msg.id);

        self.pb.messages.entry(msg.id).or_insert(msg)
    }

    /// Define a new enum with the given _full name_.
    pub fn define_enum(&mut self, full_name: String) -> &mut Enum {
        let enum_ = Enum {
            id: self.id_generator.generate(),
            name: full_name,
            ..Default::default()
        };

        self.pb.enums.entry(enum_.id).or_insert(enum_)
    }

    pub fn define_file(&mut self) -> &mut File {
        let file = self
            .pb
            .files
            .entry(self.id_generator.generate())
            .or_insert_with_key(|id| File {
                id: *id,
                ..Default::default()
            });

        self.files_import_graph.add_node(file.id);

        file
    }

    pub fn define_pkg(&mut self, name: String) -> &mut Package {
        self.pb
            .packages
            .entry(self.id_generator.generate())
            .or_insert_with_key(|id| Package {
                id: *id,
                name,
                ..Default::default()
            })
    }
}

impl<IG> PkgGraph<IG> {
    /// Add a reference from `source` to `uses`, saying that the first uses the
    /// later.
    pub fn add_msg_usage_ref(&mut self, source: Id, uses: Id, field: String) {
        self.message_use_graph.add_edge(source, uses, field);
    }
}
