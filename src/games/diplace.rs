use rand::SeedableRng;
use rand::distr::Bernoulli;
use rand::prelude::*;
use rand::rngs::SmallRng;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use crate::ShortOwned;
use crate::ShortPartizanOwned;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Diplace {
    /// Number of vertices (up to 64 due to bit field limitations)
    n: u16,
    /// Bit field for blue vertices
    blue: u64,
    /// Bit field for red vertices
    red: u64,
    /// Each u64 represents edges for one vertex
    edges: Vec<u64>,
}

impl Diplace {
    pub fn new(n: u16) -> Self {
        assert!(n <= 64, "Currently limited to 64 vertices");
        Diplace {
            n,
            blue: 0,
            red: 0,
            edges: vec![0; n as usize],
        }
    }

    fn is_blue(&self, vertex: u16) -> bool {
        (self.blue & (1 << vertex)) != 0
    }

    fn is_red(&self, vertex: u16) -> bool {
        (self.red & (1 << vertex)) != 0
    }

    fn is_green(&self, vertex: u16) -> bool {
        self.is_blue(vertex) && self.is_red(vertex)
    }

    pub fn set_blue(&mut self, vertex: u16) {
        assert!(vertex < self.n, "Vertex index out of bounds");
        self.blue |= 1 << vertex;
    }

    pub fn set_red(&mut self, vertex: u16) {
        assert!(vertex < self.n, "Vertex index out of bounds");
        self.red |= 1 << vertex;
    }

    pub fn set_green(&mut self, vertex: u16) {
        assert!(vertex < self.n, "Vertex index out of bounds");
        self.blue |= 1 << vertex;
        self.red |= 1 << vertex;
    }

    pub fn clear_colours(&mut self, vertex: u16) {
        assert!(vertex < self.n, "Vertex index out of bounds");
        self.blue &= !(1 << vertex);
        self.red &= !(1 << vertex);
    }

    // Edge operations
    pub fn add_edge(&mut self, from: u16, to: u16) {
        assert!(from < self.n && to < self.n, "Vertex index out of bounds");
        self.edges[from as usize] |= 1 << to;
    }

    pub fn remove_edge(&mut self, from: u16, to: u16) {
        assert!(from < self.n && to < self.n, "Vertex index out of bounds");
        self.edges[from as usize] &= !(1 << to);
    }

    fn closed_neighborhood_mask(&self, vertex: u16) -> u64 {
        let vertex_mask = 1u64 << vertex;
        self.edges[vertex as usize] | vertex_mask
    }

    fn remove_vertices(&self, remove_mask: u64) -> Self {
        let existing_vertices = self.blue | self.red;
        let vertices_to_remove = remove_mask & existing_vertices;
        let keep_mask = !vertices_to_remove;

        let remaining_vertices = existing_vertices & keep_mask;
        let new_n = remaining_vertices.count_ones() as u16;

        let mut new_indices = vec![0u16; self.n as usize];
        let mut next_index = 0;
        for i in 0..self.n {
            if (remaining_vertices & (1 << i)) != 0 {
                new_indices[i as usize] = next_index;
                next_index += 1;
            }
        }

        let mut new_edges = vec![0u64; new_n as usize];
        let mut new_idx = 0;
        for i in 0..self.n {
            if (existing_vertices & (1 << i)) != 0 && (keep_mask & (1 << i)) != 0 {
                let mut new_edge_mask = 0u64;
                let old_edges = self.edges[i as usize];
                for j in 0..self.n {
                    if (old_edges & (1 << j)) != 0 && (keep_mask & (1 << j)) != 0 {
                        new_edge_mask |= 1 << new_indices[j as usize];
                    }
                }
                new_edges[new_idx] = new_edge_mask;
                new_idx += 1;
            }
        }

        let mut new_blue = 0u64;
        let mut new_red = 0u64;
        for i in 0..self.n {
            if (existing_vertices & (1 << i)) != 0 && (keep_mask & (1 << i)) != 0 {
                let new_i = new_indices[i as usize];
                if self.is_blue(i) {
                    new_blue |= 1 << new_i;
                }
                if self.is_red(i) {
                    new_red |= 1 << new_i;
                }
            }
        }

        Diplace {
            n: new_n,
            blue: new_blue,
            red: new_red,
            edges: new_edges,
        }
    }
}

impl ShortOwned for Diplace {
    fn options(&self) -> impl IntoIterator<Item = Self> {
        self.left()
            .into_iter()
            .chain(self.right())
            .collect::<Vec<_>>()
    }
}

impl ShortPartizanOwned for Diplace {
    fn left(&self) -> impl IntoIterator<Item = Self> {
        let mut moves = Vec::new();

        for v in 0..self.n {
            if self.is_blue(v) {
                let remove_mask = self.closed_neighborhood_mask(v);
                moves.push(self.remove_vertices(remove_mask));
            }
        }

        moves
    }

    fn right(&self) -> impl IntoIterator<Item = Self> {
        let mut moves = Vec::new();

        for v in 0..self.n {
            if self.is_red(v) {
                let remove_mask = self.closed_neighborhood_mask(v);
                moves.push(self.remove_vertices(remove_mask));
            }
        }

        moves
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RedBlueDiplace {
    /// Number of vertices (up to 64)
    n: u16,
    /// Bit field for colours (1 = blue, 0 = red)
    colours: u64,
    /// Each u64 represents edges for one vertex
    edges: Vec<u64>,
}

impl RedBlueDiplace {
    #[must_use]
    pub fn new(n: u16) -> Self {
        assert!(n <= 64, "Currently limited to 64 vertices");
        RedBlueDiplace {
            n,
            colours: 0, // All vertices start red
            edges: vec![0; n as usize],
        }
    }

    #[must_use]
    pub fn conjugate(&self) -> Self {
        let vertex_mask = (1u64 << self.n) - 1;
        Self {
            n: self.n,
            colours: (!self.colours) & vertex_mask,
            edges: self.edges.clone(),
        }
    }

    #[must_use]
    pub fn fast_random_with<R: Rng>(n: u16, rng: &mut R) -> Self {
        debug_assert!(n <= 64, "Number of vertices must be <= 64");

        let mut graph = Self::new(n);
        let vertex_mask = (1u64 << n) - 1;

        for i in 0..n {
            let edge_mask = rng.random::<u64>() & !(1u64 << i) & vertex_mask;

            graph.edges[i as usize] = edge_mask;
        }

        graph.colours = rng.random::<u64>() & vertex_mask;

        graph
    }

    #[must_use]
    pub fn create_edge_distribution(edge_prob: f64) -> Bernoulli {
        assert!(
            (0.0..=1.0).contains(&edge_prob),
            "Edge probability must be between 0.0 and 1.0"
        );
        Bernoulli::new(edge_prob).unwrap()
    }

    #[must_use]
    pub fn vertices(&self) -> u16 {
        self.n
    }

    #[must_use]
    pub fn blue(&self) -> u32 {
        (self.colours & ((1u64 << self.n) - 1)).count_ones()
    }

    #[must_use]
    pub fn red(&self) -> u32 {
        u32::from(self.n) - self.blue()
    }

    #[must_use]
    pub fn fast_random_with_edge_prob<R: Rng>(
        n: u16,
        blue_count: u16,
        edge_dist: &Bernoulli,
        rng: &mut R,
    ) -> Self {
        debug_assert!(n <= 64, "Number of vertices must be <= 64");
        debug_assert!(blue_count <= n, "Blue count cannot exceed total vertices");

        let mut graph = Self::new(n);
        //let vertex_mask = (1u64 << n) - 1;

        for i in 0..n {
            let mut edge_mask = 0u64;
            let mut current_mask = 1u64;

            for j in 0..n {
                if i != j && rng.sample(edge_dist) {
                    edge_mask |= current_mask;
                }
                current_mask <<= 1;
            }

            graph.edges[i as usize] = edge_mask;
        }

        graph.colours = (1u64 << blue_count) - 1;

        graph
    }

    #[must_use]
    pub fn fast_random_with_blue_count<R: Rng>(n: u16, blue_count: u16, rng: &mut R) -> Self {
        debug_assert!(n <= 64, "Number of vertices must be <= 64");
        debug_assert!(blue_count <= n, "Blue count cannot exceed total vertices");

        let mut graph = Self::new(n);
        let vertex_mask = (1u64 << n) - 1;

        for i in 0..n {
            let edge_mask = rng.random::<u64>() & !(1u64 << i) & vertex_mask;

            graph.edges[i as usize] = edge_mask;
        }

        graph.colours = (1u64 << blue_count) - 1;

        graph
    }

    #[must_use]
    pub fn create_fast_rng() -> SmallRng {
        SmallRng::from_rng(&mut rand::rng())
    }

    #[must_use]
    pub fn create_seeded_rng(seed: u64) -> SmallRng {
        SmallRng::seed_from_u64(seed)
    }

    pub fn set_blue(&mut self, vertex: u16) {
        assert!(vertex < self.n, "Vertex index out of bounds");
        self.colours |= 1 << vertex;
    }

    pub fn set_red(&mut self, vertex: u16) {
        assert!(vertex < self.n, "Vertex index out of bounds");
        self.colours &= !(1 << vertex);
    }

    pub fn add_edge(&mut self, from: u16, to: u16) {
        assert!(from < self.n && to < self.n, "Vertex index out of bounds");
        self.edges[from as usize] |= 1 << to;
    }

    #[must_use]
    pub fn is_edge(&self, from: u16, to: u16) -> bool {
        assert!(from < self.n && to < self.n, "Vertex index out of bounds");
        self.edges[from as usize] & (1 << to) != 0
    }

    #[must_use]
    pub fn edge_count(&self) -> u32 {
        self.edges.iter().map(|e| e.count_ones()).sum()
    }

    pub fn remove_edge(&mut self, from: u16, to: u16) {
        assert!(from < self.n && to < self.n, "Vertex index out of bounds");
        self.edges[from as usize] &= !(1 << to);
    }

    #[must_use]
    pub fn is_blue(&self, vertex: u16) -> bool {
        assert!(vertex < self.n);
        (self.colours & (1 << vertex)) != 0
    }

    #[must_use]
    pub fn is_red(&self, vertex: u16) -> bool {
        assert!(vertex < self.n);
        (self.colours & (1 << vertex)) == 0
    }

    #[must_use]
    fn closed_neighborhood_mask(&self, vertex: u16) -> u64 {
        let vertex_mask = 1u64 << vertex;
        self.edges[vertex as usize] | vertex_mask
    }

    #[must_use]
    fn remove_vertices(&self, remove_mask: u64) -> Self {
        let vertex_mask = (1u64 << self.n) - 1;
        let vertices_to_remove = remove_mask & vertex_mask;
        let keep_mask = (!vertices_to_remove) & vertex_mask;
        let new_n = keep_mask.count_ones() as u16;

        assert!(new_n < self.n, "Number of vertices must decrease!");

        let mut new_indices = vec![0u16; self.n as usize];
        let mut next_index = 0;
        for i in 0..self.n {
            if (keep_mask & (1 << i)) != 0 {
                new_indices[i as usize] = next_index;
                next_index += 1;
            }
        }

        let mut new_edges = vec![0u64; new_n as usize];
        let mut new_idx = 0;
        for i in 0..self.n {
            if (keep_mask & (1 << i)) != 0 {
                let mut new_edge_mask = 0u64;
                let old_edges = self.edges[i as usize];
                for j in 0..self.n {
                    if (old_edges & (1 << j)) != 0 && (keep_mask & (1 << j)) != 0 {
                        new_edge_mask |= 1 << new_indices[j as usize];
                    }
                }
                new_edges[new_idx] = new_edge_mask;
                new_idx += 1;
            }
        }

        let mut new_colours = 0u64;
        for i in 0..self.n {
            if (keep_mask & (1 << i)) != 0 {
                let new_i = new_indices[i as usize];
                if self.is_blue(i) {
                    new_colours |= 1 << new_i;
                }
            }
        }

        RedBlueDiplace {
            n: new_n,
            colours: new_colours,
            edges: new_edges,
        }
    }

    pub fn from_digraph6(input: &str) -> Result<Self, Digraph6Error> {
        // Skip header if present
        let input = if input.starts_with(">>digraph6<<") {
            &input[11..]
        } else {
            input
        };

        // Check for '&' prefix
        if !input.starts_with('&') {
            return Err(Digraph6Error::InvalidPrefix);
        }

        // Get bytes after '&'
        let bytes: Vec<u8> = input.bytes().skip(1).collect();
        if bytes.is_empty() {
            // Want to return empty game here
            return Ok(Self::new(0));
            //return Err(Digraph6Error::InvalidLength);
        }

        let (n, n_bytes_used) = Self::parse_vertex_count(&bytes)?;

        let matrix_bits = n * n;
        let matrix_bytes = (matrix_bits + 5) / 6;

        if bytes.len() < n_bytes_used + matrix_bytes {
            return Err(Digraph6Error::InvalidLength);
        }

        let matrix_bytes = &bytes[n_bytes_used..n_bytes_used + matrix_bytes];
        let bits = Self::decode_matrix(matrix_bytes, n * n)?;

        let matrix = Self::bits_to_matrix(bits, n);

        Ok(matrix)
    }

    fn parse_vertex_count(bytes: &[u8]) -> Result<(usize, usize), Digraph6Error> {
        if bytes.is_empty() {
            return Err(Digraph6Error::InvalidVertexCount);
        }

        let first_byte = bytes[0];

        if first_byte <= 125 {
            return Ok(((first_byte - 63) as usize, 1));
        }

        if first_byte == 126 && bytes.len() >= 4 {
            let mut n = 0usize;
            for &b in &bytes[1..4] {
                if !(63..=126).contains(&b) {
                    return Err(Digraph6Error::InvalidEncoding);
                }
                n = (n << 6) | ((b - 63) as usize);
            }
            return Ok((n, 4));
        }

        if first_byte == 126 && bytes.get(1) == Some(&126) && bytes.len() >= 8 {
            let mut n = 0usize;
            for &b in &bytes[2..8] {
                if !(63..=126).contains(&b) {
                    return Err(Digraph6Error::InvalidEncoding);
                }
                n = (n << 6) | ((b - 63) as usize);
            }
            return Ok((n, 8));
        }

        Err(Digraph6Error::InvalidVertexCount)
    }

    fn decode_matrix(bytes: &[u8], expected_bits: usize) -> Result<Vec<bool>, Digraph6Error> {
        let mut bits = Vec::with_capacity(expected_bits);

        for &byte in bytes {
            if !(63..=126).contains(&byte) {
                return Err(Digraph6Error::InvalidEncoding);
            }

            let value = byte - 63;
            for i in (0..6).rev() {
                bits.push(value & (1 << i) != 0);
                if bits.len() == expected_bits {
                    return Ok(bits);
                }
            }
        }

        bits.truncate(expected_bits);

        if bits.len() != expected_bits {
            return Err(Digraph6Error::InvalidLength);
        }

        Ok(bits)
    }

    #[must_use]
    fn bits_to_matrix(bits: Vec<bool>, n: usize) -> Self {
        let n = n as u16;

        let mut g = Self::new(n);

        for i in 0..n {
            for j in 0..n {
                if bits[(i * n + j) as usize] {
                    if i == j {
                        g.set_blue(i);
                    } else {
                        g.add_edge(i, j);
                    }
                }
            }
        }

        g
    }

    pub fn write_dot<W: std::fmt::Write>(&self, writer: &mut W) -> std::fmt::Result {
        writeln!(writer, "digraph {{")?;

        writeln!(
            writer,
            "  node [shape=circle, width=0.2, height=0.2, fixedsize=true, label=\"\"];"
        )?;
        writeln!(writer, "  edge [arrowsize=0.5];")?;

        for v in 0..self.n {
            let colour = if self.is_blue(v) {
                "#0000FF80"
            } else {
                "#FF000080"
            };
            writeln!(writer, "  {v} [style=filled, fillcolor=\"{colour}\"];")?;
        }

        for v in 0..self.n {
            let edges = self.edges[v as usize];
            for u in 0..self.n {
                if (edges & (1 << u)) != 0 {
                    writeln!(writer, "  {v} -> {u};")?;
                }
            }
        }

        writeln!(writer, "}}")
    }

    #[must_use]
    pub fn to_dot(&self) -> String {
        let mut output = String::new();
        self.write_dot(&mut output)
            .expect("Writing to string shouldn't fail");
        output
    }
}

impl ShortOwned for RedBlueDiplace {
    fn options(&self) -> impl IntoIterator<Item = Self> {
        self.left()
            .into_iter()
            .chain(self.right())
            .collect::<Vec<_>>()
    }
}

impl ShortPartizanOwned for RedBlueDiplace {
    fn left(&self) -> impl IntoIterator<Item = Self> {
        let mut moves = Vec::new();

        for v in 0..self.n {
            if self.is_blue(v) {
                let remove_mask = self.closed_neighborhood_mask(v);
                moves.push(self.remove_vertices(remove_mask));
            }
        }

        moves
    }

    fn right(&self) -> impl IntoIterator<Item = Self> {
        let mut moves = Vec::new();

        for v in 0..self.n {
            if self.is_red(v) {
                let remove_mask = self.closed_neighborhood_mask(v);
                moves.push(self.remove_vertices(remove_mask));
            }
        }

        moves
    }
}

#[derive(Debug, Error)]
pub enum Digraph6Error {
    #[error("invalid prefix")]
    InvalidPrefix,
    #[error("invalid length")]
    InvalidLength,
    #[error("invalid encoding")]
    InvalidEncoding,
    #[error("invalid vertex count")]
    InvalidVertexCount,
}

use std::io;

impl From<Digraph6Error> for io::Error {
    fn from(error: Digraph6Error) -> Self {
        match error {
            Digraph6Error::InvalidPrefix => {
                io::Error::new(io::ErrorKind::InvalidData, "Invalid digraph6 prefix")
            }
            Digraph6Error::InvalidLength => {
                io::Error::new(io::ErrorKind::InvalidData, "Invalid digraph6 length")
            }
            Digraph6Error::InvalidEncoding => {
                io::Error::new(io::ErrorKind::InvalidData, "Invalid digraph6 encoding")
            }
            Digraph6Error::InvalidVertexCount => io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid vertex count in digraph6",
            ),
        }
    }
}
