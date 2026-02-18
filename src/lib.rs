#![deny(missing_docs)]
#![deny(unused_crate_dependencies)]

//! # gemau
//!
//! The first seed of what will hopefully be a computer algebra system for CGT.
//!
//! The current goal of this crate is to provide first-class support for partizan misère research.
//! At present, it contains functionality for studying dead ends. In the long term, this crate will
//! also aim to serve more traditional use cases like normal play.
//!
//! If you want the crate to be finished quicker, then you could consider contributing. :)
//! # Example
//!
//! ```
//! # use gemau::DeadEnd;
//! let g = DeadEnd::waiting(4);
//! let h = g + 2;
//!
//! // factors are 0, 1, 2, W_4, 1 + W_4, 2 + W_4
//! assert_eq!(h.factors().len(), 6);
//!
//! let k = DeadEnd::with_options(3..7);
//! assert_eq!(k.flex(), 1);
//! assert_eq!(k.race(), 4);
//! assert_eq!(k.birth(), 7);
//!
//! let j = DeadEnd::waiting(3);
//! let k = k + j;
//! assert_eq!(k.flex(), 9);
//! assert_eq!(k.race(), 5);
//! assert_eq!(k.birth(), 10);
//! ```

mod dead_end;

pub use dead_end::*;
