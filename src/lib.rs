//! # Diesel dynamic query ordering
//! This crate provides the [`OrderWithDirectionDsl`] trait for dynamic query ordering;
//! ```
//! #[macro_use] extern crate diesel;
//! use diesel::mysql::Mysql;
//! use diesel::QueryDsl;
//! use diesel_order_with_direction::{OrderWithDirectionDsl, QueryOrderDirection};
//! table! {
//!     posts (id) {
//!         id -> Integer,
//!     }
//! }
//!
//! let direction = QueryOrderDirection::Descending;
//!
//! let query = posts::table
//!     .into_boxed::<Mysql>()
//!     .order_with_dir(direction, posts::id)
//!     .limit(5);
//!
//! let query = posts::table
//!     .into_boxed::<Mysql>()
//!     .then_order_by_with_dir(direction, posts::id)
//!     .limit(5);
//! ```

use diesel::dsl::{Asc, Desc};
use diesel::query_dsl::methods::{OrderDsl, ThenOrderDsl};
use diesel::ExpressionMethods;
use std::fmt;

/// The direction of the query ordering.
///
/// Implements [`Display`](fmt::Display)
///
/// ```
/// # use diesel_order_with_direction::QueryOrderDirection;
/// assert_eq!(&QueryOrderDirection::Ascending.to_string(), "ASC");
/// assert_eq!(&QueryOrderDirection::Descending.to_string(), "DESC");
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum QueryOrderDirection {
    Ascending,
    Descending,
}

impl Default for QueryOrderDirection {
    fn default() -> Self {
        Self::Ascending
    }
}

impl fmt::Display for QueryOrderDirection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ascending => write!(f, "ASC"),
            Self::Descending => write!(f, "DESC"),
        }
    }
}

impl QueryOrderDirection {
    /// Append a [`QueryDsl::order`](diesel::QueryDsl::order) to the query. Same as
    /// [`query.order_with_dir(self, expr)`](OrderWithDirectionDsl::order_with_dir)
    pub fn order_query<Expr, Q>(&self, query: Q, expr: Expr) -> OrderWithDirection<Q, Expr>
    where
        Expr: ExpressionMethods,
        Q: OrderDsl<Asc<Expr>> + OrderDsl<Desc<Expr>, Output = <Q as OrderDsl<Asc<Expr>>>::Output>,
    {
        match self {
            Self::Ascending => query.order(expr.asc()),
            Self::Descending => query.order(expr.desc()),
        }
    }

    /// Append a [`QueryDsl::then_order_by`](diesel::QueryDsl::then_order_by) to the query. Same as
    /// [`query.then_order_by_with_dir(self, expr)`](OrderWithDirectionDsl::then_order_by_with_dir)
    pub fn then_order_by_query<Expr, Q>(
        &self,
        query: Q,
        expr: Expr,
    ) -> ThenOrderByWithDirection<Q, Expr>
    where
        Expr: ExpressionMethods,
        Q: ThenOrderDsl<Asc<Expr>>
            + ThenOrderDsl<Desc<Expr>, Output = <Q as ThenOrderDsl<Asc<Expr>>>::Output>,
    {
        match self {
            Self::Ascending => query.then_order_by(expr.asc()),
            Self::Descending => query.then_order_by(expr.desc()),
        }
    }
}

/// Represent the return type of [`.order_with_dir`](`OrderWithDirectionDsl::order_with_dir`)
pub type OrderWithDirection<Q, Expr> = <Q as OrderDsl<Asc<Expr>>>::Output;
/// Represent the return type of [`.then_order_by_with_dir`](`OrderWithDirectionDsl::then_order_by_with_dir`)
pub type ThenOrderByWithDirection<Q, Expr> = <Q as ThenOrderDsl<Asc<Expr>>>::Output;

pub trait OrderWithDirectionDsl: Sized {
    /// Wrapper around [`QueryDsl::order`](diesel::QueryDsl::order) with an extra [`QueryOrderDirection`] argument to
    /// dynamically determine the ordering direction. Only works with boxed queries.
    fn order_with_dir<Expr>(
        self,
        direction: QueryOrderDirection,
        expr: Expr,
    ) -> OrderWithDirection<Self, Expr>
    where
        Expr: ExpressionMethods,
        Self: OrderDsl<Asc<Expr>>
            + OrderDsl<Desc<Expr>, Output = <Self as OrderDsl<Asc<Expr>>>::Output>,
    {
        direction.order_query(self, expr)
    }

    /// Alias for `DynamicQueryOrderDsl::order_with_dir`
    fn order_by_with_dir<Expr>(
        self,
        order: QueryOrderDirection,
        expr: Expr,
    ) -> OrderWithDirection<Self, Expr>
    where
        Expr: ExpressionMethods,
        Self: OrderDsl<Asc<Expr>>
            + OrderDsl<Desc<Expr>, Output = <Self as OrderDsl<Asc<Expr>>>::Output>,
    {
        self.order_with_dir(order, expr)
    }

    /// Wrapper around [`QueryDsl::then_order_by`](diesel::QueryDsl::then_order_by) with an extra [`QueryOrderDirection`] argument to
    /// dynamically determine the ordering direction. Only works with boxed queries.
    fn then_order_by_with_dir<Expr>(
        self,
        direction: QueryOrderDirection,
        expr: Expr,
    ) -> ThenOrderByWithDirection<Self, Expr>
    where
        Expr: ExpressionMethods,
        Self: ThenOrderDsl<Asc<Expr>>
            + ThenOrderDsl<Desc<Expr>, Output = <Self as ThenOrderDsl<Asc<Expr>>>::Output>,
    {
        direction.then_order_by_query(self, expr)
    }
}

impl<Q> OrderWithDirectionDsl for Q {}
