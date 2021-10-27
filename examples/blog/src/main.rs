#[macro_use]
extern crate diesel;

use std::error::Error as StdError;

use actix_web::error::InternalError;

pub mod schema {
    table! {
        users (user_id) {
            user_id -> BigInt,
            name -> Text,
            email -> Nullable<Text>,
            username -> Nullable<Text>,
        }
    }

    table! {
        posts (post_id) {
            post_id -> BigInt,
            title -> Text,
            likes -> Integer,
            dislikes -> Integer,
            author_id -> BigInt,
            content -> Text,
            content_format -> crate::blog::post::ContentFormatSql,
        }
    }

    table! {
        comments (comment_id) {
            comment_id -> BigInt,
            post_id -> BigInt,
            author_id -> BigInt,
            content -> Text,
        }
    }
}

pub mod blog {
    use diesel::{helper_types::Nullable, NullableExpressionMethods};

    include!("./model/blog.api.v1.rs");

    pub type UserColumns = (
        __table_users::name,
        (
            Nullable<__table_users::email>,
            Nullable<__table_users::username>,
        ),
        __table_users::user_id,
    );

    impl User {
        pub fn columns() -> UserColumns {
            (
                __table_users::name,
                (
                    __table_users::email.nullable(),
                    __table_users::username.nullable(),
                ),
                __table_users::user_id,
            )
        }
    }

    pub type PostColumns = (
        __table_posts::content,
        __table_posts::title,
        __table_posts::likes,
        __table_posts::dislikes,
        __table_posts::author_id,
        __table_posts::content_format,
        __table_posts::post_id,
    );

    impl Post {
        pub fn columns() -> PostColumns {
            Default::default()
        }
    }

    pub type CommentColumns = (
        crate::schema::comments::post_id,
        crate::schema::comments::content,
        crate::schema::comments::comment_id,
        crate::schema::comments::author_id,
    );

    impl post::Comment {
        pub fn columns() -> CommentColumns {
            Default::default()
        }
    }
}

pub mod repositories;
pub mod service;

pub type Error = InternalError<Box<dyn StdError>>;

pub type Result<T, E = Error> = std::result::Result<T, E>;

fn main() {}
