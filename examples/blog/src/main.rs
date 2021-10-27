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

#[path = "./model/blog.api.v1.rs"]
pub mod blog;

pub mod repositories;
pub mod service;

pub type Error = InternalError<Box<dyn StdError>>;

pub type Result<T, E = Error> = std::result::Result<T, E>;

fn main() {}
