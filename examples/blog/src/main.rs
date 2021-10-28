#[macro_use]
extern crate diesel;

use std::{
    error::Error as StdError,
    net::{IpAddr, Ipv4Addr},
    sync::Arc,
};

use actix_web::error::InternalError;

use crate::{blog::BlobService, service::*};

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
        use diesel::sql_types::*;
        use crate::blog::post::ContentFormatSql;

        posts (post_id) {
            post_id -> BigInt,
            title -> Text,
            likes -> Integer,
            dislikes -> Integer,
            author_id -> BigInt,
            content -> Text,
            content_format -> ContentFormatSql,
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

#[path = "./blog.api.v1.rs"]
pub mod blog;

pub mod repositories;
pub mod service;

pub type Error = InternalError<Box<dyn StdError + Send + Sync>>;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[actix_web::main]
async fn main() {
    let db_url = std::env::var("DATABASE_URL").unwrap();
    let pool: DbPool = diesel::r2d2::Pool::builder()
        .build(diesel::r2d2::ConnectionManager::new(&db_url))
        .unwrap();

    let service = Arc::new(BlobServiceImpl::new(pool));

    actix_web::HttpServer::new(move || {
        actix_web::App::new().configure(|cfg| service.clone().configure(cfg))
    })
    .bind((IpAddr::V4(Ipv4Addr::UNSPECIFIED), 8080))
    .unwrap()
    .run()
    .await
    .unwrap()
}
