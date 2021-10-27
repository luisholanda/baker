use actix_web::{error::Error, HttpRequest};
use diesel::{
    pg::PgConnection,
    r2d2::{ConnectionManager, Pool},
};

use crate::blog::{post::Comment, *};

pub type DbPool = Pool<ConnectionManager<PgConnection>>;

pub struct BlobServiceImpl {
    // This should be a pool, but it is only an example, so let it be.
    pool: DbPool,
}

impl BlobServiceImpl {
    pub fn new(pool: DbPool) -> Self {
        Self { pool }
    }
}

#[async_trait::async_trait]
impl BlobService for BlobServiceImpl {
    async fn list_users(
        &self,
        req: ListUsersRequest,
        http_req: HttpRequest,
    ) -> Result<ListUsersResponse, Error> {
        todo!()
    }

    async fn list_posts(
        &self,
        req: ListPostsRequest,
        http_req: HttpRequest,
    ) -> Result<ListPostsResponse, Error> {
        todo!()
    }

    async fn list_post_comments(
        &self,
        req: ListPostCommentsRequest,
        http_req: HttpRequest,
    ) -> Result<ListPostCommentsResponse, Error> {
        todo!()
    }

    async fn create_post(
        &self,
        req: CreatePostRequest,
        http_req: HttpRequest,
    ) -> Result<Post, Error> {
        todo!()
    }

    async fn post_comment(
        &self,
        req: PostCommentRequest,
        http_req: HttpRequest,
    ) -> Result<Comment, Error> {
        todo!()
    }

    async fn like_post(&self, req: LikePostRequest, http_req: HttpRequest) -> Result<Post, Error> {
        todo!()
    }
}
