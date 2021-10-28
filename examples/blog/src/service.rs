use std::ops::Deref;

use actix_web::{error::Error, http::StatusCode, HttpRequest};
use diesel::{
    pg::PgConnection,
    r2d2::{ConnectionManager, Pool},
};

use crate::{
    blog::{post::Comment, *},
    repositories::*,
    Error as ApiError,
};

pub type DbPool = Pool<ConnectionManager<PgConnection>>;

pub struct BlobServiceImpl {
    // This should be a pool, but it is only an example, so let it be.
    pool: DbPool,
}

impl BlobServiceImpl {
    pub fn new(pool: DbPool) -> Self {
        Self { pool }
    }

    fn conn(&self) -> Result<impl Deref<Target = PgConnection>, ApiError> {
        self.pool
            .get()
            .map_err(|e| ApiError::new(e.into(), StatusCode::SERVICE_UNAVAILABLE))
    }
}

#[async_trait::async_trait(?Send)]
impl BlobService for BlobServiceImpl {
    async fn list_users(
        &self,
        _req: ListUsersRequest,
        _http_req: HttpRequest,
    ) -> Result<ListUsersResponse, Error> {
        let users = self.conn()?.list_users(PaginationParams::default())?;

        Ok(ListUsersResponse { users })
    }

    async fn list_posts(
        &self,
        req: ListPostsRequest,
        _http_req: HttpRequest,
    ) -> Result<ListPostsResponse, Error> {
        let params = PaginationParams {
            after: req.after,
            limit: req.limit.unwrap_or_default(),
        };

        let posts = self.conn()?.list_posts(params)?;

        Ok(ListPostsResponse { posts })
    }

    async fn list_post_comments(
        &self,
        req: ListPostCommentsRequest,
        _http_req: HttpRequest,
    ) -> Result<ListPostCommentsResponse, Error> {
        let params = PaginationParams {
            after: req.after,
            limit: req.limit.unwrap_or_default(),
        };

        let comments = self.conn()?.list_post_comments(req.post, params)?;

        Ok(ListPostCommentsResponse { comments })
    }

    async fn create_post(
        &self,
        mut req: CreatePostRequest,
        _http_req: HttpRequest,
    ) -> Result<Post, Error> {
        let conn = self.conn()?;

        let author = conn.create_user(req.author)?;
        req.post.author_id = author.id;

        Ok(conn.create_post(req.post)?)
    }

    async fn post_comment(
        &self,
        mut req: PostCommentRequest,
        _http_req: HttpRequest,
    ) -> Result<Comment, Error> {
        let conn = self.conn()?;

        let author = conn.create_user(req.author)?;
        req.comment.author_id = author.id;

        Ok(conn.create_comment(req.comment)?)
    }

    async fn like_post(&self, req: LikePostRequest, _http_req: HttpRequest) -> Result<Post, Error> {
        let post = if req.dislike {
            self.conn()?.dislike_post(req.post)?
        } else {
            self.conn()?.like_post(req.post)?
        };

        Ok(post)
    }
}
