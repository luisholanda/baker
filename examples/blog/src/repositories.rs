use actix_web::http::StatusCode;
use diesel::{pg::PgConnection, prelude::*};

use crate::{
    blog::{post::Comment, Post, User},
    schema::*,
    Error, Result,
};

pub struct PaginationParams {
    pub after: Option<i64>,
    pub limit: u32,
}

/// Repository for the blog implementation.
pub trait BlogRepository {
    /// List a page of users.
    fn list_users(&self, page_params: PaginationParams) -> Result<Vec<User>>;

    /// List a page of posts.
    fn list_posts(&self, page_params: PaginationParams) -> Result<Vec<Post>>;

    /// List a page of comments of a post.
    fn list_post_comments(&self, post: i64, page_params: PaginationParams) -> Result<Vec<Comment>>;

    /// Create the given user.
    fn create_user(&self, user: User) -> Result<User>;

    /// Create the given post.
    fn create_post(&self, post: Post) -> Result<Post>;

    /// Create a comment in a post.
    fn create_comment(&self, comment: Comment) -> Result<Comment>;

    /// Like a specific post.
    fn like_post(&self, post: i64) -> Result<Post>;

    /// Dislike a specific post.
    fn dislike_post(&self, post: i64) -> Result<Post>;
}

impl BlogRepository for PgConnection {
    fn list_users(&self, page_params: PaginationParams) -> Result<Vec<User>> {
        let users = match (page_params.after, page_params.limit) {
            (Some(a), 0) => users::table
                .filter(users::user_id.gt(a))
                .select(User::columns())
                .get_results(self),
            (Some(a), l) => users::table
                .filter(users::user_id.gt(a))
                .limit(l as i64)
                .select(User::columns())
                .get_results(self),
            (None, 0) => users::table.select(User::columns()).get_results(self),
            (None, l) => users::table
                .limit(l as i64)
                .select(User::columns())
                .get_results(self),
        };

        users.map_err(|e| Error::new(e.into(), StatusCode::INTERNAL_SERVER_ERROR))
    }

    fn list_posts(&self, page_params: PaginationParams) -> Result<Vec<Post>> {
        let base_query = posts::table.select(Post::columns());

        let posts = match (page_params.after, page_params.limit) {
            (Some(a), 0) => base_query.filter(posts::post_id.gt(a)).get_results(self),
            (Some(a), l) => base_query
                .filter(posts::post_id.gt(a))
                .limit(l as i64)
                .get_results(self),
            (None, 0) => base_query.get_results(self),
            (None, l) => base_query.limit(l as i64).get_results(self),
        };

        posts.map_err(|e| Error::new(e.into(), StatusCode::INTERNAL_SERVER_ERROR))
    }

    fn list_post_comments(&self, post: i64, page_params: PaginationParams) -> Result<Vec<Comment>> {
        let base_query = comments::table
            .select(Comment::columns())
            .filter(comments::post_id.eq(post));

        let comments = match (page_params.after, page_params.limit) {
            (Some(a), 0) => base_query
                .filter(comments::comment_id.gt(a))
                .get_results(self),
            (Some(a), l) => base_query
                .filter(comments::comment_id.gt(a))
                .limit(l as i64)
                .get_results(self),
            (None, 0) => base_query.get_results(self),
            (None, l) => base_query.limit(l as i64).get_results(self),
        };

        comments.map_err(|e| Error::new(e.into(), StatusCode::INTERNAL_SERVER_ERROR))
    }

    fn create_user(&self, user: User) -> Result<User> {
        todo!()
    }

    fn create_post(&self, mut post: Post) -> Result<Post> {
        let post_id = diesel::insert_into(posts::table)
            .values((
                posts::title.eq(&post.title),
                posts::author_id.eq(post.author_id),
                posts::content.eq(&post.content),
                posts::content_format.eq(post.content_format),
            ))
            .returning(posts::post_id)
            .get_result(self)
            .map_err(|e| Error::new(e.into(), StatusCode::INTERNAL_SERVER_ERROR))?;

        post.likes = 0;
        post.dislikes = 0;
        post.id = post_id;

        Ok(post)
    }

    fn create_comment(&self, mut comment: Comment) -> Result<Comment> {
        comment.id = diesel::insert_into(comments::table)
            .values((
                comments::post_id.eq(comment.post_id),
                comments::author_id.eq(comment.author_id),
                comments::content.eq(&comment.content),
            ))
            .returning(comments::comment_id)
            .get_result(self)
            .map_err(|e| Error::new(e.into(), StatusCode::INTERNAL_SERVER_ERROR))?;

        Ok(comment)
    }

    fn like_post(&self, post: i64) -> Result<Post> {
        diesel::update(posts::table)
            .filter(posts::post_id.eq(post))
            .set(posts::likes.eq(posts::likes + 1))
            .returning(Post::columns())
            .get_result(self)
            .map_err(|e| Error::new(e.into(), StatusCode::INTERNAL_SERVER_ERROR))
    }

    fn dislike_post(&self, post: i64) -> Result<Post> {
        diesel::update(posts::table)
            .filter(posts::post_id.eq(post))
            .set(posts::dislikes.eq(posts::dislikes + 1))
            .returning(Post::columns())
            .get_result(self)
            .map_err(|e| Error::new(e.into(), StatusCode::INTERNAL_SERVER_ERROR))
    }
}
