use std::error::Error;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Post {
    #[serde(rename(serialize = "userId", deserialize = "userId"))]
    user_id: i32,
    id: i32,
    title: String,
    body: String,
}

const BASE_URL: &str = "http://jsonplaceholder.typicode.com/";

async fn get_post_as_string() -> Result<String, Box<dyn Error>> {
    let url = format!("{}posts/1", BASE_URL);
    let s = reqwest::get(&url).await?.text().await?;
    Ok(s)
}

async fn get_post_as_json() -> Result<Post, Box<dyn Error>> {
    let url = format!("{}posts/1", BASE_URL);
    let v: Post = reqwest::get(&url).await?.json().await?;
    Ok(v)
}

async fn get_posts() -> Result<Vec<Post>, Box<dyn Error>> {
    let url = format!("{}posts", BASE_URL);
    let v: Vec<Post> = reqwest::get(&url).await?.json().await?;
    let v = v[0..2].to_vec();
    Ok(v)
}

async fn create_post() -> Result<String, Box<dyn Error>> {
    let url = format!("{}posts", BASE_URL);
    let o = Post {
        user_id: 1,
        id: 0,
        title: "test title".to_string(),
        body: "test body".to_string()
    };
    let s = reqwest::Client::new().post(&url).body(serde_json::to_string(&o)?).send().await?.text().await?;
    Ok(s)
}

async fn update_post() -> Result<String, Box<dyn Error>> {
    let url = format!("{}posts/1", BASE_URL);
    let o = Post {
        user_id: 1,
        id: 1,
        title: "test title".to_string(),
        body: "test body".to_string()
    };
    let s = reqwest::Client::new().put(&url).body(serde_json::to_string(&o)?).send().await?.text().await?;
    Ok(s)
}

async fn delete_post() -> Result<String, Box<dyn Error>> {
    let url = format!("{}posts/1", BASE_URL);
    let s = reqwest::Client::new().delete(&url).send().await?.text().await?;
    Ok(s)
}

pub async fn rest1() -> Result<(), Box<dyn Error>> {
    println!("{}", get_post_as_string().await?);
    println!("{:?}", get_post_as_json().await?);
    println!("{:?}", get_posts().await?);
    println!("{}", create_post().await?);
    println!("{}", update_post().await?);
    println!("{}", delete_post().await?);

    Ok(())
}

/*
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
}
Post { user_id: 1, id: 1, title: "sunt aut facere repellat provident occaecati excepturi optio reprehenderit", body: "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto" }
[Post { user_id: 1, id: 1, title: "sunt aut facere repellat provident occaecati excepturi optio reprehenderit", body: "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto" }, Post { user_id: 1, id: 2, title: "qui est esse", body: "est rerum tempore vitae\nsequi sint nihil reprehenderit dolor beatae ea dolores neque\nfugiat blanditiis voluptate porro vel nihil molestiae ut reiciendis\nqui aperiam non debitis possimus qui neque nisi nulla" }]
{
  "id": 101
}
{
  "id": 1
}
{}
*/
