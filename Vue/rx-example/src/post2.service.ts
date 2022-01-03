import { injectable } from 'vue-typescript-inject';
import { Post } from './post';

@injectable()
export class Post2Service {
  private readonly baseUrl = 'http://jsonplaceholder.typicode.com/';

  constructor() {
    this.getPostAsString();
    this.getPostAsJson();
    this.getPosts(2);
    this.createPost();
    this.updatePost();
    this.deletePost();
  }

  private async getPostAsString() {
    const url = `${this.baseUrl}posts/1`;
    const result = await fetch(url);
    const data = await result.text();
    console.log(data);
  }

  private async getPostAsJson() {
    const url = `${this.baseUrl}posts/1`;
    const result = await fetch(url);
    const data = await result.json();
    const post = Object.assign(new Post(), data);
    console.log(post);
  }

  private async getPosts(n: number) {
    const url = `${this.baseUrl}posts`;
    const result = await fetch(url);
    const data = await result.json();
    (data as Post[]).slice(0, n).map(v => {
      const post = Object.assign(new Post(), v);
      console.log(post);
    });
  }

  private async createPost() {
    const url = `${this.baseUrl}posts`;
    const result = await fetch(url, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        userId: 101,
        title: 'test title',
        body: 'test body',
      })
    });
    const data = await result.json();
    const post = Object.assign(new Post(), data);
    console.log(post);
  }

  private async updatePost() {
    const url = `${this.baseUrl}posts/1`;
    const result = await fetch(url, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        userId: 101,
        title: 'test title',
        body: 'test body',
      })
    });
    const data = await result.json();
    const post = Object.assign(new Post(), data);
    console.log(post);
  }

  private async deletePost() {
    const url = `${this.baseUrl}posts/1`;
    const result = await fetch(url, {
      method: 'DELETE',
    });
    const data = await result.text();
    console.log(data);
  }
}
