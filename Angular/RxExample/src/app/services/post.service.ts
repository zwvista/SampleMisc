import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, from } from 'rxjs';
import { map, mergeAll, take, tap } from 'rxjs/operators';
import { Post } from './post';

@Injectable()
export class PostService {
  private baseUrl = 'http://jsonplaceholder.typicode.com/';

  constructor(private http: HttpClient) { }

  restExample() {
    this.getPostAsString().subscribe();
    this.getPostAsJson().subscribe();
    this.getPosts(2).subscribe();
    this.createPost().subscribe();
    this.updatePost().subscribe();
    this.deletePost().subscribe();
  }

  private getPostAsString(): Observable<string | any[]> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.get(url, { responseType: 'text' })
      .pipe(
        tap(result => console.log(result)),
      );
  }

  private getPostAsJson(): Observable<Post | any[]> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.get<Post>(url)
      .pipe(
        map(result => Object.assign(new Post(), result)),
        tap(result => console.log('' + result)),
      );
  }

  private getPosts(n: number): Observable<Post | any[]> {
    const url = `${this.baseUrl}posts`;
    return from(this.http.get<Post[]>(url))
      .pipe(
        mergeAll(),
        map(result => Object.assign(new Post(), result)),
        take(n),
        tap(result => console.log('' + result)),
      );
  }

  private createPost(): Observable<string | any[]> {
    const url = `${this.baseUrl}posts`;
    return this.http.post(url, {
        params: {
          userId: 101,
          title: 'test title',
          body: 'test body',
        }
      })
      .pipe(
        map(result => JSON.stringify(result)),
        tap(result => console.log(result)),
      );
  }

  private updatePost(): Observable<string | any[]> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.put(url, {
        params: {
          userId: 101,
          id: 1,
          title: 'test title',
          body: 'test body',
        }
      })
      .pipe(
        map(result => JSON.stringify(result)),
        tap(result => console.log(result)),
      );
  }

  private deletePost(): Observable<string | any[]> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.delete(url)
      .pipe(
        map(result => JSON.stringify(result)),
        tap(result => console.log(result)),
      );
  }
}
