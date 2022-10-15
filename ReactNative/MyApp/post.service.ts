import 'reflect-metadata';
import { injectable } from 'inversify';
import { Observable, from } from 'rxjs';
import { map, mergeAll, take, tap } from 'rxjs/operators';
import { Post } from './post';
import { Rxios } from 'rxios';
import {Post2Service} from "./post2.service";
import {inject} from "inversify";

@injectable()
export class PostService {
  private readonly http = new Rxios();
  private readonly baseUrl = 'https://jsonplaceholder.typicode.com/';

  constructor(@inject(Post2Service) post2Service: Post2Service) {
    console.log(post2Service);
    this.getPostAsString().subscribe();
    this.getPostAsJson().subscribe();
    this.getPosts(2).subscribe();
    this.createPost().subscribe();
    this.updatePost().subscribe();
    this.deletePost().subscribe();
  }

  private getPostAsString(): Observable<string> {
    const url = `${this.baseUrl}posts/1`;
    // https://github.com/axios/axios/issues/907
    return new Rxios({transformResponse: undefined}).get<string>(url)
      .pipe(
        tap((result: any) => console.log(result)),
      );
  }

  private getPostAsJson(): Observable<Post> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.get<Post>(url)
      .pipe(
        map((result: any) => Object.assign(new Post(), result)),
        tap((result: any) => console.log('' + result)),
      );
  }

  private getPosts(n: number): Observable<Post> {
    const url = `${this.baseUrl}posts`;
    return from(this.http.get<Post[]>(url))
      .pipe(
        mergeAll(),
        map((result: any) => Object.assign(new Post(), result)),
        take(n),
        tap((result: any) => console.log('' + result)),
      );
  }

  private createPost(): Observable<string> {
    const url = `${this.baseUrl}posts`;
    return this.http.post(url, {
      params: {
        userId: 101,
        title: 'test title',
        body: 'test body',
      }
    })
      .pipe(
        map((result: any) => JSON.stringify(result)),
        tap((result: any) => console.log(result)),
      );
  }

  private updatePost(): Observable<string> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.put(url, {
      params: {
        userId: 101,
        title: 'test title',
        body: 'test body',
      }
    })
      .pipe(
        map((result: any) => JSON.stringify(result)),
        tap((result: any) => console.log(result)),
      );
  }

  private deletePost(): Observable<string> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.delete(url)
      .pipe(
        map((result: any) => JSON.stringify(result)),
        tap((result: any) => console.log(result)),
      );
  }
}
