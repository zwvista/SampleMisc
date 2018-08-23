import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { MessageService } from './message.service';
import {of, Observable, from} from 'rxjs';
import {catchError, map, mergeAll, take, tap} from 'rxjs/operators';
import { Post } from './post';

@Injectable()
export class PostService {
  private baseUrl = 'http://jsonplaceholder.typicode.com/';

  constructor(
    private http: HttpClient,
    private messageService: MessageService) {
    this.getPostAsString().subscribe();
    this.getPostAsJson().subscribe();
    this.getPosts(2).subscribe();
    this.createPost().subscribe();
    this.updatePost().subscribe();
    this.deletePost().subscribe();
  }

  private log(message: string) {
    this.messageService.add('PostService: ' + message);
  }

  private handleError<T> (operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {
      console.error(error); // log to console instead
      this.log(`${operation} failed: ${error.message}`);
      return of(result as T);
    };
  }

  private getPostAsString(): Observable<string | any[]> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.get(url, { responseType: 'text' })
      .pipe(
        tap(result => this.log(result)),
        catchError(this.handleError('getPostAsString', []))
      );
  }

  private getPostAsJson(): Observable<Post | any[]> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.get<Post>(url)
      .pipe(
        map(result => Object.assign(new Post(), result)),
        tap(result => this.log('' + result)),
        catchError(this.handleError('getPostAsJson', []))
      );
  }

  private getPosts(n: number): Observable<Post | any[]> {
    const url = `${this.baseUrl}posts`;
    return from(this.http.get<Post[]>(url))
      .pipe(
        mergeAll(),
        map(result => Object.assign(new Post(), result)),
        take(n),
        tap(result => this.log('' + result)),
        catchError(this.handleError('getPosts', []))
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
        tap(result => this.log(result)),
        catchError(this.handleError('createPost', []))
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
        tap(result => this.log(result)),
        catchError(this.handleError('updatePost', []))
      );
  }

  private deletePost(): Observable<string | any[]> {
    const url = `${this.baseUrl}posts/1`;
    return this.http.delete(url)
      .pipe(
        map(result => JSON.stringify(result)),
        tap(result => this.log(result)),
        catchError(this.handleError('deletePost', []))
      );
  }
}
