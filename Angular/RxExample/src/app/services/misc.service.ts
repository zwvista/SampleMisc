import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { concatMap, exhaustMap, map, mergeMap, switchMap } from 'rxjs/operators';

@Injectable({
  providedIn: 'root'
})
export class MiscService {

  constructor() { }

  // https://qiita.com/ovrmrw/items/b45d7bf29c8d29415bd7
  private httpGet(url, delay) {
    return new Promise(resolve => {
      setTimeout(() => {
        const obj = { ResultData: url + ' -> resolved' };
        resolve(JSON.stringify(obj));
      }, delay);
    });
  }

  concatMap1() {
    console.log('start');
    const subject = new Subject();
    subject.pipe(
      concatMap((obj: any) => {
        return this.httpGet(obj.url, obj.delay);
      }),
      map((res: string) => JSON.parse(res).ResultData)
    ).subscribe(value => {
      console.log(value);
    });
    subject.next({ url: 'http://foo', delay: 500 });
    subject.next({ url: 'http://bar', delay: 300 });
    subject.next({ url: 'http://baz', delay: 100 });
  }

  mergeMap1() {
    console.log('start');
    const subject = new Subject();
    subject.pipe(
      mergeMap((obj: any) => {
        return this.httpGet(obj.url, obj.delay);
      }),
      map((res: string) => JSON.parse(res).ResultData)
    ).subscribe(value => {
      console.log(value);
    });
    subject.next({ url: 'http://foo', delay: 500 });
    subject.next({ url: 'http://bar', delay: 300 });
    subject.next({ url: 'http://baz', delay: 100 });
  }

  switchMap1() {
    console.log('start');
    const subject = new Subject();
    subject.pipe(
      switchMap((obj: any) => {
        return this.httpGet(obj.url, obj.delay);
      }),
      map((res: string) => JSON.parse(res).ResultData)
    ).subscribe(value => {
      console.log(value);
    });
    subject.next({ url: 'http://foo', delay: 500 });
    subject.next({ url: 'http://bar', delay: 300 });
    subject.next({ url: 'http://baz', delay: 100 });
  }

  exhaustMap1() {
    console.log('start');
    const subject = new Subject();
    subject.pipe(
      exhaustMap((obj: any) => {
        return this.httpGet(obj.url, obj.delay);
      }),
      map((res: string) => JSON.parse(res).ResultData)
    ).subscribe(value => {
      console.log(value);
    });
    subject.next({ url: 'http://foo', delay: 500 });
    subject.next({ url: 'http://bar', delay: 300 });
    subject.next({ url: 'http://baz', delay: 100 });
  }
}
