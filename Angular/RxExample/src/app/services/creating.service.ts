import { Injectable } from '@angular/core';
import { BaseService } from './base.service';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import '../helpers/extensions';

@Injectable()
export class CreatingService extends BaseService {

  constructor(http: HttpClient) {
    super(http);
  }

  /*
    Create an observable that emits 'Hello' and 'World' on
    subscription.
  */
  createExample1() {
    const hello = Observable.create(observer => {
      observer.next('Hello');
      observer.next('World');
      observer.complete();
    });
    // output: 'Hello'...'World'
    const subscribe = hello.dump();
  }

  /*
    Increment value every 1s, emit even numbers.
  */
  createExample2() {
    const evenNumbers = Observable.create(function(observer) {
      let value = 0;
      const interval = setInterval(() => {
        if (value % 2 === 0) {
          observer.next(value);
        }
        value++;
      }, 1000);
      return () => clearInterval(interval);
    });
    // output: 0...2...4...6...8
    const subscribe = evenNumbers.subscribe(val => console.log(val));
    // unsubscribe after 10 seconds
    setTimeout(() => {
      subscribe.unsubscribe();
    }, 10000);
  }
}
