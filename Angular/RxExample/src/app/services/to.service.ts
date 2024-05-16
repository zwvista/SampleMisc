import { Injectable } from '@angular/core';
import { lastValueFrom, of } from 'rxjs';
import { delay } from 'rxjs/operators';

@Injectable()
export class ToService {

  constructor() { }

  toPromise1() {
    // return basic observable
    const sample = val => of(val).pipe(delay(5000));
    // convert basic observable to promise
    const example = lastValueFrom(sample('First Example'))
      // output: 'First Example'
      .then(result => {
        console.log('From Promise:', result);
      });
  }

  toPromise2() {
    // return basic observable
    const sample = val => of(val).pipe(delay(5000));
    /*
      convert each to promise and use Promise.all
      to wait for all to resolve
    */
    const example = () => {
      return Promise.all([
        lastValueFrom(sample('Promise 1')),
        lastValueFrom(sample('Promise 2'))
      ]);
    };
    // output: ["Promise 1", "Promise 2"]
    example().then(val => {
      console.log('Promise.all Result:', val);
    });
  }
}
