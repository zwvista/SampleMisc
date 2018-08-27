import { Injectable } from '@angular/core';
import { of } from 'rxjs';
import { delay } from 'rxjs/operators';

@Injectable()
export class ToService {

  constructor() { }

  toPromise1() {
    // return basic observable
    const sample = val => of(val).pipe(delay(5000));
    // convert basic observable to promise
    const example = sample('First Example')
      .toPromise()
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
        sample('Promise 1').toPromise(),
        sample('Promise 2').toPromise()
      ]);
    };
    // output: ["Promise 1", "Promise 2"]
    example().then(val => {
      console.log('Promise.all Result:', val);
    });
  }
}
