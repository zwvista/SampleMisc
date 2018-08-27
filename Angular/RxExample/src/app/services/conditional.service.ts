import { Injectable } from '@angular/core';
import {EMPTY, of} from 'rxjs';
import {defaultIfEmpty, every} from 'rxjs/operators';

@Injectable()
export class ConditionalService {

  constructor() { }

  defaultIfEmpty1() {
    // emit 'Observable.of() Empty!' when empty, else any values from source
    const exampleOne = of().pipe(defaultIfEmpty('Observable.of() Empty!'));
    // output: 'Observable.of() Empty!'
    const subscribe = exampleOne.subscribe(val => console.log(val));
  }

  defaultIfEmpty2() {
    // emit 'Observable.empty()!' when empty, else any values from source
    const example = EMPTY.pipe(defaultIfEmpty('Observable.empty()!'));
    // output: 'Observable.empty()!'
    const subscribe = example.subscribe(val => console.log(val));
  }

  every1() {
    // emit 5 values
    const source = of(1, 2, 3, 4, 5);
    const example = source.pipe(
      // is every value even?
      every(val => val % 2 === 0)
    );
    // output: false
    const subscribe = example.subscribe(val => console.log(val));
  }

  every2() {
    // emit 5 values
    const allEvens = of(2, 4, 6, 8, 10);
    const example = allEvens.pipe(
      // is every value even?
      every(val => val % 2 === 0)
    );
    // output: true
    const subscribe = example.subscribe(val => console.log(val));
  }

}
