import { Injectable } from '@angular/core';
import {concatAll, delay, map, reduce, take} from 'rxjs/operators';
import {concat, interval, of} from 'rxjs';

@Injectable()
export class AggregateService {

  constructor() { }

  concat1() {
    // emits 1,2,3
    const sourceOne = of(1, 2, 3);
    // emits 4,5,6
    const sourceTwo = of(4, 5, 6);
    // emit values from sourceOne, when complete, subscribe to sourceTwo
    const example = concat(sourceOne, sourceTwo);
    // output: 1,2,3,4,5,6
    const subscribe = example.subscribe(val =>
      console.log('Example: Basic concat:', val)
    );
  }

  concat2() {
    // emits 1,2,3
    const sourceOne = of(1, 2, 3);
    // emits 4,5,6
    const sourceTwo = of(4, 5, 6);

    // used as static
    const example = concat(sourceOne, sourceTwo);
    // output: 1,2,3,4,5,6
    const subscribe = example.subscribe(val => console.log(val));
  }

  concat3() {
    // emits 1,2,3
    const sourceOne = of(1, 2, 3);
    // emits 4,5,6
    const sourceTwo = of(4, 5, 6);

    // delay 3 seconds then emit
    const sourceThree = sourceOne.pipe(delay(3000));
    // sourceTwo waits on sourceOne to complete before subscribing
    const example = concat(sourceThree, sourceTwo);
    // output: 1,2,3,4,5,6
    const subscribe = example.subscribe(val =>
      console.log('Example: Delayed source one:', val)
    );
  }

  concat4() {
    // when source never completes, the subsequent observables never runs
    const source = concat(interval(1000), of('This', 'Never', 'Runs'));
    // outputs: 0,1,2,3,4....
    const subscribe = source.subscribe(val =>
      console.log(
        'Example: Source never completes, second observable never runs:',
        val
      )
    );
  }

  concatAll1() {
    // emit a value every 2 seconds
    const source = interval(2000);
    const example = source.pipe(
      // for demonstration, add 10 to and return as observable
      map(val => of(val + 10)),
      // merge values from inner observable
      concatAll()
    );
    // output: 'Example with Basic Observable 10', 'Example with Basic Observable 11'...
    const subscribe = example.subscribe(val =>
      console.log('Example with Basic Observable:', val)
    );
  }

  concatAll2() {
    // create and resolve basic promise
    const samplePromise = val => new Promise(resolve => resolve(val));
    // emit a value every 2 seconds
    const source = interval(2000);

    const example = source.pipe(
      map(val => samplePromise(val)),
      // merge values from resolved promise
      concatAll()
    );
    // output: 'Example with Promise 0', 'Example with Promise 1'...
    const subscribe = example.subscribe(val =>
      console.log('Example with Promise:', val)
    );
  }

  concatAll3() {
    const obs1 = interval(1000).pipe(take(5));
    const obs2 = interval(500).pipe(take(2));
    const obs3 = interval(2000).pipe(take(1));
    // emit three observables
    const source = of(obs1, obs2, obs3);
    // subscribe to each inner observable in order when previous completes
    const example = source.pipe(concatAll());
    /*
      output: 0,1,2,3,4,0,1,0
      How it works...
      Subscribes to each inner observable and emit values, when complete subscribe to next
      obs1: 0,1,2,3,4 (complete)
      obs2: 0,1 (complete)
      obs3: 0 (complete)
    */

    const subscribe = example.subscribe(val => console.log(val));
  }

  reduce1() {
    const source = of(1, 2, 3, 4);
    const example = source.pipe(reduce((acc, val) => acc + val));
    // output: Sum: 10'
    const subscribe = example.subscribe(val => console.log('Sum:', val));
  }
}
