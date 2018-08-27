import { Injectable } from '@angular/core';
import { catchError, combineAll, delay, map, mapTo, mergeAll, mergeMap, pairwise, take } from 'rxjs/operators';
import { forkJoin, interval, of, race, zip } from 'rxjs';

@Injectable()
export class CombiningService {

  constructor() { }

  combineAll1() {
    // emit every 1s, take 2
    const source = interval(1000).pipe(take(2));
    // map each emitted value from source to interval observable that takes 5 values
    const example = source.pipe(
      map(val => interval(1000).pipe(map(i => `Result (${val}): ${i}`), take(5)))
    );
    /*
      2 values from source will map to 2 (inner) interval observables that emit every 1s
      combineAll uses combineLatest strategy, emitting the last value from each
      whenever either observable emits a value
    */
    const combined = example.pipe(combineAll());
    /*
      output:
      ["Result (0): 0", "Result (1): 0"]
      ["Result (0): 1", "Result (1): 0"]
      ["Result (0): 1", "Result (1): 1"]
      ["Result (0): 2", "Result (1): 1"]
      ["Result (0): 2", "Result (1): 2"]
      ["Result (0): 3", "Result (1): 2"]
      ["Result (0): 3", "Result (1): 3"]
      ["Result (0): 4", "Result (1): 3"]
      ["Result (0): 4", "Result (1): 4"]
    */
    const subscribe = combined.subscribe(val => console.log(val));
  }

  mergeAll1() {
    const myPromise = val =>
      new Promise(resolve => setTimeout(() => resolve(`Result: ${val}`), 2000));
    // emit 1,2,3
    const source = of(1, 2, 3);

    const example = source.pipe(
      // map each value to promise
      map(val => myPromise(val)),
      // emit result from source
      mergeAll()
    );

    /*
      output:
      "Result: 1"
      "Result: 2"
      "Result: 3"
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  mergeAll2() {
    const source = interval(500).pipe(take(5));

    /*
      interval is emitting a value every 0.5s.  This value is then being mapped to interval that
      is delayed for 1.0s.  The mergeAll operator takes an optional argument that determines how
      many inner observables to subscribe to at a time.  The rest of the observables are stored
      in a backlog waiting to be subscribe.
    */
    const example = source
      .pipe(
        map(val =>
          source.pipe(
            delay(1000),
            take(3)
          )
        ),
        mergeAll(2)
      )
      .subscribe(val => console.log(val));
    /*
      The subscription is completed once the operator emits all values.
    */
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

  forkJoin1() {
    const myPromise = val =>
      new Promise(resolve =>
        setTimeout(() => resolve(`Promise Resolved: ${val}`), 5000)
      );

    /*
      when all observables complete, give the last
      emitted value from each as an array
    */
    const example = forkJoin(
      // emit 'Hello' immediately
      of('Hello'),
      // emit 'World' after 1 second
      of('World').pipe(delay(1000)),
      // emit 0 after 1 second
      interval(1000).pipe(take(1)),
      // emit 0...1 in 1 second interval
      interval(1000).pipe(take(2)),
      // promise that resolves to 'Promise Resolved' after 5 seconds
      myPromise('RESULT')
    );
    // output: ["Hello", "World", 0, 1, "Promise Resolved: RESULT"]
    const subscribe = example.subscribe(val => console.log(val));
  }

  forkJoin2() {
    const myPromise = val =>
      new Promise(resolve =>
        setTimeout(() => resolve(`Promise Resolved: ${val}`), 5000)
      );

    const source = of([1, 2, 3, 4, 5]);
    // emit array of all 5 results
    const example = source.pipe(mergeMap(q => forkJoin(...q.map(myPromise))));
    /*
      output:
      [
       "Promise Resolved: 1",
       "Promise Resolved: 2",
       "Promise Resolved: 3",
       "Promise Resolved: 4",
       "Promise Resolved: 5"
      ]
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  forkJoin3() {
    /*
      when all observables complete, give the last
      emitted value from each as an array
    */
    const example = forkJoin(
      // emit 'Hello' immediately
      of('Hello'),
      // emit 'World' after 1 second
      of('World').pipe(delay(1000)),
      //  throw error
      _throw('This will error')
    ).pipe(catchError(error => of(error)));
    // output: 'This will Error'
    const subscribe = example.subscribe(val => console.log(val));
  }

  forkJoin4() {
    /*
      when all observables complete, give the last
      emitted value from each as an array
    */
    const example = forkJoin(
      // emit 'Hello' immediately
      of('Hello'),
      // emit 'World' after 1 second
      of('World').pipe(delay(1000)),
      //  throw error
      _throw('This will error').pipe(catchError(error => of(error)))
    );
    // output: ["Hello", "World", "This will error"]
    const subscribe = example.subscribe(val => console.log(val));
  }

  pairwise1() {
    // Returns: [0,1], [1,2], [2,3], [3,4], [4,5]
    interval(1000)
      .pipe(
        pairwise(),
        take(5)
      )
      .subscribe(console.log);
  }

  race1() {
    // take the first observable to emit
    const example = race(
      // emit every 1.5s
      interval(1500),
      // emit every 1s
      interval(1000).pipe(mapTo('1s won!')),
      // emit every 2s
      interval(2000),
      // emit every 2.5s
      interval(2500)
    );
    // output: "1s won!"..."1s won!"...etc
    const subscribe = example.subscribe(val => console.log(val));
  }

  race2() {
    // Throws an error and ignores the other observables.
    const first = of('first').pipe(
      delay(100),
      map(_ => {
        throw 'error';
      })
    );
    const second = of('second').pipe(delay(200));
    const third = of('third').pipe(delay(300));
    //  nothing logged
    race(first, second, third).subscribe(val => console.log(val));
  }

  zip1() {
    const sourceOne = of('Hello');
    const sourceTwo = of('World!');
    const sourceThree = of('Goodbye');
    const sourceFour = of('World!');
    // wait until all observables have emitted a value then emit all as an array
    const example = zip(
      sourceOne,
      sourceTwo.pipe(delay(1000)),
      sourceThree.pipe(delay(2000)),
      sourceFour.pipe(delay(3000))
    );
    // output: ["Hello", "World!", "Goodbye", "World!"]
    const subscribe = example.subscribe(val => console.log(val));
  }

  zip2() {
    // emit every 1s
    const source = interval(1000);
    // when one observable completes no more values will be emitted
    const example = zip(source, source.pipe(take(2)));
    // output: [0,0]...[1,1]
    const subscribe = example.subscribe(val => console.log(val));
  }

}
