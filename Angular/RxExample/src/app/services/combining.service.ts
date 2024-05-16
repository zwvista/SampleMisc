import { Injectable } from '@angular/core';
import {
  catchError,
  combineLatestAll,
  delay,
  map,
  mapTo,
  mergeAll,
  mergeMap,
  pairwise,
  scan,
  startWith,
  take,
  tap, withLatestFrom
} from 'rxjs/operators';
import { combineLatest, forkJoin, fromEvent, interval, merge, of, race, throwError, timer, zip } from 'rxjs';

@Injectable()
export class CombiningService {

  constructor() { }

  combineLatestAll1() {
    // emit every 1s, take 2
    const source = interval(1000).pipe(take(2));
    // map each emitted value from source to interval observable that takes 5 values
    const example = source.pipe(
      map(val => interval(1000).pipe(map(i => `Result (${val}): ${i}`), take(5)))
    );
    /*
      2 values from source will map to 2 (inner) interval observables that emit every 1s
      combineLatestAll uses combineLatest strategy, emitting the last value from each
      whenever either observable emits a value
    */
    const combined = example.pipe(combineLatestAll());
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

  combineLatest1() {
    // timerOne emits first value at 1s, then once every 4s
    const timerOne = timer(1000, 4000);
    // timerTwo emits first value at 2s, then once every 4s
    const timerTwo = timer(2000, 4000);
    // timerThree emits first value at 3s, then once every 4s
    const timerThree = timer(3000, 4000);

    // when one timer emits, emit the latest values from each timer as an array
    const combined = combineLatest([timerOne, timerTwo, timerThree]);

    const subscribe = combined.subscribe(
      ([timerValOne, timerValTwo, timerValThree]) => {
        /*
      	Example:
        timerOne first tick: 'Timer One Latest: 1, Timer Two Latest:0, Timer Three Latest: 0
        timerTwo first tick: 'Timer One Latest: 1, Timer Two Latest:1, Timer Three Latest: 0
        timerThree first tick: 'Timer One Latest: 1, Timer Two Latest:1, Timer Three Latest: 1
      */
        console.log(
          `Timer One Latest: ${timerValOne},
         Timer Two Latest: ${timerValTwo},
         Timer Three Latest: ${timerValThree}`
        );
      }
    );
  }

  combineLatest2() {
    // timerOne emits first value at 1s, then once every 4s
    const timerOne = timer(1000, 4000);
    // timerTwo emits first value at 2s, then once every 4s
    const timerTwo = timer(2000, 4000);
    // timerThree emits first value at 3s, then once every 4s
    const timerThree = timer(3000, 4000);

    // combineLatest also takes an optional projection function
    const combinedProject = combineLatest([
      timerOne,
      timerTwo,
      timerThree],
      (one, two, three) => {
        return `Timer One (Proj) Latest: ${one},
                  Timer Two (Proj) Latest: ${two},
                  Timer Three (Proj) Latest: ${three}`;
      }
    );
    // log values
    const subscribe = combinedProject.subscribe(latestValuesProject =>
      console.log(latestValuesProject)
    );
  }

  combineLatest3() {
    //  helper function to set HTML
    const setHtml = id => val => (document.getElementById(id).innerHTML = val);

    const addOneClick$ = id =>
      fromEvent(document.getElementById(id), 'click').pipe(
        //  map every click to 1
        mapTo(1),
        startWith(0),
        //  keep a running total
        scan((acc, curr) => acc + curr),
        //  set HTML for appropriate element
        tap(setHtml(`${id}Total`))
      );

    const combineTotal$ = combineLatest([addOneClick$('red'), addOneClick$('black')])
      .pipe(map(([val1, val2]) => val1 + val2))
      .subscribe(setHtml('total'));
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
    const example = forkJoin([
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
    ]);
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
    const example = forkJoin([
      // emit 'Hello' immediately
      of('Hello'),
      // emit 'World' after 1 second
      of('World').pipe(delay(1000)),
      //  throw error
      throwError(() => 'This will error')
    ]).pipe(catchError(error => of(error)));
    // output: 'This will Error'
    const subscribe = example.subscribe(val => console.log(val));
  }

  forkJoin4() {
    /*
      when all observables complete, give the last
      emitted value from each as an array
    */
    const example = forkJoin([
      // emit 'Hello' immediately
      of('Hello'),
      // emit 'World' after 1 second
      of('World').pipe(delay(1000)),
      //  throw error
      throwError(() => 'This will error').pipe(catchError(error => of(error)))
    ]);
    // output: ["Hello", "World", "This will error"]
    const subscribe = example.subscribe(val => console.log(val));
  }

  merge1() {
    // emit every 2.5 seconds
    const first = interval(2500);
    // emit every 2 seconds
    const second = interval(2000);
    // emit every 1.5 seconds
    const third = interval(1500);
    // emit every 1 second
    const fourth = interval(1000);

    // emit outputs from one observable
    const example = merge(
      first.pipe(mapTo('FIRST!')),
      second.pipe(mapTo('SECOND!')),
      third.pipe(mapTo('THIRD')),
      fourth.pipe(mapTo('FOURTH'))
    );
    // output: "FOURTH", "THIRD", "SECOND!", "FOURTH", "FIRST!", "THIRD", "FOURTH"
    const subscribe = example.subscribe(val => console.log(val));
  }

  merge2() {
    // emit every 2.5 seconds
    const first = interval(2500);
    // emit every 1 second
    const second = interval(1000);
    // used as instance method
    const example = merge(first, second);
    // output: 0,1,0,2....
    const subscribe = example.subscribe(val => console.log(val));
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

  pairwise1() {
    // Returns: [0,1], [1,2], [2,3], [3,4], [4,5]
    interval(1000)
      .pipe(
        pairwise(),
        take(5)
      )
      .subscribe(console.log);
  }

  startWith1() {
    // emit (1,2,3)
    const source = of(1, 2, 3);
    // start with 0
    const example = source.pipe(startWith(0));
    // output: 0,1,2,3
    const subscribe = example.subscribe(val => console.log(val));
  }

  startWith2() {
    // emit ('World!', 'Goodbye', 'World!')
    const source = of('World!', 'Goodbye', 'World!');
    // start with 'Hello', concat current string to previous
    const example = source.pipe(
      startWith('Hello'),
      scan((acc, curr) => `${acc} ${curr}`)
    );
    /*
      output:
      "Hello"
      "Hello World!"
      "Hello World! Goodbye"
      "Hello World! Goodbye World!"
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  startWith3() {
    // emit values in sequence every 1s
    const source = interval(1000);
    // start with -3, -2, -1
    const example = source.pipe(startWith(-3, -2, -1));
    // output: -3, -2, -1, 0, 1, 2....
    const subscribe = example.subscribe(val => console.log(val));
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

  withLatestFrom1() {
    // emit every 5s
    const source = interval(5000);
    // emit every 1s
    const secondSource = interval(1000);
    const example = source.pipe(
      withLatestFrom(secondSource),
      map(([first, second]) => {
        return `First Source (5s): ${first} Second Source (1s): ${second}`;
      })
    );
    /*
      "First Source (5s): 0 Second Source (1s): 4"
      "First Source (5s): 1 Second Source (1s): 9"
      "First Source (5s): 2 Second Source (1s): 14"
      ...
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  withLatestFrom2() {
    // emit every 5s
    const source = interval(5000);
    // emit every 1s
    const secondSource = interval(1000);
    // withLatestFrom slower than source
    const example = secondSource.pipe(
      // both sources must emit at least 1 value (5s) before emitting
      withLatestFrom(source),
      map(([first, second]) => {
        return `Source (1s): ${first} Latest From (5s): ${second}`;
      })
    );
    /*
      "Source (1s): 4 Latest From (5s): 0"
      "Source (1s): 5 Latest From (5s): 0"
      "Source (1s): 6 Latest From (5s): 0"
      ...
    */
    const subscribe = example.subscribe(val => console.log(val));
  }


}
