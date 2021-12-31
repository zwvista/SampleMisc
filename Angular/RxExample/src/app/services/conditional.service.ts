import { Injectable } from '@angular/core';
import { EMPTY, interval, of, race, timer } from 'rxjs';
import {
  defaultIfEmpty,
  delay,
  every,
  filter,
  map,
  mapTo,
  scan,
  skipUntil,
  skipWhile,
  takeUntil,
  takeWhile,
  withLatestFrom
} from 'rxjs/operators';

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

  race1() {
    // take the first observable to emit
    const example = race<(number | string)[]>(
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

  skipUntil1() {
    // emit every 1s
    const source = interval(1000);
    // skip emitted values from source until inner observable emits (6s)
    const example = source.pipe(skipUntil(timer(6000)));
    // output: 5...6...7...8........
    const subscribe = example.subscribe(val => console.log(val));
  }

  skipWhile1() {
    // emit every 1s
    const source = interval(1000);
    // skip emitted values from source while value is less than 5
    const example = source.pipe(skipWhile(val => val < 5));
    // output: 5...6...7...8........
    const subscribe = example.subscribe(val => console.log(val));
  }

  takeUntil1() {
    // emit value every 1s
    const source = interval(1000);
    // after 5 seconds, emit value
    const timer$ = timer(5000);
    // when timer emits after 5s, complete source
    const example = source.pipe(takeUntil(timer$));
    // output: 0,1,2,3
    const subscribe = example.subscribe(val => console.log(val));
  }

  takeUntil2() {
    // emit value every 1s
    const source = interval(1000);
    // is number even?
    const isEven = val => val % 2 === 0;
    // only allow values that are even
    const evenSource = source.pipe(filter(isEven));
    // keep a running total of the number of even numbers out
    const evenNumberCount = evenSource.pipe(scan((acc, _) => acc + 1, 0));
    // do not emit until 5 even numbers have been emitted
    const fiveEvenNumbers = evenNumberCount.pipe(filter(val => val > 5));

    const example = evenSource.pipe(
      // also give me the current even number count for display
      withLatestFrom(evenNumberCount),
      map(([val, count]) => `Even number (${count}) : ${val}`),
      // when five even numbers have been emitted, complete source observable
      takeUntil(fiveEvenNumbers)
    );
    /*
    	Even number (1) : 0,
      Even number (2) : 2
    	Even number (3) : 4
    	Even number (4) : 6
    	Even number (5) : 8
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  takeWhile1() {
    // emit 1,2,3,4,5
    const source = of(1, 2, 3, 4, 5);
    // allow values until value from source is greater than 4, then complete
    const example = source.pipe(takeWhile(val => val <= 4));
    // output: 1,2,3,4
    const subscribe = example.subscribe(val => console.log(val));
  }

  takeWhile2() {
    //  emit 3, 3, 3, 9, 1, 4, 5, 8, 96, 3, 66, 3, 3, 3
    const source = of(3, 3, 3, 9, 1, 4, 5, 8, 96, 3, 66, 3, 3, 3);

    //  allow values until value from source equals 3, then complete
    //  output: [3, 3, 3]
    source
      .pipe(takeWhile(it => it === 3))
      .subscribe(val => console.log('takeWhile', val));

    //  output: [3, 3, 3, 3, 3, 3, 3]
    source
      .pipe(filter(it => it === 3))
      .subscribe(val => console.log('filter', val));
  }

}
