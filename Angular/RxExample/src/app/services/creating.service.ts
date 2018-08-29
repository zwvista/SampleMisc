import { Injectable } from '@angular/core';
import { EMPTY, from, fromEvent, interval, merge, Observable, of, range, throwError, timer } from 'rxjs';
import '../helpers/extensions';
import { map, mapTo, scan, startWith, switchMap, takeWhile } from 'rxjs/operators';

@Injectable()
export class CreatingService {

  constructor() { }

  create1() {
    /*
      Create an observable that emits 'Hello' and 'World' on
      subscription.
    */
    const hello = Observable.create(function(observer) {
      observer.next('Hello');
      observer.next('World');
    });

    // output: 'Hello'...'World'
    const subscribe = hello.subscribe(val => console.log(val));
  }

  create2() {
    /*
      Increment value every 1s, emit even numbers.
    */
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

  empty1() {
    // output: 'Complete!'
    const subscribe = EMPTY.subscribe({
      next: () => console.log('Next'),
      complete: () => console.log('Complete!')
    });
  }

  empty2() {
    const countdownSeconds = 10;
    const setHTML = id => val => (document.getElementById(id).innerHTML = val);
    const pauseButton = document.getElementById('pause');
    const resumeButton = document.getElementById('resume');
    const interval$ = interval(1000).pipe(mapTo(-1));

    const pause$ = fromEvent(pauseButton, 'click').pipe(mapTo(false));
    const resume$ = fromEvent(resumeButton, 'click').pipe(mapTo(true));

    const timer$ = merge(pause$, resume$)
      .pipe(
        startWith(true),
        //  if timer is paused return empty observable
        switchMap(val => (val ? interval$ : EMPTY)),
        scan((acc, curr) => (curr ? curr + acc : acc), countdownSeconds),
        takeWhile(v => v >= 0)
      )
      .subscribe(setHTML('remaining'));
  }

  from1() {
    // emit array as a sequence of values
    const arraySource = from([1, 2, 3, 4, 5]);
    // output: 1,2,3,4,5
    const subscribe = arraySource.subscribe(val => console.log(val));
  }

  from2() {
    // emit result of promise
    const promiseSource = from(new Promise(resolve => resolve('Hello World!')));
    // output: 'Hello World'
    const subscribe = promiseSource.subscribe(val => console.log(val));
  }

  from3() {
    // works on js collections
    const map = new Map();
    map.set(1, 'Hi');
    map.set(2, 'Bye');

    const mapSource = from(map);
    // output: [1, 'Hi'], [2, 'Bye']
    const subscribe = mapSource.subscribe(val => console.log(val));
  }

  from4() {
    // emit string as a sequence
    const source = from('Hello World');
    // output: 'H','e','l','l','o',' ','W','o','r','l','d'
    const subscribe = source.subscribe(val => console.log(val));
  }

  fromEvent1() {
    // create observable that emits click events
    const source = fromEvent(document, 'click');
    // map to string with given event timestamp
    const example = source.pipe(map(event => `Event time: ${event.timeStamp}`));
    // output (example): 'Event time: 7276.390000000001'
    const subscribe = example.subscribe(val => console.log(val));
  }

  interval1() {
    // emit value in sequence every 1 second
    const source = interval(1000);
    // output: 0,1,2,3,4,5....
    const subscribe = source.subscribe(val => console.log(val));
  }

  of1() {
    // emits any number of provided values in sequence
    const source = of(1, 2, 3, 4, 5);
    // output: 1,2,3,4,5
    const subscribe = source.subscribe(val => console.log(val));
  }

  of2() {
    // emits values of any type
    const source = of({ name: 'Brian' }, [1, 2, 3], function hello() {
      return 'Hello';
    });
    // output: {name: 'Brian}, [1,2,3], function hello() { return 'Hello' }
    const subscribe = source.subscribe(val => console.log(val));
  }

  range1() {
    // emit 1-10 in sequence
    const source = range(1, 10);
    // output: 1,2,3,4,5,6,7,8,9,10
    const example = source.subscribe(val => console.log(val));
  }

  throw1() {
    // emits an error with specified value on subscription
    const source = throwError('This is an error!');
    // output: 'Error: This is an error!'
    const subscribe = source.subscribe({
      next: val => console.log(val),
      complete: () => console.log('Complete!'),
      error: val => console.log(`Error: ${val}`)
    });
  }

  timer1() {
    // emit 0 after 1 second then complete, since no second argument is supplied
    const source = timer(1000);
    // output: 0
    const subscribe = source.subscribe(val => console.log(val));
  }

  timer2() {
    /*
      timer takes a second argument, how often to emit subsequent values
      in this case we will emit first value after 1 second and subsequent
      values every 2 seconds after
    */
    const source = timer(1000, 2000);
    // output: 0,1,2,3,4,5......
    const subscribe = source.subscribe(val => console.log(val));
  }
}
