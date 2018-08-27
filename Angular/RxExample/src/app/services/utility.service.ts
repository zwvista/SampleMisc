import { Injectable } from '@angular/core';
import {catchError, concatMap, delay, delayWhen, dematerialize, map, mapTo, tap, timeout} from 'rxjs/operators';
import {from, interval, merge, of, timer, Notification} from 'rxjs';

@Injectable()
export class UtilityService {

  constructor() { }

  do_tap1() {
    const source = of(1, 2, 3, 4, 5);
    // transparently log values from source with 'do'
    const example = source.pipe(
      tap(val => console.log(`BEFORE MAP: ${val}`)),
      map(val => val + 10),
      tap(val => console.log(`AFTER MAP: ${val}`))
    );
    
    // 'do' does not transform values
    // output: 11...12...13...14...15
    const subscribe = example.subscribe(val => console.log(val));
  }

  delay1() {
    // emit one item
    const example = of(null);
    // delay output of each by an extra second
    const message = merge(
      example.pipe(mapTo('Hello')),
      example.pipe(
        mapTo('World!'),
        delay(1000)
      ),
      example.pipe(
        mapTo('Goodbye'),
        delay(2000)
      ),
      example.pipe(
        mapTo('World!'),
        delay(3000)
      )
    );
    // output: 'Hello'...'World!'...'Goodbye'...'World!'
    const subscribe = message.subscribe(val => console.log(val));
  }

  delayWhen1() {
    // emit value every second
    const message = interval(1000);
    // emit value after five seconds
    const delayForFiveSeconds = () => timer(5000);
    // after 5 seconds, start emitting delayed interval values
    const delayWhenExample = message.pipe(delayWhen(delayForFiveSeconds));
    // log values, delayed for 5 seconds
    // ex. output: 5s....1...2...3
    const subscribe = delayWhenExample.subscribe(val => console.log(val));
  }

  dematerialize1() {
    // emit next and error notifications
    const source = from([
      Notification.createNext('SUCCESS!'),
      Notification.createError('ERROR!')
    ]).pipe(
      // turn notification objects into notification values
      dematerialize()
    );
    
    // output: 'NEXT VALUE: SUCCESS' 'ERROR VALUE: 'ERROR!'
    const subscription = source.subscribe({
      next: val => console.log(`NEXT VALUE: ${val}`),
      error: val => console.log(`ERROR VALUE: ${val}`)
    });
  }

  timeout1() {
    //  simulate request
    function makeRequest(timeToDelay) {
      return of('Request Complete!').pipe(delay(timeToDelay));
    }
    
    of(4000, 3000, 2000)
      .pipe(
        concatMap(duration =>
          makeRequest(duration).pipe(
            timeout(2500),
            catchError(error => of(`Request timed out after: ${duration}`))
          )
        )
      )
      /*
          *  "Request timed out after: 4000"
          *  "Request timed out after: 3000"
          *  "Request Complete!"
          */
      .subscribe(val => console.log(val));
  }

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
