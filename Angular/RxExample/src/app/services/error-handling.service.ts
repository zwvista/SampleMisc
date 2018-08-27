import {Component, Injectable, OnInit} from '@angular/core';
import {from, interval, of, throwError, timer} from 'rxjs';
import {catchError, delayWhen, finalize, map, mergeMap, retry, retryWhen, tap} from 'rxjs/operators';

@Injectable()
export class ErrorHandlingService {

  constructor() { }

  catch_catchError1() {
    // emit error
    const source = throwError('This is an error!');
    // gracefully handle error, returning observable with error message
    const example = source.pipe(catchError(val => of(`I caught: ${val}`)));
    // output: 'I caught: This is an error'
    const subscribe = example.subscribe(val => console.log(val));
  }

  catch_catchError2() {
    // create promise that immediately rejects
    const myBadPromise = () =>
      new Promise((resolve, reject) => reject('Rejected!'));
    // emit single value after 1 second
    const source = timer(1000);
    // catch rejected promise, returning observable containing error message
    const example = source.pipe(
      mergeMap(_ =>
        from(myBadPromise()).pipe(catchError(error => of(`Bad Promise: ${error}`)))
      )
    );
    // output: 'Bad Promise: Rejected'
    const subscribe = example.subscribe(val => console.log(val));
  }

  retry1() {
    // emit value every 1s
    const source = interval(1000);
    const example = source.pipe(
      mergeMap(val => {
        // throw error for demonstration
        if (val > 5) {
          return throwError('Error!');
        }
        return of(val);
      }),
      // retry 2 times on error
      retry(2)
    );
    /*
      output:
      0..1..2..3..4..5..
      0..1..2..3..4..5..
      0..1..2..3..4..5..
      "Error!: Retried 2 times then quit!"
    */
    const subscribe = example.subscribe({
      next: val => console.log(val),
      error: val => console.log(`${val}: Retried 2 times then quit!`)
    });
  }

  retryWhen1() {
    // emit value every 1s
    const source = interval(1000);
    const example = source.pipe(
      map(val => {
        if (val > 5) {
          // error will be picked up by retryWhen
          throw val;
        }
        return val;
      }),
      retryWhen(errors =>
        errors.pipe(
          // log error message
          tap(val => console.log(`Value ${val} was too high!`)),
          // restart in 5 seconds
          delayWhen(val => timer(val * 1000))
        )
      )
    );
    /*
      output:
      0
      1
      2
      3
      4
      5
      "Value 6 was too high!"
      --Wait 5 seconds then repeat
    */
    const subscribe = example.subscribe(val => console.log(val));
  }
}
