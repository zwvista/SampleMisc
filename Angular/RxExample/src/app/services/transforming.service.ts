import { Injectable } from '@angular/core';
import { fromEvent, interval } from 'rxjs';
import { buffer, bufferCount, bufferTime, bufferToggle, bufferWhen, take } from 'rxjs/operators';

@Injectable()
export class TransformingService {

  constructor() { }

  buffer1() {
    // Create an observable that emits a value every second
    const myInterval = interval(1000);
    // Create an observable that emits every time document is clicked
    const bufferBy = fromEvent(document, 'click');
    /*
      Collect all values emitted by our interval observable until we click document.
      This will cause the bufferBy Observable to emit a value, satisfying the buffer.
      Pass us all collected values since last buffer as an array.
    */
    const myBufferedInterval = myInterval.pipe(buffer(bufferBy));
    // Print values to console
    // ex. output: [1,2,3] ... [4,5,6,7,8]
    const subscribe = myBufferedInterval.subscribe(val =>
      console.log(' Buffered Values:', val)
    );
  }

  bufferCount1() {
    // Create an observable that emits a value every second
    const source = interval(1000);
    /*
      bufferCount also takes second argument, when to start the next buffer
      for instance, if we have a bufferCount of 3 but second argument (startBufferEvery) of 1:
      1st interval value:
      buffer 1: [0]
      2nd interval value:
      buffer 1: [0,1]
      buffer 2: [1]
      3rd interval value:
      buffer 1: [0,1,2] Buffer of 3, emit buffer
      buffer 2: [1,2]
      buffer 3: [2]
      4th interval value:
      buffer 2: [1,2,3] Buffer of 3, emit buffer
      buffer 3: [2, 3]
      buffer 4: [3]
    */
    const bufferEveryOne = source.pipe(bufferCount(3, 1));
    // Print values to console
    const subscribe = bufferEveryOne.subscribe(val =>
      console.log('Start Buffer Every 1:', val)
    );
  }

  bufferTime1() {
    // Create an observable that emits a value every 500ms
    const source = interval(500);
    // After 2 seconds have passed, emit buffered values as an array
    const example = source.pipe(bufferTime(2000));
    // Print values to console
    // ex. output [0,1,2]...[3,4,5,6]
    const subscribe = example.subscribe(val =>
      console.log('Buffered with Time:', val)
    );
  }

  bufferTime2() {
    // Create an observable that emits a value every 500ms
    const source = interval(500);
    /*
      bufferTime also takes second argument, when to start the next buffer (time in ms)
      for instance, if we have a bufferTime of 2 seconds but second argument (bufferCreationInterval) of 1 second:
      ex. output: [0,1,2]...[1,2,3,4,5]...[3,4,5,6,7]
    */
    const example = source.pipe(bufferTime(2000, 1000));
    // Print values to console
    const subscribe = example.subscribe(val =>
      console.log('Start Buffer Every 1s:', val)
    );
  }

  bufferToggle1() {
    // emit value every second
    const sourceInterval = interval(1000);
    // start first buffer after 5s, and every 5s after
    const startInterval = interval(5000);
    // emit value after 3s, closing corresponding buffer
    const closingInterval = val => {
      console.log(`Value ${val} emitted, starting buffer! Closing in 3s!`);
      return interval(3000);
    };
    // every 5s a new buffer will start, collecting emitted values for 3s then emitting buffered values
    const bufferToggleInterval = sourceInterval.pipe(
      bufferToggle(
        startInterval,
        closingInterval
      )
    );
    // log to console
    // ex. emitted buffers [4,5,6]...[9,10,11]
    const subscribe = bufferToggleInterval.subscribe(val =>
      console.log('Emitted Buffer:', val)
    );
  }

  bufferWhen1() {
    // emit value every 1 second
    const oneSecondInterval = interval(1000);
    // return an observable that emits value every 5 seconds
    const fiveSecondInterval = () => interval(5000);
    // every five seconds, emit buffered values
    const bufferWhenExample = oneSecondInterval.pipe(bufferWhen(fiveSecondInterval));
    // log values
    // ex. output: [0,1,2,3]...[4,5,6,7,8]
    const subscribe = bufferWhenExample.subscribe(val =>
      console.log('Emitted Buffer: ', val)
    );
  }
}
