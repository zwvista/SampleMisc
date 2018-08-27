  debounce1() {
    // emit four strings
    const example = of('WAIT', 'ONE', 'SECOND', 'Last will display');
    /*
        Only emit values after a second has passed between the last emission,
        throw away all other values
    */
    const debouncedExample = example.pipe(debounce(() => timer(1000)));
    /*
        In this example, all values but the last will be omitted
        output: 'Last will display'
    */
    const subscribe = debouncedExample.subscribe(val => console.log(val));
  }

  debounce2() {
    // emit value every 1 second, ex. 0...1...2
    const interval$ = interval(1000);
    // raise the debounce time by 200ms each second
    const debouncedInterval = interval$.pipe(debounce(val => timer(val * 200)));
    /*
      After 5 seconds, debounce time will be greater than interval time,
      all future values will be thrown away
      output: 0...1...2...3...4......(debounce time over 1s, no values emitted)
    */
    const subscribe = debouncedInterval.subscribe(val =>
      console.log(`Example Two: ${val}`)
    );
  }

  first1() {
    const source = from([1, 2, 3, 4, 5]);
    // no arguments, emit first value
    const example = source.pipe(first());
    // output: "First value: 1"
    const subscribe = example.subscribe(val => console.log(`First value: ${val}`));
  }

  first2() {
    const source = from([1, 2, 3, 4, 5]);
    // emit first item to pass test
    const example = source.pipe(first(num => num === 5));
    // output: "First to pass test: 5"
    const subscribe = example.subscribe(val =>
      console.log(`First to pass test: ${val}`)
    );
  }

  first3() {
    const source = from([1, 2, 3, 4, 5]);
    // no value will pass, emit default
    const example = source.pipe(first(val => val > 5, 'Nothing'));
    // output: 'Nothing'
    const subscribe = example.subscribe(val => console.log(val));
  }

  ignoreElements1() {
    // emit value every 100ms
    const source = interval(100);
    // ignore everything but complete
    const example = source.pipe(
      take(5),
      ignoreElements()
    );
    // output: "COMPLETE!"
    const subscribe = example.subscribe(
      val => console.log(`NEXT: ${val}`),
      val => console.log(`ERROR: ${val}`),
      () => console.log('COMPLETE!')
    );
  }

  ignoreElements2() {
    // emit value every 100ms
    const source = interval(100);
    // ignore everything but error
    const error = source.pipe(
      mergeMap(val => {
        if (val === 4) {
          return throwError(`ERROR AT ${val}`);
        }
        return of(val);
      }),
      ignoreElements()
    );
    // output: "ERROR: ERROR AT 4"
    const subscribe = error.subscribe(
      val => console.log(`NEXT: ${val}`),
      val => console.log(`ERROR: ${val}`),
      () => console.log('SECOND COMPLETE!')
    );
  }

  last1() {
    const source = from([1, 2, 3, 4, 5]);
    // no arguments, emit last value
    const example = source.pipe(last());
    // output: "Last value: 5"
    const subscribe = example.subscribe(val => console.log(`Last value: ${val}`));
  }

  last2() {
    const source = from([1, 2, 3, 4, 5]);
    // emit last even number
    const exampleTwo = source.pipe(last(num => num % 2 === 0));
    // output: "Last to pass test: 4"
    const subscribeTwo = exampleTwo.subscribe(val =>
      console.log(`Last to pass test: ${val}`)
    );
  }

  last3() {
    const source = from([1, 2, 3, 4, 5]);
    // no values will pass given predicate, emit default
    const exampleTwo = source.pipe(last(v => v > 5, 'Nothing!'));
    // output: 'Nothing!'
    const subscribeTwo = exampleTwo.subscribe(val => console.log(val));
  }

  single1() {
    // emit (1,2,3,4,5)
    const source = from([1, 2, 3, 4, 5]);
    // emit one item that matches predicate
    const example = source.pipe(single(val => val === 4));
    // output: 4
    const subscribe = example.subscribe(val => console.log(val));
  }

  sample1() {
    // emit value every 1s
    const source = interval(1000);
    // sample last emitted value from source every 2s
    const example = source.pipe(sample(interval(2000)));
    // output: 2..4..6..8..
    const subscribe = example.subscribe(val => console.log(val));
  }

  sample2() {
    const source = zip(
      // emit 'Joe', 'Frank' and 'Bob' in sequence
      from(['Joe', 'Frank', 'Bob']),
      // emit value every 2s
      interval(2000)
    );
    // sample last emitted value from source every 2.5s
    const example = source.pipe(sample(interval(2500)));
    // output: ["Joe", 0]...["Frank", 1]...........
    const subscribe = example.subscribe(val => console.log(val));
  }

  sample3() {
    const listener = merge(
      fromEvent(document, 'mousedown').pipe(mapTo(false)),
      fromEvent(document, 'mousemove').pipe(mapTo(true))
    )
      .pipe(sample(fromEvent(document, 'mouseup')))
      .subscribe(isDragging => {
        console.log('Were you dragging?', isDragging);
      });
  }

  skip1() {
    // emit every 1s
    const source = interval(1000);
    // skip the first 5 emitted values
    const example = source.pipe(skip(5));
    // output: 5...6...7...8........
    const subscribe = example.subscribe(val => console.log(val));
  }

  skip2() {
    const numArrayObs = from([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    
    //  3,4,5...
    const skipObs = numArrayObs.pipe(skip(2)).subscribe(console.log);
    
    //  3,4,5...
    const filterObs = numArrayObs
      .pipe(filter((val, index) => index > 1))
      .subscribe(console.log);
    
    // Same output!
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
    const example = source.pipe(skipWhile(val => val  5));
    // output: 5...6...7...8........
    const subscribe = example.subscribe(val => console.log(val));
  }

  takeWhile1() {
    // emit 1,2,3,4,5
    const source = of(1, 2, 3, 4, 5);
    // allow values until value from source is greater than 4, then complete
    const example = source.pipe(takeWhile(val => val  4));
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

  throttle1() {
    // emit value every 1 second
    const source = interval(1000);
    // throttle for 2 seconds, emit latest value
    const example = source.pipe(throttle(val => interval(2000)));
    // output: 0...3...6...9
    const subscribe = example.subscribe(val => console.log(val));
  }

  throttle2() {
    // emit value every 1 second
    const source = interval(1000);
    // incrementally increase the time to resolve based on source
    const promise = val =>
      new Promise(resolve =>
        setTimeout(() => resolve(`Resolved: ${val}`), val * 100)
      );
    // when promise resolves emit item from source
    const example = source.pipe(
      throttle(promise),
      map(val => `Throttled off Promise: ${val}`)
    );
    
    const subscribe = example.subscribe(val => console.log(val));
  }

  throttleTime1() {
    // emit value every 1 second
    const source = interval(1000);
    /*
      throttle for five seconds
      last value emitted before throttle ends will be emitted from source
    */
    const example = source.pipe(throttleTime(5000));
    // output: 0...6...12
    const subscribe = example.subscribe(val => console.log(val));
  }

  throttleTime2() {
    const source = merge(
      // emit every .75 seconds
      interval(750),
      // emit every 1 second
      interval(1000)
    );
    // throttle in middle of emitted values
    const example = source.pipe(throttleTime(1200));
    // output: 0...1...4...4...8...7
    const subscribe = example.subscribe(val => console.log(val));
  }

