  buffer1() {
    // Create an observable that emits a value every second
    const myInterval = interval(1000);
    // Create an observable that emits every time document is clicked
    const bufferBy = fromEvent(document, 'click');
    /*
    Collect all values emitted by our interval observable until we click document. This will cause the bufferBy Observable to emit a value, satisfying the buffer. Pass us all collected values since last buffer as an array.
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
    // After three values are emitted, pass on as an array of buffered values
    const bufferThree = source.pipe(bufferCount(3));
    // Print values to console
    // ex. output [0,1,2]...[3,4,5]
    const subscribe = bufferThree.subscribe(val =>
      console.log('Buffered Values:', val)
    );
  }

  bufferCount2() {
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

  concatMap1() {
    // emit delay value
    const source = of(2000, 1000);
    //  map value from source into inner observable, when complete emit result and move to next
    const example = source.pipe(
      concatMap(val => of(`Delayed by: ${val}ms`).pipe(delay(val)))
    );
    // output: With concatMap: Delayed by: 2000ms, With concatMap: Delayed by: 1000ms
    const subscribe = example.subscribe(val =>
      console.log(`With concatMap: ${val}`)
    );
    
    //  showing the difference between concatMap and mergeMap
    const mergeMapExample = source
      .pipe(
        //  just so we can log this after the first example has run
        delay(5000),
        mergeMap(val => of(`Delayed by: ${val}ms`).pipe(delay(val)))
      )
      .subscribe(val => console.log(`With mergeMap: ${val}`));
  }

  concatMap2() {
    // emit 'Hello' and 'Goodbye'
    const source = of('Hello', 'Goodbye');
    // example with promise
    const examplePromise = val => new Promise(resolve => resolve(`${val} World!`));
    //  map value from source into inner observable, when complete emit result and move to next
    const example = source.pipe(concatMap(val => examplePromise(val)));
    // output: 'Example w/ Promise: 'Hello World', Example w/ Promise: 'Goodbye World'
    const subscribe = example.subscribe(val =>
      console.log('Example w/ Promise:', val)
    );
  }

  concatMap3() {
    // emit 'Hello' and 'Goodbye'
    const source = of('Hello', 'Goodbye');
    // example with promise
    const examplePromise = val => new Promise(resolve => resolve(`${val} World!`));
    // result of first param passed to second param selector function before being  returned
    const example = source.pipe(
      concatMap(val => examplePromise(val), result => `${result} w/ selector!`)
    );
    // output: 'Example w/ Selector: 'Hello w/ Selector', Example w/ Selector: 'Goodbye w/ Selector'
    const subscribe = example.subscribe(val =>
      console.log('Example w/ Selector:', val)
    );
  }

  concatMapTo1() {
    // emit value every 2 seconds
    const sampleInterval = interval(500).pipe(take(5));
    const fakeRequest = of('Network request complete').pipe(delay(3000));
    // wait for first to complete before next is subscribed
    const example = sampleInterval.pipe(concatMapTo(fakeRequest));
    // result
    // output: Network request complete...3s...Network request complete'
    const subscribe = example.subscribe(val => console.log(val));
  }

  concatMapTo2() {
    // emit value every 2 seconds
    const interval$ = interval(2000);
    // emit value every second for 5 seconds
    const source = interval(1000).pipe(take(5));
    /*
      ***Be Careful***: In situations like this where the source emits at a faster pace
      than the inner observable completes, memory issues can arise.
      (interval emits every 1 second, basicTimer completes every 5)
    */
    //  basicTimer will complete after 5 seconds, emitting 0,1,2,3,4
    const example = interval$.pipe(
      concatMapTo(
        source,
        (firstInterval, secondInterval) => `${firstInterval} ${secondInterval}`
      )
    );
    /*
      output: 0 0
              0 1
              0 2
              0 3
              0 4
              1 0
              1 1
              continued...
    
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  exhaustMap1() {
    const sourceInterval = interval(1000);
    const delayedInterval = sourceInterval.pipe(delay(10), take(4));
    
    const exhaustSub = merge(
      //  delay 10ms, then start interval emitting 4 values
      delayedInterval,
      //  emit immediately
      of(true)
    )
      .pipe(exhaustMap(_ => sourceInterval.pipe(take(5))))
      /*
                     *  The first emitted value (of(true)) will be mapped
                     *  to an interval observable emitting 1 value every
                     *  second, completing after 5.
                     *  Because the emissions from the delayed interval
                     *  fall while this observable is still active they will be ignored.
                     *
                     *  Contrast this with concatMap which would queue,
                     *  switchMap which would switch to a new inner observable each emission,
                     *  and mergeMap which would maintain a new subscription for each emitted value.
                     */
      //  output: 0, 1, 2, 3, 4
      .subscribe(val => console.log(val));
  }

  exhaustMap2() {
    const firstInterval = interval(1000).pipe(take(10));
    const secondInterval = interval(1000).pipe(take(2));
    
    const exhaustSub = firstInterval
      .pipe(
        exhaustMap(f => {
          console.log(`Emission Corrected of first interval: ${f}`);
          return secondInterval;
        })
      )
      /*
                    When we subscribed to the first interval, it starts to emit a values (starting 0).
                    This value is mapped to the second interval which then begins to emit (starting 0).  
                    While the second interval is active, values from the first interval are ignored.
                    We can see this when firstInterval emits number 3,6, and so on...
    
                      Output:
                      Emission of first interval: 0
                      0
                      1
                      Emission of first interval: 3
                      0
                      1
                      Emission of first interval: 6
                      0
                      1
                      Emission of first interval: 9
                      0
                      1
                  */
      .subscribe(s => console.log(s));
  }

  exhaustMap3() {
    @Effect()
      login$ = this.actions$.pipe(
        ofType(AuthActionTypes.Login),
        map((action: Login) => action.payload),
        exhaustMap((auth: Authenticate) =>
          this.authService
            .login(auth)
            .pipe(
              map(user => new LoginSuccess({ user })),
              catchError(error => of(new LoginFailure(error)))
            )
        )
      );
  }

  groupBy1() {
    const people = [
      { name: 'Sue', age: 25 },
      { name: 'Joe', age: 30 },
      { name: 'Frank', age: 25 },
      { name: 'Sarah', age: 35 }
    ];
    // emit each person
    const source = from(people);
    // group by age
    const example = source.pipe(
      groupBy(person => person.age),
      //  return each item in group as array
      mergeMap(group => group.pipe(toArray()))
    );
    /*
      output:
      [{age: 25, name: "Sue"},{age: 25, name: "Frank"}]
      [{age: 30, name: "Joe"}]
      [{age: 35, name: "Sarah"}]
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  expand1() {
    // emit 2
    const source = of(2);
    const example = source.pipe(
      // recursively call supplied function
      expand(val => {
        // 2,3,4,5,6
        console.log(`Passed value: ${val}`);
        // 3,4,5,6
        return of(1 + val);
      }),
      // call 5 times
      take(5)
    );
    /*
    	"RESULT: 2"
    	"Passed value: 2"
    	"RESULT: 3"
    	"Passed value: 3"
    	"RESULT: 4"
    	"Passed value: 4"
    	"RESULT: 5"
    	"Passed value: 5"
    	"RESULT: 6"
    	"Passed value: 6"
    */
    // output: 2,3,4,5,6
    const subscribe = example.subscribe(val => console.log(`RESULT: ${val}`));
  }

  map1() {
    // emit (1,2,3,4,5)
    const source = from([1, 2, 3, 4, 5]);
    // add 10 to each value
    const example = source.pipe(map(val => val + 10));
    // output: 11,12,13,14,15
    const subscribe = example.subscribe(val => console.log(val));
  }

  map2() {
    // emit ({name: 'Joe', age: 30}, {name: 'Frank', age: 20},{name: 'Ryan', age: 50})
    const source = from([
      { name: 'Joe', age: 30 },
      { name: 'Frank', age: 20 },
      { name: 'Ryan', age: 50 }
    ]);
    // grab each persons name, could also use pluck for this scenario
    const example = source.pipe(map(({ name }) => name));
    // output: "Joe","Frank","Ryan"
    const subscribe = example.subscribe(val => console.log(val));
  }

  mapTo1() {
    // emit value every two seconds
    const source = interval(2000);
    // map all emissions to one value
    const example = source.pipe(mapTo('HELLO WORLD!'));
    // output: 'HELLO WORLD!'...'HELLO WORLD!'...'HELLO WORLD!'...
    const subscribe = example.subscribe(val => console.log(val));
  }

  mapTo2() {
    // emit every click on document
    const source = fromEvent(document, 'click');
    // map all emissions to one value
    const example = source.pipe(mapTo('GOODBYE WORLD!'));
    // output: (click)'GOODBYE WORLD!'...
    const subscribe = example.subscribe(val => console.log(val));
  }

  partition1() {
    const source = from([1, 2, 3, 4, 5, 6]);
    // first value is true, second false
    const [evens, odds] = source.pipe(partition(val => val % 2 === 0));
    /*
      Output:
      "Even: 2"
      "Even: 4"
      "Even: 6"
      "Odd: 1"
      "Odd: 3"
      "Odd: 5"
    */
    const subscribe = merge(
      evens.pipe(map(val => `Even: ${val}`)),
      odds.pipe(map(val => `Odd: ${val}`))
    ).subscribe(val => console.log(val));
  }

  partition2() {
    const source = from([1, 2, 3, 4, 5, 6]);
    // if greater than 3 throw
    const example = source.pipe(
      map(val => {
        if (val > 3) {
          throw `${val} greater than 3!`;
        }
        return { success: val };
      }),
      catchError(val => of({ error: val }))
    );
    // split on success or error
    const [success, error] = example.pipe(partition(res => res.success));
    /*
      Output:
      "Success! 1"
      "Success! 2"
      "Success! 3"
      "Error! 4 greater than 3!"
    */
    const subscribe = merge(
      success.pipe(map(val => `Success! ${val.success}`)),
      error.pipe(map(val => `Error! ${val.error}`))
    ).subscribe(val => console.log(val));
  }

  mergeMap_flatMap1() {
    // emit 'Hello'
    const source = of('Hello');
    // map to inner observable and flatten
    const example = source.pipe(mergeMap(val => of(`${val} World!`)));
    // output: 'Hello World!'
    const subscribe = example.subscribe(val => console.log(val));
  }

  mergeMap_flatMap2() {
    // emit 'Hello'
    const source = of('Hello');
    // mergeMap also emits result of promise
    const myPromise = val =>
      new Promise(resolve => resolve(`${val} World From Promise!`));
    // map to promise and emit result
    const example = source.pipe(mergeMap(val => myPromise(val)));
    // output: 'Hello World From Promise'
    const subscribe = example.subscribe(val => console.log(val));
  }

  mergeMap_flatMap3() {
    /*
      you can also supply a second argument which receives the source value and emitted
      value of inner observable or promise
    */
    // emit 'Hello'
    const source = of('Hello');
    // mergeMap also emits result of promise
    const myPromise = val =>
      new Promise(resolve => resolve(`${val} World From Promise!`));
    const example = source.pipe(
      mergeMap(
        val => myPromise(val),
        (valueFromSource, valueFromPromise) => {
          return `Source: ${valueFromSource}, Promise: ${valueFromPromise}`;
        }
      )
    );
    // output: "Source: Hello, Promise: Hello World From Promise!"
    const subscribe = example.subscribe(val => console.log(val));
  }

  mergeMap_flatMap4() {
    // emit value every 1s
    const source = interval(1000);
    
    const example = source.pipe(
      mergeMap(
        // project
        val => interval(5000).pipe(take(2)),
        // resultSelector
        (oVal, iVal, oIndex, iIndex) => [oIndex, oVal, iIndex, iVal],
        // concurrent
        2
      )
    );
    /*
    		Output:
    		[0, 0, 0, 0] 
    		[1, 1, 0, 0] 
    		[0, 0, 1, 1] 
    		[1, 1, 1, 1] 
    		[2, 2, 0, 0] 
    		[3, 3, 0, 0] 
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  pluck1() {
    const source = from([{ name: 'Joe', age: 30 }, { name: 'Sarah', age: 35 }]);
    // grab names
    const example = source.pipe(pluck('name'));
    // output: "Joe", "Sarah"
    const subscribe = example.subscribe(val => console.log(val));
  }

  pluck2() {
    const source = from([
      { name: 'Joe', age: 30, job: { title: 'Developer', language: 'JavaScript' } },
      // will return undefined when no job is found
      { name: 'Sarah', age: 35 }
    ]);
    // grab title property under job
    const example = source.pipe(pluck('job', 'title'));
    // output: "Developer" , undefined
    const subscribe = example.subscribe(val => console.log(val));
  }

  reduce1() {
    const source = of(1, 2, 3, 4);
    const example = source.pipe(reduce((acc, val) => acc + val));
    // output: Sum: 10'
    const subscribe = example.subscribe(val => console.log('Sum:', val));
  }

  scan1() {
    const source = of(1, 2, 3);
    //  basic scan example, sum over time starting with zero
    const example = source.pipe(scan((acc, curr) => acc + curr, 0));
    //  log accumulated values
    //  output: 1,3,6
    const subscribe = example.subscribe(val => console.log(val));
  }

  scan2() {
    const subject = new Subject();
    // scan example building an object over time
    const example = subject.pipe(
      scan((acc, curr) => Object.assign({}, acc, curr), {})
    );
    // log accumulated values
    const subscribe = example.subscribe(val =>
      console.log('Accumulated object:', val)
    );
    // next values into subject, adding properties to object
    //  {name: 'Joe'}
    subject.next({ name: 'Joe' });
    //  {name: 'Joe', age: 30}
    subject.next({ age: 30 });
    //  {name: 'Joe', age: 30, favoriteLanguage: 'JavaScript'}
    subject.next({ favoriteLanguage: 'JavaScript' });
  }

  scan3() {
    //  Accumulate values in an array, emit random values from this array.
    const scanObs = interval(1000)
      .pipe(
        scan((a, c) => [...a, c], []),
        map(r => r[Math.floor(Math.random() * r.length)]),
        distinctUntilChanged()
      )
      .subscribe(console.log);
  }

  switchMap1() {
    // emit immediately, then every 5s
    const source = timer(0, 5000);
    // switch to new inner observable when source emits, emit items that are emitted
    const example = source.pipe(switchMap(() => interval(500)));
    // output: 0,1,2,3,4,5,6,7,8,9...0,1,2,3,4,5,6,7,8
    const subscribe = example.subscribe(val => console.log(val));
  }

  switchMap2() {
    // emit every click
    const source = fromEvent(document, 'click');
    // if another click comes within 3s, message will not be emitted
    const example = source.pipe(
      switchMap(val => interval(3000).pipe(mapTo('Hello, I made it!')))
    );
    // (click)...3s...'Hello I made it!'...(click)...2s(click)...
    const subscribe = example.subscribe(val => console.log(val));
  }

  switchMap3() {
    // emit immediately, then every 5s
    const source = timer(0, 5000);
    // switch to new inner observable when source emits, invoke project function and emit values
    const example = source.pipe(
      switchMap(
        _ => interval(2000),
        (outerValue, innerValue, outerIndex, innerIndex) => ({
          outerValue,
          innerValue,
          outerIndex,
          innerIndex
        })
      )
    );
    /*
    	Output:
    	{outerValue: 0, innerValue: 0, outerIndex: 0, innerIndex: 0}
    	{outerValue: 0, innerValue: 1, outerIndex: 0, innerIndex: 1}
    	{outerValue: 1, innerValue: 0, outerIndex: 1, innerIndex: 0}
    	{outerValue: 1, innerValue: 1, outerIndex: 1, innerIndex: 1}
    */
    const subscribe = example.subscribe(val => console.log(val));
  }

  switchMap4() {
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
        switchMap(val => (val ? interval$ : empty())),
        scan((acc, curr) => (curr ? curr + acc : acc), countdownSeconds),
        takeWhile(v => v >= 0)
      )
      .subscribe(setHTML('remaining'));
  }

  window1() {
    // emit immediately then every 1s
    const source = timer(0, 1000);
    const example = source.pipe(window(interval(3000)));
    const count = example.pipe(scan((acc, curr) => acc + 1, 0));
    /*
      "Window 1:"
      0
      1
      2
      "Window 2:"
      3
      4
      5
      ...
    */
    const subscribe = count.subscribe(val => console.log(`Window ${val}:`));
    const subscribeTwo = example
      .pipe(mergeAll())
      .subscribe(val => console.log(val));
  }

  windowCount1() {
    // emit every 1s
    const source = interval(1000);
    const example = source.pipe(
      // start new window every 4 emitted values
      windowCount(4),
      tap(_ => console.log('NEW WINDOW!'))
    );
    
    const subscribeTwo = example
      .pipe(
        // window emits nested observable
        mergeAll()
        /*
                output:
                "NEW WINDOW!"
                0
                1
                2
                3
                "NEW WINDOW!"
                4
                5
                6
                7
              */
      )
      .subscribe(val => console.log(val));
  }

  windowTime1() {
    // emit immediately then every 1s
    const source = timer(0, 1000);
    const example = source.pipe(
      // start new window every 3s
      windowTime(3000),
      tap(_ => console.log('NEW WINDOW!'))
    );
    
    const subscribeTwo = example
      .pipe(
        // window emits nested observable
        mergeAll()
        /*
                output:
                "NEW WINDOW!"
                0
                1
                2
                "NEW WINDOW!"
                3
                4
                5
              */
      )
      .subscribe(val => console.log(val));
  }

  windowToggle1() {
    // emit immediately then every 1s
    const source = timer(0, 1000);
    // toggle window on every 5
    const toggle = interval(5000);
    const example = source.pipe(
      // turn window on every 5s
      windowToggle(toggle, val => interval(val * 1000)),
      tap(_ => console.log('NEW WINDOW!'))
    );
    
    const subscribeTwo = example
      .pipe(
        // window emits nested observable
        mergeAll()
        /*
                output:
                "NEW WINDOW!"
                5
                "NEW WINDOW!"
                10
                11
                "NEW WINDOW!"
                15
                16
                "NEW WINDOW!"
                20
                21
                22
              */
      )
      .subscribe(val => console.log(val));
  }

  windowWhen1() {
    // emit immediately then every 1s
    const source = timer(0, 1000);
    const example = source.pipe(
      // close window every 5s and emit observable of collected values from source
      windowWhen(() => interval(5000)),
      tap(_ => console.log('NEW WINDOW!'))
    );
    
    const subscribeTwo = example
      .pipe(
        // window emits nested observable
        mergeAll()
        /*
          output:
          "NEW WINDOW!"
          0
          1
          2
          3
          4
          "NEW WINDOW!"
          5
          6
          7
          8
          9
        */
      )
      .subscribe(val => console.log(val));
  }

