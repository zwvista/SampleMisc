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

  let1() {
    //  custom error handling logic
    const retryThreeTimes = obs =>
      obs.retry(3).catch(_ => Rx.Observable.of('ERROR!'));
    const examplePromise = val =>
      new Promise(resolve => resolve(`Complete: ${val}`));
    
    // faking request
    const subscribe = Rx.Observable.of('some_url')
      .mergeMap(url => examplePromise(url))
      //  could reuse error handling logic in multiple places with let
      .let(retryThreeTimes)
      // output: Complete: some_url
      .subscribe(result => console.log(result));
    
    const customizableRetry = retryTimes => obs =>
      obs.retry(retryTimes).catch(_ => Rx.Observable.of('ERROR!'));
    
    // faking request
    const secondSubscribe = Rx.Observable.of('some_url')
      .mergeMap(url => examplePromise(url))
      //  could reuse error handling logic in multiple places with let
      .let(customizableRetry(3))
      // output: Complete: some_url
      .subscribe(result => console.log(result));
  }

  let2() {
    // emit array as a sequence
    const source = Rx.Observable.from([1, 2, 3, 4, 5]);
    // demonstrating the difference between let and other operators
    const test = source
      .map(val => val + 1)
      /*
        	this would fail, let behaves differently than most operators
        	val in this case is an observable
        */
      // .let(val => val + 2)
      .subscribe(val => console.log('VALUE FROM ARRAY: ', val));
    
    const subscribe = source
      .map(val => val + 1)
      // 'let' me have the entire observable
      .let(obs => obs.map(val => val + 2))
      // output: 4,5,6,7,8
      .subscribe(val => console.log('VALUE FROM ARRAY WITH let: ', val));
  }

  let3() {
    // emit array as a sequence
    const source = Rx.Observable.from([1, 2, 3, 4, 5]);
    
    // let provides flexibility to add multiple operators to source observable then return
    const subscribeTwo = source
      .map(val => val + 1)
      .let(obs =>
        obs
          .map(val => val + 2)
          // also, just return evens
          .filter(val => val % 2 === 0)
      )
      // output: 4,6,8
      .subscribe(val => console.log('let WITH MULTIPLE OPERATORS: ', val));
  }

  let4() {
    // emit array as a sequence
    const source = Rx.Observable.from([1, 2, 3, 4, 5]);
    
    // pass in your own function to add operators to observable
    const obsArrayPlusYourOperators = yourAppliedOperators => {
      return source.map(val => val + 1).let(yourAppliedOperators);
    };
    const addTenThenTwenty = obs => obs.map(val => val + 10).map(val => val + 20);
    const subscribe = obsArrayPlusYourOperators(addTenThenTwenty)
      // output: 32, 33, 34, 35, 36
      .subscribe(val => console.log('let FROM FUNCTION:', val));
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
    const sample = val => Rx.Observable.of(val).delay(5000);
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
    const sample = val => Rx.Observable.of(val).delay(5000);
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

