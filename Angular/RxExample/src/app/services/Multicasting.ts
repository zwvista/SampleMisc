  multicast1() {
    // emit every 2 seconds, take 5
    const source = interval(2000).pipe(take(5));
    
    const example = source.pipe(
      // since we are multicasting below, side effects will be executed once
      tap(() => console.log('Side Effect #1')),
      mapTo('Result!')
    );
    
    // subscribe subject to source upon connect()
    const multi = example.pipe(multicast(() => new Subject()));
    /*
      subscribers will share source
      output:
      "Side Effect #1"
      "Result!"
      "Result!"
      ...
    */
    const subscriberOne = multi.subscribe(val => console.log(val));
    const subscriberTwo = multi.subscribe(val => console.log(val));
    // subscribe subject to source
    multi.connect();
  }

  multicast2() {
    // emit every 2 seconds, take 5
    const source = interval(2000).pipe(take(5));
    
    // example with ReplaySubject
    const example = source.pipe(
      // since we are multicasting below, side effects will be executed once
      tap(_ => console.log('Side Effect #2')),
      mapTo('Result Two!')
    );
    // can use any type of subject
    const multi = example.pipe(multicast(() => new ReplaySubject(5)));
    // subscribe subject to source
    multi.connect();
    
    setTimeout(() => {
      /*
       subscriber will receieve all previous values on subscription because
       of ReplaySubject
       */
      const subscriber = multi.subscribe(val => console.group(val));
    }, 5000);
  }

  publish1() {
    // emit value every 1 second
    const source = interval(1000);
    const example = source.pipe(
      // side effects will be executed once
      tap(_ => console.log('Do Something!')),
      // do nothing until connect() is called
      publish()
    );
    
    /*
      source will not emit values until connect() is called
      output: (after 5s)
      "Do Something!"
      "Subscriber One: 0"
      "Subscriber Two: 0"
      "Do Something!"
      "Subscriber One: 1"
      "Subscriber Two: 1"
    */
    const subscribe = example.subscribe(val =>
      console.log(`Subscriber One: ${val}`)
    );
    const subscribeTwo = example.subscribe(val =>
      console.log(`Subscriber Two: ${val}`)
    );
    
    // call connect after 5 seconds, causing source to begin emitting items
    setTimeout(() => {
      example.connect();
    }, 5000);
  }

