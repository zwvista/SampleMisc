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

  share1() {
    // emit value in 1s
    const source = timer(1000);
    // log side effect, emit result
    const example = source.pipe(
      tap(() => console.log('***SIDE EFFECT***')),
      mapTo('***RESULT***')
    );
    
    /*
      ***NOT SHARED, SIDE EFFECT WILL BE EXECUTED TWICE***
      output:
      "***SIDE EFFECT***"
      "***RESULT***"
      "***SIDE EFFECT***"
      "***RESULT***"
    */
    const subscribe = example.subscribe(val => console.log(val));
    const subscribeTwo = example.subscribe(val => console.log(val));
    
    // share observable among subscribers
    const sharedExample = example.pipe(share());
    /*
      ***SHARED, SIDE EFFECT EXECUTED ONCE***
      output:
      "***SIDE EFFECT***"
      "***RESULT***"
      "***RESULT***"
    */
    const subscribeThree = sharedExample.subscribe(val => console.log(val));
    const subscribeFour = sharedExample.subscribe(val => console.log(val));
  }

  shareReplay1() {
    //  simulate url change with subject
    const routeEnd = new Subject{data: any, url: string}>();
    
    //  grab url and share with subscribers
    const lastUrl = routeEnd.pipe(
      pluck('url'),
      share()
    );
    
    //  initial subscriber required
    const initialSubscriber = lastUrl.subscribe(console.log);
    
    //  simulate route change
    routeEnd.next({data: {}, url: 'my-path'});
    
    //  nothing logged
    const lateSubscriber = lastUrl.subscribe(console.log);
  }

  shareReplay2() {
    //  simulate url change with subject
    const routeEnd = new Subject{data: any, url: string}>();
    
    //  grab url and share with subscribers
    const lastUrl = routeEnd.pipe(
      tap(_ => console.log('executed')),
      pluck('url'),
      //  defaults to all values so we set it to just keep and replay last one
      shareReplay(1)
    );
    
    //  requires initial subscription
    const initialSubscriber = lastUrl.subscribe(console.log);
    
    //  simulate route change
    //  logged: 'executed', 'my-path'
    routeEnd.next({data: {}, url: 'my-path'});
    
    //  logged: 'my-path'
    const lateSubscriber = lastUrl.subscribe(console.log);
  }

  shareReplay3() {
    //  simulate url change with subject
    const routeEnd = new Subject{data: any, url: string}>();
    
    //  instead of using shareReplay, use ReplaySubject
    const shareWithReplay = new ReplaySubject();
    
    //  grab url and share with subscribers
    const lastUrl = routeEnd.pipe(
      pluck('url')
    )
    .subscribe(val => shareWithReplay.next(val));
    
    //  simulate route change
    routeEnd.next({data: {}, url: 'my-path'});
    
    //  subscribe to ReplaySubject instead
    //  logged: 'my path'
    shareWithReplay.subscribe(console.log);
  }

  shareReplay4() {
      return function shareReplayOperation(this: SubscriberT>, source: ObservableT>) {
        refCount++;
        if (!subject || hasError) {
          hasError = false;
          subject = new ReplaySubjectT>(bufferSize, windowTime, scheduler);
          subscription = source.subscribe({
            next(value) { subject.next(value); },
            error(err) {
              hasError = true;
              subject.error(err);
            },
            complete() {
              isComplete = true;
              subject.complete();
            },
          });
        }
    
    
        const innerSub = subject.subscribe(this);
    
    
        return () => {
          refCount--;
          innerSub.unsubscribe();
          if (subscription && refCount === 0 && isComplete) {
            subscription.unsubscribe();
          }
        };
      };
    }
  }

  shareReplay5() {
    //  simulate url change with subject
    const routeEnd = new Subject{data: any, url: string}>();
    //  grab url and share with subscribers
    const lastUrl = routeEnd.pipe(
      tap(_ => console.log('executed')),
      pluck('url'),
      //  defaults to all values so we set it to just keep and replay last one
      shareReplay(1)
    );
    //  requires initial subscription
    const initialSubscriber = lastUrl.subscribe(console.log)
    //  simulate route change
    //  logged: 'executed', 'my-path'
    routeEnd.next({data: {}, url: 'my-path'});
    //  logged: 'my-path'
    const lateSubscriber = lastUrl.subscribe(console.log);
  }

