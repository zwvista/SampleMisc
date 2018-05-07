﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace RxSamples.ConsoleApp
{
  class FlowControlExamples : ExamplesBase
  {
    public void Simple_Subscribe_will_rethrow_Errors()
    {
      try
      {
        var subject = new Subject<int>();
        subject.Subscribe(Console.WriteLine);
        subject.OnNext(1);
        subject.OnError(new Exception("Test Exception"));
      }
      catch (Exception ex)
      {
        Console.WriteLine("Catching exception using SEH(Sturcted Exception Handling)");
        Console.WriteLine(ex.Message);
      }
    }

    private IObservable<int> ErrorsAfter3()
    {
      return Observable.Create<int>(
        o =>
        {
          o.OnNext(1);
          o.OnNext(2);
          o.OnNext(3);
          o.OnError(new NotImplementedException("Test Exception"));
          return () => { };
        });
    }

    public void Retry_will_retry_on_error_forever()
    {
      var errorAfter3 = ErrorsAfter3();

      var retry = errorAfter3.Retry();
      WriteStreamToConsole(retry, "retry");
    }

    public void Retry_with_argument_of_2_will_retry_once()
    {
      IObservable<int> errorAfter3 = ErrorsAfter3();

      var retry = errorAfter3.Retry(2);
      WriteStreamToConsole(retry, "retry");
    }

    public void OnErrorResumeNext_will_try_another_stream_OnError()
    {
      var first = ErrorsAfter3();
      var second = Observable.Return(10);
      var onErrorResumeNext = first.OnErrorResumeNext(second);
      WriteStreamToConsole(onErrorResumeNext, "onErrorResumeNext");
    }

    public void Catch_is_the_same_as_OnErrorResumeNext_when_no_error_type_is_specified()
    {
      var first = ErrorsAfter3();
      var second = Observable.Return(10);
      var catchStream = first.Catch(second);
      WriteStreamToConsole(catchStream, "catchStream");
    }
    public void Catch_will_try_another_stream_on_a_specific_exception()
    {
      var first = ErrorsAfter3();
      var second = Observable.Return(10);
      var catchStream = first.Catch((NotImplementedException ex) => second);
      WriteStreamToConsole(catchStream, "catchStream");
    }
    public void Catch_will_OnError_if_OnError_not_the_specific_exception()
    {
      var first = ErrorsAfter3();
      var second = Observable.Return(10);
      var catchStream = first.Catch((ArgumentOutOfRangeException ex) => second);
      WriteStreamToConsole(catchStream, "catchStream");
    }

    public void Do_allows_side_effect_free_access_to_streams()
    {
      //Note that the 3rd value is seen by the Do, but not the final stream due to the take(2)
      var stream = ErrorsAfter3();
      var watchedStream = stream.Do(value => Console.WriteLine("Do recived value of {0}", value));
      var final = watchedStream.Take(2);
      WriteStreamToConsole(final, "final");
      /*Can be refactroed to*/
      //var stream2 = ErrorsAfter3()
      //  .Do(value => Console.WriteLine("Do recived value of {0}", value))
      //  .Take(2);
      //WriteStreamToConsole(stream2, "stream2");
    }

    public void Materialize_Dematerialize_and_Do_for_logging()
    {
      var stream = ErrorsAfter3();
      stream.Log().Subscribe();
    }

    public void Run_subscribes_to_all_values_and_blocks()
    {
      var stream = Observable.Interval(TimeSpan.FromMilliseconds(200)).Take(10);
      stream.Run(value=>Console.WriteLine(value), ()=>Console.WriteLine("Completed"));
      /*Can be refactored to just use the method group as per below*/
      //stream.Run(Console.WriteLine);
    }
  }
  
  public static class ExampleExtensions
  {
    /// <summary>
    /// Logs implicit notifications to console. Showcase the Materialise, Dematerialise and Do extension methods.
    /// </summary>
    /// <example>
    /// <code>myStream.Log().Subscribe(....);</code>
    /// </example>
    public static IObservable<T> Log<T>(this IObservable<T> stream)
    {
      return stream.Materialize()
          .Do(n => Console.WriteLine((object)n))
          .Dematerialize();
    }
  }
}