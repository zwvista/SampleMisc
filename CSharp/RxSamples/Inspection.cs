﻿using System;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reflection;

namespace RxSamples
{
    public static class Inspection
    {
        public static void Test()
        {
            Any1();
            Any2();
            All();
            Contains();
            DefaultIfEmpty1();
            DefaultIfEmpty2();
            ElementAt();
            SequenceEqual();
        }

        private static void Any1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject = new Subject<int>();
            subject.Subscribe(Console.WriteLine, () => Console.WriteLine("Subject completed"));
            var any = subject.Any();
            any.Subscribe(b => Console.WriteLine("The subject has any values? {0}", b));
            subject.OnNext(1);
            subject.OnCompleted();
        }

        private static void Any2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject = new Subject<int>();
            subject.Subscribe(Console.WriteLine,
            ex => Console.WriteLine("subject OnError : {0}", ex),
            () => Console.WriteLine("Subject completed"));
            var any = subject.Any();
            any.Subscribe(b => Console.WriteLine("The subject has any values? {0}", b),
            ex => Console.WriteLine(".Any() OnError : {0}", ex),
            () => Console.WriteLine(".Any() completed"));
            subject.OnError(new Exception());
        }

        private static void All()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject = new Subject<int>();
            subject.Subscribe(Console.WriteLine, () => Console.WriteLine("Subject completed"));
            var all = subject.All(i => i < 5);
            all.Subscribe(b => Console.WriteLine("All values less than 5? {0}", b));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(6);
            subject.OnNext(2);
            subject.OnNext(1);
            subject.OnCompleted();
        }

        private static void Contains()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject = new Subject<int>();
            subject.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Subject completed"));
            var contains = subject.Contains(2);
            contains.Subscribe(
            b => Console.WriteLine("Contains the value 2? {0}", b),
            () => Console.WriteLine("contains completed"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnCompleted();
        }

        private static void DefaultIfEmpty1()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject = new Subject<int>();
            subject.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Subject completed"));
            var defaultIfEmpty = subject.DefaultIfEmpty();
            defaultIfEmpty.Subscribe(
            b => Console.WriteLine("defaultIfEmpty value: {0}", b),
            () => Console.WriteLine("defaultIfEmpty completed"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnCompleted();
        }

        private static void DefaultIfEmpty2()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject = new Subject<int>();
            subject.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Subject completed"));
            var defaultIfEmpty = subject.DefaultIfEmpty();
            defaultIfEmpty.Subscribe(
            b => Console.WriteLine("defaultIfEmpty value: {0}", b),
            () => Console.WriteLine("defaultIfEmpty completed"));
            var default42IfEmpty = subject.DefaultIfEmpty(42);
            default42IfEmpty.Subscribe(
            b => Console.WriteLine("default42IfEmpty value: {0}", b),
            () => Console.WriteLine("default42IfEmpty completed"));
            subject.OnCompleted();
        }

        private static void ElementAt()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject = new Subject<int>();
            subject.Subscribe(
            Console.WriteLine,
            () => Console.WriteLine("Subject completed"));
            var elementAt1 = subject.ElementAt(1);
            elementAt1.Subscribe(
            b => Console.WriteLine("elementAt1 value: {0}", b),
            () => Console.WriteLine("elementAt1 completed"));
            subject.OnNext(1);
            subject.OnNext(2);
            subject.OnNext(3);
            subject.OnCompleted();
        }

        private static void SequenceEqual()
        {
            Console.WriteLine(MethodBase.GetCurrentMethod()?.Name);
            var subject1 = new Subject<int>();
            subject1.Subscribe(
            i => Console.WriteLine("subject1.OnNext({0})", i),
            () => Console.WriteLine("subject1 completed"));
            var subject2 = new Subject<int>();
            subject2.Subscribe(
            i => Console.WriteLine("subject2.OnNext({0})", i),
            () => Console.WriteLine("subject2 completed"));
            var areEqual = subject1.SequenceEqual(subject2);
            areEqual.Subscribe(
            i => Console.WriteLine("areEqual.OnNext({0})", i),
            () => Console.WriteLine("areEqual completed"));
            subject1.OnNext(1);
            subject1.OnNext(2);
            subject2.OnNext(1);
            subject2.OnNext(2);
            subject2.OnNext(3);
            subject1.OnNext(3);
            subject1.OnCompleted();
            subject2.OnCompleted();
        }
    }
}
