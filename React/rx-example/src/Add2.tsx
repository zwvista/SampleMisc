import * as React from 'react';
import './App.css';

import { PostService } from './post.service';
import 'reflect-metadata';
import {useInjection} from "inversify-react";
import { combineLatest, fromEvent, Observable } from 'rxjs';
import { map, pluck, startWith } from 'rxjs/operators';
import {Post2Service} from "./post2.service";
import {useEffect, useState} from "react";

function Add2() {
  const postService = useInjection(PostService);
  const post2Service = useInjection(Post2Service);
  const [number1, setNumber1] = useState('1');
  const [number2, setNumber2] = useState('2');
  const [number3, setNumber3] = useState('3');
  const [result, setResult] = useState('');
  const [result2, setResult2] = useState('');

  function onChangeNumber(e: any, setState: any) {
    const f = () => {
      const g = (s: string) => Number(s) || 0;
      setResult(String(g(number1) + g(number2) + g(number3)));
    };
    if (e != null) {
      const elem = e.nativeEvent.target as HTMLInputElement;
      setState(elem.value);
    }
    f();
  }

  useEffect(() => {
    console.log(postService);
    console.log(post2Service);
    const f = (id: string) => {
      const e = document.getElementById(id) as HTMLInputElement;
      return fromEvent(e, 'input').pipe<unknown, unknown>(pluck('target', 'value'), startWith(e.value)) as Observable<string>;
    };
    const g = (s: string) => Number(s) || 0;
    combineLatest([f('number1'), f('number2'), f('number3')])
      .pipe(map((results: string[]) => String(g(results[0]) + g(results[1]) + g(results[2]))))
      .subscribe(result2 => setResult2(result2));
    onChangeNumber(null, null);
  });

  return (
    <div className="App">
      <p>
        <input name="number1" className="number" value={number1} onChange={e => onChangeNumber(e, setNumber1)} /> +
        <input name="number2" className="number" value={number2} onChange={e => onChangeNumber(e, setNumber2)} /> +
        <input name="number3" className="number" value={number3} onChange={e => onChangeNumber(e, setNumber3)} /> =
        <label>{result}</label>
      </p>
      <p>
        <input id="number1" className="number" defaultValue="1" /> +
        <input id="number2" className="number" defaultValue="2" /> +
        <input id="number3" className="number" defaultValue="3" /> =
        <label>{result2}</label>
      </p>
    </div>
  );
}

export default Add2;
