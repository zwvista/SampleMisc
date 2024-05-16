import * as React from 'react';
import './App.css';

import { PostService } from './post.service';
import 'reflect-metadata';
import {resolve} from "inversify-react";
import { combineLatest, fromEvent, Observable } from 'rxjs';
import { map, pluck, startWith } from 'rxjs/operators';
import {Post2Service} from "./post2.service";

class Add extends React.Component {
  @resolve postService!: PostService;
  @resolve post2Service!: Post2Service;

  state = {
    number1: '1',
    number2: '2',
    number3: '3',
    result: '',
    result2: '',
  };

  componentDidMount() {
    console.log(this.postService);
    console.log(this.post2Service);
    const f = (id: string) => {
      const e = document.getElementById(id) as HTMLInputElement;
      return fromEvent(e, 'input').pipe<unknown, unknown>(pluck('target', 'value'), startWith(e.value)) as Observable<string>;
    };
    const g = (s: string) => Number(s) || 0;
    combineLatest([f('number1'), f('number2'), f('number3')])
      .pipe(map((results: string[]) => String(g(results[0]) + g(results[1]) + g(results[2]))))
      .subscribe(result2 => this.setState({result2}));
    this.onChangeNumber(null);
  }

  render() {
    return (
      <div className="App">
        <p>
          <input name="number1" className="number" value={this.state.number1} onChange={this.onChangeNumber} /> +
          <input name="number2" className="number" value={this.state.number2} onChange={this.onChangeNumber} /> +
          <input name="number3" className="number" value={this.state.number3} onChange={this.onChangeNumber} /> =
          <label>{this.state.result}</label>
        </p>
        <p>
          <input id="number1" className="number" defaultValue="1" /> +
          <input id="number2" className="number" defaultValue="2" /> +
          <input id="number3" className="number" defaultValue="3" /> =
          <label>{this.state.result2}</label>
        </p>
      </div>
    );
  }

  onChangeNumber = (e: any) => {
    const f = () => {
      const g = (s: string) => Number(s) || 0;
      this.setState({
        result: String(g(this.state.number1) + g(this.state.number2) + g(this.state.number3))
      });
    };
    if (e != null) {
      const elem = e.nativeEvent.target as HTMLInputElement;
      // https://stackoverflow.com/questions/30782948/why-calling-react-setstate-method-doesnt-mutate-the-state-immediately
      this.setState({[elem.name]: elem.value}, () => f());
    } else {
      f();
    }
  };
}

export default Add;
