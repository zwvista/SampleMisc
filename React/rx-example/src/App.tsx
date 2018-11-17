import * as React from 'react';
import './App.css';

import logo from './logo.svg';
import { PostService } from './post.service';
import { Inject, Module } from 'react.di';

@Module({
  providers: [
    PostService,
  ],
})
class App extends React.Component {
  @Inject postService!: PostService;

  componentDidMount() {
    console.log(this.postService);
  }

  public render() {
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to React</h1>
        </header>
        <p className="App-intro">
          To get started, edit <code>src/App.tsx</code> and save to reload.
        </p>
      </div>
    );
  }
}

export default App;
