import React from 'react';
import ReactDOM from 'react-dom/client';
import './index.css';
import App from './App';
import reportWebVitals from './reportWebVitals';
import { Container } from 'inversify';
import {Provider} from "inversify-react";
import {PostService} from "./post.service";
import {Post2Service} from "./post2.service";

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement
);
root.render(
    <Provider container={() => {
        const container = new Container();
        container.bind(PostService).toSelf();
        container.bind(Post2Service).toSelf();
        return container;
    }}>
        <App />
    </Provider>
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
