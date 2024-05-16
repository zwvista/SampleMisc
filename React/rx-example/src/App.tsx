import * as React from 'react';
import 'react-tabs/style/react-tabs.css';
import Add from "./Add";
import Add2 from "./Add2";
import {Tab, TabList, TabPanel, Tabs} from "react-tabs";
import {useState} from "react";
import Lolly, {lollyReducer} from "./Lolly";
import {Provider} from "react-redux";
import {combineReducers, configureStore} from "@reduxjs/toolkit";

const rootReducer = combineReducers({
  lolly: lollyReducer,
})
const store = configureStore({ reducer: rootReducer })

function App() {
  const [tabIndex, setTabIndex] = useState(3);

  function handleSelect(index: number, last: number, event: Event) {
    setTabIndex(index);
  }

  return (
    <Tabs
        onSelect={handleSelect}
        selectedIndex={tabIndex}
    >
      <TabList>
        <Tab>Foo</Tab>
        <Tab>Foo2</Tab>
        <Tab>Bar</Tab>
        <Tab>Baz</Tab>
      </TabList>
      <TabPanel>
        <Add />
      </TabPanel>
      <TabPanel>
        <Add2 />
      </TabPanel>
      <TabPanel>
        <Provider store={store}>
          <Lolly />
        </Provider>
      </TabPanel>
      <TabPanel>
        <h2>Hello from Baz</h2>
      </TabPanel>
    </Tabs>
  );
}

export default App;
