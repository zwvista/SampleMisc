import * as React from 'react';
import 'react-tabs/style/react-tabs.css';
import Add from "./Add";
import {Tab, TabList, TabPanel, Tabs} from "react-tabs";
import {useState} from "react";

function App() {
  const [index, setIndex] = useState(2);

  function handleSelect(index1: number, last: number, event: Event) {
    setIndex(index1);
  }

  return (
    <Tabs
        onSelect={handleSelect}
        selectedIndex={index}
    >
      <TabList>
        <Tab>Foo</Tab>
        <Tab>Bar</Tab>
        <Tab>Baz</Tab>
      </TabList>
      <TabPanel>
        <Add />
      </TabPanel>
      <TabPanel>
        <h2>Hello from Bar</h2>
      </TabPanel>
      <TabPanel>
        <h2>Hello from Baz</h2>
      </TabPanel>
    </Tabs>
  );
}

export default App;
