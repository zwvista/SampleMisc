/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 *
 * Generated with the TypeScript template
 * https://github.com/react-native-community/react-native-template-typescript
 *
 * @format
 */

import React, {type PropsWithChildren} from 'react';
import {
  SafeAreaView,
  StyleSheet,
} from 'react-native';

import { Container } from 'inversify';
import {Provider} from "inversify-react";
import {PostService} from "./post.service";
import {Post2Service} from "./post2.service";
import Add from './Add';

const App = () => {
  return (
    <Provider container={() => {
        const container = new Container();
        container.bind(PostService).toSelf().inSingletonScope();
        container.bind(Post2Service).toSelf().inSingletonScope();
        return container;
    }}>
      <SafeAreaView style={styles.container}>
        <Add />
      </SafeAreaView>
    </Provider>
  );
};

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: 'center',
    justifyContent: 'center',
  }
});
export default App;
