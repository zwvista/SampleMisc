import * as React from 'react';

import { PostService } from './post.service';
import 'reflect-metadata';
import {useInjection} from "inversify-react";
import {Post2Service} from "./post2.service";
import {useEffect, useState} from "react";
import { StyleSheet, Text, TextInput, View } from 'react-native';

const Add = () => {
  const postService = useInjection(PostService);
  const post2Service = useInjection(Post2Service);
  const [number1, setNumber1] = useState('1');
  const [number2, setNumber2] = useState('2');
  const [number3, setNumber3] = useState('3');
  const [result, setResult] = useState('');

  const toNumber = (s: string) => Number(s) || 0;

  useEffect(() => {
    console.log(postService);
    console.log(post2Service);
  }, []);
  useEffect(() => {
    setResult(String(toNumber(number1) + toNumber(number2) + toNumber(number3)));
  }, [number1, number2, number3]);

  return (
    <View>
      <View style={styles.inputWrap}>
        <Text style={styles.label}></Text>
        <TextInput style={styles.textInput} keyboardType='numeric' value={number1} onChangeText={setNumber1} />
      </View>
      <View style={styles.inputWrap}>
        <Text style={styles.label}>+</Text>
        <TextInput style={styles.textInput} keyboardType='numeric' value={number2} onChangeText={setNumber2} />
      </View>
      <View style={styles.inputWrap}>
        <Text style={styles.label}>+</Text>
        <TextInput style={styles.textInput} keyboardType='numeric' value={number3} onChangeText={setNumber3} />
      </View>
      <View style={styles.inputWrap}>
        <Text style={styles.label}>=</Text>
        <Text style={styles.textResult}>{result}</Text>
      </View>
    </View>
  );
};

const styles = StyleSheet.create({
  inputWrap: {
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
  },
  label: {
    width: 30,
  },
  textInput: {
    height: 40,
    width: '50%',
    borderColor: 'gray',
    borderWidth: 1,
    textAlign:'right',
  },
  textResult: {
    width: '50%',
    textAlign:'right',
  },
});

export default Add;
