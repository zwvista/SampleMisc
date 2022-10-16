import * as React from 'react';

import { PostService } from './post.service';
import 'reflect-metadata';
import {useInjection} from "inversify-react";
import {Post2Service} from "./post2.service";
import {useEffect, useState} from "react";
import { ScrollView, Text, TextInput } from 'react-native';

function Add() {
  const postService = useInjection(PostService);
  const post2Service = useInjection(Post2Service);
  const [number1, setNumber1] = useState('1');
  const [number2, setNumber2] = useState('2');
  const [number3, setNumber3] = useState('3');
  const [result, setResult] = useState('');

  function onChangeNumber(t: any, setState: any) {
    const f = () => {
      const g = (s: string) => Number(s) || 0;
      setResult(String(g(number1) + g(number2) + g(number3)));
    };
    if (t) setState(t);
    f();
  }

  useEffect(() => {
    console.log(postService);
    console.log(post2Service);
    onChangeNumber(null, null);
  });

  return (
    <ScrollView>
      <TextInput value={number1} onChangeText={t => onChangeNumber(t, setNumber1)} />
      <Text>+</Text>
      <TextInput value={number2} onChangeText={t => onChangeNumber(t, setNumber2)} />
      <Text>+</Text>
      <TextInput value={number3} onChangeText={t => onChangeNumber(t, setNumber3)} />
      <Text>=</Text>
      <Text>{result}</Text>
    </ScrollView>
  );
}

export default Add;
