<template>
  <div>
    <p>
      <input class="number" v-model="number1" @input="onChangeNumber()"> +
      <input class="number" v-model="number2" @input="onChangeNumber()"> +
      <input class="number" v-model="number3" @input="onChangeNumber()"> =
      <label>{{result}}</label>
    </p>
    <p>
      <input id="number1" class="number" value="1"> +
      <input id="number2" class="number" value="2"> +
      <input id="number3" class="number" value="3"> =
      <label>{{result2}}</label>
    </p>
  </div>
</template>

<script lang="ts">
import { Component, Vue } from 'vue-property-decorator';
import { PostService } from './post.service';
import { Post2Service } from './post2.service';
import { inject } from 'vue-typescript-inject';
import { combineLatest, fromEvent, Observable } from 'rxjs';
import { map, pluck, startWith } from 'rxjs/operators';

@Component({
  providers: [
    PostService, Post2Service
  ],
})
export default class App extends Vue {
  // @inject() postService!: PostService;
  @inject() post2Service!: Post2Service;

  number1 = '1';
  number2 = '2';
  number3 = '3';
  result = '';
  result2 = '';

  mounted() {
    const f = (id: string) => {
      const e = document.getElementById(id) as HTMLInputElement;
      return fromEvent(e, 'input').pipe<string, string>(pluck('target', 'value'), startWith(e.value)) as Observable<string>;
    };
    const g = (s: string) => Number(s) || 0;
    combineLatest(f('number1'), f('number2'), f('number3'))
      .pipe(map((results: string[]) => String(g(results[0]) + g(results[1]) + g(results[2]))))
      .subscribe(result => this.result2 = result);
    this.onChangeNumber();
  }

  onChangeNumber() {
    const g = (s: string) => Number(s) || 0;
    this.result = String(g(this.number1) + g(this.number2) + g(this.number3));
  }
}
</script>

<style>
  .number {
    width: 50px;
    text-align: right;
  }
</style>
