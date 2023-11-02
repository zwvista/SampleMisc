<template>
  <div>
    <span>{{ getNum() }}</span>
    <button @click="increase()">Increase</button>
    <span>{{ getStr() }}</span>
  </div>

</template>

<script lang="ts">
  import 'reflect-metadata'
  import Vue from 'vue';
  import Component from "vue-class-component";
  import VueTypeScriptInject, { injectable, inject } from "vue-typescript-inject";

  @injectable() // identify service class
  class ServiceA {
    public num = 0;

    public increase() {
      this.num += 1;
    }
  }

  @injectable() // identify service class
  class ServiceB {
    constructor(private readonly _serviceA: ServiceA) {} // will be auto injected

    public get str() {
      return "" + this._serviceA.num;
    }
  }

  @Component({
    providers: [ServiceA, ServiceB] // register service providers
  })
  export default class HelloWorld extends Vue {
    services = {};
    @inject() _serviceA: ServiceA = null; // same as @inject(ServiceA)
    @inject(ServiceB) _serviceB: ServiceB = null;

    public increase() {
      this._serviceA.increase();
    }

    public getNum() {
      return this._serviceA.num;
    }

    public getStr() {
      return this._serviceB.str;
    }

    created() {
      this.$set(this.services, "_serviceA", this._serviceA);
      this.$set(this.services, "_serviceB", this._serviceB);
    }
  }

</script>

