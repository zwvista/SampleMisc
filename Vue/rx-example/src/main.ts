import 'reflect-metadata';
import Vue from 'vue'
import App from './App.vue'
import VueTypeScriptInject from 'vue-typescript-inject';

Vue.config.productionTip = false;

Vue.use(VueTypeScriptInject); // register vue-typescript-inject


new Vue({
  render: h => h(App),
}).$mount('#app');
