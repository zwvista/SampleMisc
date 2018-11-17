import Vue from 'vue'
import App from './App.vue'

Vue.config.productionTip = false

import 'reflect-metadata';
import VueTypeScriptInject from 'vue-typescript-inject';
Vue.use(VueTypeScriptInject); // register vue-typescript-inject


new Vue({
  render: h => h(App),
}).$mount('#app')
