const store = new Vuex.Store({
  state: {
    message: "Hello From Vuex",
    count: 0,
  },
  mutations: {
    increment(state, payload) {
      state.count += payload;
    }
  },
  actions: {
    increment(context, payload) {
      context.commit("increment", payload);
    }
  },
  getters: {
    message: state => state.message.toUpperCase(),
    count: state => state.count,
  },
});
new Vue({
  el: "#app",
  data: {
    welcome: "Hello World",
  },
  computed: {
    message: () => store.getters.message,
    count: () => store.getters.count,
  },
  methods: {
    pressed() {
      store.dispatch("increment", 20);
    }
  }
});
