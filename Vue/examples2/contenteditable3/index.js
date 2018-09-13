new Vue({
  el: '#app',
  data: {
    greetText: '',
    color: 'green'
  },
  methods: {
    highlightContent (evt) {
      this.greetText = evt.target.innerText
    }
  }
})