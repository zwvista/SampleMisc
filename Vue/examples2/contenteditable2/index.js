new Vue({
  el: '#app',
  data: {
    greetText: [],
    color: 'green'
  },
  methods: {
    content () {
      var html = ''
      for(var text of this.greetText) {
        html += '<span>' + text + ' ' + '</span>'
      }
      return html
    },
    highlightContent (evt) {
      var texts = evt.target.innerText.split(' ')
      this.greetText = texts.map(t => t.toLocaleUpperCase())
    }
  }
})