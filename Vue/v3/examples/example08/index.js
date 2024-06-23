const { createApp, ref } = Vue

createApp({
  setup() {
    const name = ref('Vue.js')

    function greet(event) {
      alert(`Hello ${name.value}!`)
      // `event` is the native DOM event
      if (event) {
        alert(event.target.tagName)
      }
    }
                
    return {
      greet
    }
  }
}).mount('#app')
