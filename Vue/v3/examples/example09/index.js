const { createApp } = Vue

createApp({
  setup() {
    function say(message) {
      alert(message)
    }
                    
    return {
      say
    }
  }
}).mount('#app')
