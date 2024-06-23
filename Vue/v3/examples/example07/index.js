const { createApp, ref } = Vue

createApp({
  setup() {
    const counter = ref(0)
            
    return {
      counter
    }
  }
}).mount('#app')
