const { createApp, ref } = Vue

createApp({
  setup() {
    const awesome = ref(true)
    
    return {
      awesome
    }
  }
}).mount('#app')
