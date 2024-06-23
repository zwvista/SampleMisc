const { createApp, ref } = Vue

createApp({
  setup() {
    const selected = ref('')
                    
    return {
      selected
    }
  }
}).mount('#app')
