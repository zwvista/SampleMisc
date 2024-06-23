const { createApp, ref } = Vue

createApp({
  setup() {
    const checked = ref(true)
                    
    return {
      checked
    }
  }
}).mount('#app')
