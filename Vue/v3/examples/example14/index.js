const { createApp, ref } = Vue

createApp({
  setup() {
    const picked = ref('One')
                    
    return {
      picked
    }
  }
}).mount('#app')
