const { createApp, ref } = Vue

createApp({
  setup() {
    const checkedNames = ref([])
                    
    return {
      checkedNames
    }
  }
}).mount('#app')
