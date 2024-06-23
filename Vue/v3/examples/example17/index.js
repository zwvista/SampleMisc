const { createApp, ref } = Vue

createApp({
  setup() {
    const selected = ref('A')

    const options = ref([
      { text: 'One', value: 'A' },
      { text: 'Two', value: 'B' },
      { text: 'Three', value: 'C' }
    ])
                        
    return {
      selected,
      options
    }
  }
}).mount('#app')
