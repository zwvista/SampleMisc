const { createApp, ref } = Vue

createApp({
  setup() {
    const parentMessage = ref('Parent')
    const items = ref([{ message: 'Foo' }, { message: 'Bar' }])
        
    return {
      parentMessage,
      items
    }
  }
}).mount('#app')
