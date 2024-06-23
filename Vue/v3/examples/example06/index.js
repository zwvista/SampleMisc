const { createApp, reactive } = Vue

createApp({
  setup() {
    const myObject = reactive({
      title: 'How to do lists in Vue',
      author: 'Jane Doe',
      publishedAt: '2016-04-10'
    })
            
    return {
      myObject
    }
  }
}).mount('#app')
