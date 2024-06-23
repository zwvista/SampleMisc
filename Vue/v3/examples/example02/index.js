const { createApp, ref } = Vue

createApp({
  setup() {
    const count = ref(0)

    function increment() {
      count.value++
    }

    return {
      count,
      increment
    }
  }
}).mount('#app')
