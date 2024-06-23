const { createApp, ref } = Vue

createApp({
  components: {
    'button-counter': {
      setup() {
        const count = ref(0)
        return { count }
      },
      template: `
      <button @click="count++">
        You clicked me {{ count }} times.
      </button>
      `
    }
  }
}).mount('#app')
