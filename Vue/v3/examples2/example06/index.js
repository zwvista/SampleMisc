const { createApp, ref } = Vue

createApp({
  setup() {
    const msg = ref('Hello World!')
    return { msg }
  },
  components: {
    'child': {
      props: ['modelValue'],
      emits: ['update:modelValue'],
      template: `
        <span>My input</span>
        <input
          :value="modelValue"
          @input="$emit('update:modelValue', $event.target.value)"
        />
      `
    }
  }
}).mount('#app')
