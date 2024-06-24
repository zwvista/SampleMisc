const { createApp, ref } = Vue

createApp({
  setup() {
    const first = ref('John')
    const last = ref('Doe')
    return { first, last }
  },
  components: {
    'user-name': {
      props: ['firstName', 'lastName'],
      emits: ['update:firstName', 'update:lastName'],
      template: `
        <input
          type="text"
          :value="firstName"
          @input="$emit('update:firstName', $event.target.value)"
        />
        <input
          type="text"
          :value="lastName"
          @input="$emit('update:lastName', $event.target.value)"
        />
      `
    }
  }
}).mount('#app')
