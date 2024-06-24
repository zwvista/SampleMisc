const { createApp, ref } = Vue

createApp({
  components: {
    'my-component': {
      setup() {
        const greetingMessage = 'hello'
        return { greetingMessage }
      },
      template: `
        <div>
          <slot :text="greetingMessage" :count="1"></slot>
        </div>
      `
    }
  }
}).mount('#app')
