const { createApp, ref } = Vue

createApp({
  components: {
    'fancy-button': {
      template: `
        <button class="fancy-btn">
          <slot/> <!-- slot outlet -->
        </button>
      `
    }
  }
}).mount('#app')
