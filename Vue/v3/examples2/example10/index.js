const { createApp, ref } = Vue

createApp({
  components: {
    'submit-button': {
      template: `
        <button type="submit">
          <slot>
            Submit <!-- fallback content -->
          </slot>
        </button>
      `
    }
  }
}).mount('#app')
