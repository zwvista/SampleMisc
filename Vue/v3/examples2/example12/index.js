const { createApp, ref } = Vue

createApp({
  components: {
    'card': {
      template: `
        <div class="card">
          <div v-if="$slots.header" class="card-header">
            <slot name="header" />
          </div>
          
          <div v-if="$slots.default" class="card-content">
            <slot />
          </div>
          
          <div v-if="$slots.footer" class="card-footer">
            <slot name="footer" />
          </div>
        </div>
      `
    }
  }
}).mount('#app')
