const { createApp, ref } = Vue

createApp({
  components: {
    'base-layout': {
      template: `
        <div class="container">
          <header>
            <slot name="header"></slot>
          </header>
          <main>
            <slot></slot>
          </main>
          <footer>
            <slot name="footer"></slot>
          </footer>
        </div>
      `
    }
  }
}).mount('#app')
