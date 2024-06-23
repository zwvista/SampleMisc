const { createApp, ref } = Vue

createApp({
  setup() {
    const posts = ref([
      { id: 1, title: 'My journey with Vue' },
      { id: 2, title: 'Blogging with Vue' },
      { id: 3, title: 'Why Vue is so fun' }
    ])
    return { posts }
  },
  components: {
    'alert-box': {
      props: ['title'],
      template: `
        <div class="alert-box">
          <strong>Error!</strong>
          <br/>
          <slot />
        </div>
      `
    }
  }
}).mount('#app')
