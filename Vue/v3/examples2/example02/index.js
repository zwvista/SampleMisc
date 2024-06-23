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
    'blog-post': {
      props: ['title'],
      template: `
      <h4>{{ title }}</h4>
      `
    }
  }
}).mount('#app')
