const { createApp, ref } = Vue

createApp({
  setup() {
    const posts = ref([
      { id: 1, title: 'My journey with Vue' },
      { id: 2, title: 'Blogging with Vue' },
      { id: 3, title: 'Why Vue is so fun' }
    ])
  
    const postFontSize = ref(1)
    return { posts, postFontSize }
  },
  components: {
    'blog-post': {
      props: ['title'],
      emits: ['enlarge-text'],
      template: `
        <h4>{{ title }}</h4>
        <button @click="$emit('enlarge-text')">Enlarge text</button>
      `
    }
  }
}).mount('#app')
