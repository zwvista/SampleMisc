const { createApp, reactive, computed } = Vue

createApp({
  setup() {
    const author = reactive({
      name: 'John Doe',
      books: [
        'Vue 2 - Advanced Guide',
        'Vue 3 - Basic Guide',
        'Vue 4 - The Mystery'
      ]
    })
    
    // a computed ref
    const publishedBooksMessage = computed(() => {
      return author.books.length > 0 ? 'Yes' : 'No'
    })
    
    return {
      author,
      publishedBooksMessage
    }
  }
}).mount('#app')
