const { createApp, ref } = Vue

createApp({
  components: {
    'fancy-list': {
      setup() {
        const items = ref([])

        // mock remote data fetching
        setTimeout(() => {
          items.value = [
            { body: 'Scoped Slots Guide', username: 'Evan You', likes: 20 },
            { body: 'Vue Tutorial', username: 'Natalia Tepluhina', likes: 10 }
          ]
        }, 1000)
        return { items }
      },
      props: ['api-url', 'per-page'],
      template: `
        <ul>
          <li v-if="!items.length">
            Loading...
          </li>
          <li v-for="item in items">
            <slot name="item" v-bind="item"/>
          </li>
        </ul>
      `
    }
  }
}).mount('#app')
