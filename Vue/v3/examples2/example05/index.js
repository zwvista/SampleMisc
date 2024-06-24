const { createApp, ref } = Vue

createApp({
  setup() {
    const currentTab = ref('Home')

    const tabs = {
      Home: 'home',
      Posts: 'posts',
      Archive: 'archive',
    }
    return { currentTab, tabs }
  },
  components: {
    'home': {
      template: `
        <div class="tab">
          Home component
        </div>
      `
    },
    'posts': {
      template: `
        <div class="tab">
          Posts component
        </div>
      `
    },
    'archive': {
      template: `
        <div class="tab">
          Archive component
        </div>
      `
    },
  }
}).mount('#app')
