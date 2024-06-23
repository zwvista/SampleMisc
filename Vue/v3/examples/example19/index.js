const { createApp, ref, onMounted } = Vue

createApp({
  setup() {
    const list = ref([1, 2, 3])

    const itemRefs = ref([])
    
    onMounted(() => {
      alert(itemRefs.value.map(i => i.textContent))
    })
                                
    return {
      list,
      itemRefs
    }
  }
}).mount('#app')
