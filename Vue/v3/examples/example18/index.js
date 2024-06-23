const { createApp, ref, watch } = Vue

createApp({
  setup() {
    const question = ref('')
    const answer = ref('Questions usually contain a question mark. ;-)')
    const loading = ref(false)
    
    // watch works directly on a ref
    watch(question, async (newQuestion, oldQuestion) => {
      if (newQuestion.includes('?')) {
        loading.value = true
        answer.value = 'Thinking...'
        try {
          const res = await fetch('https://yesno.wtf/api')
          answer.value = (await res.json()).answer
        } catch (error) {
          answer.value = 'Error! Could not reach the API. ' + error
        } finally {
          loading.value = false
        }
      }
    })
                            
    return {
      question,
      answer,
      loading
    }
  }
}).mount('#app')
