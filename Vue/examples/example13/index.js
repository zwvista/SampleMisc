Vue.component('blog-post', {
  props: ['title'],
  template: '<h3>{{ title }}</h3>'
})
new Vue({ el: '#blog-post-demo' })
