// register the component
Vue.component('component', {
  props: {
  	data: {
    	type: Object
    }
  },
  data: function () {
  	return {
    	typeOf: typeof this.data
    }
  },
  template: '<div>Data: {{data}}<br>Type: {{typeOf}}</div>',
  mounted: function () {
  	console.log(this.data)
  }
})

// create a root instance
new Vue({
  el: '#example'
})