Vue.component('editable',{
  template:'<div contenteditable="true" @input="update"></div>',
  props:['content'],
  mounted:function(){
    this.$el.innerText = this.content;
  },
  methods:{
    update:function(event){
      this.$emit('update',event.target.innerText);
    }
  }
})

var example = new Vue({
  el: '#example',
  data: {
    text:"This text is editable!"
  }
});