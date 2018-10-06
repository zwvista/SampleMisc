const events = new Vue({}),
parentComponent = new Vue({
  el: '#parent',
  ready() {
    events.$on('eventGreet', () => {
      this.parentMsg = `I heard the greeting event from Child component ${++this.counter} times..`;
    });
  },
  data: {
    parentMsg: 'I am listening for an event..',
    counter: 0
  }
}),
childComponent = new Vue({
  el: '#child',
  methods: {
    greet: function () {
      events.$emit('eventGreet');
      this.childMsg = `I am firing greeting event ${++this.counter} times..`;
    }
  },
  data: {
    childMsg: 'I am getting ready to fire an event.',
    counter: 0
  }
});
