// https://stackoverflow.com/questions/46845833/vue-how-to-bind-table-column-to-a-data-object
var app = new Vue({
  el: '#app',
  data: {
    assignedApplications: [
      { party_id: 1, num: 247 },
      { party_id: 2, num: 0 },
      { party_id: 3, num: 44 },
      { party_id: 4, num: 76 },
      { party_id: 5, num: 9 },
    ]
  },
  methods: {
    update: function() {
      let newData = [
        { party_id: 1, num: 243},
        { party_id: 2, num: 80},
        { party_id: 4, num: 0},
      ];
      this.mergeInNewData(newData);
    },
    mergeInNewData(newData) {
      for (let i = 0; i < newData.length; i++) {
        let changedData = newData[i];
        for (let j = 0; j < this.assignedApplications.length; j++) {
          if (this.assignedApplications[j].party_id === changedData.party_id) {
            this.assignedApplications[j].num = changedData.num;
          }
        }
      }
    }
  }
})
