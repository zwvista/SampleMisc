var app = new Vue({
  el: '#app',
  data: {
    selectedDate: '2018/09/01',
    selectedCompany: '',
    selectedArea: '',
    companyList: [],
    areaList: [],
    fetchTime: '2018/09/01',
    timeList: [],
    priceList: [],
    chargeList: [],
    dischargeList: [],
    autoList: [],
    manualList: [],
    manualOriginalList: [],
    manualSelectedList: [],
  },
  methods: {
    multiEnterClick: function() {
      var value = prompt("一括入力", "");
      if (value === null) return;
      for(var i = 0; i < 48; i++) {
        if (this.manualSelectedList[i]) {
          this.manualList[i] = value;
          console.log(i + " " + this.manualList[i]);
          $(".td-manual:nth-child(" + (i+1) + ")").children('div').html(value);
        }
      }
    },
    clearClick: function() {
      if (!confirm("clear")) return;
      for(var i = 0; i < 48; i++) {
        value = "";
        this.manualList[i] = value;
        $(".td-manual:nth-child(" + (i+1) + ")").children('div').html(value);
      }
    },
    resetClick: function() {
      if (!confirm("reset")) return;
      for(var i = 0; i < 48; i++) {
        value = this.manualOriginalList[i];
        this.manualList[i] = value;
        $(".td-manual:nth-child(" + (i+1) + ")").children('div').html(value);
      }
    },
    searchClick: function() {
      if (this.selectedCompany === '') {
        alert('会社を入力してください。'); return;
      }
      if (this.selectedArea === '') {
        alert('エリアを入力してください。'); return;
      }
    },
    isBlank: function(v) {
      return v === null || v === '';
    },
    checkAuto: function() {
      for (var i = 0; i < 48; i++) {
        var va = this.autoList[i];
        var vm = this.manualList[i];
        if (!this.isBlank(va) && !this.isBlank(vm)) {
          $("#message").val('既に自動設定されている日時に対し、手動設定はできません。');
          return false;
        }
      }
      return true;
    },
    check: function() {
      $("#message").val('');
      if (!this.checkAuto()) return false;
    },
    checkClick: function() {
      this.check();
    },
    execClick: function() {
      this.check();
    },
    manualClick: function(event, index) {
      var $td = $(event.currentTarget);
      var $div = $td.children("div");
      $('.div-manual').not($div).prop('contenteditable', "false");
      if ($div.prop('contenteditable') !== "true") {
        $td.toggleClass("td-manual-selected");
        app.manualSelectedList[index] = !app.manualSelectedList[index];
        console.log(app.manualSelectedList[index]);
        $div.prop('contenteditable', "true");
      }
    },
    manualBlur: function(event, index) {
      $('.div-manual').prop('contenteditable', "false");
      var $div = $(event.currentTarget);
      app.manualList[index] = $div.html();
      console.log(app.manualList[index]);
    }
  }
})
for (var i = 0; i < 48; i++) {
  app.timeList.push("00:00");
  app.priceList.push(11 + i * 0.1);
  app.chargeList.push(11 + i * 0.1);
  app.dischargeList.push(11 + i * 0.1);
  if (i == 0 || i == 4) {
    app.autoList.push(11 + i * 0.1);
  } else {
    app.autoList.push(null);
  }
  if (i % 4 == 2 || i % 4 == 3) {
    app.manualList.push(11 + i * 0.1);
    app.manualOriginalList.push(11 + i * 0.1);
  } else {
    app.manualList.push(null);
    app.manualOriginalList.push(null);
  }
  app.manualSelectedList.push(false);
}
app.companyList.push({id: 0, name: 'A会社'});
app.companyList.push({id: 1, name: 'B会社'});
app.areaList.push({id: 0, name: 'エリア1'});
app.areaList.push({id: 1, name: 'エリア2'});
app.areaList.push({id: 2, name: 'エリア3'});
app.areaList.push({id: 3, name: 'エリア4'});
app.areaList.push({id: 4, name: 'エリア5'});
app.areaList.push({id: 5, name: 'エリア6'});
app.areaList.push({id: 6, name: 'エリア7'});
app.areaList.push({id: 7, name: 'エリア8'});
app.areaList.push({id: 8, name: 'エリア9'});
app.areaList.push({id: 9, name: 'エリア10'});

$(function() {
  $('#date1').datepicker();
  $('#div-scroll-top').on('scroll', function () {
    $('#div-scroll-bottom').scrollLeft($(this).scrollLeft());
  });
  $('#div-scroll-bottom').on('scroll', function () {
    $('#div-scroll-top').scrollLeft($(this).scrollLeft());
  });
});


