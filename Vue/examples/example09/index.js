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
    message: '',
  },
  methods: {
    multiEnterClick() {
      var value = prompt("Please enter your value", "一括入力");
      if (value === null) return;
      for(var i = 0; i < 48; i++) {
        if (this.manualSelectedList[i]) {
          this.manualList[i] = value;
          console.log(i + " " + this.manualList[i]);
          $(".td-manual:nth-child(" + (i+1) + ")").html(value);
        }
      }
    },
    clearClick() {
      if (!confirm("clear")) return;
      for(var i = 0; i < 48; i++) {
        value = "";
        this.manualList[i] = value;
        $(".td-manual:nth-child(" + (i+1) + ")").html(value);
      }
    },
    resetClick() {
      if (!confirm("reset")) return;
      for(var i = 0; i < 48; i++) {
        value = this.manualOriginalList[i];
        this.manualList[i] = value;
        $(".td-manual:nth-child(" + (i+1) + ")").html(value);
      }
    },
    searchClick() {
      if (this.selectedCompany === '') {
        alert('company'); return;
      }
      if (this.selectedArea === '') {
        alert('area'); return;
      }
    },
    isBlank(v) {
      return v === null || v === '';
    },
    checkAuto() {
      for (var i = 0; i < 48; i++) {
        var va = this.autoList[i];
        var vm = this.manualList[i];
        if (!this.isBlank(va) && !this.isBlank(vm)) {
          //$("#message").val('auto');
          app.message = 'auto';
          return false;
        }
      }
      return true;
    },
    check() {
      //$("#message").val('');
      app.message = '';
      if (!this.checkAuto()) return false;
    },
    checkClick() {
      this.check();
    },
    execClick() {
      this.check();
    },
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
  $(".td-manual").click(function () {
    $('.td-manual').not($(this)).prop('contenteditable', false);
    if ($(this).prop('contenteditable') !== "true") {
      $(this).toggleClass("td-manual-selected");
      var index = $(this).closest("td").index();
      app.manualSelectedList[index] = !app.manualSelectedList[index];
      console.log(app.manualSelectedList[index]);
      $(this).prop('contenteditable', true);
    }
  });
  $(".td-manual").blur(function () {
    $('.td-manual').prop('contenteditable', false);
    var index = $(this).closest("td").index();
    app.manualList[index] = $(this).html();
    console.log(app.manualList[index]);
  });
});


