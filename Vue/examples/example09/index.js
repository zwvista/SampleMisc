var app = new Vue({
  el: '#app',
  data: {
    selectedDate: '2018/09/01',
    selectedCompany: '',
    selectedArea: '',
    companyList: [],
    areaList: [],
    fetchTime: '',
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
    batchInputClick: function() {
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
    emptyAll: function() {
      this.priceList = [];
      this.chargeList = [];
      this.dischargeList = [];
      this.autoList = [];
      this.manualList = [];
      this.manualOriginalList = [];
      this.manualSelectedList = [];
    },
    clearAll: function() {
      this.emptyAll();
      for (var i = 0; i < 48; i++) {
        this.priceList.push(null);
        this.chargeList.push(null);
        this.dischargeList.push(null);
        this.autoList.push(null);
        this.manualList.push(null);
        this.manualOriginalList.push(null);
        this.manualSelectedList.push(false);
        $(".td-manual:nth-child(" + (i+1) + ")")
          .removeClass("td-readonly").removeClass("td-manual-selected");
      }
      $(".buttons-disabled").prop("disabled", true);
      $("#message").val('');
      this.fetchTime = '';
    },
    showAll: function() {
      this.fetchTime = '2018/09/01 00:00:00';
      this.emptyAll();
      for (var i = 0; i < 48; i++) {
        var value = Math.round((11 + i * 0.1) * 10) / 10;
        this.priceList.push(value);
        this.chargeList.push(i == 1 ? null : value);
        this.dischargeList.push(i == 2 ? null : value);
        this.autoList.push(i == 0 || i == 4 ? value : null);
        if (i % 4 == 2 || i % 4 == 3) {
          this.manualList.push(value);
          this.manualOriginalList.push(value);
        } else {
          this.manualList.push(null);
          this.manualOriginalList.push(null);
        }
        this.manualSelectedList.push(false);
      }
      this.checkOneHour();
    },
    searchClick: function() {
      if (this.selectedCompany === '') {
        alert('会社を入力してください。'); this.clearAll(); return;
      }
      if (this.selectedArea === '') {
        alert('エリアを入力してください。'); this.clearAll(); return;
      }
      $(".buttons-disabled").prop("disabled", false);
      this.showAll();
    },
    checkAuto: function() {
      for (var i = 0; i < 48; i++)
        if (this.autoList[i] && this.manualList[i])
          return false;
      return true;
    },
    checkOneHour: function() {
      for (var i = 0; i < 48; i++) {
        $(".td-manual:nth-child(" + (i+1) + ")").removeClass("td-readonly");
      }
      var result = true;
      for (var i = 0; i < 48; i++) {
        var indexes = [];
        if (i > 0) indexes.push(i - 1);
        if (i > 1) indexes.push(i - 2);
        if (i < 47) indexes.push(i + 1);
        if (i < 46) indexes.push(i + 2);
        for(var k = 0; k < indexes.length; k++) {
          var j = indexes[k];
          if (this.manualList[j]) {
            if (!this.manualList[i]) {
              $(".td-manual:nth-child(" + (i+1) + ")").addClass("td-readonly");
            } else if (j === i - 2 || j === i + 2) {
              $(".td-manual:nth-child(" + (i+1) + ")").addClass("td-readonly");
              result = false;
            }
          }
        }
      }
      return result;
    },
    checkDRFormat: function() {
      for (var i = 0; i < 48; i++)
        if (this.manualList[i] && !/^[+-]?(\d){1,4}(\.\d)?$/.test(this.manualList[i]))
          return false;
      return true;
    },
    checkChargeEmpty: function() {
      for (var i = 0; i < 48; i++)
        if (!this.chargeList[i] && +this.manualList[i] > 0)
          return false;
      return true;
    },
    checkDischargeEmpty: function() {
      for (var i = 0; i < 48; i++)
        if (!this.dischargeList[i] && -this.manualList[i] > 0)
          return false;
      return true;
    },
    checkChargeTooMuch: function() {
      for (var i = 0; i < 48; i++)
        if (this.chargeList[i] && +this.manualList[i] > 0 && +this.manualList[i] > +this.chargeList[i])
          return false;
      return true;
    },
    checkDischargeTooMuch: function() {
      for (var i = 0; i < 48; i++)
        if (this.dischargeList[i] && -this.manualList[i] > 0 && -this.manualList[i] > +this.chargeList[i])
          return false;
      return true;
    },
    commonCheck: function() {
      $("#message").val('');
      if (!this.checkAuto()) {
        $("#message").val('エラー：既に自動設定されている日時に対し、手動設定はできません。');
        return false;
      }
      if (!this.checkDRFormat()) {
        $("#message").val('エラー：設定できるDR量は-9999.9から9999.9の範囲です。');
        return false;
      }
      if (!this.checkChargeEmpty()) {
        $("#message").val('エラー：充電余力がない時間帯のＤＲ要求はできません。');
        return false;
      }
      if (!this.checkChargeEmpty()) {
        $("#message").val('エラー：充電余力がない時間帯のＤＲ要求はできません。');
        return false;
      }
      if (!this.checkDischargeEmpty()) {
        $("#message").val('エラー：放電余力がない時間帯のＤＲ要求はできません。');
        return false;
      }
      if (!this.checkChargeTooMuch()) {
        $("#message").val('エラー：手動設定したDR量が、充電余力のDR量を超過しています。');
        return false;
      }
      if (!this.checkDischargeTooMuch()) {
        $("#message").val('エラー：手動設定したDR量が、放電余力のDR量を超過しています。');
        return false;
      }
      $("#message").val('正常：入力した設定値は正常です。');
      return true;
    },
    checkClick: function() {
      this.commonCheck();
    },
    execClick: function() {
      this.commonCheck();
    },
    manualClick: function(event, index) {
      var $td = $(event.currentTarget);
      var $div = $td.children("div");
      $('.div-manual').not($div).prop('contenteditable', "false");
      if ($div.prop('contenteditable') !== "true") {
        $td.toggleClass("td-manual-selected");
        this.manualSelectedList[index] = !this.manualSelectedList[index];
        console.log(this.manualSelectedList[index]);
        $div.prop('contenteditable', "true");
      }
    },
    manualBlur: function(event, index) {
      $('.div-manual').prop('contenteditable', "false");
      var $div = $(event.currentTarget);
      this.manualList[index] = $div.html();
      console.log(this.manualList[index]);
    }
  }
})
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
for (var i = 0; i < 48; i++) {
  var j = Math.floor(i / 2);
  app.timeList.push((j < 10 ? "0": "") + j + ":" + (i % 2 == 0 ? "00" : "30"));
}
app.clearAll();

$(function() {
  $('#date1').datepicker();
  $('#div-scroll-top').on('scroll', function () {
    $('#div-scroll-bottom').scrollLeft($(this).scrollLeft());
  });
  $('#div-scroll-bottom').on('scroll', function () {
    $('#div-scroll-top').scrollLeft($(this).scrollLeft());
  });
});


