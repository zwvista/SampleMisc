var app = new Vue({
  el: '#app',
  data: {
    companyList: [],
    areaList: [],
    timeList: [],
    priceList: [],
    chargeList: [],
    dischargeList: [],
    autoList: [],
    manualList: [],
    manualSelectedList: [],
  },
  methods: {
    multiEnterClick() {
      var value = prompt("Please enter your value", "一括入力");
      if (value != null) {
        for(var i = 0; i < 48; i++) {
          if (app.manualSelectedList[i]) {
            app.manualList[i] = value;
            console.log(i + " " + app.manualList[i]);
          }
        }
      }
    }
  }
})
for (var i = 0; i < 48; i++) {
  app.timeList.push("00:00");
  app.priceList.push("13");
  app.chargeList.push("10.0");
  app.dischargeList.push("10.0");
  app.autoList.push("10.0");
  app.manualList.push("10.0");
  app.manualSelectedList.push(false);
}

$(function() {
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
    var index = $(this).closest("td").index();
    app.manualList[index] = $(this).html();
    console.log(app.manualList[index]);
    $(this).prop('contenteditable', false);
  });
});


