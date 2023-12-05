function replaceNAWithS(row, data, index) {
  for (var i = 0; i < data.length; i++) {
    if (data[i] === null || data[i] === "") {
      $("td:eq(" + i + ")", row).text("s");
    }
  }
}
