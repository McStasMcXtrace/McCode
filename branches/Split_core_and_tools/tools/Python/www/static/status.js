
var baseUrl = "/out/" + runid + "/";


function loadData() {
    // load text files
    loadUrl("out", "out.txt");
    loadUrl("err", "err.txt");
    loadUrl("mcstas", "mcstas/mcstas.sim");

    // load images
    loadImg("imgLayout", "layout.gif");
    loadImg("imgPlot", "plot.gif");
    loadImg("imgPlotLog", "plotlog.gif");
}


function loadUrl(id, file) {
    var myUrl = baseUrl + file;
    waitContent(myUrl,
                function() {
                    var tag = $("#"+id);
                    tag.html("<a href='" + myUrl + "'>" + tag.html() + "</a>");
                });
}


function loadImg(id, file) {
    waitContent(baseUrl + file,
                function () {
                    $("#"+id).attr("src", baseUrl + file);
                });
}


function waitContent(url, cb) {
    // wait until content (url) is ready, then call cb
    $.ajax({
        url: url + "?" + Date.now(), // by-pass browser caching
        type: "HEAD",
        success: cb,
        error: function() {
            setTimeout(function() {
                waitContent(url, cb); // retry
            }, 1000);
        }
    });
}
