
var baseUrl = '/out/' + runid + '/';


function loadData() {
    // load text files
    loadUrl('out', 'out.txt');
    loadUrl('err', 'err.txt');
    loadUrl('mcstas', 'mcstas/mcstas.sim');

    // load list of components
    path = baseUrl + 'comps.json';
    waitContent(
        path,
        function() {
            // grab JSON
            $.getJSON(
                path,
                createCompImgs
            );
        });
}

function createCompImgs(comps) {
    $.each(comps.sort(), function(i, comp) {
        // create img tag
        $.each(
            ['lin', 'log'],
            function(_, mode) {
                var plotid = 'plot'+mode+i;
                // create a tag
                var taga = $('<a>').attr('id', plotid + 'a');
                // create img tag
                taga.append($('<img>').attr('id', plotid + 'i').
                            css('width', '280px'));
                // add to list of plots
                $('#'+mode+'Plots').append(taga);

                // wait till content is ready
                var linUrl = baseUrl+'plot'+'-'+comp+'-'+mode+'.gif';
                waitContent(linUrl,
                            function () {
                                $('#'+plotid+'a').attr('href', linUrl);
                                $('#'+plotid+'i').attr('src', linUrl);
                            });
            });
           });
}


function loadUrl(id, file) {
    var myUrl = baseUrl + file;
    waitContent(
        myUrl,
        function() {
            var tag = $('#'+id);
            tag.html('<a href="' + myUrl + '">' + tag.html() + '</a>');
        }
    );
}


function loadImg(id, file) {
    waitContent(baseUrl + file,
                function () {
                    $('#'+id).attr('src', baseUrl + file);
                });
}


function waitContent(url, cb) {
    // wait until content (url) is ready, then call cb
    $.ajax({
        url: url + '?' + Date.now(), // by-pass browser caching
        type: 'HEAD',
        success: cb,
        error: function() {
            setTimeout(function() {
                waitContent(url, cb); // retry
            }, 1000);
        }
    });
}
