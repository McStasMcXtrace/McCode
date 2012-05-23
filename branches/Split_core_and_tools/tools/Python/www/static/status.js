
var baseUrl = '/out/' + runid + '/';


function doInit() {
    setLog();

    $('#linLegend').click(switchLog);
    $('#logLegend').click(switchLog);
    loadData();

    // add a shortcut handler
    $(document).keyup(
        function(e) {
            switch(e.which) {
                case 108: // l
                case  76: // L
                switchLog();
            }
        });
}


function isLog() {
    /* Whether to use log plots */
    return window.location.hash == '#log';
}

function switchLog() {
    /* Flip plots: log -> lin, lin -> log */
    window.location.hash = isLog() ? '#lin' : '#log';
    setLog();
}

function setLog() {
    /* Show the chosen plots as defined by isLog() */
    if (isLog()) {
        // switch to logarithmic plots
        $('#linFieldset').css('display', 'none');
        $('#logFieldset').css('display', 'block');
    } else {
        // switch to linear plots
        $('#linFieldset').css('display', 'block');
        $('#logFieldset').css('display', 'none');
    }
}


function loadData() {
    // load text files
    loadUrl('out', 'out.txt');
    loadUrl('err', 'err.txt');
    loadUrl('mcstas', 'mcstas-' + runid + '.tar.gz');

    // load instrument layout
    loadImg('imgLayout', 'layout.gif', '150px');

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
    $.each(comps.sort(), function(_, comp) {
        // create img tag
        $.each(
            ['lin', 'log'],
            function(_, mode) {
                var parent = mode + 'Plots';
                var id = uniqueID(parent);
                $('#'+parent).append($('<span>').attr('id', id).css('margin', 'auto'));
                loadImg(id, 'plot'+'-'+comp+'-'+mode+'.gif', '250px');
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

function uniqueID(prefix) {
    /* Generate a new ID */
    var uid;
    var n;
    do {
        n = Math.floor(Math.random() * 0xffffffff);
        uid = prefix + '_' + n;
    } while ($('#'+uid).length != 0) ;
    // uid is unused
    return uid;
}


function loadImg(id, file, width) {
    var myUrl = baseUrl + file;
    waitContent(
        myUrl,
        function () {
            var uid = uniqueID(id);
            var taga = $('<a>').attr('id', uid + 'a');
            // create img tag
            taga.append($('<img>').attr('id', uid + 'i').css('width', width));
            // add to imgs
            $('#'+id).append(taga);

            // wait till content is ready
            waitContent(myUrl,
                        function () {
                            $('#'+uid+'a').attr('href', myUrl);
                            $('#'+uid+'i').attr('src', myUrl);
                        });
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
