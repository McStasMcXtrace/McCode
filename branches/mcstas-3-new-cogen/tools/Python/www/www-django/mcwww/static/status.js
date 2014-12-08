
var simUrl  = '/sim/status/' + runid + '/';
var baseUrl = '/static/out/' + runid + '/';


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

var COMPS = [];


function loadData() {
    // load text files
    loadUrl('out', 'out.txt');
    loadUrl('err', 'err.txt');

    loadUrl('instr', sim + '.instr');
    loadUrl('mcstas', 'mcstas-' + runid + '.tar.gz');

    // load instrument layout
    loadImg('imgLayout', 'layout.png', '150px');
    loadUrl('vrml', 'layout.wrl');

    // load list of components
    populateComps(
        function() {
            if (compN != 'None') {
                loadComponent(compN);
            } else {
                createCompImgs();
            }
            if (npoints > 1) {
                // add links to components
                var div = $('#comps');
                $.each(COMPS,
                       function (i, comp) {
                           // clean out time-stamp
                           var cleanComp = comp.replace(new RegExp('_\\d{10,}\\.'), '.');
                           div.append($('<li>').append($('<a>')
                                                       .attr({'href': simUrl + i})
                                                       .html(cleanComp + '\n')));
                       });
            }
        });
}


function loadComponent(compN) {
    /* load component from all steps from a scan */
    function load(i) {
        var jsonPath = baseUrl + '/mcstas/' + i + '/comps.json';
        waitContent(
            jsonPath,
            function() {
                // grab JSON
                $.getJSON(
                    jsonPath,
                    function(comps) {
                        createCompImg('mcstas/' + i, comps[compN]);
                        if (i < npoints) {
                            load(i + 1);
                        }
                    });
                });
    }
    load(0);
}


function populateComps(callback, path) {
    path = 'mcstas' + (path == undefined ? '' : path);
    var absPath = baseUrl + path + '/comps.json';
    waitContent(
        absPath,
        function() {
            // grab JSON
            $.getJSON(
                absPath,
                function (compList) {
                    if (compList == 'mccode.dat') {
                        populateComps(callback, '/0');
                    } else {
                        COMPS = compList;
                        callback();
                    }
                }
            );
        });
}

function createCompImgs() {
    var comps = npoints > 1 ? ['mccode.dat'] : COMPS;
    $.each(comps, function(_, comp) {
               createCompImg('mcstas', comp);
           });
}


function createCompImg(path, comp) {
    // create img tag
    $.each(
        ['lin', 'log'],
        function(_, mode) {
            var parent = mode + 'Plots';
            var id = uniqueID(parent);
            $('#'+parent).append($('<span>').attr('id', id).css('margin', 'auto'));
            var plot = path + '/plot' + '-' + comp + '-' + mode + '.png'; // changed gif->png Mark Lewis 13-5-14
            loadImg(id, plot, '250px', '/plot/' + runid + '/' + plot);
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


function loadImg(id, file, width, hrefUrl) {
    var myUrl = baseUrl + file;
    hrefUrl = hrefUrl ? hrefUrl : myUrl;
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
                            $('#'+uid+'a').attr('href', hrefUrl);
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
