"use strict";


function enforce_limits() {
    $.each(["seed", "samples", "npoints"], function(_, id) {
               ensure_positive_int(id);
           });

    ensure_max("seed",    2147483647);
    ensure_max("samples", max_samples);
    ensure_max("npoints", max_npoints);
}

function ensure_max(id, max) {
    var input = $('#' + id),
        value = parseInt(input.attr("value"));
    if(value > max) {
        input.attr("value", max);
    }
}

function ensure_positive_int(id) {
    var input = $('#' + id),
        value = parseInt(input.attr("value"));
    if (isNaN(value) || value < 0) {
        value = 1;
    }
    input.attr("value", value);
}


function set_changed() {
    enforce_limits();
    $("#btnSave").val("Save");
}

function update_defaults(sim) {
    // update documentation location
    $('#docLink').attr("href", "/doc/" + sim);

    // update parameters
    var ps = $("#params");
    // clear old
    ps.html("");

    // set new
    for(var param in defaults[sim]) {
        // container, label and input
        var con  = $("<li>"),
            lbl  = $("<label>"),
            lbld = $("<label>"),
            inp  = $("<input>");
        lbl.html(param + ":").attr("for", param).attr("id", "lbl"+param);
        lbld.html("[ " + defaults[sim][param] + " ]")
            .css("float", "right").css("font-family", "monotype");

        inp.attr("name", param).attr("id", param);
        // choose between user value or default
        if (chosen[sim]        != undefined &&
            chosen[sim][param] != undefined) {
            inp.val(chosen[sim][param]);
        } else {
            inp.val(defaults[sim][param]);
        }
        // add event listeners
        inp.bind("keyup", function() {
            chosen[sim][$(this).attr("name")] = $(this).val();
        });
        inp.bind("change", set_changed);
        // add to param list
        ps.append(con.append(lbl, inp, lbld));
    };
}


function save(cb) {
    // update button
    var btn = $("#btnSave");

    if(!cb) {
        btn.attr("disabled", true).val("Saving ...");
    }

    // collect data
    var data = $("#form").serialize();

    // run callback if defined
    function mcb(success) {
        if(cb) {
            cb(success);
        }
    }

    // function for dispalying errors
    function displayErrors(errs, msg) {
        $("#messages").html(
            $("<p>").attr("id", "flash").html(msg));
        $.each(errs, function(i, err) {
            $("#lbl"+err).css("color", "#ff0000");
        });
    }

    // save
    $.ajax({
        type: 'POST',
        url: "/job/update/" + jobid,
        data: data,
        dataType: "json",
        timeout: 5000,
        success: function(data) {
            var oks = data.oks;
            $.each(oks, function(i, ok) {
                $("#lbl"+ok).css("color", "#000000");
            });
            // check errors
            var errs = data.errors;
            if (errs.length > 0) {
                // some errors occured
                displayErrors(errs, "Errors occured while saving!");
                btn.val("Not saved (errors)")
                    .attr("disabled", false);
                mcb(false);
                return;
            }
            // reset button
            btn.val("Save")
                .attr("disabled", false)
                .css("font-weight", "normal");
            mcb(true);
        },
        error: function(xhr, reason) {
            if (reason == "timeout") {
                alert("Timeout! Your session may have expired.\n" +
                      "Refresh page to login and try again (" + xhr.status + ")");
            } else {
                // damn, what's up?
                alert("An error occured while saving your stuff :/ " +
                      "(" + xhr.status + ")");
            }
            btn.val("Not saved (error!)")
                .attr("disabled", false)
                .css("font-weight", "normal");
            mcb(false);
        }});

    return false;
}


function run() {
    save(function(success) {
        if(!success) {
            alert("Configuration contains errors :/");
        } else {
            var nonce = $('input[name="__nonce"]');
            var form = $('<form />').attr({'target': '_blank',
                                           'action': '/sim/' + jobid,
                                           'method': 'POST'}).append(nonce);
            $('#hiddenDiv').html(form);
            form.submit();
            $('#form').append(nonce);
        }
    });
}


function showLatest() {
    return window.location.hash == '#showLatest';
}

function switchLatest() {
    window.location.hash = showLatest() ? '' : '#showLatest';
    setLatest();
}


function setLatest() {
    if(showLatest()) {
        $.getJSON(
            '/sim/latest',
            function(res) {
                var ul = $('<ul>');
                $.each(res.runs, function(i, run) {
                           ul.append($('<a>').attr('href', '/sim/status/'+run.id).append(
                                         $('<li>').html(new Date(run.time*1000).toUTCString() +
                                                        ' - ' +
                                                        run.instrument)));
                       });
                $('#latest').append(ul);
            }
        );
    } else {
        $('#latest').html('');
    }
}