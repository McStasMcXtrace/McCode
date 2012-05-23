

function set_changed() {
    $("#btnSave").val("Save");
}

function update_defaults(sim) {
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

    // save
    $.post("/job/update/" + jobid, data, "json")
        .success(function(data) {
            oks = data.oks;
            for (var i = 0; i < oks.length; i++) {
                $("#lbl"+oks[i]).css("color",     "#000000");
            }
            // check errors
            err = data.errors;
            if (err.length > 0) {
                // some errors occured
                $("#messages").html(
                    $("<p>").attr("id", "flash").html(
                        "Errors occured while saving!"));
                for (var i = 0; i < err.length; i++) {
                    $("#lbl"+err[i]).css("color",     "#ff0000");
                }
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
        })
        .error(function(data, status) {
            // damn, what's up?
            alert("An error occured while saving your stuff :/");
            btn.val("Save")
                .attr("disabled", false)
                .css("font-weight", "normal");
            mcb(false);
        });

    return false;
}


function run() {
    save(function(success) {
        if(!success) {
            alert("Configuration contains errors :/");
        } else {
            var form = $('<form>').attr('target', '_blank')
                                  .attr('action', '/sim/' + jobid)
                                  .attr('method', 'POST')
                                  .append($('input[name="__nonce"]'));
            form.submit();
        }
    });
}
