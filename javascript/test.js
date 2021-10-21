var vrvToolkit = new verovio.toolkit();

$.ajax({
    url: "mei/Beethoven_StringQuartet_Op18_No2.mei"
    , dataType: "text"
    , success: function(data) {
        var svg = vrvToolkit.renderData(data, {});
        $("#svg_output").html(svg);
    }
});
