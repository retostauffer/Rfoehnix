
///# Some links on the home screen
///home:
///    links:
///    - text: "python foehnix!"
///      href: https://matthiasdusch.github.io/python-foehnix
///    - text: "<img src=\"https://travis-ci.org/retostauffer/Rfoehnix.svg?branch=master\" />"
///      href: ""
$(document).ready(function(){
    var target = $("div.developers")
    if ( target.length == 0 ) return;

    $("div.developers").append("<div class=\"foehnix-links\"></div>");
    $("div.foehnix-links").append("<h2>Links and Batches</h2><ul></ul>");

    // Travis badge
    $(".foehnix-links > ul").append("<li>"
        + "<a href=\"https://matthiasdusch.github.io/python-foehnix\" target=\"_new\">"
        + "<img src=\"https://travis-ci.org/retostauffer/Rfoehnix.svg?branch=master\" /></a>"
        + "</a>")

    // Code coverage badge
    $(".foehnix-links > ul").append("<li>"
        + "<a href=\"https://codecov.io/gh/retostauffer/Rfoehnix\">"
        + "<img src=\"https://codecov.io/gh/retostauffer/Rfoehnix/branch/master/graph/badge.svg\">"
        + "</img>"
        + "</a>")

    // Link to python package
    $(".foehnix-links > ul").append("<li>"
        + "<a href=\"https://matthiasdusch.github.io/foehnix-python/_build/html/index.html\">"
        + "foehnix Python Package!!!</a>"
        + "</a>")
    ///foehnix in action? http://ertel2.uibk.ac.at:8080/ertel/foehndiag.php
});

$(document).ready(function(){
    // Custom action hide/show source code
    $(".hide-Rinput").on("click", function() {
        var elem = $(this).children(".sourceCode")
        if (elem.length > 0) {
            $.each(elem, function() {
                if ($(this).css("display") == "none") {
                    $(this).css("display", "block");
                } else {
                    $(this).css("display", "none");
                }
            });
        }
    });
});
