
///# Some links on the home screen
///home:
///    links:
///    - text: "python foehnix!"
///      href: https://matthiasdusch.github.io/python-foehnix
///    - text: "<img src=\"https://travis-ci.org/retostauffer/Rfoehnix.svg?branch=master\" />"
///      href: ""
$(document).ready(function(){

    $("div.developers").append("<div class=\"foehnix-links\"></div>");
    $("div.foehnix-links").append("<h2>Links and Batches</h2><ul></ul>");

    $(".foehnix-links > ul").append("<li>"
        + "<a href=\"https://matthiasdusch.github.io/python-foehnix\" target=\"_new\">"
        + "<img src=\"https://travis-ci.org/retostauffer/Rfoehnix.svg?branch=master\" /></a>"
        + "</a>")

    $(".foehnix-links > ul").append("<li>"
        + "<a href=\"https://matthiasdusch.github.io/foehnix-python/_build/html/index.html\">
        + "foehnix Python Package!!!</a>"
        + "</a>")
        + 
    http://ertel2.uibk.ac.at:8080/ertel/foehndiag.php
});
