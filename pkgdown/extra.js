
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
