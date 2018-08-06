// Function to toggle source code chunks
function toggle_code() {
    $('div.slide:not(.hidden) > div.sourceCode > pre.sourceCode, div.answer').toggle();
}

// Auto Hide exercise source code
$('div.slide.exercise > div.sourceCode > pre.sourceCode, div.answer').hide();

// Add "show/hide code"" link after header on exercise slides
$('div.exercise > h1').after("<p><a href='javascript:toggle_code()'>Show/Hide Answer</a></p>");

// Color in-line code blue
$("code").not("pre > code").css('color','#1f3394')