// Function to toggle source code chunks
function toggle_code() {
    $('div.slide:not(.hidden) > div.sourceCode > pre.sourceCode, div.answer').toggle();
}

// Auto Hide exercise source code
$('div.slide.exercise > div.sourceCode > pre.sourceCode, div.answer').hide();

// Add "show/hide code"" link after header on exercise slides
$('div.exercise > h1').after("<p><a href='javascript:toggle_code()'>Show/Hide Answer</a></p>");

// Color in-line code blue
$("code").not("pre > code").css('color','#1f3394');



w3c_slidy.add_initial_prompt = function () {
  var prompt = this.create_element("div");
  prompt.setAttribute("class", "initial_prompt");

  var p1 = this.create_element("p");
  prompt.appendChild(p1);
  p1.setAttribute("class", "help");

  if (this.keyboardless)
    p1.innerHTML = "Swipe left to move to next slide.<br />" + 
                   "Swipe up/down to toggle table of contents.<br />" + 
                   "Use two fingers to scroll.";
  else
    p1.innerHTML = "Space, Right Arrow or swipe left to move to " +
                   "next slide, click help below for more details";

  this.add_listener(prompt, "click", function (e) {
    document.body.removeChild(prompt);
    w3c_slidy.stop_propagation(e);
  
    if (e.cancel != undefined)
      e.cancel = true;
    
    if (e.returnValue != undefined)
      e.returnValue = false;
    
    return false;
  });

  document.body.appendChild(prompt);
  this.initial_prompt = prompt;
  setTimeout(function() {document.body.removeChild(prompt);}, 5000);
}