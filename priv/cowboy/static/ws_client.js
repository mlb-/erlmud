// When this file is loaded, we are assuming to have jQuery and websocket
// support (via Modernizr).

function ready(){
  var io = scaffold();
  var input = io.input;
  var output = io.output;
  var ws = ws_setup(output);

  input.bind('keydown', function(e){
    if(e.keyCode == 13){
      e.preventDefault();
      var value = input.val();
      input.val("");
      if(value){
        ws.send(value);
        output(value);
      }
    }
  });
}

function scaffold(){
  // Cleanup unnecessary DOM elements
  $('.support').remove();

  // Scaffold new DOM
  var frame = $('<div>');

  var input = $('<input type="text">')
    .attr({id: 'input'});
  var output = $('<div>', {id: 'output'})
    .css({
      overflow: "scroll",
      height: "300px"
    });

  var scrollTo = $('<div>', {id: 'scrollTo'});

  output.append(scrollTo);

  frame.append(output);
  frame.append(input);

  $('body').append(frame);

  input.focus();
  return {
    input: input,
      output: function(msg){
        scrollTo.before(msg + "<br/>");
        scrollTo[0].scrollIntoView();
      }
  };
}

function ws_setup(output){
  var ws = new WebSocket(
      document.location.toString().replace("http:", "ws:")
      );
  ws.onopen = function() {
    // websocket is connected
    output("Welcome to erlmud!");
    // send hello data to server.
    ws.send("look");
  };
  ws.onmessage = function (evt) {
    var receivedMsg = evt.data;
    output(receivedMsg);
  };
  ws.onclose = function() {
    // websocket was closed
    output("websocket was closed");
  };
  return ws;
}

// vim: fdm=indent
