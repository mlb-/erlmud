// When this file is loaded, we are assuming to have jQuery and websocket
// support (via Modernizr).

function ready(){
  var ws = new WebSocket(document.location.toString().replace("http:", "ws:"));
  ws.onopen = function() {
    // websocket is connected
    $("#status").html("websocket connected!<br/>");
    // send hello data to server.
    ws.send("hello server!");
  };
  ws.onmessage = function (evt) {
    var receivedMsg = evt.data;
    $("#status").append(receivedMsg + "<br/>");
  };
  ws.onclose = function() {
    // websocket was closed
    $("#status").append("websocket was closed");
  };
}
