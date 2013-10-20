// @author: kajtek@idorobots.org

function makeChatBox(socket, nick) {
    var box = document.createElement("form");
    box.id = "chat-box";

    var textarea = document.createElement("textarea");
    textarea.cols = "100";
    textarea.rows = "30";
    textarea.setAttribute("readonly", "readonly");
    box.appendChild(textarea);
    box.appendChild(document.createElement("br"))

    var text = document.createElement("input");
    text.type = "text";
    text.size = "90";
    text.value = "You say...";
    var foo = function () {
    	text.value = "";
	text.onclick = undefined;
	text.oninput = undefined;
    };
    text.onclick = foo;
    text.oninput = foo;
    box.appendChild(text);
    
    var send = document.createElement("input");
    send.type = "submit";
    send.value = "Send!";

    box.onsubmit = function () {
	dispatch(socket, text.value);
	text.value = "";
	return false;
    };
    box.appendChild(send);
    box.appendChild(document.createElement("br"))

    var gamebox = document.getElementById("game-box");
    gamebox.appendChild(box);

    receiveInfo("Welcome to the ZPI-MUD, " + nick + "... Type \"help\" for help.");
    return box;
}

function dispatch(socket, text) {
    if(text != "") {
        action = text.split(" ");
	switch (action[0]) {
	case "help":
	    receiveInfo("Available commands:");
	    receiveInfo("move|walk|go|run DESTINATION - onwards you go!");
	    receiveInfo("look - look around.");
	    receiveInfo("help - displays this message.");
	    break;

	case "look":
	    sendAction(socket, "look", "");
	    break;
	    
	case "move":
	    sendAction(socket, "go", action.slice(1).join(" "));
	    break;

	case "run":
	    sendAction(socket, "go", action.slice(1).join(" "));
	    break;

	case "walk":
	    sendAction(socket, "go", action.slice(1).join(" "));
	    break;

	case "go":
	    sendAction(socket, "go", action.slice(1).join(" "));
	    break;

	default:
	    sendMsg(socket, text);
	    break;
	}
        return;
    }
}

function sendAction(socket, action, args) {
    console.log("Sending an action!");
    socket.emit("do", {
	"action" : action,
	"args" : args
    });
}

function sendMsg(socket, text) {
    console.log("Sending a message!");
    socket.emit("say", {
    "type" : "says",
	"text" : text
    });
}

function receiveMsg(nick, action, text) {
    console.log("Received a message!");
    var chatBox = document.getElementById("chat-box");
    if(chatBox !== null) {
	var textarea = chatBox.children[0];
	textarea.value += nick + " " + action + ": " + text + "\n";
	textarea.scrollTop = textarea.scrollHeight - textarea.clientHeight;
    }
}

function receiveInfo(text) {
    console.log("Received some info!");
    var chatBox = document.getElementById("chat-box");
    if(chatBox !== null) {
	var textarea = chatBox.children[0];
	textarea.value += "*" + text + "*\n";
	textarea.scrollTop = textarea.scrollHeight - textarea.clientHeight;
    }
}

function init() {
    console.log("Starting the game!");

    var login = document.getElementById("login-box");
    var server = document.getElementById("server-name").value;
    var nick = document.getElementById("login-name").value;
    var socket = io.connect(server);

    document.getElementById("login-button").onclick = function () {
	console.log("Logging in as " + nick + "...");
	nick = document.getElementById("login-name").value;
        socket.emit("authorize", { "nick" : nick });
    };
    
    socket.on("connect", function () {
	console.log("Connected to the server!");

	socket.on("disconnect", function () {
    	    console.log("Disconnected from the server!");
	    alert("Lost connection to the server! Try refreshing the page... :(");
	});

	socket.on("hive_error", function (reason) {
	    console.log("An error occurred: " + reason.error + " - " + reason.description);
	});

	socket.on("authorize", function (result) {
    	    if (result.permission == "granted") {
		console.log("Authorized!");
		login.innerHTML = "";

		socket.on("bad_action", function (description) {
		    receiveInfo(description);
		});
		
		socket.on("location_info", function (msg) {
		    receiveInfo("You are in " + msg.name + ". " + msg.description);

		    msg.items.forEach(function (i) {
		    	receiveInfo("You can see " + i + " in here...");
		    });

		    msg.players.forEach(function (p) {
		    	if(p != nick) {
			    receiveInfo("There's " + p + " in here...");
			}
		    });

		    Object.keys(msg.locations).forEach(function (l) {
		    	receiveInfo("You can go " + l + "...")
		    });
		});

		socket.on("msg", function (msg) {
		    receiveMsg(msg.nick, msg.type, msg.text);
		});

		socket.on("player_enters", function (msg) {
		    if(msg.nick != nick) {
	    		receiveInfo(msg.nick + " entered " + msg.location + "...");
		    }
		    else {
			receiveInfo("You enter " + msg.location + "...");
		    }
		});

		socket.on("player_leaves", function (msg) {
	    	    if(msg.nick != nick) {
			receiveInfo(msg.nick + " left " + msg.location + "...");
		    }
		    else {
		    	receiveInfo("You leave " + msg.location + "...");
		    }
		});

		makeChatBox(socket, nick).children[2].focus();
	    }
	    else {
		console.log("Not authorized!");
		alert("Failed to login, please select another nickname!");
	    }
	});

	socket.emit("authorize", { "nick" : nick });
    });
}

