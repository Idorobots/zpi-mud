// @note: Licensed under the MIT license. See included LICENSE file for details.
// @author: kajtek@idorobots.org

var GLOBAL_ID_MAPPING = {};

function makeChatBox(socket, nick) {
    var textarea = document.createElement("div");
    textarea.id = "text-box";

    var box = document.createElement("form");

    var text = document.createElement("input");
    text.id = "input-text";
    text.type = "text";
    box.appendChild(text);
    
    var send = document.createElement("input");
    send.id = "input-button";
    send.type = "submit";
    send.value = "Send!";

    box.onsubmit = function () {
	dispatch(socket, nick, text.value);
	text.value = "";
	return false;
    };
    box.appendChild(send);
    box.appendChild(document.createElement("br"))

    var gamebox = document.getElementById("game-box");
    gamebox.appendChild(textarea);
    gamebox.appendChild(document.createElement("br"))
    gamebox.appendChild(box);
    gamebox.style.display = "";

    receiveInfo(mkGroup(mkText("Welcome to the ZPI-MUD, "),
			mkCharacter(nick),
			mkText("... Type "),
			mkInput("help"),
			mkText(" for help.")));
    return box;
}

function dispatch(socket, nick, text) {
    text = sanitize(text);
    if(text != "") {
        action = text.split(" ");
	switch (action[0]) {
	case "help":
	    receiveInfo(mkText("Available commands:"));
	    receiveInfo(mkGroup(mkInput("move|walk|go|run DESTINATION", "move "),
				mkText(" - onwards you go!")));
	    receiveInfo(mkGroup(mkInput("examine|look AT", "examine "),
				mkText(" - look at something,")));
	    receiveInfo(mkGroup(mkInput("drop|throw ITEM", "drop"),
				mkText(" - drop item from your inventory,")));
	    receiveInfo(mkGroup(mkInput("take|grab|steal ITEM", "take "),
				mkText(" - put an item in your inventory,")));
	    receiveInfo(mkGroup(mkInput("inventory"),
				mkText(" - shows your inventory,")));
	    receiveInfo(mkGroup(mkInput("hit|kick|kill|attack PLAYER", "hit "),
				mkText(" - attack somebody,")));
	    receiveInfo(mkGroup(mkInput("help", "help"),
				mkText(" - displays this message.")));
	    break;

	case "examine":
	    sendAction(socket, "examine", processArgs(action));
    	    receiveInput(text);
	    break;
	    
	case "look":
	    sendAction(socket, "examine", processArgs(action));
    	    receiveInput(text);
	    break;

	case "inventory":
	    sendAction(socket, "examine", nick);
    	    receiveInput(text);
	    break;
	    
	case "drop":
	    sendAction(socket, "drop", processArgs(action));
    	    receiveInput(text);
	    break;
	    
	case "throw":
	    sendAction(socket, "drop", processArgs(action));
    	    receiveInput(text);
	    break;
	    
	case "grab":
	    sendAction(socket, "take", processArgs(action));
    	    receiveInput(text);
	    break;
	    
	case "take":
	    sendAction(socket, "take", processArgs(action));
    	    receiveInput(text);
	    break;
	    
	case "steal":
	    sendAction(socket, "take", processArgs(action));
    	    receiveInput(text);
	    break;
	    
	case "move":
	    sendAction(socket, "move", processArgs(action));
    	    receiveInput(text);
	    break;

	case "run":
	    sendAction(socket, "move", processArgs(action));
    	    receiveInput(text);
	    break;

	case "walk":
	    sendAction(socket, "move", processArgs(action));
    	    receiveInput(text);
	    break;

	case "go":
	    sendAction(socket, "move", processArgs(action));
    	    receiveInput(text);
	    break;

	case "attack":
	    sendAction(socket, "attack", processArgs(action));
    	    receiveInput(text);
	    break;

	case "hit":
	    sendAction(socket, "attack", processArgs(action));
    	    receiveInput(text);
	    break;

	case "kick":
	    sendAction(socket, "attack", processArgs(action));
    	    receiveInput(text);
	    break;

	case "kill":
	    sendAction(socket, "attack", processArgs(action));
    	    receiveInput(text);
	    break;

	default:
	    sendMsg(socket, text);
	    break;
	}
        return;
    }
}

function processArgs(args) {
    var arg = args.slice(1).join(" ").trim();
    return (arg in GLOBAL_ID_MAPPING) ? GLOBAL_ID_MAPPING[arg] : arg;
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
    receive(mkNGroup("text-msg",
		     mkCharacter(nick),
		     mkText(" " + action + ": "),
		     mkText(text)));
}

function receiveInfo(info) {
    console.log("Received some info!");
    receive(mkNGroup("text-info", info));
}

function receiveInput(input) {
    console.log("Received some input!");
    receive(mkNGroup("text-input", mkInput(input)));
}

function receive(text) {
    var textarea = document.getElementById("text-box");
    if(textarea !== null) {
	textarea.appendChild(text);
	textarea.scrollTop = textarea.scrollHeight - textarea.clientHeight;
    }
}

function mkNGroup(className) {
    var g = mkArrGroup(Array.prototype.slice.apply(arguments).slice(1));
    g.className = className;
    return g;
}

function mkGroup() {
    return mkArrGroup(Array.prototype.slice.apply(arguments));
}

function mkArrGroup(stuff) {
    var g = document.createElement("div");
    for(i = 0; i < stuff.length; ++i) {
    	g.appendChild(stuff[i]);
    }
    return g;
}

function mkText(text) {
    var d = document.createElement("div");
    d.className = "plain-text";
    d.innerHTML = text;
    return d;
}

function mkInput(input) {
    var toAdd = input;
    if(arguments.length > 1) toAdd = arguments[1];
    
    var i = mkText(input);
    i.className = "input-text";
    i.onclick = function () {
    	addInput("", toAdd);
    }
    return i;
}

function mkError(text) {
    var d = document.createElement("div");
    d.className = "error-text";
    d.innerHTML = text;
    return d;
}

function mkCharacter(name) {
    var c = document.createElement("div");
    c.className = "character-name";
    c.innerHTML = name;
    c.onclick = function() {
    	addInput("examine", name);
    };

    return c;
}

function mkItem(name) {
    var i = document.createElement("div");
    i.className = "item-name";
    i.innerHTML = name;
    i.onclick = function() {
    	addInput("examine", name);
    };

    return i;
}

function mkLocation(name) {
    var l = document.createElement("div");
    l.className = "location-name";
    l.innerHTML = name;
    l.onclick = function() {
    	addInput("examine", name);
    };
    
    return l;
}

function mkPath(path) {
    var l = document.createElement("div");
    l.className = "location-path";
    l.innerHTML = path;
    l.onclick = function() {
    	addInput("go", path);
    };

    return l;
}

function addInput(command, text) {
    var input = document.getElementById("input-text");
    if(input !== null) {
	if(input.value == "") {
	    input.value += command;
	    if(command != "") input.value += " ";
	}
	input.value += text;
    }
    document.getElementById("input-text").focus();
}

function sanitize(input) {
    return input.trim().replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/"/g, '&quot;');
}

function init() {
    console.log("Starting the game!");

    var login = document.getElementById("login-box");
    var server = document.getElementById("server-name").value;
    var nick = document.getElementById("login-name").value;
    var pass = document.getElementById("login-pass").value;
    var socket = io.connect(server);

    document.getElementById("login-button").onclick = function () {
	console.log("Logging in as " + nick + "...");
	nick = document.getElementById("login-name").value;
	pass = document.getElementById("login-pass").value;
	if(pass == "") pass = null;
	
        socket.emit("authorize", { "nick" : nick, "password" : SHA1(nick + pass) });
    };
    
    socket.on("connect", function () {
	console.log("Connected to the server!");

	socket.on("disconnect", function () {
    	    console.log("Disconnected from the server!");
	    alert("Lost connection to the server! Try refreshing the page... :(");
	});

	socket.on("hive_error", function (reason) {
	    console.log("An error occurred: " + reason.error + " - " + reason.description);
	    alert(reason.description);
	});

	socket.on("authorize", function (result) {
    	    if (result.permission == "granted") {
		console.log("Authorized!");
		login.style.display = "none";
		
		GLOBAL_ID_MAPPING["you"] = nick;
		GLOBAL_ID_MAPPING["You"] = nick;
		GLOBAL_ID_MAPPING["self"] = nick;
		GLOBAL_ID_MAPPING["myself"] = nick;
		GLOBAL_ID_MAPPING["me"] = nick;

		socket.on("bad_action", function (description) {
		    receiveInfo(mkError(description));
		});
		
		socket.on("location_info", function (msg) {
		    GLOBAL_ID_MAPPING[msg.name] = msg.id;
		    GLOBAL_ID_MAPPING["around"] = msg.id;
		    GLOBAL_ID_MAPPING["surroundings"] = msg.id;
		    
		    receiveInfo(mkGroup(mkText("You are in "),
					mkLocation(msg.name),
					mkText(". " + msg.description)));

		    Object.keys(msg.items).forEach(function (i) {
			GLOBAL_ID_MAPPING[msg.items[i]] = i;

		    	receiveInfo(mkGroup(mkText("You can see "),
					    mkItem(msg.items[i]),
					    mkText(" in here...")));
		    });

		    msg.players.forEach(function (p) {
		    	if(p != nick) {
			    receiveInfo(mkGroup(mkText("There's "),
						mkCharacter(p),
						mkText(" in here...")));
			}
		    });

		    Object.keys(msg.locations).forEach(function (l) {
		    	receiveInfo(mkGroup(mkText("You can go "),
					    mkPath(l)));
		    });
		});

		socket.on("character_info", function (msg) {
		    if(msg.nick != nick) {
			receiveInfo(mkGroup(mkText("You examine "),
					    mkCharacter(msg.nick),
					    mkText(".")));
			receiveInfo(mkText("His/her stats:"));
		    } else {
			receiveInfo(mkText("Your stats:"));
		    }

		    Object.keys(msg.stats).forEach(function (s) {
		    	receiveInfo(mkText("- " + s + " - " + msg.stats[s]));
		    });

		    if(msg.nick != nick) {
			receiveInfo(mkText("His/her inventory:"));
		    } else {
			receiveInfo(mkText("Your inventory:"));
		    }


		    Object.keys(msg.inventory).forEach(function (i) {
			GLOBAL_ID_MAPPING[msg.inventory[i]] = i;

		    	receiveInfo(mkGroup(mkText("- "),
					    mkItem(msg.inventory[i])));
		    });
		});

		socket.on("item_info", function (msg) {
		    receiveInfo(mkGroup(mkText("You examine "),
					mkItem(msg.name),
					mkText(" - " + msg.description)));
		    receiveInfo(mkText("Its modifiers:"));

		    Object.keys(msg.modifiers).forEach(function (s) {
			var mod = msg.modifiers[s];
		    	receiveInfo(mkText(s + " - " + (mod > 0 ? "+" + mod : mod)));
		    });
		});

		socket.on("inventory_update", function (msg) {
		    var who = msg.nick;
		    if(who == nick) who = "You";

		    GLOBAL_ID_MAPPING[msg.name] = msg.id;
		    
		    if(msg.type == "take") {
			receiveInfo(mkGroup(mkCharacter(who),
					    mkText(" picked up "),
					    mkItem(msg.name),
					    mkText(".")));
		    } else {
			receiveInfo(mkGroup(mkCharacter(who),
					    mkText(" dropped "),
					    mkItem(msg.name),
					    mkText(".")));
		    }
		});

		socket.on("msg", function (msg) {
		    receiveMsg(msg.nick, msg.type, msg.text);
		});

		socket.on("battle", function (msg) {
		    var attacker = (msg.attacker == nick) ? "You" : msg.attacker;
		    var defender = (msg.defender == nick) ? "you" : msg.defender;
		    
		    switch(msg.type) {
		    case "hit":
		    	receiveInfo(mkGroup(mkCharacter(attacker),
					    mkText(" smacked "),
					    mkCharacter(defender),
					    mkText(" for " + msg.value + " damage!")));
			break;

		    case "miss":
		    	receiveInfo(mkGroup(mkCharacter(attacker),
					    mkText(" missed "),
					    mkCharacter(defender),
					    mkText("!")));
			break;

		    case "kill":
			receiveInfo(mkGroup(mkCharacter(attacker),
					    mkText(" smacked "),
					    mkCharacter(defender),
					    mkText(" for " + msg.value + " damage!")));
			receiveInfo(mkGroup(mkCharacter(attacker),
					    mkText(" killed "),
					    mkCharacter(defender),
					    mkText("!")));
			break;
		    }
		});
		
		socket.on("player_enters", function (msg) {
		    if(msg.nick != nick) {
	    		receiveInfo(mkGroup(mkCharacter(msg.nick),
					    mkText(" entered "),
					    mkLocation(msg.location),
					    mkText("...")));
		    }
		    else {
			receiveInfo(mkGroup(mkText("You enter "),
					    mkLocation(msg.location),
					    mkText("...")));
		    }
		});

		socket.on("player_leaves", function (msg) {
	    	    if(msg.nick != nick) {
			receiveInfo(mkGroup(mkCharacter(msg.nick),
					    mkText(" left "),
					    mkLocation(msg.location),
					    mkText("...")));
			
		    }
		    else {
		    	receiveInfo(mkGroup(mkText("You leave "),
					    mkLocation(msg.location),
					    mkText("...")));
		    }
		});

		makeChatBox(socket, nick);
		addInput("", "");
	    }
	    else {
		console.log("Not authorized!");
		alert("Failed to login!");
	    }
	});

	socket.emit("authorize", { "nick" : nick, "password" : SHA1(nick + pass) });
    });
}

function preinit() {
    document.getElementById("game-box").style.display = "none";
}
