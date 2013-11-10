var GLOBAL_ID_MAPPING = {};

/**
 *
 *  Secure Hash Algorithm (SHA1)
 *  http://www.webtoolkit.info/
 *
 **/

function SHA1 (msg) {
    function rotate_left(n,s) {
	var t4 = ( n<<s ) | (n>>>(32-s));
	return t4;
    };
    
    function lsb_hex(val) {
	var str="";
	var i;
	var vh;
	var vl;
	
	for( i=0; i<=6; i+=2 ) {
	    vh = (val>>>(i*4+4))&0x0f;
	    vl = (val>>>(i*4))&0x0f;
	    str += vh.toString(16) + vl.toString(16);
	}
	return str;
    };
    
    function cvt_hex(val) {
	var str="";
	var i;
	var v;
	
	for( i=7; i>=0; i-- ) {
	    v = (val>>>(i*4))&0x0f;
	    str += v.toString(16);
	}
	return str;
    };
    
    
    function Utf8Encode(string) {
	string = string.replace(/\r\n/g,"\n");
	var utftext = "";
	
	for (var n = 0; n < string.length; n++) {
	    
	    var c = string.charCodeAt(n);
	    
	    if (c < 128) {
		utftext += String.fromCharCode(c);
	    }
	    else if((c > 127) && (c < 2048)) {
		utftext += String.fromCharCode((c >> 6) | 192);
		utftext += String.fromCharCode((c & 63) | 128);
	    }
	    else {
		utftext += String.fromCharCode((c >> 12) | 224);
		utftext += String.fromCharCode(((c >> 6) & 63) | 128);
		utftext += String.fromCharCode((c & 63) | 128);
	    }
	    
	}
	
	return utftext;
    };
    
    var blockstart;
    var i, j;
    var W = new Array(80);
    var H0 = 0x67452301;
    var H1 = 0xEFCDAB89;
    var H2 = 0x98BADCFE;
    var H3 = 0x10325476;
    var H4 = 0xC3D2E1F0;
    var A, B, C, D, E;
    var temp;
    
    msg = Utf8Encode(msg);
    
    var msg_len = msg.length;
    
    var word_array = new Array();
    for( i=0; i<msg_len-3; i+=4 ) {
	j = msg.charCodeAt(i)<<24 | msg.charCodeAt(i+1)<<16 |
	    msg.charCodeAt(i+2)<<8 | msg.charCodeAt(i+3);
	word_array.push( j );
    }
    
    switch( msg_len % 4 ) {
    case 0:
	i = 0x080000000;
	break;
    case 1:
	i = msg.charCodeAt(msg_len-1)<<24 | 0x0800000;
	break;
	
    case 2:
	i = msg.charCodeAt(msg_len-2)<<24 | msg.charCodeAt(msg_len-1)<<16 | 0x08000;
	break;
	
    case 3:
	i = msg.charCodeAt(msg_len-3)<<24 | msg.charCodeAt(msg_len-2)<<16 | msg.charCodeAt(msg_len-1)<<8	| 0x80;
	break;
    }
    
    word_array.push( i );
    
    while( (word_array.length % 16) != 14 ) word_array.push( 0 );
    
    word_array.push( msg_len>>>29 );
    word_array.push( (msg_len<<3)&0x0ffffffff );
    
    
    for ( blockstart=0; blockstart<word_array.length; blockstart+=16 ) {
	
	for( i=0; i<16; i++ ) W[i] = word_array[blockstart+i];
	for( i=16; i<=79; i++ ) W[i] = rotate_left(W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16], 1);
	
	A = H0;
	B = H1;
	C = H2;
	D = H3;
	E = H4;
	
	for( i= 0; i<=19; i++ ) {
	    temp = (rotate_left(A,5) + ((B&C) | (~B&D)) + E + W[i] + 0x5A827999) & 0x0ffffffff;
	    E = D;
	    D = C;
	    C = rotate_left(B,30);
	    B = A;
	    A = temp;
	}
	
	for( i=20; i<=39; i++ ) {
	    temp = (rotate_left(A,5) + (B ^ C ^ D) + E + W[i] + 0x6ED9EBA1) & 0x0ffffffff;
	    E = D;
	    D = C;
	    C = rotate_left(B,30);
	    B = A;
	    A = temp;
	}
	
	for( i=40; i<=59; i++ ) {
	    temp = (rotate_left(A,5) + ((B&C) | (B&D) | (C&D)) + E + W[i] + 0x8F1BBCDC) & 0x0ffffffff;
	    E = D;
	    D = C;
	    C = rotate_left(B,30);
	    B = A;
	    A = temp;
	}
	
	for( i=60; i<=79; i++ ) {
	    temp = (rotate_left(A,5) + (B ^ C ^ D) + E + W[i] + 0xCA62C1D6) & 0x0ffffffff;
	    E = D;
	    D = C;
	    C = rotate_left(B,30);
	    B = A;
	    A = temp;
	}
	
	H0 = (H0 + A) & 0x0ffffffff;
	H1 = (H1 + B) & 0x0ffffffff;
	H2 = (H2 + C) & 0x0ffffffff;
	H3 = (H3 + D) & 0x0ffffffff;
	H4 = (H4 + E) & 0x0ffffffff;
	
    }
    
    var temp = cvt_hex(H0) + cvt_hex(H1) + cvt_hex(H2) + cvt_hex(H3) + cvt_hex(H4);
    
    return temp.toLowerCase();
    
}

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
		login.innerHTML = "";

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
		    if(msg.type == "take") {
			receiveInfo(mkGroup(mkText("You pick up "),
					    mkItem(msg.name),
					    mkText(".")));
		    } else {
			receiveInfo(mkGroup(mkText("You drop "),
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

