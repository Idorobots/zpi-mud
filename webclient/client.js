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
	dispatch(socket, nick, text.value);
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

function dispatch(socket, nick, text) {
    if(text != "") {
        action = text.split(" ");
	switch (action[0]) {
	case "help":
	    receiveInfo("Available commands:");
	    receiveInfo("move|walk|go|run DESTINATION - onwards you go!");
	    receiveInfo("examine|look AT - look at something,");
	    receiveInfo("drop|throw ITEM - drop item from your inventory,");
	    receiveInfo("take|grab|steal ITEM - put an item in your inventory,");
	    receiveInfo("inventory - shows your inventory,");
	    receiveInfo("hit|kick|kill|attack PLAYER - attack somebody,");
	    receiveInfo("help - displays this message.");
	    break;

	case "examine":
	    sendAction(socket, "examine", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;
	    
	case "look":
	    sendAction(socket, "examine", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "inventory":
	    sendAction(socket, "examine", nick);
    	    receiveInput(text);
	    break;
	    
	case "drop":
	    sendAction(socket, "drop", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;
	    
	case "throw":
	    sendAction(socket, "drop", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;
	    
	case "grab":
	    sendAction(socket, "take", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;
	    
	case "take":
	    sendAction(socket, "take", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;
	    
	case "steal":
	    sendAction(socket, "take", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;
	    
	case "move":
	    sendAction(socket, "move", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "run":
	    sendAction(socket, "move", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "walk":
	    sendAction(socket, "move", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "go":
	    sendAction(socket, "move", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "attack":
	    sendAction(socket, "attack", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "hit":
	    sendAction(socket, "attack", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "kick":
	    sendAction(socket, "attack", action.slice(1).join(" "));
    	    receiveInput(text);
	    break;

	case "kill":
	    sendAction(socket, "attack", action.slice(1).join(" "));
    	    receiveInput(text);
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

function receiveInput(text) {
    console.log("Received some input!");
    var chatBox = document.getElementById("chat-box");
    if(chatBox !== null) {
	var textarea = chatBox.children[0];
	textarea.value += "> " + text + "\n";
	textarea.scrollTop = textarea.scrollHeight - textarea.clientHeight;
    }
}

function init() {
    console.log("Starting the game!");

    var login = document.getElementById("login-box");
    var server = document.getElementById("server-name").value;
    var nick = document.getElementById("login-name").value;
    var pass = document.getElementById("login-pass").value;

    if(pass == "") pass = null;
    
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

		socket.on("bad_action", function (description) {
		    receiveInfo(description);
		});
		
		socket.on("location_info", function (msg) {
		    receiveInfo("You are in " + msg.name + " (" + msg.id + "). " + msg.description);

		    Object.keys(msg.items).forEach(function (i) {
		    	receiveInfo("You can see " + msg.items[i] + " (" + i  + ") in here...");
		    });

		    msg.players.forEach(function (p) {
		    	if(p != nick) {
			    receiveInfo("There's " + p + " in here...");
			}
		    });

		    Object.keys(msg.locations).forEach(function (l) {
		    	receiveInfo("You can go " + l + " (" + msg.locations[l] + ")...")
		    });
		});

		socket.on("character_info", function (msg) {
		    if(msg.nick != nick) {
			receiveInfo("You examine " + msg.nick + ".");
			receiveInfo("His/her stats:");
		    } else {
			receiveInfo("Your stats:");
		    }

		    Object.keys(msg.stats).forEach(function (s) {
		    	receiveInfo("- " + s + " - " + msg.stats[s]);
		    });

		    if(msg.nick != nick) {
			receiveInfo("His/her inventory:");
		    } else {
			receiveInfo("Your inventory:");
		    }


		    Object.keys(msg.inventory).forEach(function (i) {
		    	receiveInfo("- " + msg.inventory[i] + " (" + i + ")");
		    });
		});

		socket.on("item_info", function (msg) {
		    receiveInfo("You examine " + msg.name + " - " + msg.description);
		    receiveInfo("Its modifiers:");

		    Object.keys(msg.modifiers).forEach(function (s) {
			var mod = msg.modifiers[s];
		    	receiveInfo(s + " - " + (mod > 0 ? "+" + mod : mod));
		    });
		});

		socket.on("inventory_update", function (msg) {
		    if(msg.type == "take") {
			receiveInfo("You pick up " + msg.name + ".");
		    } else {
			receiveInfo("You drop " + msg.name + ".");
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
		    	receiveInfo(attacker + " smacked " + defender + " for " + msg.value + " damage!");
			break;

		    case "miss":
		    	receiveInfo(attacker + " missed " + defender + "!");
			break;

		    case "kill":
			receiveInfo(attacker + " smacked " + defender + " for " + msg.value + " damage!");
			receiveInfo(attacker + " killed " + defender + "!");
			break;
		    }
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
		alert("Failed to login!");
	    }
	});

	socket.emit("authorize", { "nick" : nick, "password" : SHA1(nick + pass) });
    });
}

