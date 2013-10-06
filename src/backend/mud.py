# @author: kajtek@idorobots.org

import json
from httplib2 import Http
import BaseHTTPServer
from BaseHTTPServer import *

h = Http()
players = []
locations = {"starting_tavern_rooms" :
                 {"id" : "starting_tavern_rooms",
                  "name" : "the second Tavern flood",
                  "description" : "There are some rooms in here..",
                  "players" : [],
                  "items" : ["beer_bottle"],
                  "locations" : {"downstairs" : "starting_tavern"}},
             "starting_tavern" :
                 {"id" : "starting_tavern",
                  "name" : "the Tavern",
                  "description" : "A generic tavern, just like in any RPG.",
                  "players" : [],
                  "items" : [],
                  "locations" : {"upstairs" : "starting_tavern_rooms"}}}

class BackendHTTPRequestHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == "/authorize":
            # A new user is trying to connect...
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            nick = state["trigger"]["args"][0]["nick"]
            sid = state["sid"]
            location = locations["starting_tavern"]
            if nick not in players:
                # If the chosen nickname isn't already in use, we grant the user permission to use it.
                self._join(nick, sid, location),
                self._reply(200, json.dumps([{"action" : "reply",
                                              "args" : {"name" : "authorize",
                                                        "args" : [{"permission" : "granted"}]}},
                                             # We describe the location...
                                             {"action" : "reply",
                                              "args" : {"name" : "location_info",
                                                        "args" : [location]}},
                                             # We also store the nickname in his state for later use.
                                             {"action" : "store",
                                              "args" : {"nick" : nick,
                                                        "location" : location["id"]}}]))
                players.append(nick)
                return
            else:
                self._reply(200, json.dumps([{"action" : "reply",
                                              "args" : {"name" : "authorize",
                                                        "args" : [{"permission" : None}]}}]))
                return

        if self.path == "/say":
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            nick = state["state"]["nick"]
            location = state["state"]["location"]
            text = state["trigger"]["args"][0]["text"]
            self._publish(location, [{"action" : "reply",
                                      "args" : {"name" : "msg",
                                                "args" : [{"nick" : nick,
                                                           "type" : "says",
                                                           "text" : text}]}}])
            self._reply(200, "")
            return

        if self.path == "/do":
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))

            sid = state["sid"]
            nick = state["state"]["nick"]
            curr_location = state["state"]["location"]
            action = state["trigger"]["args"][0]["action"]
            args = state["trigger"]["args"][0]["args"]

            # TODO Hash-map dispatcher.
            if action == "go":
                self._go(curr_location, args, nick, sid)
                return
            
            if action == "look":
                self._look(curr_location, args, nick, sid)
                return
            
            else:
                self._reply(200, json.dumps([{"action" : "reply",
                                              "args" : {"name" : "bad_action",
                                                        "args" : ["You can't do that!"]}}]))
            return
        
        if self.path == "/cleanup":
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            sid = state["sid"]
            nick = state["state"]["nick"]
            curr_location = state["state"]["location"]
            if curr_location in locations:
                self._leave(nick, sid, locations[curr_location])

            if nick in players:
                players.remove(nick)

            self._reply(200, "")
            return

        else:
            # A bad API call. Well damn.
            self.send_response(404)
            self.wfile.write(json.dumps({"error" : "bad_request", "description" : "Unhandled endpoint!"}))
        return

    def _look(self, curr_location, arg, nick, sid):
        curr = locations[curr_location]
        self._reply(200, json.dumps([{"action" : "reply",
                                      "args" : {"name" : "location_info",
                                                "args" : [curr]}}]))
        return

    def _go(self, curr_location, new_location, nick, sid):
        curr = locations[curr_location]
        if new_location in curr["locations"]:
            new = locations[curr["locations"][new_location]]
            # We remove the user from his current location and move him to the new one.
            self._leave(nick, sid, curr)
            self._join(nick, sid, new)
            # Finally, we describe the new location a bit, and store the new location for later.
            self._reply(200, json.dumps([{"action" : "reply",
                                          "args" : {"name" : "location_info",
                                                    "args" : [new]}},
                                         {"action" : "store",
                                          "args" : {"location" : new["id"]}}]))
        else:
            self._reply(200, json.dumps([{"action" : "reply",
                                          "args" : {"name" : "bad_action",
                                                    "args" : ["You can't go there!"]}}]))
            return


    def _reply(self, code, reply):
        self.send_response(code)
        self.send_header("Content-Type", "text/plain")
        self.end_headers()
        self.wfile.write(reply)
        return

    def _join(self, nick, sid, location):
        location["players"].append(nick)
        channel = "locations." + location["id"]
        h.request("http://localhost:1235/api/abcde12345/pubsub/subscribe/" + sid,
                  "POST",
                  json.dumps([channel]))
        h.request("http://localhost:1235/api/abcde12345/pubsub/action/" + channel,
                  "POST",
                  json.dumps([{"action" : "reply",
                               "args" : {"name" : "player_enters",
                                         "args" : [{"location" : location["name"],
                                                    "nick" : nick}]}}]))
        return
    
    def _leave(self, nick, sid, location):
        location["players"].remove(nick)
        channel = "locations." + location["id"]
        h.request("http://localhost:1235/api/abcde12345/pubsub/subscribe/" + sid,
                  "DELETE",
                  json.dumps([channel]))
        h.request("http://localhost:1235/api/abcde12345/pubsub/action/" + channel,
                  "POST",
                  json.dumps([{"action" : "reply",
                               "args" : {"name" : "player_leaves",
                                         "args" : [{"location" : location["name"],
                                                    "nick" : nick}]}}]))
        return

    def _publish(self, location, actions):
        channel = "locations." + location
        h.request("http://localhost:1235/api/abcde12345/pubsub/action/" + channel,
                  "POST",
                  json.dumps(actions)),
        return
    
httpd = BaseHTTPServer.HTTPServer(('127.0.0.1', 8081), BackendHTTPRequestHandler)
sa = httpd.socket.getsockname()

print "Serving HTTP on", sa[0], "port", sa[1], "..."
httpd.serve_forever()
