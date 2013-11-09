package test;

import io.socket.*;
import org.json.*;

class Main {
    public static void main(String[] args) {
        if(args.length < 3) {
            System.out.println("Usage: client SERVER NICK PASSWORD");
            return;
        }

        try {
            final String server = args[0];
            final String nick = args[1];
            final String password = args[2];
            final String hash = SHA1.hash(nick + password);

            System.out.println("Connecting to " + server + "...");
            final SocketIO socket = new SocketIO(server);
            socket.connect(new IOCallback() {
                    @Override
                    public void onMessage(String msg, IOAcknowledge ack) {
                        // Ignored...
                    }

                    @Override
                    public void onMessage(JSONObject msg, IOAcknowledge ack) {
                        // Ignored...
                    }

                    @Override
                    public void onError(SocketIOException socketIOException) {
                        System.out.println("an Error occured");
                        socketIOException.printStackTrace();
                    }

                    @Override
                    public void onDisconnect() {
                        System.out.println("Connection terminated.");
                    }

                    @Override
                    public void onConnect() {
                        System.out.println("Connection established!");

                        try {
                            socket.emit("authorize",
                                        new JSONObject().put("nick", nick).put("password", hash));
                        } catch (JSONException jsonException) {
                            System.out.println("an Error occured");
                            jsonException.printStackTrace();
                        }
                    }

                    @Override
                    public void on(String event, IOAcknowledge ack, Object... args) {
                        System.out.println("Received an event: '" + event + "'");
                    }
                });

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}