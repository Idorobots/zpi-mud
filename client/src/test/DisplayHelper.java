package test;

import io.socket.IOAcknowledge;

import java.util.HashMap;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class DisplayHelper {

	// returns a text to display in the chatwindow
	public static String displayEvent(String event, IOAcknowledge ack, Object... args){
		String text="";
		
		JSONObject json = (JSONObject) args[0];
		
		try{
			if(event.equals("msg")){
	        	String name = json.getString("nick");
	        	String type = json.getString("type");
	        	String text_msg = json.getString("text");
	        	text = name+ " "+type+": "+text_msg;
	        	
	        }
			
			if(event.equals("bad_action")){
	        	String des = json.getString("description");

				text+= des;
			}
			if(event.equals("character_info")){
				//todo: add examinig different people
	        	String nick = json.getString("nick");
	        	
	        	text = "Your stats "+nick+": ";
	        	JSONObject stats = json.getJSONObject("stats");
	        	JSONArray jsonarray = stats.names();
	        	
	        	text +="\n";
	        	for(int i=0; i<jsonarray.length(); i++ ){
	    			text +="-";
	    			String key = jsonarray.getString(i);
	    			String value = stats.getString(key);
		    			text += key +"-" + value+"-\n";
	    		
	    		}
	    	
	        	//todo: check if it's not this player.
	            String inv = json.getString("inventory");
	            inv = inv.replace("\"", "");
	            inv= inv.replace("[", "");
	            inv= inv.replace("]", "");

	            
	    		String [] inventory = inv.split(",");
	    		if(inventory.length==1){
	    			text += "Inventory is empty.\n";
	    		}
	    		else{
		        	text+="Inventory: \n";
		        	int i=1;
		    		for(String s: inventory){
			
		    		}
	    		}
	    		


	    		   
	        }
			if(event.equals("location_info")){
	        	String name = json.getString("name");
	        	String id = json.getString("id");
	        	String description = json.getString("description");
	        	text = "*You are in " + name + " (" + id + "). " + description;
	        	text+="*\n";
	        	
	        	JSONObject items = json.getJSONObject("items");
	        	JSONArray jsonarray = items.names();
	        	
	    		HashMap<String, String> mappedItems = new HashMap<String, String>();

	    		
	    		for(int i=0; i<jsonarray.length(); i++ ){
	    			text +="*You can see ";
	    			String key = jsonarray.getString(i);
	    			String value = items.getString(key);
		    			text += key +"(" + value+") in here.*\n";
	    		
	    		}
	        	
	        	//todo: check if it's not this player.
	            String players = json.getString("players");
	            players = players.replace("\"", "");
	            players= players.replace("[", "");
	            players= players.replace("]", "");

	            
	    		String [] listedplayers = players.split(",");
	    		for(String s: listedplayers){
	    			text += "*There's ";
	    			text += s;
	    			text +=" in here*\n";
	    		}
	    		
	        	
	        	JSONObject locations = json.getJSONObject("locations");
	        	 jsonarray = locations.names();
	        	
	    		HashMap<String, String> mapppedLocations = new HashMap<String, String>();

	    		text +="*You can go ";
	    		for(int i=0; i<jsonarray.length(); i++ ){
	    			String key = jsonarray.getString(i);
	    			String value = locations.getString(key);
	    			text += key +"(" + value+"), ";
	    		}
	    		text= text.substring(0,text.length()-2);
	    		text+="*\n";
	
			}
			if(event.equals("item_info")){
	            String name = json.getString("name");
	            String descr = json.getString("description");

				text+= "You examine " + name + " - " + descr + "\n";
				text+= "Its modifiers:";
				
				JSONObject modifiers = json.getJSONObject("modifiers");
	        	JSONArray jsonarray = modifiers.names();
	        	
	        	//todo: check if it is displayed correctly - when sending events is implemented
	        	for(int i=0; i<jsonarray.length(); i++ ){
	    			String key = jsonarray.getString(i);
	    			String value = modifiers.getString(key);
	    			text += key +": ";
	    			int valueInt = Integer.parseInt(value);
	    			text+= ((valueInt>0) ? "+" + value  : value);
	    		}
	    		text+="\n";
	    		
				
			}
			if(event.equals("inventory_update")){
				String msgType = json.getString("type");
				String name = json.getString("name");
				if(msgType.equals("take")){
					text+= "You pick up " +name+".\n";
				}
				if(msgType.equals("drop")){
					text+= "You drop "+name +".\n";
				}
				
			}
			if(event.equals("player_enters")){
				
				String currentNick = json.getString("nick");
				String location = json.getString("location");
				text+= "You enter " + location +"...\n";
				//to do: playerXX enters..
			}
			if(event.equals("player_leaves")){

				String currentNick = json.getString("nick");
				String location = json.getString("location");
				text+= "You leave " + location +"...\n";
				//to do: playerXX leaves..
			
			}

			if(event.equals("battle")){
				
				
				String defenderNick = json.getString("defender");
				String attackerNick = json.getString("attacker");
				
				//todo: remove this hardcoded part
				String attacker = (attackerNick.equals("Username")) ? "You" : attackerNick;
				String defender = (defenderNick.equals("Username")) ? "you" : defenderNick;
				
				String msgType = json.getString("type");
				if(msgType.equals("hit")){
					String value = json.getString("value");
					text+= attacker + " smacked " + defender + " for " + value + " damage!\n";
				}
				if(msgType.equals("miss")){
					text += attacker + " missed " + defender + "!\n";
				}
				if(msgType.equals("kill")){
					String value = json.getString("value");
					text+= attacker + " smacked " + defender + " for " + value + " damage!\n";
					text+= attacker + " killed " + defender + "!\n";
				}
			}
			//todo: the rest of message types
			
		} catch(JSONException e){
			text = "invalid command, unknown error!";
		}
        
		
		return text;
	}
}
