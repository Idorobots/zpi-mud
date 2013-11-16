package test;

import io.socket.IOAcknowledge;

import java.util.HashMap;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class DisplayHelper {

	//todo: if there's time - use StringUtils for string operations.
	private String playerName ;
	
	// returns a text to display in the chatwindow
	public static String displayEvent(String playerName, String event, IOAcknowledge ack, Object... args){
		playerName = playerName; 

		String text="";
		
		JSONObject json = (JSONObject) args[0];
		
		try{
			if(event.equals("msg")){
	        	String name = json.getString("nick");
	        	String type = json.getString("type");
	        	String text_msg = json.getString("text");

	        	if (name.equals(playerName)){
	        		//the current players typed a message:
	        		text =  "You" + type.substring(0, type.length()-2)+": "+text_msg;
	        	}
	        	else{
	        		//someone else said something
		        	text = name+ " "+type+": "+text_msg;

	        	}
	        	
	        }
			
			if(event.equals("bad_action")){
	        	String des = json.getString("description");

				text+= des;
			}
			
			if(event.equals("character_info")){
				//todo: add examinig different people
	        	String nick = json.getString("nick");
	        	
	        	if(nick.equals(playerName)){
	        		//it's you
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
		        	
		        	text += "Your inventory ";
		        	
	        	}
	        	else{
	        		//it's a different player. 
	        		text += nick +"'s stats: ";
	        		JSONObject stats = json.getJSONObject("stats");
		        	JSONArray jsonarray = stats.names();
		        	
		        	text +="\n";
		        	for(int i=0; i<jsonarray.length(); i++ ){
		    			text +="-";
		    			String key = jsonarray.getString(i);
		    			String value = stats.getString(key);
			    			text += key +"-" + value+"-\n";
		    		
		    		}
		        	
		        	text += "His/her inventory ";
	        	}
	    	
	            String inv = json.getString("inventory");
	            inv = inv.replace("\"", "");
	            inv= inv.replace("[", "");
	            inv= inv.replace("]", "");

	            
	    		String [] inventory = inv.split(",");
	    		if(inventory.length==1){
	    			text += "is empty.\n";
	    		}
	    		else{
		        	text+=": \n";
		        	int i=1;
		    		for(String s: inventory){
		    			 text+= s +", ";
		    		}
	    		}
	    		//todo: better formatting


	    		   
	        }
			
			
			if(event.equals("location_info")){
	        	String name = json.getString("name");
	        	String id = json.getString("id");
	        	String description = json.getString("description");
	        	text = "*You are in " + name + " (" + id + "). " + description;
	        	text+="*\n";
	        	
	        	JSONObject items = json.getJSONObject("items");
	        	JSONArray jsonarray = items.names();
	        	
	    		
	    		for(int i=0; i<jsonarray.length(); i++ ){
	    			text +="*You can see ";
	    			String key = jsonarray.getString(i);
	    			String value = items.getString(key);
		    			text += key +"(" + value+") in here.*\n";
	    		
	    		}
	        	
	            String players = json.getString("players");
	            players = players.replace("\"", "");
	            players= players.replace("[", "");
	            players= players.replace("]", "");

	            
	    		String [] listedplayers = players.split(",");
	    		for(String s: listedplayers){
		        	//todo: check if it's not this player.

	    			if(!s.equals(playerName)){
		    			text += "*There's ";
		    			text += s;
		    			text +=" in here*\n";
	    			}
	    		}
	    		
	        	
	        	JSONObject locations = json.getJSONObject("locations");
	        	 jsonarray = locations.names();
	        	

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
				if(currentNick.equals(playerName )){
					text+= "You enter " + location +"...\n";
				}
				else{
					text+= currentNick+" enters...";
				}
				
			}
			if(event.equals("player_leaves")){

				String currentNick = json.getString("nick");
				String location = json.getString("location");
				
				if(currentNick.equals(playerName )){
					text+= "You leave " + location +"...\n";
				}
				else{
					text+= currentNick +" leaves "+ location+ "...\n";
				}
			
			}

			if(event.equals("battle")){
				
				
				String defenderNick = json.getString("defender");
				String attackerNick = json.getString("attacker");
				
				String attacker = (attackerNick.equals(playerName)) ? "You" : attackerNick;
				String defender = (defenderNick.equals(playerName)) ? "you" : defenderNick;
				
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
			//todo: the rest of message types ???
			
		} catch(JSONException e){
			text = "invalid command, unknown error!";
		}
        
		
		return text;
	}
}
