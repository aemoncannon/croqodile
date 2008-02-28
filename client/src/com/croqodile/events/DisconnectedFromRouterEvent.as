package com.croqodile.events {
    import flash.events.Event;
    
    public class DisconnectedFromRouterEvent extends Event {
	
	public static var type:String = "disconnectedFromRouter";
	
	public function DisconnectedFromRouterEvent(){
	    super(DisconnectedFromRouterEvent.type);
	}
	
    }
}

