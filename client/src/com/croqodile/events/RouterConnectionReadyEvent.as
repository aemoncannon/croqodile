package com.croqodile.events {
    import flash.events.Event;
    
    public class RouterConnectionReadyEvent extends Event {
	
	public static var type:String = "routerConnectionReady";
	
	public function RouterConnectionReadyEvent(){
	    super(RouterConnectionReadyEvent.type);
	}
	
    }
}