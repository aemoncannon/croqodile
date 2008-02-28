package com.croqodile.events {
    import flash.events.Event;
    
    public class ExternalMessageEvent extends Event {
	
	public static var type:String = "externalMessaage";
	public var msg:String;

	public function ExternalMessageEvent(msg:String){
	    super(ExternalMessageEvent.type);
	    this.msg = msg;
	}
	
    }
}

