package com.croqodile.events {
    import flash.events.Event;
	import com.croqodile.ExternalMessage;
    
    public class ExternalMessageEvent extends Event {
		
		public static var type:String = "externalMessaageEvent";
		public var msg:ExternalMessage;

		public function ExternalMessageEvent(msg:ExternalMessage){
			super(ExternalMessageEvent.type);
			this.msg = msg;
		}
		
    }
}

