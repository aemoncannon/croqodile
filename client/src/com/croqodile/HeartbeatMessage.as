package com.croqodile{
    import flash.events.*;
    
    public class HeartbeatMessage extends ExternalMessage{
		
		public function HeartbeatMessage(timestamp:Number):void{
			super(timestamp);
		}

		override public function toString():String {
			return "HeartbeatMessage(" + _timestamp + ")";
		}

    }
    
}


