package com.croqodile{
    import flash.events.*;
    
    public class HeartbeatMessage extends ExternalMessage{
		
		public function HeartbeatMessage(num:Number, timestamp:Number):void{
			super(num, timestamp);
		}

		override public function toString():String {
			return "HeartbeatMessage(" + _timestamp + ")";
		}

    }
    
}


