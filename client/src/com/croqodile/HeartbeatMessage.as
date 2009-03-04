package com.croqodile{
    import flash.events.*;
    
    public class HeartbeatMessage extends ExternalMessage{
		
		public function HeartbeatMessage(num:Number, timestamp:Number):void{
			super(num, timestamp);
		}

		override protected function get type():uint{ return MSG_TYPE_HEARTBEAT; }

		override public function toString():String {
			return "HeartbeatMessage(" + _timestamp + ")";
		}

    }
    
}


