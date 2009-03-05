package com.croqodile{
    import flash.events.*;
    import flash.utils.*;
    
    public class HeartbeatMessage extends ExternalMessage{
		
		public function HeartbeatMessage(num:Number, timestamp:Number):void{
			super(num, timestamp);
		}

		override protected function get type():uint{ return MSG_TYPE_HEARTBEAT; }

		override protected function writePayloadTo(b:IDataOutput):void{ 
			b.writeUnsignedInt(0);
		}

		override public function equals(o:Object):Boolean{
			return (
				super.equals(o) && 
				o is HeartbeatMessage
			);
		}

		override public function toString():String {
			return "HeartbeatMessage(" + _timestamp + ")";
		}

    }
    
}


