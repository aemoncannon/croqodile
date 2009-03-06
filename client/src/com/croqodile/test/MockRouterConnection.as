package com.croqodile.test {
    import flash.events.*;
    import flash.utils.*;
    import flash.net.Socket;
    import com.croqodile.events.*;
    import com.croqodile.*;
    
    public class MockRouterConnection extends RouterConnection {

		private var _num:Number = 0;
		private var _time:Number = 0;
		private var _msgQ:Array = [];
		
		public function MockRouterConnection(config:Object) {
			super(config);
		}	
		
		override public function sendMessage(msg:ExternalMessage):void {
			_num++;
			_time++;
			msg.unsafeStamp(_num, _time);
			_msgQ.push(msg);
		}

		public function pump():void{
			if(_msgQ.length > 0){
				dispatchEvent(new ExternalMessageEvent(_msgQ.shift()));
			}
		}
		
	}
}

