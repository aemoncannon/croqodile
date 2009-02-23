package com.croqodile.test.support {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.croqodile.*;
    import com.croqodile.simplegame.*;
    import flash.net.Socket;
    import com.croqodile.events.*;
    import flash.utils.ByteArray;
    import flash.events.*;
    
	public class MockSocket extends Socket {
	    
	    private var _data:String = "";
	    private var _connected:Boolean = false;

	    override public function get bytesAvailable():uint {
			return _data.length;
	    }

	    override public function get connected():Boolean {
			return _connected;
	    }


	    public function MockSocket(config:Object){
			super();
	    }

	    public function triggerClose():void {
			dispatchEvent(new Event(Event.CLOSE));
	    }

	    public function triggerData(data:String):void{
			_data = data;  
			dispatchEvent(new ProgressEvent(ProgressEvent.SOCKET_DATA));
	    }

	    override public function readUnsignedByte():uint{
			if(_data.length > 0){
				var byte:Number = _data.charCodeAt(0);
				_data = _data.substr(1);
				return byte;
			}
			else {
				throw new Error("No bytes to read from MockSocket!");
			}
	    }

	    override public function connect(host:String, port:int):void {
			_connected = true;
			dispatchEvent(new Event(Event.CONNECT));
	    }

	    override public function close():void{}

	    override public function writeBytes(bytes:ByteArray, offset:uint = 0, length:uint = 0):void{}

	    override public function writeByte(value:int):void{}

	    override public function flush():void{}

	}
}

