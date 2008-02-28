package com.croqodile.test.support {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.senocular.utils.Output;
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
		return this._data.length;
	    }

	    override public function get connected():Boolean {
		return this._connected;
	    }


	    public function MockSocket(config:Object){
		super();
	    }

	    public function triggerClose():void {
		this.dispatchEvent(new Event(Event.CLOSE));
	    }

	    public function triggerData(data:String):void{
		this._data = data;  
		this.dispatchEvent(new ProgressEvent(ProgressEvent.SOCKET_DATA));
	    }

	    override public function readUnsignedByte():uint{
		if(this._data.length > 0){
		    var byte:Number = this._data.charCodeAt(0);
		    this._data = this._data.substr(1);
		    return byte;
		}
		else {
		    throw new Error("No bytes to read from MockSocket!");
		}
	    }

	    override public function connect(host:String, port:int):void {
		Output.trace("Connecting Mock Socket to " + host + ":" + port);
		this._connected = true;
		this.dispatchEvent(new Event(Event.CONNECT));
	    }

	    override public function close():void{}

	    override public function writeBytes(bytes:ByteArray, offset:uint = 0, length:uint = 0):void{}

	    override public function writeByte(value:int):void{}

	    override public function flush():void{}

	}
}

