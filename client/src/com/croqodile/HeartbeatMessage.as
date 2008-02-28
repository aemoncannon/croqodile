package com.croqodile{
    import flash.display.MovieClip;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.Controller;
    import com.croqodile.Message;
    import com.croqodile.serialization.json.JSON;
    import flash.events.*;
    
    public class HeartbeatMessage extends ExternalMessage{
	
	public static function create(timestamp:Number,
				      island:IslandReplica):HeartbeatMessage{
	    var newMsg:HeartbeatMessage = new HeartbeatMessage();
	    newMsg._timestamp = timestamp;
	    newMsg._island = island;
	    return newMsg;
	}
	
	override public function execute():void {}
	
	override public function toString():String {
	    return "HeartbeatMessage(" + this._timestamp + ")";
	}
    }
    
}


