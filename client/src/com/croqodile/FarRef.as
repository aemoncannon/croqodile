package com.croqodile{
    import flash.display.MovieClip;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.IslandReplica;
    import com.croqodile.IslandObject;
    import com.croqodile.ExternalMessage;
    import com.croqodile.Controller;
    import flash.events.*;
    
    public class FarRef{
	
	private var _guid:int;
	private var _island:IslandReplica;

	public function FarRef(guid:int, island:IslandReplica){
	    this._guid = guid;
	    this._island = island;
	}

	public function guid():int {
	    return this._guid;
	}

	public function target():IslandObject {
	    return IslandObject.byGuid(this._guid);
	}

	public function send(msg:String, args:Array):void{
	    this._island.controller().propagateFarSend(ExternalMessage.createRouterString(this._guid, 
											  msg, 
											  args,
											  this._island));
	}

    }
}


