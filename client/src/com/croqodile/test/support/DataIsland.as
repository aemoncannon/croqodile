package com.croqodile.test.support {
    import flash.display.MovieClip;
    import flash.display.Stage;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.test.support.*;
    import flash.events.*;
    
    public class DataIsland extends IslandReplica {
	private var _avatars:Array = [];
	private var _sunRisen:Boolean = false;

	public function DataIsland(config:Object){
	    super();
	}

	override public function freeze():Object{
	    var data:Object = new Object();
	    data.avatars = [];
	    for(var i:int = 0; i < this._avatars.length; i ++){
		var avatar:Avatar = this._avatars[i];
		data.avatars.push(avatar.freeze());
	    }
	    return data;
	}
	
	override public function unfreeze(data:Object):void {
	    this._avatars = [];
	    for(var i:int = 0; i < data.avatars.length; i++){
		var avatarData:Object = data.avatars[i];
		var avatar:Avatar = new Avatar(this, avatarData.userId);
		avatar.unfreeze(avatarData);
		this._avatars.push(avatar);
	    }
	}


	// Define some accessors useful for testing. //

	public function avatars():Array{
	    return this._avatars;
	}
	
	public function msgQ():MessageQ{
	    return this._msgQ;
	}

	public function islandTime():Number{
	    return this._islandTime;
	}

	public function internalMessageCounter():int{
	    return this._internalMessageCounter;
	}

	////////////////////////
        // external interface //
        ////////////////////////

	public function createAvatar(userId:String):void {
	    var avatar:Avatar = new Avatar(this, userId);
	    var ref:FarRef = avatar.farRef();
	    this.signalEvent(new AvatarCreatedEvent(ref, userId));
	    avatar.startAnimating();
	    this._avatars.push(avatar);
	}

	/* Useful for testing the scheduling of message scheduling*/
	public function setOff(offset:Number, msgName:String, ...args):void {
	    this.futureSend(offset, msgName, args);
	}

	override public function sunrise():void {
	    Output.trace("Sunrise called");
	}


    }
}


