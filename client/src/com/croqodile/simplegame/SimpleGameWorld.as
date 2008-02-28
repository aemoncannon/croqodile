package com.croqodile.simplegame {
    import flash.display.MovieClip;
    import flash.display.Stage;
    import flash.system.Security;
    import com.senocular.utils.Output;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.simplegame.*;
    import flash.events.*;
    
    public class SimpleGameWorld extends IslandReplica {
	
	private var _stage:Stage;
	private var _avatars:Array = [];
	
	public function SimpleGameWorld(config:Object){
	    super();
	    this._stage = config.stage;
	}

	public function createAvatar(userId:String):void {
	    var avatar:Avatar = new Avatar(this, userId);
	    this._avatars.push(avatar);
	    this._stage.addChild(avatar.clip());
	    this.signalEvent(new AvatarCreatedEvent(avatar.farRef(), userId));
	    avatar.startAnimating();
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
	    for each(var a:Avatar in this._avatars){
		    this._stage.removeChild(a.clip());
		}
	    this._avatars = [];

	    for(var i:int = 0; i < data.avatars.length; i++){
		var avatarData:Object = data.avatars[i];
		var avatar:Avatar = new Avatar(this, avatarData.userId);
		avatar.unfreeze(avatarData);
		this._stage.addChild(avatar.clip());
		this._avatars.push(avatar);
	    }

	}
	
    }
}


