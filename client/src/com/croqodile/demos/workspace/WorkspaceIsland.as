package com.croqodile.workspace {

    import flash.display.MovieClip;
    import flash.display.Stage;
    import flash.display.Sprite;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.workspace.*;
    import flash.events.*;
    
    public class WorkspaceIsland extends IslandReplica {
	private var _canvas:Canvas;
	private var _avatars:Array = [];
	
	public function WorkspaceIsland(config:Object){
	    super();
	    this._canvas = config.canvas;
	    this.clearWithWhite();
	}
	

	
	override public function freeze():Object{
	    var data:Object = new Object();
	    data.avatars = [];
	    for each(var avatar:Avatar in this._avatars){
		    data.avatars.push(avatar.freeze());
		}
	    return data;
	}


	override public function unfreeze(data:Object):void {
	    for each(var avatarData:Object in data.avatars){
		    var avatar:Avatar = new Avatar(this, avatarData.userId);
		    avatar.unfreeze(avatarData);
		    this._avatars.push(avatar);
		}
	    
	}
	
	public function canvas():Sprite{
	    return this._canvas;
	}
	
	public function render():void {
	    for each(var av:Avatar in this._avatars){
		    av.render();
		}
	}
	
	private function clearWithWhite():void{
	    this._canvas.draw();
	}

	///////////////////////////////
        // Internal Interface	     //
        ///////////////////////////////

	public function avatarMoveTo(avatar:Avatar, x:Number, y:Number){}	

	public function avatarGrab(avatar:Avatar, x:Number, y:Number){}	

	public function avatarRelease(avatar:Avatar, x:Number, y:Number){}	
	
	////////////////////////
        // External Interface //
        ////////////////////////

	/* Executed only once, at the beginning of Island-Time.*/
	override public function sunrise():void {
	}
	
	public function createAvatar(userId:String):void {
	    var avatar:Avatar = new Avatar(this, userId);
	    this._avatars.push(avatar);
	    this.signalEvent(new AvatarCreatedEvent(avatar.farRef(), userId));
	}
	
    }
}


