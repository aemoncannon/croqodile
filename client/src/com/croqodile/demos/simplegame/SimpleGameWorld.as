package com.croqodile.simplegame {
    import flash.display.MovieClip;
    import flash.display.Stage;
    import flash.system.Security;
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
			var avatar:Avatar = new Avatar( userId);
			_avatars.push(avatar);
			_stage.addChild(avatar.clip());
			signalEvent(new AvatarCreatedEvent(avatar.farRef(), userId));
			avatar.startAnimating();
		}
		
		override public function freeze():Object{
			var data:Object = new Object();
			data.avatars = [];
			for(var i:int = 0; i < _avatars.length; i ++){
				var avatar:Avatar = _avatars[i];
				data.avatars.push(avatar.freeze());
			}
			return data;
		}
		
		override public function unfreeze(data:Object):void {
			for each(var a:Avatar in _avatars){
				_stage.removeChild(a.clip());
			}
			_avatars = [];

			for(var i:int = 0; i < data.avatars.length; i++){
				var avatarData:Object = data.avatars[i];
				var avatar:Avatar = new Avatar( avatarData.userId);
				avatar.unfreeze(avatarData);
				_stage.addChild(avatar.clip());
				_avatars.push(avatar);
			}

		}
		
    }
}


