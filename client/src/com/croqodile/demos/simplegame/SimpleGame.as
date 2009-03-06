package com.croqodile.simplegame {
    import flash.display.*;
    import flash.ui.Keyboard;
    import com.croqodile.*;
    import com.croqodile.simplegame.*;
    import com.croqodile.events.*;
    import com.croqodile.di.*;
    import flash.events.*;
    
    public class SimpleGame extends Sprite{
		
		private var _controller:Controller;
		private var _worldRef:FarRef;
		private var _avatarRef:FarRef;
		
		public function SimpleGame(islandId:String){

			var config:Object = DIRunner.run([
					{name: "island", klass: SimpleGameWorld, args: {id: islandId, stage: this.stage}, injectArgs: {}},
					{name: "controller", klass: SnapshottingController, args: {}, injectArgs: {island: "island"}} 
				]);
			
			_controller = config.controller;
			_controller.addEventListener(RouterConnectionReadyEvent.type, routerConnectionReady);
		}
		
		////////////////////
        // Event Handlers //
        ////////////////////
		
		public function routerConnectionReady(event:Event):void{
			_worldRef = _controller.island().farRef();
			_controller.addEventListener(AvatarCreatedEvent.type, avatarCreated);
			_worldRef.send("createAvatar",[_controller.userId()]);
			_controller.addEventListener(DisconnectedFromRouterEvent.type, disconnectedFromRouter);
		}
		
		public function disconnectedFromRouter(event:Event):void{}
		
		private function onKeyDown(evt:KeyboardEvent):void{
			evt.stopPropagation();
			
			if(evt.keyCode == Keyboard.LEFT){
				_avatarRef.send("setXVelocity", [-5]);
			}
			else if(evt.keyCode == Keyboard.RIGHT){
				_avatarRef.send("setXVelocity", [5]);
			}
			else if(evt.keyCode == Keyboard.UP){
				_avatarRef.send("setYVelocity", [-5]);
			}
			else if(evt.keyCode == Keyboard.DOWN){
				_avatarRef.send("setYVelocity", [5]);
			}
		}
		
		private function onKeyUp(evt:KeyboardEvent):void{
			evt.stopPropagation();
			
			if(evt.keyCode == Keyboard.LEFT){
				_avatarRef.send("setXVelocity", [0]);
			}
			else if(evt.keyCode == Keyboard.RIGHT){
				_avatarRef.send("setXVelocity", [0]);
			}
			else if(evt.keyCode == Keyboard.UP){
				_avatarRef.send("setYVelocity", [0]);
			}
			else if(evt.keyCode == Keyboard.DOWN){
				_avatarRef.send("setYVelocity", [0]);
			}
		}
		
		public function avatarCreated(event:Event):void{
			var e:AvatarCreatedEvent = AvatarCreatedEvent(event);
			if(_controller.userId() == e.userId){
				_avatarRef = e.avatarRef;
				stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
				stage.addEventListener(KeyboardEvent.KEY_UP, onKeyUp);
			}
		}
		
		
    }
}


