package com.croqodile.simplegame {
    import flash.display.*;
    import flash.ui.Keyboard;
    import com.senocular.utils.Output;
    import com.croqodile.*;
    import com.croqodile.simplegame.*;
    import com.croqodile.events.*;
    import com.croqodile.di.*;
    import flash.events.*;
    
    public class SimpleGame extends Sprite{
		
		private var _controller:Controller;
		private var _worldRef:FarRef;
		private var _avatarRef:FarRef;
		
		public function SimpleGame(){
			this.stage.stageWidth = 500;
			this.stage.stageHeight = 500;

			var config:Object = DIRunner.run([
					{name: "island", klass: SimpleGameWorld, args: {stage: this.stage}, injectArgs: {}},
					{name: "controller", klass: SnapshottingController, args: {}, injectArgs: {island: "island"}} ]);
			
			this._controller = config.controller;
			this._controller.addEventListener(RouterConnectionReadyEvent.type, routerConnectionReady);
			this.addChild(new Output());
			Output.trace("SimpleGame starting...");
		}
		
		////////////////////
        // Event Handlers //
        ////////////////////
		
		public function routerConnectionReady(event:Event):void{
			Output.trace("Router connection ready.");
			this._worldRef = this._controller.island().farRef();
			this._controller.addEventListener(AvatarCreatedEvent.type, avatarCreated);
			this._worldRef.send("createAvatar",[this._controller.userId()]);
			this._controller.addEventListener(DisconnectedFromRouterEvent.type, disconnectedFromRouter);
		}
		
		public function disconnectedFromRouter(event:Event):void{
			Output.trace("Disconnected from router... :(");
		}
		
		private function onKeyDown(evt:KeyboardEvent):void{
			evt.stopPropagation();
			
			if(evt.keyCode == Keyboard.LEFT){
				this._avatarRef.send("setXVelocity", [-5]);
			}
			else if(evt.keyCode == Keyboard.RIGHT){
				this._avatarRef.send("setXVelocity", [5]);
			}
			else if(evt.keyCode == Keyboard.UP){
				this._avatarRef.send("setYVelocity", [-5]);
			}
			else if(evt.keyCode == Keyboard.DOWN){
				this._avatarRef.send("setYVelocity", [5]);
			}
		}
		
		private function onKeyUp(evt:KeyboardEvent):void{
			evt.stopPropagation();
			
			if(evt.keyCode == Keyboard.LEFT){
				this._avatarRef.send("setXVelocity", [0]);
			}
			else if(evt.keyCode == Keyboard.RIGHT){
				this._avatarRef.send("setXVelocity", [0]);
			}
			else if(evt.keyCode == Keyboard.UP){
				this._avatarRef.send("setYVelocity", [0]);
			}
			else if(evt.keyCode == Keyboard.DOWN){
				this._avatarRef.send("setYVelocity", [0]);
			}
		}
		
		public function avatarCreated(event:Event):void{
			var e:AvatarCreatedEvent = AvatarCreatedEvent(event);
			if(this._controller.userId() == e.userId){
				this._avatarRef = e.avatarRef;
				this.stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
				this.stage.addEventListener(KeyboardEvent.KEY_UP, onKeyUp);
			}
		}
		
		
    }
}


