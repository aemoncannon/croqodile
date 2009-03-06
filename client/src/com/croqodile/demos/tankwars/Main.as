package com.croqodile.demos.tankwars {

    import flash.display.MovieClip;
    import flash.display.Stage;
    import flash.display.Sprite;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import flash.events.*;
    import org.cove.ape.*;
    
    public class Main extends Sprite {

		private var _controller:Controller;

		/* Use this carefully; unless necessary,
		prefer a FarRef for accessing the island. */
		private var _island:TankWarsIsland;

		private var _islandRef:FarRef;
		private var _avatarRef:FarRef;
		private var _APEContainer:Sprite;

		private static const CHAT_KEY_CODE:uint = 84; // 't'

		public function Main():void{
			var canvasComp:UIComponent = new UIComponent();
			canvasComp.x = 0;
			canvasComp.y = 0;
			canvasComp.width = stage.stageWidth;
			canvasComp.height = stage.stageHeight;
			_APEContainer = new Sprite();
			addChild(canvasComp);
			canvasComp.addChild(_APEContainer);

			var flashVars:Object = LoaderInfo(root.loaderInfo).parameters;

			var config:Object = DIRunner.run([
					{   name: "island", klass: TankWarsIsland, 
						args: {
							id: flashVars.islandId, 
							canvas: _APEContainer
						},
						injectArgs: {} 
					},
					{   name: "controller", klass: Controller, 
						args: { 
							userId: flashVars.userId, 
							host: flashVars.host, 
							port: parseInt(flashVars.port)
						}, 
						injectArgs: {island: "island"}
					} 
				]);

			_controller = config.controller;
			_controller.addEventListener(RouterConnectionReadyEvent.type, routerConnectionReady);
			_controller.connect();
		}

		public function routerConnectionReady(event:Event):void{
			_island = TankWarsIsland(_controller.island);
			_islandRef = _controller.island().farRef();
			_controller.addEventListener(DisconnectedFromRouterEvent.type, disconnectedFromRouter);
			
			_controller.addEventListener(AvatarCreatedEvent.type, onAvatarCreated);
			_islandRef.send("createAvatar", [ _controller.userId ]);
			
			addEventListener(Event.ENTER_FRAME, onEnterFrame);
		}


		public function disconnectedFromRouter(event:Event):void{
			throw new Error("Lost connection to the router :(");
		}


		public function onAvatarCreated(event:Event):void{
			var e:AvatarCreatedEvent = AvatarCreatedEvent(event);
			if(_controller.userId == e.userId){
				_avatarRef = e.avatarRef;
				stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
			}
		}


		private function onKeyDown(evt:KeyboardEvent):void{
			evt.stopPropagation();
			evt.preventDefault();

			switch(evt.keyCode){
				
				case Keyboard.LEFT:
				_avatarRef.send("addForce", [-5,0]);
				break;
				
				case Keyboard.RIGHT:
				_avatarRef.send("addForce", [5,0]);
				break;

				case Keyboard.UP:
				_avatarRef.send("addForce", [0,-5]);
				break;
				
				case Keyboard.DOWN:
				_avatarRef.send("addForce", [0,5]);
				break;

				case Keyboard.DOWN:
				_avatarRef.send("addForce", [0,5]);
				break;

				// 				case CHAT_KEY_CODE:
				// 				stage.removeEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
				// 				stage.addEventListener(KeyboardEvent.KEY_DOWN, onChatKeyDown);
				// 				break;
			}
		}


		// 		private function onChatKeyDown(evt:KeyboardEvent):void{
		// 			evt.stopPropagation();

		// 			chatInput.setFocus();
		
		// 			switch(evt.keyCode){
		
		// 				case Keyboard.ENTER:
		// 				_avatarRef.send("sayWords", [chatInput.text]);
		// 				chatInput.visible = false;
		// 				setFocus(); //Get rid of focus
		// 				stage.removeEventListener(KeyboardEvent.KEY_DOWN, onChatKeyDown);
		// 				stage.addEventListener(KeyboardEvent.KEY_DOWN, onKeyDown);
		// 				break;
		// 			}
		// 		}


		// 		private function clearChatInput():void{
		// 			var range:TextRange = new TextRange(chatInput, false, 0);
		// 			range.text = "";
		// 			range.htmlText = "";
		// 		}


		private function onEnterFrame(evt:Event):void{
			_island.render();
		}

		
    }
}

