package com.croqodile.toybox {

    import flash.display.MovieClip;
    import flash.display.Stage;
    import flash.display.Sprite;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.toybox.*;
    import flash.events.*;
    import org.cove.ape.*;
    
    public class ToyboxIsland extends IslandReplica {
		public static const ARENA_X:Number = 20;
		public static const ARENA_Y:Number = 20;
		public static const ARENA_WIDTH:Number = 800;
		public static const ARENA_HEIGHT:Number = 500;
		public static const ARENA_WALL_THICKNESS:Number = 10;
		
		private var _canvas:Sprite;
		private var _avatars:Array = [];
		private var _things:Array = [];
		private var _blocks:Array = [];
		private var _walls:Array;
		
		public function ToyboxIsland(config:Object){
			super();
			
			this._canvas = config.canvas;
			
			APEngine.init(1/3);
			APEngine.defaultContainer = this._canvas;
			
			// Create the walls. These will never change.
			this._walls = [new RectangleParticle(ARENA_WIDTH/2,0,ARENA_WIDTH,ARENA_WALL_THICKNESS,0,true),
				new RectangleParticle(ARENA_WIDTH/2,ARENA_HEIGHT,ARENA_WIDTH,ARENA_WALL_THICKNESS,0,true),
				new RectangleParticle(0,ARENA_HEIGHT/2,ARENA_WALL_THICKNESS,ARENA_HEIGHT,0,true),
				new RectangleParticle(ARENA_WIDTH,ARENA_HEIGHT/2,ARENA_WALL_THICKNESS,ARENA_HEIGHT,0,true)
			];
			
			for each(var ea:RectangleParticle in this._walls){
				APEngine.addParticle(ea);
			}
		}
		

		
		override public function freeze():Object{
			var data:Object = new Object();
			
			data.blocks = [];
			for each(var block:Block in this._blocks){
				data.blocks.push(block.freeze());
			}
			
			data.things = [];
			for each(var thing:Thing in this._things){
				data.things.push(thing.freeze());
			}
			
			data.avatars = [];
			for each(var avatar:Avatar in this._avatars){
				data.avatars.push(avatar.freeze());
			}
			
			return data;
		}



		
		override public function unfreeze(data:Object):void {
			
			for each(var blockData:Object in data.blocks){
				var block:Block = new Block(this);
				block.unfreeze(blockData);
				this._blocks.push(block);
			}
			
			for each(var thingData:Object in data.things){
				var thing:Thing = new Thing(this);
				thing.unfreeze(thingData);
				this._things.push(thing);
			}
			
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
			for each(var th:Thing in this._things){
				th.render();
			}
			
			for each(var b:Block in this._blocks){
				b.render();
			}
			
			for each(var w:RectangleParticle in this._walls){
				w.paint();
			}
		}
		

		
		////////////////////////
        // External Interface //
        ////////////////////////


		
		/* Executed only once, at the beginning of Island-Time.*/
		override public function sunrise():void {
			
			var block:Block = null;
			for(var i:int = 0; i < 3; i ++){
				block = new Block(this,
					this.rand().numInRange(20, ARENA_WIDTH),
					this.rand().numInRange(20, ARENA_HEIGHT),
					this.rand().numInRange(10, 100),
					this.rand().numInRange(10, 100),
					this.rand().numInRange(0.0, 4.0)
				);
				this._blocks.push(block);
			}
			
			var thing:Thing = null;
			for(i = 0; i < 20; i ++){
				thing = new Thing(this);
				this._things.push(thing);
			}
			this.futureSend(50, "stepPhysics", []);
		}
		
		public function createAvatar(userId:String):void {
			var avatar:Avatar = new Avatar(this, userId);
			this._avatars.push(avatar);
			this.signalEvent(new AvatarCreatedEvent(avatar.farRef(), userId));
		}
		
		public function stepPhysics():void{
			APEngine.step();
			this.futureSend(20, "stepPhysics", []);
		}
		
		
    }
}


