package com.croqodile.demos.tankwars {

    import flash.display.*;
    import flash.system.Security;
    import flash.utils.*;
    import com.croqodile.*;
    import flash.events.*;
    import org.cove.ape.*;
    
    public class TankWarsIsland extends IslandReplica {
		public static const ARENA_X:Number = 20;
		public static const ARENA_Y:Number = 20;
		public static const ARENA_WIDTH:Number = 1000;
		public static const ARENA_HEIGHT:Number = 600;
		public static const ARENA_WALL_THICKNESS:Number = 5;
		
		private var _canvas:Sprite;
		private var _avatars:Array = [];
		private var _things:Array = [];
		private var _blocks:Array = [];
		private var _walls:Array = [];
		
		public function TankWarsIsland(config:Object){
			super(config);
			
			_canvas = config.canvas;
			
			APEngine.init(1.0/3.0);
			
			// Create the walls. These will never change.
			_walls = [
				new Block(this, ARENA_WIDTH/2, ARENA_WALL_THICKNESS/2, ARENA_WIDTH, ARENA_WALL_THICKNESS, 0, 5.0, 0, true),
				new Block(this, ARENA_WIDTH/2, ARENA_HEIGHT - ARENA_WALL_THICKNESS/2, ARENA_WIDTH, ARENA_WALL_THICKNESS, 0, 5.0, 0, true),
				new Block(this, ARENA_WALL_THICKNESS/2, ARENA_HEIGHT/2, ARENA_WALL_THICKNESS, ARENA_HEIGHT, 0, 5.0, 0, true),
				new Block(this, ARENA_WIDTH - ARENA_WALL_THICKNESS/2, ARENA_HEIGHT/2, ARENA_WALL_THICKNESS, ARENA_HEIGHT, 0, 5.0, 0, true)
			];
		}


		override public function writeTo(b:IDataOutput):void{
			super.writeTo(b);
			b.writeUnsignedInt(_blocks.length);
			for each(var block:Block in _blocks){
				block.writeTo(b);
			}
			b.writeUnsignedInt(_things.length);
			for each(var thing:Thing in _things){
				thing.writeTo(b);
			}
			b.writeUnsignedInt(_avatars.length);
			for each(var avatar:Avatar in _avatars){
				avatar.writeTo(b);
			}
		}


		override public function readFrom(b:IDataInput):void {
			super.readFrom(b);
			var i:int;
			var len:int = b.readUnsignedInt();
			for(i = 0; i < len; i++){
				_blocks.push(Block.readFrom(b, this));
			}

			len = b.readUnsignedInt();
			for(i = 0; i < len; i++){
				_things.push(Thing.readFrom(b, this));
			}

			len = b.readUnsignedInt();
			for(i = 0; i < len; i++){
				_avatars.push(Avatar.readFrom(b, this));
			}
		}
		
		public function get canvas():Sprite{
			return _canvas;
		}
		
		public function render():void {
			for each(var av:Avatar in _avatars){
				av.render();
			}
			for each(var th:Thing in _things){
				th.render();
			}
			
			for each(var b:Block in _blocks){
				b.render();
			}
			
			for each(var w:Block in _walls){
				w.render();
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
					rand.numInRange(20, ARENA_WIDTH),
					rand.numInRange(20, ARENA_HEIGHT),
					rand.numInRange(10, 100),
					rand.numInRange(10, 100),
					rand.numInRange(0.0, 4.0)
				);
				_blocks.push(block);
			}
			
			var thing:Thing = null;
			for(i = 0; i < 20; i ++){
				thing = new Thing(this);
				_things.push(thing);
			}
			futureSend(50, "stepPhysics", []);
		}
		
		public function createAvatar(userId:String):void {
			var avatar:Avatar = new Avatar(this, userId);
			_avatars.push(avatar);
			signalEvent(new AvatarCreatedEvent(avatar.farRef(), userId));
		}
		
		public function stepPhysics():void{
			APEngine.step();
			futureSend(20, "stepPhysics", []);
		}
		
		
    }
}


