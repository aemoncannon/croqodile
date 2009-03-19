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
		private var _physObjects:Array = [];
		
		public function TankWarsIsland(config:Object){
			super(config);
			_canvas = config.canvas;
			APEngine.init(1.0/3.0);

			// Create the walls. These will never change.
			_physObjects.push(new Block(this, ARENA_WIDTH/2, ARENA_WALL_THICKNESS/2, ARENA_WIDTH, ARENA_WALL_THICKNESS, 0, 5.0, 0, true));
			_physObjects.push(new Block(this, ARENA_WIDTH/2, ARENA_HEIGHT - ARENA_WALL_THICKNESS/2, ARENA_WIDTH, ARENA_WALL_THICKNESS, 0, 5.0, 0, true));
			_physObjects.push(new Block(this, ARENA_WALL_THICKNESS/2, ARENA_HEIGHT/2, ARENA_WALL_THICKNESS, ARENA_HEIGHT, 0, 5.0, 0, true));
			_physObjects.push(new Block(this, ARENA_WIDTH - ARENA_WALL_THICKNESS/2, ARENA_HEIGHT/2, ARENA_WALL_THICKNESS, ARENA_HEIGHT, 0, 5.0, 0, true));
		}


		override public function writeTo(b:IDataOutput):void{
			super.writeTo(b);
			var i:int;
			var len:int = _physObjects.length;
			b.writeUnsignedInt(len);
			for(i = 0; i < len; i++){
				PhysObj(_physObjects[i]).writeTo(b);
			}
		}

		override public function readFrom(b:IDataInput):void {
			super.readFrom(b);
			var i:int;
			var len:int = b.readUnsignedInt();
			for(i = 0; i < len; i++){
//				addPhysObj(PhysObj.readFrom(b, this)); // <-- not gonna work
			}
		}
		
		public function get canvas():Sprite{
			return _canvas;
		}
		
		public function render():void {
			for each(var o:PhysObj in _physObjects){
				o.render();
			}
		}

		public function addPhysObj(o:PhysObj):void {
			o.intern();
			_physObjects.push(o);
			APEngine.addParticle(o.particle);
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
				addPhysObj(block)
			}
			
			var thing:Thing = null;
			for(i = 0; i < 20; i ++){
				thing = new Thing(this);
				addPhysObj(thing)
			}
			futureSend(50, "step", []);
		}
		
		public function createAvatar(userId:String):void {
			var avatar:Avatar = new Avatar(this, userId);
			addPhysObj(avatar);
			signalEvent(new AvatarCreatedEvent(avatar.farRef(), userId));
		}
		
		public function step():void{
			APEngine.step();
			for each(var o:PhysObj in _physObjects){
				o.step();
			}
			futureSend(20, "step", []);
		}
		
		
    }
}


