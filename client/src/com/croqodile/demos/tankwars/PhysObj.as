package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
    import flash.display.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    
    public class PhysObj extends IslandObject {
		
		protected var _particle:AbstractParticle;
		protected var _view:Sprite;
		
		public function PhysObj(island:IslandReplica){
			super(island);
		}

		public function addForce(x:Number, y:Number):void{
			_particle.addForce(new Vector2D(x,y));
		}
		
		public function render():void{}
		
		override public function writeTo(b:IDataOutput):void{
			super.writeTo(b);
			b.writeBoolean(_particle.collidable);
			b.writeBoolean(_particle.fixed);
			b.writeDouble(_particle.friction);
			b.writeDouble(_particle.mass);
			b.writeDouble(_particle.position.x);
			b.writeDouble(_particle.position.y);
			b.writeDouble(_particle.px);
			b.writeDouble(_particle.py);
			b.writeDouble(_particle.velocity.x);
			b.writeDouble(_particle.velocity.y);
			b.writeBoolean(_particle.visible);
		}
		
		override public function readFrom(b:IDataInput):void{
			super.readFrom(b)
			_particle.collidable = b.readBoolean();
			_particle.fixed = b.readBoolean();
			_particle.friction = b.readDouble();
			_particle.mass = b.readDouble();
			var x:Number = b.readDouble();
			var y:Number = b.readDouble();
			_particle.position = new Vector2D(x, y);
			_particle.px = b.readDouble();
			_particle.py = b.readDouble();
			x = b.readDouble();
			y = b.readDouble();
			_particle.velocity = new Vector2D(x, y);
			_particle.visible = b.readBoolean();
		}

		protected function init():void{
			APEngine.addParticle(_particle);
			render();
		}


		protected static function writeBytesForCreateRandom(b:ByteArray, island:IslandReplica, x0:Number, y0:Number, w:Number, h:Number):void{
			IslandObject.writeBytesForCreateRandom(b, island, x0, y0, w, h);
			b.writeBoolean(true);
			b.writeBoolean(false);
			b.writeDouble(0.5);
			b.writeDouble(5);
			var x:Number = island.rand.numInRange(x0, w);
			var y:Number = island.rand.numInRange(y0, h);
			b.writeDouble(x);
			b.writeDouble(y);
			b.writeDouble(x);
			b.writeDouble(y);
			b.writeDouble(0);
			b.writeDouble(0);
			b.writeBoolean(true);
		}
		
    }
}

