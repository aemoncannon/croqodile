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
		
		public function PhysObj(island:IslandReplica, particle:AbstractParticle){
			super(island);
			_particle = particle;
		}

		public static function typeById(str:String):Class{
			return ({
					"Avatar" : Avatar,
					"Block" : Block,
					"Thing" : Thing
				})[str];
		}

		public function get typeId():String{ throw new Error("Subclass responsibility"); return null;}

		public function get particle():AbstractParticle{ return _particle; }
		
		public function render():void{}

		public function step():void{}
		
		override public function writeTo(b:IDataOutput):void{
			super.writeTo(b);
			b.writeBoolean(_particle.collidable);
			b.writeBoolean(_particle.fixed);
			b.writeDouble(_particle.friction);
			trace("write friction: " + _particle.friction)
			b.writeDouble(_particle.elasticity);
			trace("write elasticity: " + _particle.elasticity)
			b.writeDouble(_particle.mass);
			trace("write mass: " + _particle.mass)
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
			trace("read friction: " + _particle.friction)
			_particle.elasticity = b.readDouble();
			trace("read elasticity: " + _particle.elasticity)
			_particle.mass = b.readDouble();
			trace("read mass: " + _particle.mass)
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



    }
}

