package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    
    public class PhysObj extends IslandObject {
		
		protected var _particle:AbstractParticle;
		
		public function PhysObj(island:IslandReplica, particle:AbstractParticle){
			super(island);
			_particle = particle;
			APEngine.addParticle(particle);
		}
		
		public function addForce(x:Number, y:Number):void{
			_particle.addForce(new Vector2D(x,y));
		}
		
		public function render():void{
			_particle.paint();
		}
		
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
			_particle.position = new Vector2D(b.readDouble(), b.readDouble());
			_particle.px = b.readDouble();
			_particle.py = b.readDouble();
			_particle.velocity = new Vector2D(b.readDouble(), b.readDouble());
			_particle.visible = b.readBoolean();
		}
		
    }
}

