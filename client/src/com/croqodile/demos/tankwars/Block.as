package com.croqodile.demos.tankwars {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    
    public class Block extends IslandObject {
		
		private var _particle:BlockParticle;
		
		public function Block(island:IslandReplica, 
			x:Number = 300,
			y:Number = 200,
			width:Number = 50, 
			height:Number = 50, 
			rotation:Number = 0,
			mass:Number = 5.0,
			friction:Number = 0.8){
			super(island);
			var part:BlockParticle = new BlockParticle(
				x, //x
				y, //y
				width, //width
				height, //height
				rotation, //rotation
				false, //fixed?
				mass, //mass
				0.3, //elasticity
				friction); //friction
			APEngine.addParticle(part);
			_particle = part;
		}
		
		public function addForce(x:Number, y:Number):void{
			_particle.addForce(new Vector(x,y));
		}
		
		public function render():void{
			BlockParticle(_particle).paint();
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
			b.writeDouble(_particle.width);
			b.writeDouble(_particle.height);
			b.writeDouble(_particle.rotation);
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
			_particle.position = new Vector(b.readDouble(), b.readDouble());
			_particle.px = b.readDouble();
			_particle.py = b.readDouble();
			_particle.width = b.readDouble();
			_particle.height = b.readDouble();
			_particle.rotation = b.readDouble();
			_particle.velocity = new Vector(b.readDouble(), b.readDouble());
			_particle.visible = b.readBoolean();
		}

		public static function readFrom(b:IDataInput, island:IslandReplica):Block{
			var block:Block = new Block(island);
			block.readFrom(b);
			return block;
		}
		
    }
}

