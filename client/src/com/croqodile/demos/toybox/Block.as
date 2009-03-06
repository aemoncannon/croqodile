package com.croqodile.toybox {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.croqodile.*;
    import com.croqodile.toybox.*;
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
		
		public function freeze():Object{
			var data:Object = {
				collidable: _particle.collidable,
				fixed: _particle.fixed,
				friction: _particle.friction,
				mass: _particle.mass,
				positionX: _particle.position.x,
				positionY: _particle.position.y,
				px: _particle.px,
				py: _particle.py,
				width: _particle.width,
				height: _particle.height,
				rotation: _particle.rotation,
				velX: _particle.velocity.x,
				velY: _particle.velocity.y,
				visible: _particle.visible
			}
			return data;
		}
		
		public function unfreeze(data:Object):void{
			_particle.collidable = data.collidable;
			_particle.fixed = data.fixed;
			_particle.friction = data.friction;
			_particle.mass = data.mass;
			_particle.position = new Vector(data.positionX, data.positionY);
			_particle.px = data.px;
			_particle.py = data.py;
			_particle.width = data.width;
			_particle.height = data.height;
			_particle.rotation = data.rotation;
			_particle.velocity = new Vector(data.velX, data.velY);
			_particle.visible = data.visible;
		}
		
    }
}

