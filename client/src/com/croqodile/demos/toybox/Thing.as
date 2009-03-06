package com.croqodile.toybox {
    import flash.display.MovieClip;
    import flash.events.Event;
    import flash.events.MouseEvent;
    import com.croqodile.*;
    import com.croqodile.toybox.*;
    import org.cove.ape.*;
    
    public class Thing extends IslandObject {
		
		private var _particle:AbstractParticle;
		
		public function Thing(island:IslandReplica){
			super(island);
			var part:CircleParticle = new CircleParticle(
				300, //x
				200, //y
				10, //radius
				false, //fixed?
				0.2, //mass
				0.3, //elasticity
				0.1); //friction
			APEngine.addParticle(part);
			_particle = part;
		}
		
		public function addForce(x:Number, y:Number):void{
			_particle.addForce(new Vector(x,y));
		}
		
		public function render():void{
			CircleParticle(_particle).paint();
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
			_particle.velocity = new Vector(data.velX, data.velY);
			_particle.visible = data.visible;
		}
		
    }
}

