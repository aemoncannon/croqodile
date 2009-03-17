package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import flash.display.*;
    import org.cove.ape.*;
    
    public class Thing extends PhysObj {
		
		public function Thing(island:IslandReplica){
			var part:CircleParticle = new CircleParticle(
				300, //x
				200, //y
				10, //radius
				false, //fixed?
				0.2, //mass
				0.3, //elasticity
				0.1  //friction
			);
			super(island, part);

			_view = new Sprite();
			var g:Graphics = _view.graphics;
			g.lineStyle(1.0, 0, 1.0);
			g.beginFill(0x555555, 0x333333);
			g.drawCircle(0, 0, part.radius);
			g.endFill();
			var canvas:Sprite = TankWarsIsland(island).canvas;
			canvas.addChild(_view);
		}

		override public function render():void{
			if(_particle.px != _view.x || _particle.py != _view.y){
				_view.x = _particle.px;
				_view.y = _particle.py;
			}
		}


		public static function readFrom(b:IDataInput, island:IslandReplica):Thing{
			var thing:Thing = new Thing(island);
			thing.readFrom(b);
			return thing;
		}

		
    }
}

