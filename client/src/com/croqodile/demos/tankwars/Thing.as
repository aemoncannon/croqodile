package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import flash.display.*;
    import org.cove.ape.*;
    
    public class Thing extends PhysObj {
		
		public function Thing(island:IslandReplica){
			super(island);

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

		override public function readFrom(b:IDataInput):void{
			_particle = new CircleParticle();
			super.readFrom(b)
			init();
		}

		public static function readFrom(b:IDataInput, island:IslandReplica):Thing{
			var thing:Thing = new Thing(island);
			thing.readFrom(b);
			return thing;
		}

		public static function createRandom(island:IslandReplica, x0:Number, y0:Number, w:Number, h:Number):Thing{
			var b:ByteArray = new ByteArray();
			writeBytesForCreateRandom(b, island, x0, y0, w, h);
			return readFrom(b, island);
		}
		protected static function writeBytesForCreateRandom(b:ByteArray, island:IslandReplica, x0:Number, y0:Number, w:Number, h:Number):void{
			PhysObj.writeBytesForCreateRandom(b, island, x0, y0, w, h);
		}

		
    }
}

