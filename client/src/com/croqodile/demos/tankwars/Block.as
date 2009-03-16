package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
    import flash.display.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    
    public class Block extends PhysObj {
		
		public function Block(island:IslandReplica){
			super(island);
			_view = new Sprite();
			var canvas:Sprite = TankWarsIsland(island).canvas;
			canvas.addChild(_view);
		}

		protected function paint():void{
			var w:Number = BlockParticle(_particle).width;
			var h:Number = BlockParticle(_particle).height;
			var g:Graphics = _view.graphics;
			g.clear();
			g.lineStyle(1.0, 0, 1.0);
			g.beginFill(0x555555, 0x222222);
			g.drawRect(-w/2, -h/2, w, h);
			g.endFill();
		}
		
		override public function render():void{
			var r:Number = (BlockParticle(_particle).rotation * 180.0) / Math.PI;
			if(_particle.px != _view.x || _particle.py != _view.y || r != _view.rotation){
				_view.x = _particle.px;
				_view.y = _particle.py;
				_view.rotation = r;
			}
		}

		override public function writeTo(b:IDataOutput):void{
			super.writeTo(b);
			var p:BlockParticle = BlockParticle(_particle);
			b.writeDouble(p.width);
			b.writeDouble(p.height);
			b.writeDouble(p.rotation);
		}
		
		override public function readFrom(b:IDataInput):void{
			_particle = new BlockParticle();
			super.readFrom(b)
			var p:BlockParticle = BlockParticle(_particle);
			p.width = b.readDouble();
			p.height = b.readDouble();
			p.rotation = b.readDouble();
			init();
		}

		public static function readFrom(b:IDataInput, island:IslandReplica):Block{
			var block:Block = new Block(island);
			block.readFrom(b);
			return block;
		}

		public static function createRandom(island:IslandReplica, x0:Number, y0:Number, w:Number, h:Number):Block{
			var b:ByteArray = new ByteArray();
			
			return readFrom(b, island);
		}

	}
}

