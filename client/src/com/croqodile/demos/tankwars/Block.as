package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
    import flash.display.*;
    import com.croqodile.*;
    import com.croqodile.demos.tankwars.*;
    import org.cove.ape.*;
    
    public class Block extends PhysObj {
		

		public function Block(island:IslandReplica, 
			x:Number = 300,
			y:Number = 200,
			width:Number = 50, 
			height:Number = 50, 
			rotation:Number = 0,
			mass:Number = 5.0,
			friction:Number = 0.8,
			fixed:Boolean = false
		)
		{
			var part:BlockParticle = new BlockParticle(
				x, //x
				y, //y
				width, //width
				height, //height
				rotation, //rotation
				fixed, //fixed?
				mass, //mass
				0.3, //elasticity
				friction //friction
			); 
			super(island, part);
			_view = new Sprite();
			var canvas:Sprite = TankWarsIsland(island).canvas;
			canvas.addChild(_view);
			paint();
			render();
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
			super.readFrom(b)
			var p:BlockParticle = BlockParticle(_particle);
			p.width = b.readDouble();
			p.height = b.readDouble();
			p.rotation = b.readDouble();
			paint();
			render();
		}

		public static function readFrom(b:IDataInput, island:IslandReplica):Block{
			var block:Block = new Block(island);
			block.readFrom(b);
			return block;
		}
		
    }
}

