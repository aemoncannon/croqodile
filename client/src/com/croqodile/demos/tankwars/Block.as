package com.croqodile.demos.tankwars {
    import flash.events.*;
    import flash.utils.*;
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
			friction:Number = 0.8)
		{
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
			super(island, part);
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
		}

		public static function readFrom(b:IDataInput, island:IslandReplica):Block{
			var block:Block = new Block(island);
			block.readFrom(b);
			return block;
		}
		
    }
}

