package com.croqodile.util{
	import flash.utils.*;

	public class TracingDataInput implements IDataInput{

		private var _in:IDataInput;

		public function TracingDataInput(input:IDataInput):void{
			_in = input;
		}

		public function get bytesAvailable():uint{ return _in.bytesAvailable; }

		public function get endian():String{ return _in.endian; }
		public function set endian(val:String):void{ _in.endian = val; }

		public function get objectEncoding():uint{ return _in.objectEncoding; }
		public function set objectEncoding(val:uint):void{ _in.objectEncoding = val; }


		
		public function readBoolean():Boolean{
			var val:Boolean = _in.readBoolean();
			trace("readBoolean " + val); 
			return val;
		}

 	 	
		public function readByte():int{
			var val:int = _in.readByte();
			trace("readByte " + val); 
			return val;
		}

 	 	
		public function readBytes(bytes:ByteArray, offset:uint = 0, length:uint = 0):void{ 
			trace("readBytes"); 
			_in.readBytes(bytes, offset, length);
		}

 	 	
		public function readDouble():Number{ 
			var val:Number = _in.readDouble();
			trace("readDouble " + val); 
			return val;
		}

 	 	
		public function readFloat():Number{
			var val:Number = _in.readFloat();
			trace("readFloat " + val); 
			return val;
		}

 	 	
		public function readInt():int{
			var val:int = _in.readInt();
			trace("readInt " + val); 
			return val;
		}

 	 	
		public function readMultiByte(length:uint, charSet:String):String{
			var val:String = _in.readMultiByte(length, charSet);
			trace("readMultiByte " + val); 
			return val;
		}

 	 	
		public function readObject():*{ 
			var val:* = _in.readObject();
			trace("readObject" + val);
			return val;
		}

 	 	
		public function readShort():int{ 
			var val:int = _in.readShort(); 
			trace("readShort " + val); 
			return val;
		}

 	 	
		public function readUnsignedByte():uint{
			var val:uint = _in.readUnsignedByte();
			trace("readUnsignedByte " + val); 
			return val;
		}

 	 	
		public function readUnsignedInt():uint{
			var val:uint = _in.readUnsignedInt();
			trace("readUnsignedInt " + val); 
			return val;
		}

 	 	
		public function readUnsignedShort():uint{
			var val:uint = _in.readUnsignedShort();
			trace("readUnsignedShort " + val); 
			return val;
		}

 	 	
		public function readUTF():String{
			var val:String = _in.readUTF();
			trace("readUTF " + val); 
			return val;
		}

 	 	
		public function readUTFBytes(length:uint):String{
			var val:String = _in.readUTFBytes(length);
			trace("readUTFBytes " + val); 
			return val;
		}

	}

}