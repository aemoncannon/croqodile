package com.croqodile.util{
	import flash.utils.*;

	public class TracingDataOutput implements IDataOutput{

		private var _out:IDataOutput;

		public function TracingDataOutput(out:IDataOutput):void{
			_out = out;
		}

		public function get endian():String{ return _out.endian; }
		public function set endian(val:String):void{ _out.endian = val; }

		public function get objectEncoding():uint{ return _out.objectEncoding; }
		public function set objectEncoding(val:uint):void{ _out.objectEncoding = val; }

		
		public function writeBoolean(value:Boolean):void{ 
			trace("writeBoolean(" + value + ")"); 
			_out.writeBoolean(value); 
		}
 	 	
		public function writeByte(value:int):void{ 
			trace("writeByte(" + value + ")"); 
			_out.writeByte(value); 
		}
 	 	
		public function writeBytes(bytes:ByteArray, offset:uint = 0, length:uint = 0):void{ 
			trace("writeBytes(" + bytes + "," + offset + "," + length + ")"); 
			_out.writeBytes(bytes, offset, length); 
		}
 	 	
		public function writeDouble(value:Number):void{ 
			trace("writeDouble(" + value + ")"); 
			_out.writeDouble(value); 
		}
 	 	
		public function writeFloat(value:Number):void{ 
			trace("writeFloat(" + value + ")"); 
			_out.writeFloat(value); 
		}
 	 	
		public function writeInt(value:int):void{ 
			trace("writeInt(" + value + ")"); 
			_out.writeInt(value); 
		}
 	 	
		public function writeMultiByte(value:String, charSet:String):void{ 
			trace("writeMultiByte(" + value + "," + charSet + ")"); 
			_out.writeMultiByte(value, charSet); 
		}
 	 	
		public function writeObject(object:*):void{ 
			trace("writeObject(" + object + ")"); 
			_out.writeObject(object); 
		}
 	 	
		public function writeShort(value:int):void{ 
			trace("writeShort(" + value + ")"); 
			_out.writeShort(value); 
		}
 	 	
		public function writeUnsignedInt(value:uint):void{ 
			trace("writeUnsignedInt(" + value + ")"); 
			_out.writeUnsignedInt(value); 
		}
 	 	
		public function writeUTF(value:String):void{ 
			trace("writeUTF(" + value + ")"); 
			_out.writeUTF(value); 
		}
 	 	
		public function writeUTFBytes(value:String):void{ 
			trace("writeUTFBytes(" + value + ")"); 
			_out.writeUTFBytes(value); 
		}


	}

}