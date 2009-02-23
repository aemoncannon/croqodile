package com.croqodile.whiteboard {
    import flash.display.*;
    import flash.geom.Rectangle;
    import flash.utils.ByteArray;
    import flash.system.Security;
    import flash.utils.Timer;
    import com.croqodile.*;
    import com.croqodile.whiteboard.*;
    import com.croqodile.serialization.base64.Base64;
    import flash.events.*;
    
    public class WhiteboardIsland extends IslandReplica {
		
		private var _canvas:Sprite;
		
		public function WhiteboardIsland(config:Object){
			super();
			_canvas = config.canvas;
			clearWithWhite();
		}
		
		override public function freeze():Object{
			var data:Object = new Object();
			var bitmap:BitmapData = new BitmapData(_canvas.width, _canvas.height);
			bitmap.draw(_canvas);
			var bitmapBytes:ByteArray = bitmap.getPixels(new Rectangle(0, 0, bitmap.width, bitmap.height));
			bitmapBytes.compress();
			data.imageData = Base64.encodeByteArray(bitmapBytes);
			bitmap.dispose();
			return data;
		}
		
		override public function unfreeze(data:Object):void {
			var bitmapBytes:ByteArray = Base64.decodeToByteArray(data.imageData);
			bitmapBytes.uncompress();
			var bitmap:BitmapData = new BitmapData(_canvas.width, _canvas.height);
			bitmapBytes.position = 0;
			bitmap.setPixels(new Rectangle(0, 0, bitmap.width, bitmap.height), bitmapBytes);
			_canvas.graphics.clear();
			_canvas.graphics.beginBitmapFill(bitmap);
			_canvas.graphics.drawRect(0, 0, _canvas.width,_canvas.height);
			_canvas.graphics.endFill();
		}
		
		private function drawSegment(seg:Object):void{
			_canvas.graphics.lineStyle(seg.thickness, int(seg.color));
			_canvas.graphics.moveTo(seg.startX, seg.startY);
			_canvas.graphics.lineTo(seg.endX, seg.endY);
		}

		private function clearWithWhite():void{
			_canvas.graphics.beginFill(0xFFFFFF);
			_canvas.graphics.drawRect(0, 
				0, 
				_canvas.width,
				_canvas.height);
			_canvas.graphics.endFill();
		}
		
		////////////////////////
        // External Interface //
        ////////////////////////
		
		public function addSegment(segment:Object):void{
			drawSegment(segment);
		}
		
    }
}


