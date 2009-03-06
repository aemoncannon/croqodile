var url = window.location.href;
var urlParams = url.toQueryParams();
var flashVars = { islandId: urlParams.islandId };
alert(urlParams.islandId);
var params = { allowfullscreen: "true", allownetworkaccess: "true", bgcolor: "#ffffff" };
swfobject.embedSWF("../bin/toybox.swf", "app", "1280", "720", "10", null, flashVars, params);