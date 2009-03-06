var url = window.location.href;
var flashVars = url.toQueryParams();
var params = { allowfullscreen: "true", allownetworkaccess: "true", bgcolor: "#ffffff" };
swfobject.embedSWF("../bin/tankwars.swf", "app", "1280", "720", "10", null, flashVars, params);