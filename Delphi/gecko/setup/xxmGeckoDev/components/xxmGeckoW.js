"use strict";
onmessage=function(e){
	try{
		var xxmLib;
		xxmLib=ctypes.open(e.data.lib);
		var xxm=xxmLib.declare("xxmOpen",ctypes.winapi_abi,ctypes.voidptr_t,ctypes.char.ptr,ctypes.char.ptr)(e.data.verb,e.data.uri);
		
		//TODO: upload post data
		
		postMessage({x:"open",prefix:xxmLib.declare("xxmPrefix",ctypes.winapi_abi,ctypes.char.ptr,ctypes.voidptr_t)(xxm).readString()});
		
		var xxmNext=xxmLib.declare("xxmNext",ctypes.winapi_abi,ctypes.char.ptr,ctypes.voidptr_t);
		var xxmSet=function(x,y){
			var xxmSet1=xxmLib.declare("xxmSet",ctypes.winapi_abi,ctypes.void_t,ctypes.voidprt_t,ctypes.char.ptr);
			xxmSet1(x,y);
		};
		var done=false;
		var msg;
		var headers={};
		while(!done){
			msg=xxmNext(xxm).readString();
			(({
				hdr:function(x){var i=0;while(i<x.length&&x[i]!=" ")i++;headers[x.substr(0,i).toLowerCase()]=x.substr(i+1);},
				xxx:function(x){postMessage({x:"start",headers:headers
					//TODO:
					,status:200,statusText:"OK"
				});},
				ref:function(x){xxmSet(xxm,e.data.referer);},
				dta:function(x){postMessage({x:"data",n:parseInt(x)});},
				rdr:function(x){postMessage({x:"redir",rel:false,url:x});},
				rdl:function(x){postMessage({x:"redir",rel:true,url:x});},
				usa:function(x){xxmSet(xxm,e.data.userAgent);},
				end:function(x){done=true;}
			})[msg.substr(0,3)]||function(x){postMessage({x:msg.substr(0,3)});})(msg.substr(3));
		}
		
		xxmLib.declare("xxmClose",ctypes.winapi_abi,ctypes.void_t,ctypes.voidptr_t)(xxm);
		postMessage({x:"final"});
	}
	catch(ex){
		postMessage({x:"error",ex:ex});
	}
};