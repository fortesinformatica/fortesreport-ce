//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import "DSCallbackChannelManager.h"
#import "SBJSON.h"
#import "DBException.h"
#import "TJSONValue.h"

#import "TJSONTrue.h"
#import "TJSONArray.h"
#import "DBXConnection.h"


@implementation DSCallbackChannelManager

@synthesize ChannelName;
@synthesize ManagerID;
@synthesize SecurityToken;
@synthesize maxRetries;
@synthesize retryDelay;
-(id) init{
	self = [super init];
	return self;
  
}
-(id)initWithConnection:(DSRESTConnection *) aconnection  withChannel:  (NSString *) channelName 
		  withManagerID:(NSString*) managerID withDelegate: (id) adelegate {
	if (!(self = [self init])) return nil;
	if (self) {
		maxRetries = 5;
		retryDelay = 1;
		delegate = adelegate;
		lock = [[NSLock alloc]init];
		self.ChannelName = channelName;
		self.ManagerID = managerID;	
		connection =  aconnection;
		dsadmin = [[DSAdmin alloc]initWithConnection:connection];
		
		self.SecurityToken =[NSString stringWithFormat:@"%i%i",(arc4random()%100000)+1,(arc4random()%100000)+1]; 
	}
	return self;
	
}

-(id)initWithConnection:(DSRESTConnection *) aconnection  withChannel:  (NSString *) channelName 
		   withDelegate: (id) adelegate {
	return [self initWithConnection:connection withChannel:channelName
					  withManagerID:[DSCallbackChannelManager generateManagerID] withDelegate:adelegate];
}


-(void) dealloc {
	if (wThread) {
		[wThread stop];
	}
	
}
-(void) doOnError:(NSException *) ex {
	
	if ((delegate) &&([delegate respondsToSelector:@selector(onCallbackError:withManager:)])){
	   [delegate onCallbackError:ex withManager:self];
   }else {
	   @throw ex;
   }


}
-(NSLock*) getLock {
	return lock;
}


/**
 * Registering another callback with the client channel
 * 
 * @param CallbackId

 */
/**
 * Registering another callback with the client channel
 * 
 * @param CallbackId
 * @throws Exception
 */

-(bool) registerClientCallback:(NSString*) callbackId{
	bool res ;
	res = [dsadmin RegisterClientCallbackServer:ManagerID								 
				 withCallbackId:callbackId withChannelNames:ChannelName withSecurityToken:SecurityToken];
		
	return res;
	
}


-(bool) NotifyCallback: (NSString *) clientid  withCallbackId: (NSString *) callbackid withMsg: (TJSONValue *) msg withResponse: (out TJSONValue **) response{

	return [dsadmin NotifyCallback:clientid withCallbackId:callbackid withMsg:msg withResponse:response];
}


-(bool) BroadcastToChannel: (NSString *) channelname  withMsg: (TJSONValue *) msg{
	return [dsadmin BroadcastToChannel:channelname withMsg:msg];
}


-(bool) registerCallback:(NSString*) callbackId WithDBXCallBack: (DBXCallback*) callback {
	[[self getLock] lock];
	DSRESTConnection * clone =[connection Clone:YES];
	@try {
		@try{
		if (!wThread) {	
	
			wThread = [[WorkerThread alloc] initWithCallbackChannelManager:self withConnection:clone withCallback:callbackId withDBXCallBack:callback ];      
				
			[wThread start];
		} else {
			[wThread addCallBack:callback WithID:callbackId];
			if(	![self registerClientCallback:callbackId]){
				
				[wThread removeCallback:callbackId];
				return NO;
			}
		}
		}@catch (NSException * ex) {
			[self doOnError:ex];
	  }		
		return YES;
	} @finally {
		[[self getLock] unlock];
	}
}

/**
 * Removing a callback from a Client Channel 
 * 
 * @param CallbackId
 * @return
 * @throws Exception
 */
-(bool) unregisterCallback:(NSString *) callbackId{
	
	[[self getLock] lock];
	@try {		
		bool res;
		@try {
			
			res = [dsadmin UnregisterClientCallback:ManagerID withCallbackId:callbackId withSecurityToken:SecurityToken];
			if (res) {
				[wThread removeCallback:callbackId];
			}
		}
		@catch (NSException * e) {
			[self doOnError:e];
		}
		
		return res;
	} @finally {
		[[self getLock] unlock];
	}
}


/**
 * Stopping the Heavyweight Callback
 * 
 * @return
 * @throws Exception
 */
-(bool) closeClientChannel {
	[[self getLock] lock];
	@try {
		bool res;
		@try {
			res = [dsadmin CloseClientChannel:ManagerID withSecurityToken:SecurityToken];
			
			[wThread stop];
			[wThread interrupt];
			wThread = nil;
		}
		@catch (NSException * e) {
			[self doOnError:e];
		}
		return res;
	} @finally {
			[[self getLock] unlock];
	}
}
-(void) stop{
	[self closeClientChannel];

}

@end
@implementation DSCallbackChannelManager (callbackUtility)
+(NSString *) generateManagerID{
	return[ NSString stringWithFormat:@"%i" ,arc4random()%100000];
}

@end
  
@implementation WorkerThread

@synthesize parent;


-(void) execThread{
	@try {
		[self run];
	}
	@finally {
	}
	
}
-(void) start{
	[internalThread start];
}
-(void) stop{
	stopped =YES;
}
-(void) interrupt{
 if ([internalThread isExecuting]) {
	 [internalThread cancel];
 }
}

-(id) init{
	self = [super init];
	if (self) {
		lock =[[NSLock alloc]init];
		internalThread = [[NSThread  alloc] initWithTarget: self selector:@selector(execThread) object:nil];
		callbacks = [[NSMutableDictionary alloc]initWithCapacity:0];
	}
	return self;
}

-(id) initWithCallbackChannelManager:(DSCallbackChannelManager*) callBackChannelManager 
					  withConnection:(DSRESTConnection *) aconnection
						withCallback: (NSString*) callbackId 
					 withDBXCallBack: (DBXCallback*) callback{
	self = [self init];
	if (self) {

		dsadmin = [[DSAdmin alloc]initWithConnection:aconnection ];
		parent = callBackChannelManager;
		firstCallBackID = callbackId;
		firstCallBack = callback;	
	}
	return self;
}
-(void) dealloc{
	[self stop];
	[self interrupt];
}
-(void) addCallBack:(DBXCallback *) acallBack WithID:(NSString *) callbackId{
	[self cbListLock];
	@try {
		[callbacks setObject: acallBack forKey:callbackId];
	}
	@finally {
		[self cbListUnLock];
	}
	
}
-(void) removeCallback:(NSString*) callbackId {
	[ self cbListLock];
	@try {
		[callbacks removeObjectForKey:callbackId ];
	} @finally {
		[self cbListUnLock];
	}
}

-(void) cbListLock{
	[lock lock ];
}
-(void)cbListUnLock{
	[lock unlock];
}
-(void) terminate {
	stopped = YES;
}
/**
 * send the the contents of the server response 
 * at the "execute" method of our DBXCallback class
 * 
 * @param json
 *      the contents of the server response
 
 */
-(void) invokeEvent:(TJSONObject *) json{
    TJSONArray* arr = [json getJSONArrayForKey:@"invoke"];
  
	NSString* callbackID = [arr stringByIndex:0];
    TJSONValue* v = [arr valueByIndex:1];
	int n = [arr intByIndex:2];
	
	if ([callbacks objectForKey:callbackID]) {
		DBXCallback * cb = [callbacks objectForKey:callbackID];
		[cb execute:v andJSONTYPE:n];
	} else
		@throw [DBXException
				exceptionWithName:@"InvalidCallBackResponse"
				reason:  [NSString stringWithFormat: @"Invalid callback response %@",[json description] ]
				userInfo:nil];
	
	
}

-(void) broadcastEvent:(TJSONObject *) json {
	
	 NSDictionary *  keys = callbacks;
	TJSONArray* arr =  [json getJSONArrayForKey:@"broadcast"];
	TJSONValue * v = [arr valueByIndex:0];
	int n = [arr intByIndex:1];
 	 for (NSString*  callbackskeys in  keys) {
		 DBXCallback * cb = [callbacks objectForKey :callbackskeys];
		 if(cb){	 
			 [cb execute:v andJSONTYPE:n];
		 }else {
			 @throw [DBXException
				 exceptionWithName:@"InvalidCallBackResponse"
				 reason:  [NSString stringWithFormat: @"Invalid callback response %@",[json description]]
				 userInfo:nil];
		 
		 }
	 }

}
/**
 * Getting a response from the Server
 * There are two ways in which we respond to the server,
 * broadcast (all client registered into the channel) or
 * invoke (only a specific CallbackId)
 * 
 * @param Arg
 *      the server response
 
 */
-(void) executeCallback:(TJSONObject*) arg {
	[self cbListLock];
	@try {

	if ([arg hasKey:@"broadcast"]) {
		[self broadcastEvent:arg];
	} else if ([arg hasKey:@"invoke"]) {
		[self invokeEvent:arg];
	}else if ([arg hasKey:@"close"]) {
		stopped =[arg getBoolForKey:@"close"];
	}  
	else
	{
		stopped = YES;
		@throw [DBXException
				exceptionWithName:@"InvalidCallBackResult"
				reason:  [NSString stringWithFormat: @"Invalid callback result type"]
				userInfo:nil];
	}
	}@finally {
		[self cbListUnLock];		
	}	
	
}


-(void) doOnError:(NSException *) ex{
	objc_msgSend(parent, @selector(doOnError:),ex);
}
-(TJSONObject *) channelCallbackExecute {
	TJSONValue* value = [[TJSONTrue alloc]init];
	TJSONValue * res;
	NSTimeInterval lastattempt = 0;
	int retries = 0;
    while (!stopped) {
		@try{
			lastattempt =[[NSDate date]timeIntervalSince1970 ];
			res = [dsadmin ConsumeClientChannel:parent.ChannelName withClientManagerId:parent.ManagerID withCallbackId:@"" 
							   withChannelNames:parent.ChannelName
							  withSecurityToken:parent.SecurityToken withResponseData:value];
			break;

		}@catch (NSException *ex) {
			res = nil;
			if (([ex.reason rangeOfString:@"-1001"].location != NSNotFound) || //connection timedout
				    ([ex.reason rangeOfString:@"-1004"].location != NSNotFound))//socket error
				 {
				
				long diff = [[NSDate date]timeIntervalSince1970 ]-lastattempt;		
				 if (diff >= dsadmin.Connection.connectionTimeout +1 ) {
					 retries = 0;
				 } 
			if ([ parent maxRetries ] == retries) {
			     
				@throw ex;
	
			}	
				retries ++;
				
				@try {
					[NSThread sleepForTimeInterval:[parent retryDelay]];
				}
				@catch (NSException * e) {
					
				}	
				
			}else {
			   @throw ex;
			}

		}	
		
	}
	return  (TJSONObject *) res;
}
-(void) run{
	
	stopped = NO;
	@try {
		[[parent getLock]lock];
		@try {
		TJSONValue * value = [[TJSONTrue alloc]init];
		TJSONObject * res = (TJSONObject *)  [dsadmin ConsumeClientChannel:parent.ChannelName 
															withClientManagerId:parent.ManagerID 
																 withCallbackId:firstCallBackID 
															   withChannelNames:parent.ChannelName 
														 withSecurityToken:parent.SecurityToken withResponseData:value];
		if ([res hasKey:@"invoke"]) {			 
			if (![[[res getJSONArrayForKey:@"invoke"]JSONObjectByIndex:1 ]getBoolForKey:@"created" ] ){
				@throw [DBXException
						exceptionWithName:@"InvalidCallBackResponse"
						reason:  [NSString stringWithFormat: @"cannot register call back %@",[res toString]]
						userInfo:nil];
			
					}
		}else {
			@throw [DBXException
					exceptionWithName:@"InvalidCallBackResponse"
					reason:  [NSString stringWithFormat: @"cannot register call back %@",[res toString]]
					userInfo:nil];
		 
		 
		}		 
		[self addCallBack:firstCallBack WithID:firstCallBackID];
		}@finally {
			[[parent getLock]unlock ];
		}
		while (!stopped) {
			
		 TJSONObject * jobj = [ self channelCallbackExecute];
		if (jobj) {
			[self executeCallback:jobj];
	
		}
	}
		
	} @catch (NSException *ex) {
		stopped = YES;
		[self doOnError:ex];
	
		
	}
	 
}



@end

