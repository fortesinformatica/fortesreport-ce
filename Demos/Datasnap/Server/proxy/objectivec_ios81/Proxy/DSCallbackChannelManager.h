//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************


#import <Foundation/Foundation.h>
#import "DBXCallback.h"
#import "DSAdmin.h"
typedef enum  {
	Register = 0,
	Consume = 1,
	Unregister = 2,
	Close = 3,
	Execute = 4
} methodTypes;
@class WorkerThread;

/**
 *@brief Handle callback comunications.
 */
@interface DSCallbackChannelManager : NSObject {
@protected	
	NSMutableURLRequest*  httpClient;
	NSString *ChannelName;
	NSString *Protocol;
	NSString *CommunicationTimeout;
	NSString *ConnectionTimeout;
	NSString *HostName;
	NSString *Path;
	NSString *Port;
	NSString *ManagerID;
	NSString *SecurityToken;
	id delegate;
	DSRESTConnection * connection;
	DSAdmin * dsadmin;
@private	
	int maxRetries;
	NSTimeInterval retryDelay;
	WorkerThread *wThread;
	NSLock *lock;


}
/**
 * Max retries count when connection is lost or timedout
 */
@property (nonatomic) int maxRetries; 
/**
 * Delay to wait for next call
 */
@property (nonatomic) NSTimeInterval retryDelay;
/**
 *The name of the channel to connect to.
 */
@property (nonatomic,strong) NSString *ChannelName;  
/**
 *  Unique connection id.
 */
@property (nonatomic,strong) NSString *ManagerID;

/**
* Unique security token. 
*/ 
@property (nonatomic,strong)NSString * SecurityToken;
/**
 *Intialize the connection withe the params 
 *@param aConnection the active connection 
 *@param ChannelName the channel name
 *@param ManagerID  unit manager id
 *@param delegate  the object class which respoonses to connection events 
*/

-(id)initWithConnection:(DSRESTConnection *) aconnection  withChannel:  (NSString *) channelName 
		  withManagerID:(NSString*) managerID withDelegate: (id) adelgate;
-(id)initWithConnection:(DSRESTConnection *) aconnection  withChannel:  (NSString *) channelName 
		   withDelegate: (id) adelegate;
-(NSLock*) getLock ;

	/**
	 * Registering another callback with the client channel
	 * 
	 * @param CallbackId
	 * @returns YES if succeded otherwise NO
	 */
-(bool) registerClientCallback:(NSString*) callbackId;

	/**
	 * method used by the client for Registering or Adding a callback with the
	 * client channel
	 * 
	 * @param CallbackId
	 * @param Callback
	 *            the class that implements the method "execute"
	 * @return true if the registration or adding is OK or false
	 * @throws Exception
	 */
-(bool) registerCallback:(NSString*) callbackId WithDBXCallBack: (DBXCallback*) callback;
	/**
	 * Unregistering a callback with the client channel
	 * 
	 * @param CallbackId
	 * @throws Exception
	 */
-(bool) unregisterCallback:(NSString *) callbackId;
	/**
	 * Stopping the Heavyweight Callback
	 * 
	 * @return
	 * @throws Exception
	 */
-(bool) closeClientChannel;


/**
 *Sends notifications to the callbacks
 * @param ClientId [in] - Type on server: string
 * @param CallbackId [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @param Response [out] - Type on server: TJSONValue
 * @return result - Type on server: Boolean
 */
-(bool) NotifyCallback: (NSString *) clientid  withCallbackId: (NSString *) callbackid withMsg: (TJSONValue *) msg withResponse: (out TJSONValue **) response;
    
/**
 * Send notification to the channels
 * @param ChannelName [in] - Type on server: string
 * @param Msg [in] - Type on server: TJSONValue
 * @return result - Type on server: Boolean
 */
-(bool) BroadcastToChannel: (NSString *) channelname  withMsg: (TJSONValue *) msg;
@end
/**
 *@brief Callback manager utilities
 */
@interface DSCallbackChannelManager (callbackUtility)
/**
 *  generates a new random manager id
 *  @return a string with a new manager id 
 **/ 
+(NSString *) generateManagerID;

@end

/**
 *@brief Worker thead is the internal thread that handle asyncronous request 
 * on  the server
*/
@interface WorkerThread : NSObject
{
@private 
	NSLock * lock;
	DSCallbackChannelManager * parent;
	bool stopped;
	DSAdmin * dsadmin;
	NSThread *internalThread;
	NSMutableDictionary *callbacks;
    NSString *   firstCallBackID;
	DBXCallback * firstCallBack;

   

}
@property (nonatomic,strong) DSCallbackChannelManager * parent;
   

-(id) initWithCallbackChannelManager:(DSCallbackChannelManager*) callBackChannelManager 
	  withConnection:(DSRESTConnection *) aconnection
	   withCallback: (NSString*) callbackId 
      withDBXCallBack: (DBXCallback*) callback;

-(void) run;
/**
 starts the thread
*/
-(void) start;
/**
 *  stops the thread
 */
-(void) stop;
/**
 * interrupts the thread
 */
-(void) interrupt;

/**
 *  Locks synchronize the access to callback list
 */
-(void) cbListLock;
/**
 *  Unlock synchronize the access to callback list
 */

-(void) cbListUnLock;

/**
 *  Adds a new call back to the list
 */

-(void) addCallBack:(DBXCallback *) acallBack WithID:(NSString *) callbackId;

/**
 *  Remove a callback from the list
 */

-(void) removeCallback:(NSString*) callbackId;
@end

@interface NSObject (DSCallBackManagerDelegate)
/**
 * Responds to excecption raised in a callback.
 * When an exception is raised from te server this event is fired to notify the clients.
 */  

-(void) onCallbackError:(NSException* ) ex withManager:(DSCallbackChannelManager *) manager; 
@end


 
