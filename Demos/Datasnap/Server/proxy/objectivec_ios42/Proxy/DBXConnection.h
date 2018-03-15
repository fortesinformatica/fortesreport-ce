//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
 /*! \mainpage ObjectiveC iOS 4.2 DataSnap Connector
 *
 * \section intro_sec Introduction
 *
 *   ObjectiveC iOS 4.2 DataSnap Connector is a framework that allows to connect
 *   to a Datasnap REST server. 
 *    
 *
  */


/**
 * @brief This class implements a syncronous connection using an asyncronous call.
 * The standard  sendSynchronousRequest of NSURLConnection doesn't allow
 * to handle events sended to the delegate. Using asyncronus connection and
 * waiting for it can allow to response to connection's events.  
 */ 
@interface DBXConnection : NSObject
{
@private
	NSMutableData *receivedData;
	NSURLConnection *dbxconn;
	NSURLResponse ** urlresponse;
	NSError ** urlerror;
	BOOL complete,cancelled,isSynchronous;
	id internalDelegate;
}
/**
 *Detects wheter the connecton is asycronous or not.
 */
@property (nonatomic) BOOL  isSynchronous;
/**
 *Initialize an dbx conenection
 * 
 *@param request: [in] It is the request. Standard NSRequestUrl.
 *@param response [in/out] It is the reponse returned by the server. It may be nil
 *@param error [in/out]  It is the error returned by the server if the request fails.
 *@param delegate [in] It is the delegate objected that receive connection events.
 *@param  return  it returns an DBXConnection  
 * 
 */
- (id)initWithRequest:(NSURLRequest *)request returningResponse:(NSURLResponse **)response error:(NSError **)error usingDelegate:(id) adelegete;
 /**
  *  Returns the request data
  *  @param return NSMutableData data from connection.  
  */   
- (NSMutableData*) getReceivedData;


@end
 /*
 @class  DBXConnectionSynchronousLoading  DBXConnection.h "DBXConnection.h"
 This category holds methods to create a syncronous connection for dbxConnection
 */
@interface DBXConnection (DBXConnectionSynchronousLoading)
/**
 *Initialize a synchronous dbx conenection
 * 
 *@param request: [in] It is the request. Standard NSRequestUrl.
 *@param response [in/out] It is the reponse returned by the server. It may be nil
 *@param error [in/out]  It is the error returned by the server if the request fails.
 *@param delegate [in] It is the delegate objected that receive connection events.
 *@param  return  it returns an DBXConnection  
 * 
 */	
+ (NSData *)sendSynchronousRequest:(NSURLRequest *)request returningResponse:(NSURLResponse **)response error:(NSError **)error
usingDelegate: (id) adelegate;


@end








