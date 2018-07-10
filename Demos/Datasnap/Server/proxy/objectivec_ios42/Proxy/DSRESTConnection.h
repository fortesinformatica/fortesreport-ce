//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DSRestParameter.h"

@class DSRESTConnection;

typedef enum  {GET,POST,PUT,DELETE} requestTypes ;



@interface DSRESTCommand : NSObject {
	requestTypes RequestType;
	NSString *FullyQualifiedMethodName;
	NSMutableArray *parameters;
	DSRESTConnection *Connection;
	NSString * text;
	
	
    
}
/**
 * Connection  to execute the command.
 */

@property (nonatomic,retain) DSRESTConnection *Connection;

/**
 * Kid of request GET,PUT,POST,DELETE
 */

@property (nonatomic) requestTypes RequestType;
/**
 * Fully qualified method name to execute on the server
 */
@property (copy,nonatomic) NSString * text;
/**
 * List of command parameters.
 */
@property (nonatomic,retain) NSArray *parameters;


/**
 * Initialize a command with the actual connection.
*  @param the connection
*  @return an instance of itself. 
 */

-(id) initWithConnection: (DSRESTConnection *) aconnection;

/**
 * Prepares internal parameter list using information passed
 */
-(void) prepare: (NSArray *) metadatas;

/**
 * Gets internal parameter by indexd
 * @param  parameter index
 * @return the parameter.  
 */
-(DSRestParameter *) getParameterByIndex:(int) index;

/**
 * Executes the command
 */
-(void) execute;
/**
 * Prepares internal param list asking the server
 */
-(void) prepare;


@end

/**
 * @brief Allows you to manage and make REST requests based on the protocol, the target
 * host , the context and other features. Supports authentication and
 * authorization.
 * 
 * Use the properties and methods of DSRESTConnection to: <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Set the general parameters of a connection as
 * target host and port, context, protocol , URL path etc... <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Setting Username and Password for authentication
 * and authorization. <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Execute a REST request from a {@link DSRESTCommand}
 * . <br>
 * &nbsp;&nbsp;&nbsp;&nbsp;- Rebuild and save the parameters contained in the
 * response.
 * 
 */

@interface DSRESTConnection : NSObject {
@private
	int Port;
	NSString *UrlPath;
	NSString *Host;
	NSString *protocol;
	NSString *Context;
	NSString * SessionID;
	long SessionIDExpires;
	NSString* UserName;
	NSString* Password;
	id internalDelegate;
	NSTimeInterval connectionTimeout;

}
/**
 * The port the server is listening to.
 */
@property (nonatomic) int Port;
/**
 * Additional ulr information
 */
@property (nonatomic,copy) NSString *UrlPath;
/**
 * Host name or ip address of the server.
 */
 
@property (nonatomic,copy) NSString *Host;
/**
 * Protocol to be used for connection Http or Https
 */
@property (nonatomic,copy) NSString *protocol;

/**
 * Connection context;
 */
@property (nonatomic,copy) NSString *Context;
/**
 * Connection internal session id.
 */
@property (nonatomic,retain) NSString *SessionID;
/**
 * SessionID experidion period
 */
@property (nonatomic) long SessionIDExpires;
/**
 * User name for protected access.
 */

@property (nonatomic,copy) NSString *UserName;
/**
 *  Password. For protected access.
 */ 
@property (nonatomic,copy) NSString *Password;

/**
 * Period of time to try to reconnect.
 */
@property (nonatomic) NSTimeInterval connectionTimeout;

/**
 * Factory method to quick  create a command.
 */
-(DSRESTCommand *) CreateCommand;
/**
 * Build a request url from a command
 */
-(NSString *) buildRequestURL:(DSRESTCommand *) command;

/**
 *  Executes a command
 *  @param the command to execute  
 */
-(void) execute: (DSRESTCommand *) command;
/**
 * Close session.
 * 
 */
-(void) closeSession;
/**
 * Intialized a connection with a delegate to respond to connection events
 *  @param object that responds/defines to connection events  
 */
-(id) initWithDelegate: (id) aDelegate;
/**
 * Clones the connection
 * @return a clone of the connection.
 */
-(DSRESTConnection*) Clone;
-(DSRESTConnection*) Clone:(bool) includeSession;
/**
* Sets the internal delegate to respond to connection events
* @param aDelegate the object that responds to connection events 
 */
-(void) setDelegate:(id)aDelegate;
/**
 * Returns the internal delegate which responds to connection events
 *@return the internal delegate
 */

-(id) getDelegate;
@end
