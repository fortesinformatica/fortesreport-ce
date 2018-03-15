//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

#import <Foundation/Foundation.h>
#import "DSRESTConnection.h"

@interface DSAdminRestClient: NSObject{
@protected	 DSRESTConnection * Connection;
	
}
@property (nonatomic,retain) DSRESTConnection * Connection;

-(id) initWithConnection:(DSRESTConnection *) aConnection;
	



@end
