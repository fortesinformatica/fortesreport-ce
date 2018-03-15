//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

/**
 * Lock class provides methods for threads synchronization
 *
 */
public class Lock {
	  private int signals = 0;
	  private int bound   = 0;

	  public Lock(int upperBound){
	    this.bound = upperBound;
	  }
	  
	  /**
	   * Acquires the lock.
	   * @throws InterruptedException
	   */
	  public synchronized void lock() throws InterruptedException{
	    while(this.signals == bound) wait();
	    this.signals++;
	    this.notify();
	  }

	  /**
	   * Release the lock.
	   * @throws InterruptedException
	   */
	  public synchronized void unlock() throws InterruptedException{
	    while(this.signals == 0) wait();
	    this.signals--;
	    this.notify();
	  }
	}
