//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

package com.embarcadero.javablackberry;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Random;

import com.embarcadero.javablackberry.DSAdmin.NotifyCallbackReturns;

/**
 * Handle callback communications.
 */
public class DSCallbackChannelManager {

	private String ChannelName;
	String Protocol;
	String CommunicationTimeout;
	String ConnectionTimeout;
	String HostName;
	String Path;
	String Port;
	private int MaxRetries = 5;
	private int RetryDelay = 1000;
	private String ManagerID;
	private String SecurityToken;
	private WorkerThread wThread;
	private Lock lock;
	private DSRESTConnection Connection;
	private DSAdmin dsadmin;

	/**
	 * Class constructor, initialized the new object with the passed value
	 * 
	 * @param Connection
	 * @param ChannelName
	 * @param ManagerID
	 */
	public DSCallbackChannelManager(DSRESTConnection Connection,
			String ChannelName, String ManagerID) {
		super();
		Initialize(Connection, ChannelName, ManagerID);
	}

	protected void Initialize(DSRESTConnection Connection, String ChannelName,
			String ManagerID) {
		lock = new Lock(1);
		this.ChannelName = ChannelName;
		this.ManagerID = ManagerID;
		this.Connection = Connection;
		this.Connection.setConnectionTimeout(30000);
		this.dsadmin = new DSAdmin(this.Connection);
		Random random = new Random();
		this.SecurityToken = String.valueOf(random.nextInt(100000)) + "."
				+ String.valueOf(random.nextInt(100000));
	}

	/**
	 * Class constructor, initialized the new object with the passed value
	 * 
	 * @param Connection
	 * @param ChannelName
	 */
	public DSCallbackChannelManager(DSRESTConnection Connection,
			String ChannelName) {
		super();
		Initialize(Connection, ChannelName, getNewManagerID());
	}

	/**
	 * Return the lock object contained in this object
	 * 
	 * @return
	 */
	protected Lock getLock() {
		return lock;
	}

	/**
	 * Registering another callback with the client channel
	 * 
	 * @param CallbackId
	 * @throws Exception
	 * @return
	 */
	private boolean registerClientCallback(String CallbackId) throws Exception {
		return dsadmin.RegisterClientCallbackServer(getManagerID(), CallbackId,
				ChannelName, getSecurityToken());
	}

	private DSCallbackChannelManagerEventListener eventListener = null;

	public void setEventListener(
			DSCallbackChannelManager.DSCallbackChannelManagerEventListener eventListener) {
		this.eventListener = eventListener;
	}

	public void DoOnException(DSCallbackChannelManager mngr, Throwable e) {
		stopWThread();
		if (eventListener != null)
			eventListener.onException(mngr, e);
	}

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
	public boolean registerCallback(String CallbackId, DBXCallback Callback)
			throws Exception {
		getLock().lock();
		try {
			if (wThread == null) {
				wThread = new WorkerThread(CallbackId, Callback,
						Connection.Clone(true), this);
				wThread.start();
			} else {
				wThread.addCallback(CallbackId, Callback);
				if (!registerClientCallback(CallbackId)) {
					wThread.removeCallback(CallbackId);
					return false;
				}
			}
			return true;
		} catch (Throwable e) {
			DoOnException(this, e);
			return false;
		} finally {
			getLock().unlock();
		}
	}

	/**
	 * Stopping the Heavyweight Callback
	 * 
	 * @return
	 * @throws Exception
	 * 
	 */
	public boolean closeClientChannel() throws Exception {
		getLock().lock();
		boolean res = false;
		try {
			res = dsadmin
					.CloseClientChannel(getManagerID(), getSecurityToken());
			if (wThread != null)
				wThread.terminate();
		} catch (Throwable e) {
			DoOnException(this, e);
		} finally {
			wThread = null;
			getLock().unlock();
		}
		return res;

	}

	/**
	 * Stop the wThread
	 */
	private void stopWThread() {
		wThread.stopped = true;
		wThread = null;
	}

	/**
	 * invokes closeClientChannel()
	 * 
	 * @throws Exception
	 */
	public void stop() throws Exception {
		closeClientChannel();
	}

	/**
	 * Wraps the NotifyCallback method of the DSAdmin class
	 * 
	 * @param CallbackId
	 * @param Msg
	 * @return
	 */

	public NotifyCallbackReturns notifyCallback(String CallbackId,
			TJSONValue Msg) {
		try {
			return dsadmin.NotifyCallback(getManagerID(), CallbackId, Msg);
		} catch (DBXException e) {
			DoOnException(this, e);
			return null;
		}
	}

	/**
	 * Wraps the broadcastToChannel method of the DSAdmin class
	 * 
	 * @param Msg
	 * @return
	 */
	public boolean broadcastToChannel(TJSONValue Msg) {
		try {
			return dsadmin.BroadcastToChannel(getChannelName(), Msg);
		} catch (DBXException e) {
			DoOnException(this, e);
			return false;
		}
	}

	/**
	 * Removing a callback from a Client Channel
	 * 
	 * @param CallbackId
	 * @return
	 * @throws Exception
	 */
	public boolean unregisterCallback(String CallbackId) throws Exception {
		getLock().lock();
		boolean res = false;
		try {
			res = dsadmin.UnregisterClientCallback(ChannelName, CallbackId,
					getSecurityToken());
			wThread.removeCallback(CallbackId);
		} catch (Exception e) {
			DoOnException(this, e);
		} finally {
			getLock().unlock();
		}
		return res;
	}

	/**
	 * Returns the name of the channel to connect to.
	 * 
	 * @return
	 */
	public String getChannelName() {
		return ChannelName;
	}

	/**
	 * Returns Unique connection id.
	 * 
	 * @return
	 */
	public String getManagerID() {
		return ManagerID;
	}

	/**
	 * Returns Unique Security Token.
	 * 
	 * @return
	 */
	public String getSecurityToken() {
		return SecurityToken;
	}

	// / CALLBACKs WORKER THREAD
	// ///////////////////////////////////////////////////////

	static class WorkerThread extends Thread {
		protected boolean stopped;
		private Lock lock = new Lock(1);
		private DSAdmin dsadmin;
		private String firstCallback;
		private DSCallbackChannelManager mngr;
		private Hashtable callbacks;

		public WorkerThread(String CallbackId, DBXCallback Callback,
				DSRESTConnection connection, DSCallbackChannelManager mngr) {
			super();
			this.dsadmin = new DSAdmin(connection);
			this.mngr = mngr;
			callbacks = new Hashtable();
			firstCallback = CallbackId;
			addCallback(CallbackId, Callback);
		}

		public void removeCallback(String callbackId) {
			cbListLock();
			try {
				callbacks.remove(callbackId);
			} finally {
				cbListUnLock();
			}
		}

		public void clearList() {
			callbacks.clear();
		}

		public void addCallback(String callbackId, DBXCallback callback) {
			cbListLock();
			try {
				callbacks.put(callbackId, callback);
			} finally {
				cbListUnLock();
			}
		}

		public void terminate() {
			stopped = true;
		}

		public void run() {
			try {
				try {
					cbListLock();
					stopped = false;
					boolean response = false;
					TJSONValue t = new TJSONTrue();

					// retry mechanism
					int r = 0;
					while (!response) {
						Thread.sleep(mngr.getRetryDelay());
						TJSONValue res = dsadmin.ConsumeClientChannel(
								mngr.getChannelName(), mngr.getManagerID(),
								firstCallback, mngr.getChannelName(),
								mngr.getSecurityToken(), t);
						if (!((TJSONObject) res).has("error")) {
							response = ((TJSONObject) res)
									.getJSONArray("invoke").getAsJsonObject(1)
									.getBoolean("created").booleanValue();
							if (!response)// The server return created false, so
											// can't register the callback
								throw new DBXException("Cannot register "
										+ mngr.getChannelName()
										+ " server returns false");
						}
						r++;
						if (r > mngr.getMaxRetries()) // if is still not
														// registered after the
														// retry mechanism
							throw new DBXException("Cannot register "
									+ mngr.getChannelName());
					}
				} finally {
					cbListUnLock();
				}
				TJSONObject jobj;
				while (!stopped) {
					jobj = channelCallbackExecute();
					if (jobj != null)
						executeCallback(jobj);
				}
				this.interrupt();
			} catch (Exception ex) {
				stopped = true;
				this.interrupt();
				mngr.DoOnException(mngr, ex);
			}

		}

		/**
		 * Getting a response from the Server There are two ways in which we
		 * respond to the server, broadcast (all client registered into the
		 * channel) or invoke (only a specific CallbackId)
		 * 
		 * @param Arg
		 *            the server response
		 * @throws Exception
		 */
		private void executeCallback(TJSONObject arg) throws Exception {
			cbListLock();
			try {
				if (arg.has("broadcast")) {
					broadcastEvent(arg);
				} else if (arg.has("invoke")) {
					invokeEvent(arg);
				} else if (arg.has("close")) {
					stopped = arg.getBoolean("close").booleanValue();
				} else {
					throw new DBXException("Invalid callback result type");
				}
			} finally {
				cbListUnLock();
			}
		}

		/**
		 * send the the contents of the server response at the "execute" method
		 * of our DBXCallback class
		 * 
		 * @param json
		 *            the contents of the server response
		 * @throws DBXException
		 */
		private void invokeEvent(TJSONObject json) throws Exception {
			TJSONArray arr = json.getJSONArray("invoke");
			String callbackID = arr.getAsJsonString(0).value;
			TJSONValue v = arr.get(1);
			int n = arr.getInt(2).intValue();
			DBXCallback cb = (DBXCallback) callbacks.get(callbackID);
			if (cb != null)
				cb.execute(v, n);
			else
				throw new DBXException("Invalid callback response");
		}

		private void broadcastEvent(TJSONObject json) throws Exception {
			Enumeration keys = callbacks.keys();
			TJSONArray arr = json.getJSONArray("broadcast");
			TJSONValue value = arr.get(0);
			int n = arr.getInt(1).intValue();
			while (keys.hasMoreElements()) {
				String callbackskeys = (String) keys.nextElement();
				DBXCallback cb = (DBXCallback) callbacks.get(callbackskeys);
				if (cb != null)
					cb.execute(value, n);
				else
					throw new DBXException("Invalid callback response");
			}

		}

		/**
		 * @return TJSONObject
		 * @throws Exception
		 */
		private TJSONObject channelCallbackExecute() throws Exception {
			TJSONValue res = null;
			long lastRequestAttempt = 0;
			int retries = 0;
			while (!stopped) {
				try {
					TJSONValue Value = new TJSONTrue();
					lastRequestAttempt = System.currentTimeMillis();
					res = dsadmin.ConsumeClientChannel(mngr.getChannelName(),
							mngr.getManagerID(), "", mngr.getChannelName(),
							mngr.getSecurityToken(), Value);
					break;
				} catch (DBXException e) {
					Throwable InternalException = e.getInternal();
					if (InternalException instanceof IOException) {
						// If the socket has been connected for
						// "ConnectionTimeout" or more,
						// the retries count should be resetted
						if (System.currentTimeMillis() - lastRequestAttempt >= dsadmin
								.getConnection().getConnectionTimeout() + 1000)
							retries = 0;
						if (retries == this.mngr.getMaxRetries()) {
							mngr.DoOnException(mngr, InternalException);
							res = null;
							break;
						}
						retries++;
						try {
							Thread.sleep(this.mngr.getRetryDelay());
						} catch (InterruptedException ie) {
						}
					} else {
						mngr.DoOnException(mngr, e);
						res = null;
						break;
					}
				}
			}
			return (TJSONObject) res;

		}

		/**
		 * prepare the Post request and then use the execute method
		 * 
		 * @param Url
		 * @param Param
		 * @return
		 * @throws InterruptedException
		 * @throws Exception
		 */

		private void cbListLock() {
			try {
				lock.lock();
			} catch (InterruptedException e) {
				mngr.DoOnException(mngr, e);
			}
		}

		private void cbListUnLock() {
			try {
				lock.unlock();
			} catch (InterruptedException e) {
				mngr.DoOnException(mngr, e);
			}
		}

	}

	/**
	 * 
	 * @return a New String represents a ManagerID
	 */
	public static String getNewManagerID() {
		Random random = new Random();
		return String.valueOf(random.nextInt(100000)) + "."
				+ String.valueOf(random.nextInt(100000));
	}

	public void setMaxRetries(int maxRetries) {
		MaxRetries = maxRetries;
	}

	public int getMaxRetries() {
		return MaxRetries;
	}

	public void setRetryDelay(int retryDelay) {
		RetryDelay = retryDelay;
	}

	public int getRetryDelay() {
		return RetryDelay;
	}

	public interface DSCallbackChannelManagerEventListener {
		public void onException(DSCallbackChannelManager mngr, Throwable e);
	}
}
