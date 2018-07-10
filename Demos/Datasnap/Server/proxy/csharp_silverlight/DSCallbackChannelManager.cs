//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;
using Newtonsoft.Json.Linq;

namespace Embarcadero.Datasnap.WindowsPhone7
{

    public interface IDSCallbackChannelManagerEventListener
    {
        void OnException(Exception ex);
    }

    /**
    * Handle callback communications. 
    */
    public class DSCallbackChannelManager
    {
        private String ChannelName;
        private String ManagerID;
        private String SecurityToken;
        private WorkerThread wThread;
        private Thread thread;
        private Object locker;
        private DSRESTConnection Connection;
        private DSAdmin dsadmin;
        private DSAdmin.ExceptionCallback ExCallback;
        private int MaxRetries = 5;
        private int RetryDelay = 1000;

        public void setMaxRetries(int maxRetries)
        {
            MaxRetries = maxRetries;
        }

        public int getMaxRetries()
        {
            return MaxRetries;
        }

        public void setRetryDelay(int retryDelay)
        {
            RetryDelay = retryDelay;
        }

        public int getRetryDelay()
        {
            return RetryDelay;
        }



        /**
         * @param Connection DSRESTConnection         
         * @param ChannelName String
         * @param ManagerID String
         */
        public DSCallbackChannelManager(DSRESTConnection Connection,
                String ChannelName, String ManagerID, DSAdmin.ExceptionCallback ExceptionCallback)
            : base()
        {
            locker = new Object();
            this.ExCallback = ExceptionCallback;
            this.ChannelName = ChannelName;
            this.ManagerID = ManagerID;
            this.Connection = Connection;
            this.dsadmin = new DSAdmin(this.Connection, ExCallback);
            Random random = new Random();
            this.SecurityToken = Convert.ToString(random.Next(100000)) + "." + Convert.ToString(random.Next(100000));
        }

        public DSCallbackChannelManager(DSRESTConnection Connection,
                String ChannelName)
            : base()
        {
            locker = new Object();
            this.ChannelName = ChannelName;
            this.ManagerID = getNewManagerID();
            this.Connection = Connection;
            this.dsadmin = new DSAdmin(this.Connection, ExCallback);
            Random random = new Random();
            this.SecurityToken = Convert.ToString(random.Next(100000)) + "." + Convert.ToString(random.Next(100000));
        }

        public void NotifyCallback(String ClientId, String CallbackId, TJSONValue Msg, DSAdmin.NotifyCallbackCallback callback = null, DSAdmin.ExceptionCallback ExCal = null)
        {
            dsadmin.NotifyCallback(ClientId, CallbackId, Msg, callback, ExCal);
        }

        public void BroadcastToChannel(String ChannelName, TJSONValue Msg, DSAdmin.BroadcastToChannelCallback callback = null, DSAdmin.ExceptionCallback ExCal = null)
        {
            dsadmin.BroadcastToChannel(ChannelName, Msg, callback, ExCal);
        }


        private IDSCallbackChannelManagerEventListener EventListener = null;

        public void SetEventListener(IDSCallbackChannelManagerEventListener EventListener)
        {

        }

        protected void lockIt()
        {
            Monitor.Enter(this);
        }

        protected void unlockIt()
        {
            Monitor.Exit(this);
        }

        /**
         * Registering another callback with the client channel
         * 
         * @param CallbackId
         * @throws Exception
         */
        private void registerClientCallback(String CallbackId,
                DSAdmin.RegisterClientCallbackServerCallback RegisterClientCallbackServerCallback = null)
        {
            dsadmin.RegisterClientCallbackServer(getManagerID(), CallbackId,
                    ChannelName, getSecurityToken(),
                    RegisterClientCallbackServerCallback,
                    (Exception ex) =>
                    {
                        if (EventListener != null)
                            EventListener.OnException(ex);
                    });
        }

        public delegate void OnRegisterCallbackFinish();
        /**
         * method used by the client for Registering or Adding a callback with the client channel
         *
         * @param CallbackId String
         * @param Callback the class that implements the method "execute"
         * @param OnRegisterCallbackFinish
         */
        public void registerCallback(String CallbackId, DBXCallback Callback, OnRegisterCallbackFinish onRegisterCallbackFinish = null)
        {
            TJSONValue Value = new TJSONTrue();
            if (wThread == null)
            {
                wThread = new WorkerThread(Connection.Clone(true), this);
                thread = new Thread(new ThreadStart(wThread.run));
                dsadmin.ConsumeClientChannel(ChannelName,
                  getManagerID(), CallbackId, ChannelName,
                  getSecurityToken(), Value, (TJSONValue res) =>
                        {
                            if (res is TJSONObject && ((TJSONObject)res).has("invoke"))
                            {
                                thread.Start();
                            }
                            if (onRegisterCallbackFinish != null)
                                onRegisterCallbackFinish();
                        });
            }
            else
            {
                registerClientCallback(CallbackId);
                if (onRegisterCallbackFinish != null)
                    onRegisterCallbackFinish();
            }
            wThread.addCallback(CallbackId, Callback);
        }


        /**
         * Stopping the Heavyweight Callback
         * 
         * @return bool
         * @throws Exception
         */
        public bool closeClientChannel()
        {
            lockIt();
            try
            {
                dsadmin.CloseClientChannel(getManagerID(), getSecurityToken());
                wThread.terminate();
                try
                {
                    if (!thread.Join(500))
                        thread.Abort();
                }
                catch (Exception) { }
                thread = null;
                return true;
            }
            finally
            {
                unlockIt();
            }
        }

        /**
        * invokes closeClientChannel()
        */
        public void stop()
        {
            closeClientChannel();
        }

        /**
         * Removing a callback from a Client Channel
         * 
         * @param CallbackId
         * @throws Exception
         */
        public void unregisterCallback(String CallbackId, DSAdmin.UnregisterClientCallbackCallback Callback = null)
        {
            lockIt();
            try
            {
                dsadmin.UnregisterClientCallback(ChannelName,
                        CallbackId, getSecurityToken(), Callback,
                        (Exception ex) => { if (EventListener != null) EventListener.OnException(ex); });
            }
            finally
            {
                unlockIt();
            }
        }

        /**
     * Returns the name of the channel to connect to. 
     * @return String channelname
     */
        public String getChannelName()
        {
            return ChannelName;
        }

        /**
         * Returns Unique connection id. 
         * @return String channelID
         */
        public String getManagerID()
        {
            return ManagerID;
        }

        /**
	     * Returns Unique Security Token. 
	     * @return String SecurityToken
	     */
        public String getSecurityToken()
        {
            return SecurityToken;
        }
        //////////////////////////////////////////////////////////
        //// CALLBACKs WORKER THREAD
        //////////////////////////////////////////////////////////
        protected class WorkerThread
        {
            protected bool stopped;
            private Object locker = new Object();
            private DSAdmin dsadmin;
            private DSCallbackChannelManager mngr;
            private Dictionary<String, DBXCallback> callbacks;

            public WorkerThread(DSRESTConnection connection,
                    DSCallbackChannelManager mngr)
                : base()
            {
                this.dsadmin = new DSAdmin(connection, mngr.ExCallback);
                this.mngr = mngr;
                callbacks = new Dictionary<string, DBXCallback>();
            }

            public void removeCallback(String callbackId)
            {
                cbListLock();
                try
                {
                    callbacks.Remove(callbackId);
                }
                finally
                {
                    cbListUnLock();
                }
            }

            public void addCallback(String callbackId, DBXCallback callback)
            {
                cbListLock();
                try
                {
                    callbacks.Add(callbackId, callback);
                }
                finally
                {
                    cbListUnLock();
                }
            }

            public void terminate()
            {
                stopped = true;
            }


            public delegate void Exec(JObject jobj);
            public void run()
            {
                stopped = false;
                try
                {
                    TJSONValue res;
                    while (!stopped)
                    {
                        res = channelCallbackExecute();
                        if (res != null)
                            executeCallback((JObject)res.getInternalObject());
                        res = null;
                    }
                }
                catch (Exception)
                {
                    stopped = true;
                }
            }

            /**
             * Getting a response from the Server There are two ways in which we respond to the server, broadcast (all client registered into the channel) or invoke (only a specific CallbackId) 
             * @param arg the server response
             */
            private void executeCallback(JObject arg)
            {
                cbListLock();
                JToken o;
                try
                {
                    if (arg.TryGetValue("broadcast", out o))
                    {
                        broadcastEvent(arg);
                    }
                    else if (arg.TryGetValue("invoke", out o))
                    {
                        invokeEvent(arg);
                    }
                    else if (arg.TryGetValue("close", out o))
                    {
                        stopped = arg.Value<bool>("close");
                    }
                    else
                        throw new DBXException("Invalid callback result type");
                }
                finally
                {
                    cbListUnLock();
                }
            }

            /**
             * send the the contents of the server response at the "execute" method of our DBXCallback class  
             * @param json the contents of the server response
             */
            private void invokeEvent(JObject json)
            {
                TJSONArray arr = new TJSONArray(json.Value<JArray>("invoke"));
                String callbackID = arr.getAsJsonString(0).getValue();
                arr.remove(0);
                DBXCallback cb = callbacks[callbackID];
                if (cb != null)
                    cb.Execute(arr.get(0), Convert.ToInt32(arr.getInt(1).Value));
                else
                    throw new DBXException("Invalid callback response");
            }

            private void broadcastEvent(JObject json)
            {
                List<string> keys = new List<string>(callbacks.Keys);
                TJSONArray arr = new TJSONArray(json.Value<JArray>("broadcast"));
                foreach (String callbackskeys in keys)
                {
                    DBXCallback cb = callbacks[callbackskeys];
                    if (cb != null)
                        cb.Execute(arr.get(0), Convert.ToInt32(arr.getInt(1).Value));
                    else
                        throw new DBXException("Invalid callback response");
                }
            }


            /**
             * @return JObject
             */
            private TJSONValue channelCallbackExecute()
            {
                Object o = new Object();
                TJSONValue Value = new TJSONTrue();
                TJSONValue res = null;                
                long lastRequestAttempt = 0;
                int retries = 0;
                while (!stopped) {
                    lastRequestAttempt = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
                    res = null;
                    Exception raisedException = null;
                    Monitor.Enter(o);
                    try
                    {
                        dsadmin.ConsumeClientChannel(mngr.getChannelName(), mngr
                                .getManagerID(), "", mngr.getChannelName(), mngr
                                .getSecurityToken(), Value,
                                (r) =>
                                {
                                    Monitor.Enter(o);
                                    try
                                    {
                                        res = r;
                                        Monitor.PulseAll(o);
                                    }
                                    finally
                                    {
                                        Monitor.Exit(o);
                                    }
                                }, (e) =>
                                {
                                    Monitor.Enter(o);
                                    try
                                    {
                                        raisedException = e;
                                        Monitor.PulseAll(o);
                                    }
                                    finally
                                    {
                                        Monitor.Exit(o);
                                    }
                                }
                        );
                        Monitor.Wait(o);

                        //analize the callback's results
                        if (raisedException != null)
                        {
                            if ((DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond) - lastRequestAttempt >= dsadmin.getConnection().getConnectionTimeout() + 1000)
                                retries = 0;
                            if (retries == this.mngr.getMaxRetries())
                            {
                                terminate();
                                res = null;
                                mngr.Connection.syncContext.Send(new SendOrPostCallback(x => mngr.ExCallback.DynamicInvoke(raisedException)), null);
                            }
                            retries++;
                            Thread.Sleep(this.mngr.getRetryDelay());
                        }
                        else
                            break;
                    }
                    finally { 
                        Monitor.Exit(o); 
                    }
                } //while
                return res;            
            }


            private void cbListLock()
            {
                Monitor.Enter(this);
            }

            private void cbListUnLock()
            {
                Monitor.Exit(this);
            }
        }

        /**
         * 
         * @return a New String represents a ManagerID
         */
        public static String getNewManagerID()
        {
            Random random = new Random();
            return Convert.ToString(random.Next(100000)) +
                    "." +
                   Convert.ToString(random.Next(100000));
        }
    }
}
