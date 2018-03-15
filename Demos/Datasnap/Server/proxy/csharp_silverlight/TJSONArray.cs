//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;
using System.Collections.Generic;
using Newtonsoft.Json.Linq;

namespace Embarcadero.Datasnap.WindowsPhone7
{

    /**
     * 
     * Implements a JSON array.
     * 
     */

    public class TJSONArray : TJSONValue
    {

        protected List<TJSONValue> Elements;

        /**
         * Parse the passed String into a new TJSONArray 
         * @param JSONString
         * @return TJSONArray
         */
        public static TJSONArray Parse(String JSONString)
        {
            return new TJSONArray(JArray.Parse(JSONString));
        }

        /**
         * Initialized the instance with a TJSONValueList
         * @param JSONValues
         */
        public TJSONArray(List<TJSONValue> JSONValues)
            : base()
        {
            Elements = JSONValues;
        }

        public override String ToString()
        {
            return asJSONArray().ToString(Newtonsoft.Json.Formatting.None);
        }

        /**
         * Initialized the instance
         */
        public TJSONArray()
            : base()
        {
            Elements = new List<TJSONValue>();
        }

        /**
         * Initialized the instance with a JSONArray; 
         * @param json
         */
        public TJSONArray(JArray json)
            : base()
        {
            Elements = buildElements(json);
        }

        protected List<TJSONValue> buildElements(JArray arr)
        {
            try
            {
                List<TJSONValue> res = new List<TJSONValue>();
                for (int i = 0; i < arr.Count; i++)
                {
                    switch (arr[i].Type)
                    {
                        case JTokenType.Null: { res.Add(new TJSONNull()); break; }
                        case JTokenType.String: { res.Add(new TJSONString(arr.Value<string>(i))); break; }
                        case JTokenType.Float: { res.Add(new TJSONNumber(arr.Value<float>(i))); break; }
                        case JTokenType.Integer: { res.Add(new TJSONNumber(arr.Value<long>(i))); break; }
                        case JTokenType.Array: { res.Add(new TJSONArray(arr.Value<JArray>(i))); break; }
                        case JTokenType.Object: { res.Add(new TJSONObject(arr.Value<JObject>(i))); break; }
                        case JTokenType.Boolean: { if (arr.Value<Boolean>(i)) res.Add(new TJSONTrue()); else res.Add(new TJSONFalse()); break; }
                    }
                }
                return res;
            }
            catch (Exception)
            {
                return null;
            }

        }

        /**
         *Converts  into JSONArray
        */
        public JArray asJSONArray()
        {
            JArray arr = new JArray();
            foreach (TJSONValue v in Elements)
                arr.Add(v.getInternalObject());
            return arr;
        }

        public override Object getInternalObject()
        {
            return asJSONArray();
        }

        /**
         * Adds a TJSonValue
         * @param value a TJSONValue
         * @returns a itlsef 
         */
        public TJSONArray add(TJSONValue value)
        {
            Elements.Add(value);
            return this;
        }

        public TJSONArray add(int value)
        {
            JArray app = (JArray)getInternalObject();
            app.Add(value);
            Elements = buildElements(app);
            return this;
        }

        public TJSONArray add(long value)
        {
            JArray app = (JArray)getInternalObject();
            app.Add(value);
            Elements = buildElements(app);
            return this;
        }

        public TJSONArray add(bool value)
        {
            JArray app = (JArray)getInternalObject();
            app.Add(value);
            Elements = buildElements(app);
            return this;
        }

        public TJSONArray add(double value)
        {
            JArray app = (JArray)getInternalObject();
            try
            {
                app.Add(value);
            }
            catch (Exception e)
            {
                throw new DBXException(e.Message);
            }
            Elements = buildElements(app);
            return this;
        }

        public TJSONArray add(String value)
        {
            JArray app = (JArray)getInternalObject();
            app.Add(value);
            Elements = buildElements(app);
            return this;
        }

        public TJSONArray add(Object value)
        {
            JArray app = (JArray)getInternalObject();
            app.Add(value);
            Elements = buildElements(app);
            return this;
        }

        /**
         *  Returns a string value by the index
         *  @param index  the index of the value
         *  @return the value as string  
         */
        public String getString(int index)
        {
            TJSONValue p;
            return ((p = get(index)) == null) ? null : ((TJSONString)p).getValue();
        }

        /**
         *  Returns a double value by the index
         *  @param index  the index of the value
         *  @return the value as double  
         */
        public Double? getDouble(int index)
        {
            TJSONValue p = get(index);
            if (p == null)
                return null;
            Double? d = ((TJSONNumber)p).getValue();
            if (d.HasValue)
                return d.Value;
            return null;
        }

        /**
         *  Returns a TJSONObject value by the index
         *  @param index  the index of the value
         *  @return the value as TSJONObject  
         */
        public TJSONObject getJSONObject(int index)
        {
            TJSONValue p;
            return ((p = get(index)) == null) ? null : (TJSONObject)p;
        }

        /**
         *  Returns an integer value by the index
         *  @param index  the index of the value
         *  @return the value as integer  
         */
        public long? getInt(int index)
        {
            TJSONValue p = get(index);
            if (p == null)
                return null;
            long? d = ((TJSONNumber)p).getValueInt();
            if (d.HasValue)
                return d.Value;
            return null;
        }

        /**
         *  Returns a boolean value by the index
         *  @param index  the index of the value
         *  @return the value as boolean  
         */
        public Boolean? getBoolean(int index)
        {
            TJSONValue p = get(index);
            if (p == null)
                return null;
            if (p is TJSONTrue)
                return true;
            else
                return false;
        }

        /**
         * Returns a {@link TJSONArray} value by the index 
         * @param index
         * @return TJSONArray
         */
        public TJSONArray getJSONArray(int index)
        {
            TJSONValue p;
            return ((p = get(index)) == null) ? null : (TJSONArray)p;
        }

        /**
         * Returns a {@link TJSONValue} value by the index 
         * @param index
         * @return  TJSONValue
         */
        public TJSONValue get(int index)
        {
            return Elements[index];
        }

        /**
        * Convert into a formatted json string.
        */
        public TJSONString getAsJsonString(int index)
        {
            return (TJSONString)get(index);
        }

        /**
         * Returns a TJSONObject value by the index 
         * @param index
         * @return  TJSONObject
         */
        public TJSONObject getAsJsonObject(int index)
        {
            return (TJSONObject)get(index);
        }

        /**
         * Returns a {@link TJSONArray} value by the index
         * @param index
         * @return TJSONArray
         */
        public TJSONArray getAsJsonArray(int index)
        {
            return (TJSONArray)get(index);
        }

        /**
         * Remove a element by the index
         * @param index  the index of the value
         * @returns a itlsef 
         */
        public TJSONArray remove(int index)
        {
            Elements.RemoveAt(index);
            return this;
        }

        /**
         * Return the size of the element      
         */
        public long size()
        {
            return Elements.Count;
        }

        public override JSONValueType getJsonValueType()
        {
            return JSONValueType.JSONArray;
        }
    }

}
