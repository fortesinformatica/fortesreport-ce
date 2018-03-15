//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

using System;

namespace Embarcadero.Datasnap.WindowsPhone7
{
    public class TJSONNumber : TJSONValue {
	    protected double? valueDouble = null;
        protected long? valueInt = null;
        public bool? isDouble = null;

        public TJSONNumber() : base() {
            Clear();
        }

        	/**
    	 * Gets the internal value. 
    	 * @return Double?
    	 */
	    public Double? getValue() {
            if (isDouble.HasValue)
            {
                if (isDouble.Value)
                {
                    if (valueDouble != null)
                        return valueDouble;
                }
                else
                {
                    if (valueInt != null)
                        return valueInt;
                }
            }
            return null;
	    }

        /**
         *
         */                 
        public long? getValueInt()
        {

            if (valueInt != null)
               return valueInt;
            return null;
        }
       

        protected void Clear() {
            valueInt = null;
            valueDouble = null;
            isDouble = null;
        }

        /**
         *
         */
        public bool isInteger() {
            if (isDouble.HasValue)
                return !isDouble.Value;
            else 
                return false;        
        }
	
		/**
    	 * Class constructor, initialized the internal value by parse the String passed 
    	 * @param value
    	 */
	    public TJSONNumber(String value) : base()
        {
            Clear();
            long longres;
            if (long.TryParse(value, out longres))
                valueInt = longres;
            else
                this.valueDouble = DBXDefaultFormatter.getInstance().StringToDouble(value);
            isDouble = valueDouble != null;


            
	    }

     /**
	 * Class constructor, initialized the internal value with the double passed
	 * @param value
	 */
	    public TJSONNumber(double value) : base()
        {
            Clear();
            valueDouble = value;
            isDouble = true;
	    }

        /**
	 * Class constructor, initialized the internal value with the long passed
	 * @param value
	 */
	    public TJSONNumber(long value) : base()
        {
            Clear();
		    valueInt = value;
            isDouble = false;
	    }

        /**
	 * Class constructor, initialized the internal value with the int passed
	 * @param value
	 */
	    public TJSONNumber(int value) : base()
        {
            Clear();
            valueInt = value;
            isDouble = false;
        }

	    public override Object getInternalObject() {
            if (isDouble == true)
                return valueDouble;
            else
                return valueInt;
	    }

	    public override String ToString() {
            if (isDouble.HasValue)
            {
                if (isDouble.Value)
                {
                    if (valueDouble != null)
                        return DBXDefaultFormatter.getInstance().doubleToString(valueDouble.Value);
                }
                else
                {
                    if (valueInt != null)
                        return DBXDefaultFormatter.getInstance().Int64ToString(valueInt.Value);
                }
            }
            return NullString;
	    }

	    public override JSONValueType getJsonValueType() {
		    return JSONValueType.JSONNumber;
	    }
    }
}
