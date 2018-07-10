//*******************************************************
//
//               Delphi DataSnap Framework
//
// Copyright(c) 1995-2011 Embarcadero Technologies, Inc.
//
//*******************************************************

namespace Embarcadero.Datasnap.WindowsPhone7
{

  /**
   * Wraps a DBXValue and allows it to represent itself as a JSON
   */
    public class DBXParameter : DBXValueType {
	    private DBXWritableValue Value;
	    public DBXParameter() : base(){
		    Value = new DBXWritableValue();
		    Value.Clear();		
	    }
	    public DBXWritableValue getValue() {
		    return Value;
	    }

     /**
	 * It set the data Type according to {@link DBXDataTypes} values. 
	 */
        public override void setDataType(int dataType)
        {
		    Value.setDBXType(dataType);
	    }
	
	
	/**
	 * JSON representation of a TParam
	 * 
	 * @return a JArray that represents a TParam 
	 */
	    public TJSONArray tojson() {
            TJSONArray arr = new TJSONArray();
		    arr.add(new TJSONString(getName()));
		    arr.add(new TJSONNumber(getDataType()));
		    arr.add(new TJSONNumber(getOrdinal()));
		    arr.add(new TJSONNumber(getSubType()));
		    arr.add(new TJSONNumber(getScale()));
		    arr.add(new TJSONNumber(getSize()));
		    arr.add(new TJSONNumber(getPrecision()));
		    arr.add(new TJSONNumber(getChildPosition()));
            if (getNullable()) arr.add(new TJSONTrue());
            else arr.add(new TJSONFalse());
            if (getHidden()) arr.add(new TJSONTrue());
            else arr.add(new TJSONFalse());
		    arr.add(new TJSONNumber(getParameterDirection()));
            if (getValueParameter()) arr.add(new TJSONTrue());
            else arr.add(new TJSONFalse());
            if (getLiteral()) arr.add(new TJSONTrue());
            else arr.add(new TJSONFalse());
		    return arr;
	    }

     /**
	 *  Returns the specified DBXType value of the wrapped value
	 */
        public override int getDataType()
        {
		    return getValue().getDBXType();
	    }
    }
}
