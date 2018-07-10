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

    /**
     * 
     *  Represents and incorporates all relevant information to form a parameter you 
     *  can send or receive in a request.
     */
    public class DSRESTParameter
    {
        public String Name;
        public int Direction;

       /**
    	 * Return the {@link DBXType} of this parameter
    	 * 
    	 * @return the DBXType value contained in this object.
    	 */
	 
        public int getDBXType()
        {
            return getValue().getDBXType();
        }

        public String TypeName;
        private DBXWritableValue value;

        /**
      	 * Returns the {@link DBXValue} of this parameter
      	 * 
      	 * @return the DBXValue value contained in this object.
      	 */

        public DBXWritableValue getValue()
        {
            return value;
        }

        /**
    	 * The constructor class, builds a new DSRESTParameter assigning the values ​​passed as input.
    	 * 
    	 * @param Name the name of the parameter.
    	 * @param Direction a value of type integer that represents the parameter direction, respecting the conventions of the {@link DSRESTParamDirection}.  
    	 * @param DBXType an integer value that represents the DBXType of this parameter , respecting the conventions of the {@link DBXValueType}.
    	 * @param TypeName a string that contains the TypeName of this parameter.
    	 */

        public DSRESTParameter(String Name, int Direction, int DBXType,
                String TypeName)
        {
            this.Name = Name;
            this.Direction = Direction;
            this.TypeName = TypeName;
            this.value = new DBXWritableValue();
            this.value.setDBXType(DBXType);
        }
    }
}
