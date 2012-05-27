import lisp.IValue;
import lisp.Int;
import lisp.LispRuntimeException;

public class _plus {
	public static IValue invoke(IValue a, IValue b){
		if(!(a instanceof Int) || !(b instanceof Int))
			throw new LispRuntimeException("Parameters of function '+' are expected to be integers");
	
		return new Int(((Int) a).getValue() + ((Int) b).getValue()); 
	}
}
