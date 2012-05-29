import lisp.Int;
import lisp.LispForm;
import lisp.LispRuntimeException;

public class _mul {
	public static LispForm invoke(LispForm a, LispForm b){
		if(!(a instanceof Int) || !(b instanceof Int))
			throw new LispRuntimeException("Parameters of function '*' are expected to be integers");
	
		return new Int(((Int) a).getValue() * ((Int) b).getValue()); 
	}
}
