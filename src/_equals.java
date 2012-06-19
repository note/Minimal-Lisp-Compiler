import lisp.Int;
import lisp.LispForm;
import lisp.LispRuntimeException;
import lisp.List;
import lisp.Symbol;


public class _equals {
	public static LispForm invoke(LispForm a, LispForm b){
		if(!(a instanceof Int) || !(b instanceof Int))
			throw new LispRuntimeException("Parameters of function '>' are expected to be integers");
	
		if(((Int) a).getValue() == ((Int) b).getValue())
			return new Symbol("T");
		else
			return List.createEmptyList();
	}
}
