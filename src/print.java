import lisp.IValue;
import lisp.Int;
import lisp.LispRuntimeException;
import lisp.List;

public class print {
	public static IValue invoke(IValue num){
		if(!(num instanceof Int))
			throw new LispRuntimeException("Argument of function 'print' is expected to be an integer");
		
		System.out.println(((Int) num).getValue());
		return List.createEmptyList();
	}
}
