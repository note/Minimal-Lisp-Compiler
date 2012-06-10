package lisp.specialoperators;

import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Backquote extends SpecialOperator {
	public Backquote(){
		super("backquote");
	}
	
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		if(parameters.size() != 1)
			throw new SyntaxException("Special operator backquote expects 1 argument (got " + parameters.size() + " arguments");
		
		parameters.get(0).compileIfComma(symbolTable);
	}
}
