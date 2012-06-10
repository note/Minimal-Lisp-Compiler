package lisp.specialoperators;

import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.Symbol;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Comma extends SpecialOperator {
	public Comma(){
		super("comma");
	}
	
	public void compileIfComma(SymbolTable symbolTable) throws SyntaxException{
		java.util.List<LispForm> parameters = getParameters();
		if(parameters.size() != 1)
			throw new SyntaxException("Special operator , expects 1 argument (got " + parameters.size() + " arguments");
		
		parameters.get(0).compile(symbolTable);
	}
}
