package lisp.specialoperators;

import org.objectweb.asm.MethodVisitor;

import lisp.Factory;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Quote extends SpecialOperator{
	public Quote(){
		super("quote");
	}
	
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		if(parameters.size() != 1)
			throw new SyntaxException("Special operator quote expects 1 argument (got " + parameters.size() + " arguments");
		
		parameters.get(0).generateYourself(symbolTable);
	}
}
