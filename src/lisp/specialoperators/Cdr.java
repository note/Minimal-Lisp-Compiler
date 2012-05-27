package lisp.specialoperators;

import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.List;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Cdr extends SpecialOperator {

	public Cdr() {
		super("car");
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters(); 
		if(parameters.size() != 1)
			throw new SyntaxException("Special operator cdr expects 1 argument (got " + parameters.size() + " arguments");
		
		Generator.generateCheckIfList(parameters, symbolTable, 0, "First argument of cdr is expected to be a list");
		Generator.generateCastToList();
		
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "getTail", "()Llisp/List;");
		
	}

}
