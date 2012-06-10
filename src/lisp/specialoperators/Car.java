package lisp.specialoperators;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

import org.objectweb.asm.Opcodes;

public class Car extends SpecialOperator {

	public Car() {
		super("car");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters(); 
		if(parameters.size() != 1)
			throw new SyntaxException("Special operator car expects 1 argument (got " + parameters.size() + " arguments");
		
		parameters.get(0).compile(symbolTable);
		Generator.generateCheckIfList(symbolTable, "First argument of car is expected to be a list");
		Generator.generateCastToList();
		
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "getHead", "()Llisp/LispForm;");
	}

}
