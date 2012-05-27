package lisp.specialoperators;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.List;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Car extends SpecialOperator {

	public Car() {
		super("car");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters(); 
		if(parameters.size() != 1)
			throw new SyntaxException("Special operator car expects 1 argument (got " + parameters.size() + " arguments");
		
		Generator.generateCheckIfList(parameters, symbolTable, 0, "First argument of car is expected to be a list");
		Generator.generateCastToList();
		
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "getHead", "()Llisp/LispForm;");
	}

}
