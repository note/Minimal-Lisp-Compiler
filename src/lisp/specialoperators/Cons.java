package lisp.specialoperators;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Cons extends SpecialOperator{
	public Cons() {
		super("cons");
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
		if(parameters.size() != 2)
			throw new SyntaxException("Special operator cons expects 2 arguments (got " + parameters.size() + " arguments");
		
		Generator.generateCheckIfList(parameters.get(1), symbolTable, "Second argument of cons is expected to be a list");
		Generator.generateCastToList();
		
		mv.visitInsn(Opcodes.DUP);
		parameters.get(0).compile(symbolTable);
		
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "addAtFront", "(Llisp/LispForm;)V");
	}

	
}
