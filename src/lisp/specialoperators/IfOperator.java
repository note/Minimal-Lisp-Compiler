package lisp.specialoperators;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class IfOperator extends SpecialOperator{
	public IfOperator() {
		super("if");
	}

	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
		if(parameters.size() != 2 && parameters.size() != 3)
			throw new SyntaxException("Special operator if expects 2 or 3 arguments (got " + parameters.size() + " arguments");
		
		parameters.get(0).compile(symbolTable);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,"lisp/RT/Runtime", "isNil","(Llisp/LispForm;)Z");
		Label notNil = new Label();
		mv.visitJumpInsn(Opcodes.IFEQ, notNil);
		
		Label end = new Label();
		if(parameters.size() == 3)
			parameters.get(2).compile(symbolTable);
		else
			Generator.generateEmptyList();
		mv.visitJumpInsn(Opcodes.GOTO, end);
		
		mv.visitLabel(notNil);
		parameters.get(1).compile(symbolTable);
		mv.visitLabel(end);
	}
}
