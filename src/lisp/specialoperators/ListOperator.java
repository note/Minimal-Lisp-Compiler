package lisp.specialoperators;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class ListOperator extends SpecialOperator {

	public ListOperator() {
		super("list");
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters(); 
		MethodVisitor mv = Factory.getMethodVisitor();
		
		mv.visitTypeInsn(Opcodes.NEW, "lisp/List");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/List", "<init>", "()V");
		for(LispForm param : parameters){
			mv.visitInsn(Opcodes.DUP);
			param.compile(symbolTable);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "addChild", "(Llisp/LispForm;)V");
		}
	}
}
