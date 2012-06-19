package lisp.specialoperators;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.SpecialOperator;
import lisp.SymbolTable;

public class Gensym extends SpecialOperator {
	private static int nextIndex = 0;
	
	public Gensym(){
		super("gensym");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) {
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitTypeInsn(Opcodes.NEW, "lisp/Symbol");
		mv.visitInsn(Opcodes.DUP);
		mv.visitLdcInsn("__##GENSYM_" + nextIndex);
		++nextIndex;
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/Symbol", "<init>", "(Ljava/lang/String;)V");
	}
}
