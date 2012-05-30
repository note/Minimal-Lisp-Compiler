package lisp;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;



public class Int extends LispForm{
	int value;
	
	public Int(String value){
		this.value = Integer.parseInt(value);
	}
	
	public Int(int value){
		this.value = value;
	}
	
	public int getValue(){
		return value;
	}

	@Override
	public void compile(SymbolTable symbolTable) {
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitTypeInsn(Opcodes.NEW, "lisp/Int");
		mv.visitInsn(Opcodes.DUP);
		mv.visitLdcInsn(value);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/Int", "<init>", "(I)V");
	}
	
	public String toString(){
		return Integer.toString(value);
	}

	@Override
	public void generateYourself(SymbolTable symbolTable)
			throws SyntaxException {
		compile(symbolTable);
	}
	
}
