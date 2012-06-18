package lisp;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import java.lang.Math;


public class Variable extends Symbol{
	
	public Variable(String name){
		super(name);
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		MethodVisitor mv = Factory.getMethodVisitor();
		
		if(name.equals("T")){
			mv.visitTypeInsn(Opcodes.NEW, "lisp/Symbol");
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(name);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/Symbol", "<init>", "(Ljava/lang/String;)V");
		}else if(name.equals("NIL")){
			Generator.generateEmptyList();
		}else{
			int addr = symbolTable.getVariableAddress(name);
			if(addr < 0) // if addr is negative it means it's argument of function we are currently in
				mv.visitVarInsn(Opcodes.ALOAD, Math.abs(addr) - 1);
			else{
				mv.visitLdcInsn(addr);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/MemoryPool", "peek", "(I)Llisp/LispForm;");
			}
		}
	}

}
