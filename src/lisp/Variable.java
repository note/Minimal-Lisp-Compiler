package lisp;

import org.objectweb.asm.Opcodes;
import java.lang.Math;


public class Variable extends Symbol{
	
	public Variable(String name){
		super(name);
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		int addr = symbolTable.getAddress(name);
		
		if(addr < 0) // if addr is negative it means it's argument of function we are currently in
			Factory.getMethodVisitor().visitVarInsn(Opcodes.ALOAD, Math.abs(addr) - 1);
		else{
			Factory.getMethodVisitor().visitLdcInsn(addr);
			Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/MemoryPool", "peek", "(I)Llisp/IValue;");
		}
	}

}
