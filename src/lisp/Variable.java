package lisp;

import org.objectweb.asm.Opcodes;



public class Variable extends Symbol{
	
	public Variable(String name){
		super(name);
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		Factory.getMethodVisitor().visitLdcInsn(symbolTable.getAddress(name));
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/MemoryPool", "peek", "(I)I");
	}

}
