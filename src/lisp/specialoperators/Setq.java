package lisp.specialoperators;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.Symbol;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Setq extends SpecialOperator {
	public Setq() {
		super("setq");
	}

	public void compile(SymbolTable symbolTable) throws SyntaxException{
		java.util.List<LispForm> parameters = getParameters();
		if(parameters.size() < 2)
			throw new SyntaxException("Special operator setq expects at least two arguments (got " + parameters.size() + " arguments)");
		
		if(parameters.size() % 2 == 1)
			throw new SyntaxException("Special operator setq expects even number of arguments.");
		
		MethodVisitor mv = Factory.getMethodVisitor();
		for(int i=0; i<parameters.size(); i += 2){
			if(!(parameters.get(i) instanceof Symbol))
				throw new SyntaxException("Special operator setq expects a symbol as the first argument");
		
			int addr = symbolTable.getAddress(((Symbol) parameters.get(i)).getName());
		
			mv.visitLdcInsn(addr);
			parameters.get(i + 1).compile(symbolTable);
			if(i == parameters.size() - 2) // if it's the last assignment - then must call setAndReturn
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/MemoryPool", "setAndReturn", "(ILlisp/LispForm;)Llisp/LispForm;");
			else
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/MemoryPool", "set", "(ILlisp/LispForm;)V");
		}
	}
}
