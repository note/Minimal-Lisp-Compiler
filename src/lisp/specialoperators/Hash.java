package lisp.specialoperators;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.Symbol;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Hash extends SpecialOperator {
	public Hash(){
		super("hash");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException{
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitTypeInsn(Opcodes.NEW, "lisp/FunObj");
		mv.visitInsn(Opcodes.DUP);
		//stack: FunObj, FunObj
		
		java.util.List<LispForm> parameters = getParameters();
		if(parameters.size() < 1)
			throw new SyntaxException("Incorrect use of '#'");
		
		parameters.get(0).compile(symbolTable);
		//stack: FunObj, FunObj, LispForm
		
		Generator.generateCheckIfSymbol(symbolTable, "First arguments of hash is expected to be a symbol");
		Generator.generateCastToSymbol();
		//stack: FunObj, FunObj, Symbol
		
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/Symbol", "getName", "()Ljava/lang/String;");
		
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/FunObj", "<init>", "(Ljava/lang/String;)V");
		//stack: FunObj
	}
}
