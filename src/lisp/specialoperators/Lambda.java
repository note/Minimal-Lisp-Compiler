package lisp.specialoperators;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Function;
import lisp.Generator;
import lisp.LispForm;
import lisp.List;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Lambda extends SpecialOperator{
	String functionName;
	
	public Lambda(){
		super("lambda");
		functionName = "_lambda_" + Factory.getNextLambdaIndex();
	}
	
	protected java.util.List<LispForm> wrapBodyWithProgn() throws SyntaxException{
		return wrapWithPrognStartingFromIndex(1);
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters(); 
		if(parameters.size() < 1)
			throw new SyntaxException("Special operator lambda expects at least one argument (got " + parameters.size() + " arguments");
		
		Generator.createFunctionClass(functionName);
		parameters = wrapBodyWithProgn();
		SymbolTable newSymbolTable = new SymbolTable(symbolTable, createAddrMap((List) parameters.get(0), "lambda"));
		Factory.pushMethodVisitor(Factory.getClassWriter().visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "invoke", generateMethodDescriptor(((List) parameters.get(0)).size()), null, null), false);
		
		parameters.get(1).compile(newSymbolTable);
		Factory.getMethodVisitor().visitInsn(Opcodes.ARETURN);
		
		Factory.getMethodVisitor().visitMaxs(0, 0); // stacks sizes are computed automatically thanks to ClassWriter.COMPUTE_FRAMES option. But it is still neccessary to call this method with any arguments (they will be ignored).
		Factory.popMethodVisitor();
		Generator.saveFile(functionName, Factory.getClassWriter());
		Factory.increaseLambdaIndex();
		
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitTypeInsn(Opcodes.NEW, "lisp/Function");
		mv.visitInsn(Opcodes.DUP);
		mv.visitLdcInsn(functionName);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/Function", "<init>", "(Ljava/lang/String;)V");
	}

	@Override
	public Function getLambda(SymbolTable st) {
		return new Function(functionName);
	}
	
}
