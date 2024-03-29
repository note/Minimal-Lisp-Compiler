package lisp.specialoperators;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.Int;
import lisp.LispForm;
import lisp.List;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;
import lisp.Variable;

public class Funcall extends SpecialOperator {
	public Funcall(String name){
		super(name);
	}
	
	public Funcall() {
		super("funcall");
	}
	
	protected void checkParameters(SymbolTable symbolTable, String operatorName) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
		
		parameters.get(0).compile(symbolTable);
		Generator.generateCheckIfSymbol(symbolTable, "First argument of " + operatorName + " is expected to be a symbol");
		Generator.generateCastToSymbol();
		//stack: Symbol
		
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/Symbol", "getName", "()Ljava/lang/String;");
		mv.visitInsn(Opcodes.DUP); //function name will be needed when calling Runtime.funcall()
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC,"lisp/RT/Runtime", "getFunctionParametersLength","(Ljava/lang/String;)I");
		
		Label functionFound = new Label();
		mv.visitInsn(Opcodes.DUP);
		//stack: String(functionName), NumberOfParameters, NumberOfParameters
		mv.visitJumpInsn(Opcodes.IFGE, functionFound);

		Generator.generateRuntimeException("Function " + name + " not found");

		mv.visitLabel(functionFound);
		//stack: String(functionName), NumberOfParameters

	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
		
		if(parameters.size() < 1)
			throw new SyntaxException("Special operator funcall expects at least 1 argument (got " + parameters.size() + " arguments");
		
		checkParameters(symbolTable, "funcall");
		
		mv.visitLdcInsn(parameters.size() - 1);
		Label end = new Label();
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, end);

		Generator.generateRuntimeException("Invalid number of arguments: " + parameters.size());

		mv.visitLabel(end);
		java.util.List<LispForm> actualArguments = new java.util.ArrayList<LispForm>(parameters);
		actualArguments.remove(0);
		Generator.generatePushParameters(symbolTable, actualArguments);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "funcall", "(Ljava/lang/String;[Llisp/LispForm;)Llisp/LispForm;");
	}
}
