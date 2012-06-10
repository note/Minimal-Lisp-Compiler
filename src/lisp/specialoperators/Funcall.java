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
	public Funcall() {
		super("funcall");
	}
	
	private void generatePushParameters(SymbolTable st) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitLdcInsn(parameters.size() - 1);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, "lisp/LispForm");
		
		for (int i = 1; i < parameters.size(); ++i)
			if (parameters.get(i) instanceof Int
					|| parameters.get(i) instanceof List
					|| parameters.get(i) instanceof Variable){
				
				mv.visitInsn(Opcodes.DUP); //arrayref
				mv.visitLdcInsn(i-1); //index
				parameters.get(i).compile(st); //value
				
				mv.visitInsn(Opcodes.AASTORE);
			}else
				throw new SyntaxException(
						"Cannot generate code for pushing function parameter");
		
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
		if(parameters.size() != 2)
			throw new SyntaxException("Special operator funcall expects at least 1 argument (got " + parameters.size() + " arguments");
		
		parameters.get(0).compile(symbolTable);
		Generator.generateCheckIfSymbol(symbolTable, "First arguments of funcall is expected to be a symbol");
		Generator.generateCastToSymbol();
		
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/Symbol", "getName", "()Ljava/lang/String;");
		mv.visitInsn(Opcodes.DUP); //function name will be needed when calling Runtime.funcall()
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC,"lisp/RT/Runtime", "getFunctionParametersLength","(Ljava/lang/String;)I");
		
		Label functionFound = new Label();
		mv.visitInsn(Opcodes.DUP);
		mv.visitJumpInsn(Opcodes.IFGE, functionFound);

		Generator.generateRuntimeException("Function " + name + " not found");

		mv.visitLabel(functionFound);

		mv.visitLdcInsn(parameters.size() - 1);
		Label end = new Label();
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, end);

		Generator.generateRuntimeException("Invalid number of arguments: " + parameters.size());

		mv.visitLabel(end);
		generatePushParameters(symbolTable);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "funcall", "(Ljava/lang/String;[Ljava/lang/Object;)Llisp/LispForm;");
	}
}
