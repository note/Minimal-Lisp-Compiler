package lisp;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Function extends Symbol {
	/**
	 * The difference between name and functionName is that name is the string typed by user that may be an alias while functionName 
	 * is name of .class file that contains code for that function.
	 * For example: when user type "+" then name == "+" and functionName == "_plus"
	 * Function name is used to communicate with lisp.RT.Runtime
	 */
	private String functionName;
	
	public Function(String name) {
		super(name);
		functionName = SymbolTable.getFunctionName(name);
	}

	private void generateResolveFunction() {
		Factory.getMethodVisitor().visitLdcInsn(functionName);
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "getFunctionParametersLength","(Ljava/lang/String;)I");
	}

	private void generatePushParameters(SymbolTable st) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		for (int i = 0; i < parameters.size(); ++i)
			if (parameters.get(i) instanceof Int
					|| parameters.get(i) instanceof List
					|| parameters.get(i) instanceof Variable)
				parameters.get(i).compile(st);
			else
				throw new SyntaxException(
						"Cannot generate code for pushing function parameter");
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();

		generateResolveFunction();

		Label functionFound = new Label();
		mv.visitInsn(Opcodes.DUP);
		mv.visitJumpInsn(Opcodes.IFGE, functionFound);

		Generator.generateRuntimeException("Function " + name + " not found");

		mv.visitLabel(functionFound);
		
		//stack: numberOfParametersInt
		Label hasRest = new Label();
		mv.visitLdcInsn(functionName);
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "hasRest","(Ljava/lang/String;)Z");
		mv.visitJumpInsn(Opcodes.IFNE, hasRest);

		mv.visitLdcInsn(parameters.size());
		Label ok = new Label();
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, ok);

		Generator.generateRuntimeException("Invalid number of arguments: " + parameters.size());

		mv.visitLabel(ok);
		
		generatePushParameters(symbolTable);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, functionName, "invoke",generateMethodDescriptor(parameters.size()));
		Label end = new Label();
		mv.visitJumpInsn(Opcodes.GOTO, end);
		
		mv.visitLabel(hasRest);
		mv.visitInsn(Opcodes.POP);
		//todo: check arguments length 
		//mv.visitMethodInsn(Opcodes.INVOKESTATIC, functionName, "invoke",generateMethodDescriptor(parameters.size()));
		mv.visitLdcInsn(functionName);
		Generator.generatePushParameters(symbolTable, parameters);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "callWithRest", "(Ljava/lang/String;[Llisp/LispForm;)Llisp/LispForm;");
		
		mv.visitLabel(end);
	}
}
