package lisp;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Function extends Symbol{
	
	public Function(String name){
		super(name);
	}
	
	private void generateResolveFunction(){
		Factory.getMethodVisitor().visitLdcInsn(name);
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "getFunctionParametersLength", "(Ljava/lang/String;)I");
	}
	
	private void generatePushParameters(SymbolTable st) throws SyntaxException{
		java.util.List<LispForm> parameters = getParameters();
		for(int i=0; i<parameters.size(); ++i)
			if(parameters.get(i) instanceof Int || parameters.get(i) instanceof List || parameters.get(i) instanceof Variable)
				parameters.get(i).compile(st);
			else
				throw new SyntaxException("Cannot generate code for pushing function parameter");
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
		
		mv.visitLdcInsn(parameters.size());
		Label end = new Label();
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, end);
		
		Generator.generateRuntimeException("Invalid number of arguments: " + parameters.size());
		
		mv.visitLabel(end);
		generatePushParameters(symbolTable);		
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, lisp.RT.Runtime.getFunctionName(name), "invoke", generateMethodDescriptor(parameters.size()));
	}
}
