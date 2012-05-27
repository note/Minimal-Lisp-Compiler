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
	
	private void generateRuntimeException(String message){
		MethodVisitor mv = Factory.getMethodVisitor();
		mv.visitTypeInsn(Opcodes.NEW, "lisp/LispRuntimeException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitLdcInsn(message);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/LispRuntimeException", "<init>", "(Ljava/lang/String;)V");
		mv.visitInsn(Opcodes.ATHROW);
	}
	
	private void generatePushParameters(SymbolTable st) throws SyntaxException{
		MethodVisitor mv = Factory.getMethodVisitor();
		java.util.List<LispForm> parameters = getParameters();
		for(int i=0; i<parameters.size(); ++i)
			if(parameters.get(i) instanceof Int)
				mv.visitLdcInsn(((Int) parameters.get(i)).getValue());
			else if(parameters.get(i) instanceof List || parameters.get(i) instanceof Variable)
				parameters.get(i).compile(st);
			else
				mv.visitLdcInsn(0);
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
		
		generateResolveFunction();
		
		Label functionFound = new Label();
		mv.visitInsn(Opcodes.DUP);
		mv.visitJumpInsn(Opcodes.IFGE, functionFound);
		
		generateRuntimeException("Function " + name + " not found");
		
		mv.visitLabel(functionFound);
		
		mv.visitLdcInsn(parameters.size());
		Label end = new Label();
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, end);
		
		generateRuntimeException("Invalid number of arguments: " + parameters.size());
		
		mv.visitLabel(end);
		generatePushParameters(symbolTable);		
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, lisp.RT.Runtime.getFunctionName(name), "invoke", getMethodDescriptor(parameters.size()));
	}
}
