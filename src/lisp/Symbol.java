package lisp;

import java.util.ArrayList;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Symbol implements ILispForm{
	protected java.util.List<ILispForm> parameters = new ArrayList<ILispForm>();
	protected String name;
	
	public Symbol(String name){
		this.name = name;
	}
	
	public void setParameters(java.util.List<ILispForm> parameters){
		this.parameters = parameters;
	}
	
	public void addParameter(ILispForm param){
		this.parameters.add(param);
	}
	
	public String getName(){
		return name;
	}
	
	public void resolveFunction(){
		Factory.getMethodVisitor().visitLdcInsn(name);
		Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "getFunctionParametersLength", "(Ljava/lang/String;)I");
	}
	
	protected String getMethodDescriptor(int paramsNum){
		String methodDesc = "(";
		for(int i=0; i<paramsNum; ++i)
			methodDesc += "I";
		methodDesc += ")I";
		return methodDesc;
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		MethodVisitor mv = Factory.getMethodVisitor();
		
		resolveFunction();
		
		Label functionFound = new Label();
		mv.visitInsn(Opcodes.DUP);
		mv.visitJumpInsn(Opcodes.IFGE, functionFound);
		
		//mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
		mv.visitTypeInsn(Opcodes.NEW, "lisp/LispRuntimeException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitLdcInsn("Function " + name + " not found");
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/LispRuntimeException", "<init>", "(Ljava/lang/String;)V");
		mv.visitInsn(Opcodes.ATHROW);
		
		mv.visitLabel(functionFound);
//		mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
		
		mv.visitLdcInsn(parameters.size());
		Label end = new Label();
		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, end);
		
//		mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
		mv.visitTypeInsn(Opcodes.NEW, "lisp/LispRuntimeException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitLdcInsn("Invalid number of arguments: " + parameters.size());
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/LispRuntimeException", "<init>", "(Ljava/lang/String;)V");
		mv.visitInsn(Opcodes.ATHROW);
		
		mv.visitLabel(end);
//		mv.visitFrame(Opcodes.F_SAME, 0, null, 0, null);
		
		for(int i=0; i<parameters.size(); ++i)
			if(parameters.get(i) instanceof Int)
				mv.visitLdcInsn(((Int) parameters.get(i)).getValue());
			else if(parameters.get(i) instanceof List)
				parameters.get(i).compile(symbolTable);
			else
				mv.visitLdcInsn(0);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, name, "invoke", getMethodDescriptor(parameters.size()));
		
		//todo: very simple aproach so far. the value returned by resolveFunction should be checked (if return value is -1 then function cannot be found)
	}
	
	public static void main(String [] args){
		Symbol function = new Symbol("A");
		//function.compile(new SymbolTable());
	}

}
