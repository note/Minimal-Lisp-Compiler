package lisp;

import java.util.ArrayList;

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
		/*ClassWriter cw = new ClassWriter(0);
		
		cw.visit(Opcodes.V1_5, Opcodes.ACC_PUBLIC + Opcodes.ACC_ABSTRACT,
				"Main", null, "java/lang/Object",
				new String[] {});
		
		MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null);
		
		mv.visitCode();
		mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
		resolveFunction(mv);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V");
        mv.visitInsn(Opcodes.RETURN);
        mv.visitMaxs(2, 1);
		cw.visitEnd();
		
		saveFile(cw);*/
		MethodVisitor mv = Factory.getMethodVisitor();
		
		resolveFunction();
		//todo: very simple aproach so far. the value returned by resolveFunction should be checked (if return value is -1 then function cannot be found)
		
		for(int i=0; i<parameters.size(); ++i)
			if(parameters.get(i) instanceof Int)
				mv.visitLdcInsn(((Int) parameters.get(i)).getValue());
			else
				mv.visitLdcInsn(0);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, name, "invoke", getMethodDescriptor(parameters.size()));
	}
	
	public static void main(String [] args){
		Symbol function = new Symbol("A");
		//function.compile(new SymbolTable());
	}

}
