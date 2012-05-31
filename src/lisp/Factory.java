package lisp;

import java.util.Vector;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Factory {
	private static ClassWriter classWriterInstance;
	private static Vector<MethodVisitor> methodVisitors = new Vector<MethodVisitor>();
	private static Vector<Boolean> isLambda = new Vector<Boolean>();
	
	public static ClassWriter getClassWriter(){
		return classWriterInstance;
	}
	
	public static void setClassWriter(ClassWriter cw){
		classWriterInstance = cw;
	}
	
	public static MethodVisitor getMethodVisitor(){
		return methodVisitors.get(methodVisitors.size() - 1);
	}
	
	public static MethodVisitor getNonLambdaMethodVisitor() throws SyntaxException{
		for(int i=Factory.isLambda.size()-1; i>=0; --i)
			if(!Factory.isLambda.get(i))
				return Factory.methodVisitors.get(i);
		
		throw new SyntaxException("Missing top level function");
	}
	
	public static void pushMethodVisitor(MethodVisitor mv, boolean isLambda){
		Factory.isLambda.add(isLambda);
		methodVisitors.add(mv);
	}
	
	public static void popMethodVisitor(){
		Factory.isLambda.remove(Factory.isLambda.size() - 1);
		methodVisitors.remove(methodVisitors.size() - 1);
	}
}
