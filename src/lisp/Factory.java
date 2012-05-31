package lisp;

import java.util.Stack;
import java.util.Vector;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class Factory {
	private static int nextLambdaIndex = 0;
	private static Stack<ClassWriter> classWriters = new Stack<ClassWriter>();
	private static Stack<MethodVisitor> methodVisitors = new Stack<MethodVisitor>();
	
	public static int getNextLambdaIndex(){
		return nextLambdaIndex;
	}
	
	public static void increaseLambdaIndex(){
		++nextLambdaIndex;
	}
	
	public static ClassWriter getClassWriter(){
		return classWriters.peek();
	}
	
	public static void pushClassWriter(ClassWriter cw){
		classWriters.push(cw);
	}
	
	public static void popClassWriter(){
		classWriters.pop();
	}
	
	public static MethodVisitor getMethodVisitor(){
		return methodVisitors.peek();
	}
	
	public static void pushMethodVisitor(MethodVisitor mv, boolean isLambda){
		methodVisitors.push(mv);
	}
	
	public static void popMethodVisitor(){
		methodVisitors.pop();
	}
}
