package lisp.RT;

import java.util.ArrayList;
import java.util.Stack;

public class MemoryPool {
	private static ArrayList<Stack<Integer>> variables = new ArrayList<Stack<Integer>>();
	
	static{
		for(int i=0; i<100; ++i) //todo we know initial number of variables in compile-time 
			variables.add(new Stack<Integer>()); 
	}
	
	public static void push(int addr, int value){
		variables.get(addr).push(value);
	}
	
	public static int pop(int addr){
		return variables.get(addr).pop();
	}
	
	public static int peek(int addr){
		return variables.get(addr).peek();
	}
}
