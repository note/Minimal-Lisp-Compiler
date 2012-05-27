package lisp.RT;

import java.util.ArrayList;
import java.util.Stack;

import lisp.IValue;

public class MemoryPool {
	private static ArrayList<Stack<IValue>> variables = new ArrayList<Stack<IValue>>();
	
	static{
		for(int i=0; i<100; ++i) //todo initial number of variables is known in compile-time 
			variables.add(new Stack<IValue>()); 
	}
	
	public static void push(int addr, IValue value){
		variables.get(addr).push(value);
	}
	
	public static IValue pop(int addr){
		return variables.get(addr).pop();
	}
	
	public static IValue peek(int addr){
		return variables.get(addr).peek();
	}
}
