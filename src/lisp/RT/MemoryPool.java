package lisp.RT;

import java.util.ArrayList;
import java.util.Stack;

import lisp.LispForm;

public class MemoryPool {
	private static ArrayList<Stack<LispForm>> variables = new ArrayList<Stack<LispForm>>();
	
	static{
		for(int i=0; i<100; ++i) //todo initial number of variables is known in compile-time 
			variables.add(new Stack<LispForm>()); 
	}
	
	public static void push(int addr, LispForm value){
		variables.get(addr).push(value);
	}
	
	public static LispForm pop(int addr){
		return variables.get(addr).pop();
	}
	
	public static void remove(int addr){
		variables.get(addr).pop();
	}
	
	/**
	 * It might seem to be awkward that set function returns anything but it is for simplicity (setq will use that function and must return value of the last argument)
	 */
	public static LispForm setAndReturn(int addr, LispForm newValue){
		variables.get(addr).pop();
		variables.get(addr).push(newValue);
		return newValue;
	}
	
	public static void set(int addr, LispForm newValue){
		variables.get(addr).pop();
		variables.get(addr).push(newValue);
	}
	
	public static LispForm peek(int addr){
		return variables.get(addr).peek();
	}
}
