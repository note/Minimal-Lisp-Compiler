package lisp;

import java.util.HashMap;

import lisp.specialoperators.Defun;
import lisp.specialoperators.Progn;

public class SymbolTable {
	private static final HashMap<String, Class<? extends SpecialOperator>> specialOperatorsMap = new HashMap<String, Class<? extends SpecialOperator>>();
	
	static{
		specialOperatorsMap.put("defun", Defun.class);
		specialOperatorsMap.put("progn", Progn.class);
	}
	
	public boolean isSpecialOperator(String name){
		return specialOperatorsMap.containsKey(name);
	}
	
	public SpecialOperator getNewSpecialOperatorInstance(String name){
		Class<? extends SpecialOperator> clazz = specialOperatorsMap.get(name);
		if(clazz != null)
			try {
				return clazz.newInstance();
			} catch (InstantiationException e) {
				return null;
			} catch (IllegalAccessException e) {
				return null;
			}
		return null;
	}
}
