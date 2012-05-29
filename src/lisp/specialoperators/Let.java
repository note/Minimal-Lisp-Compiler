package lisp.specialoperators;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.LispForm;
import lisp.Int;
import lisp.List;
import lisp.SpecialOperator;
import lisp.Symbol;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class Let extends SpecialOperator{
	private static int nextAddr = 0;
	
	public Let() {
		super("let");
	}
	
	private HashMap<String, LispForm> createInitMap(List list) throws SyntaxException{
		HashMap<String, LispForm> res = new HashMap<String, LispForm>();
		for(LispForm it : list.getChildren()){
			if(!(it instanceof List) || ((List) it).getChildren().size() != 2 || !(((List) it).getChildren().get(0) instanceof Symbol))
				throw new SyntaxException("Malformed initialization list");
			
			Symbol varName = (Symbol) ((List) it).getChildren().get(0);
			res.put(varName.getName(),  ((List) it).getChildren().get(1));
		}
		return res;
	}
	
	private HashMap<String, Integer> createAddrMap(List list) throws SyntaxException{
		HashMap<String, Integer> res = new HashMap<String, Integer>();
		for(LispForm it : list.getChildren()){
			if(!(it instanceof List) || ((List) it).getChildren().size() != 2 || !(((List) it).getChildren().get(0) instanceof Symbol))
				throw new SyntaxException("Malformed initialization list");
			
			Symbol varName = (Symbol) ((List) it).getChildren().get(0);
			res.put(varName.getName(), nextAddr);
			++nextAddr;
		}
		return res;
	}
	
	private java.util.List<LispForm> wrapBodyWithProgn() throws SyntaxException{
		java.util.List<LispForm> parameters = getParameters(); 
		java.util.List<LispForm> res = new ArrayList<LispForm>();
		res.add(parameters.get(0));
		res.add(parameters.get(1));
		
		List progn = List.createForm("progn");
		for(int i=1; i<parameters.size(); ++i) //i=1, we omit the first parameter
			progn.addChild(parameters.get(i));
		res.add(progn);
		return res;
	}
	
	public void compile(SymbolTable symbolTable) throws SyntaxException{
		java.util.List<LispForm> parameters = getParameters(); 
		if(parameters.size() < 1)
			throw new SyntaxException("Special operator let expects at least one argument (got " + parameters.size() + " arguments");
		
		if(!(parameters.get(0) instanceof List))
			throw new SyntaxException("First element of let is expected to be a list");
		
		SymbolTable newSymbolTable = new SymbolTable(symbolTable, createAddrMap((List) parameters.get(0)));
		
		HashMap<String, LispForm> initMap = createInitMap((List) parameters.get(0));
		Iterator it = initMap.entrySet().iterator();
	    while (it.hasNext()) {
	        Map.Entry<String, LispForm> pairs = (Map.Entry<String, LispForm>)it.next();
	        
	        Factory.getMethodVisitor().visitLdcInsn(newSymbolTable.getAddress(pairs.getKey()));
	        pairs.getValue().compile(symbolTable); //initialization list is compiled with the old symbol table
			Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/MemoryPool", "push", "(ILlisp/LispForm;)V");
	    }
			
		parameters = wrapBodyWithProgn();
		parameters.get(2).compile(newSymbolTable);
		
		it = initMap.entrySet().iterator();
		while (it.hasNext()) {
	        Map.Entry<String, Integer> pairs = (Map.Entry<String, Integer>)it.next();
	        
	        Factory.getMethodVisitor().visitLdcInsn(newSymbolTable.getAddress(pairs.getKey()));
			Factory.getMethodVisitor().visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/MemoryPool", "remove", "(I)V");
	    }
	}
}
