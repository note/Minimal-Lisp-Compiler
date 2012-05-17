package lisp.tests;

import static org.junit.Assert.*;
import lisp.Token;
import lisp.Tokenizer;

import org.junit.Test;

public class TokenizerTest {
	Tokenizer tokenizer;
	
	public TokenizerTest(){
		tokenizer = new Tokenizer();
	}

	public void doTest(String input, int expected){
		tokenizer.loadInput(input);
		assertEquals(expected, tokenizer.nextToken().getCode());
	}
	
	public void doTest(String input, int expected, String value){
		tokenizer.loadInput(input);
		Token token = tokenizer.nextToken();
		assertEquals(expected, token.getCode());
		assertEquals(value, token.getValue());
	}
	
	@Test
	public void testNextToken() {
		doTest("  (cos", Token.OPENING_PARENTHESIS);
		
		doTest(") 4", Token.CLOSING_PARENTHESIS);
		
		doTest("44", Token.INT, "44");
		doTest(" 	12", Token.INT);
		doTest(" 	44 here", Token.INT, "44");
		doTest("+43499", Token.INT);
		doTest(" -7 ", Token.INT);
		doTest("324)", Token.INT, "324");
		
		doTest("abc ( )", Token.SYMBOL, "abc");
		doTest("read-in)", Token.SYMBOL, "read-in");
		doTest("33e", Token.SYMBOL, "33e");
		
		doTest("\"some\"", Token.STRING, "some");
		doTest("\"hello world\"", Token.STRING);
		
		doTest("34d0", Token.DOUBLE);
		doTest("34.43d0", Token.DOUBLE);
		doTest("0.d55", Token.DOUBLE);
		doTest("3.3", Token.FLOAT);
	}

}
