package lisp.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.List;

import lisp.LispForm;
import lisp.Parser;
import lisp.SyntaxException;
import lisp.Token;
import lisp.Tokenizer;

import org.junit.Test;

public class ParserTest {
	
	private Tokenizer correctInput(){
		Tokenizer mockedTokenizer = mock(Tokenizer.class);
		
		when(mockedTokenizer.nextToken())
		.thenReturn(new Token(Token.OPENING_PARENTHESIS, "("))
		.thenReturn(new Token(Token.SYMBOL, "+"))
		.thenReturn(new Token(Token.INT, "3"))
		.thenReturn(new Token(Token.INT, "5"))
		.thenReturn(new Token(Token.CLOSING_PARENTHESIS, ")"))
		.thenReturn(new Token(Token.EOF, ""));
		
		return mockedTokenizer;
	}
	
	private Tokenizer openedInput(){
		Tokenizer mockedTokenizer = mock(Tokenizer.class);
		
		when(mockedTokenizer.nextToken())
		.thenReturn(new Token(Token.OPENING_PARENTHESIS, "("))
		.thenReturn(new Token(Token.SYMBOL, "+"))
		.thenReturn(new Token(Token.INT, "3"))
		.thenReturn(new Token(Token.INT, "5"))
		.thenReturn(new Token(Token.EOF, ""));
		
		return mockedTokenizer;
	}
	
	private Tokenizer unmatchedInput(){
		Tokenizer mockedTokenizer = mock(Tokenizer.class);
		
		when(mockedTokenizer.nextToken())
		.thenReturn(new Token(Token.OPENING_PARENTHESIS, "("))
		.thenReturn(new Token(Token.SYMBOL, "+"))
		.thenReturn(new Token(Token.INT, "3"))
		.thenReturn(new Token(Token.INT, "5"))
		.thenReturn(new Token(Token.CLOSING_PARENTHESIS, ")"))
		.thenReturn(new Token(Token.CLOSING_PARENTHESIS, ")"))
		.thenReturn(new Token(Token.EOF, ""));
		
		return mockedTokenizer;
	}
	
	private Tokenizer notSymbolInput(){
		Tokenizer mockedTokenizer = mock(Tokenizer.class);
		
		when(mockedTokenizer.nextToken())
		.thenReturn(new Token(Token.OPENING_PARENTHESIS, "("))
		.thenReturn(new Token(Token.INT, "3"))
		.thenReturn(new Token(Token.INT, "5"))
		.thenReturn(new Token(Token.CLOSING_PARENTHESIS, ")"))
		.thenReturn(new Token(Token.EOF, ""));
		
		return mockedTokenizer;
	}

	@Test
	public void testRead() {
		Parser parser = new Parser();
		try {
			assertEquals(1, parser.parse(correctInput()).size());
		} catch (SyntaxException e) {
			fail("Unexpected Syntax Exception");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			List<LispForm> tree = parser.parse(correctInput());
			System.out.println("cos");
		} catch (SyntaxException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			parser.parse(openedInput());
			fail("Should throw Syntax Exception");
		} catch (SyntaxException e) {} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			parser.parse(unmatchedInput());
			fail("Should throw Syntax Exception");
		} catch (SyntaxException e) {} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
