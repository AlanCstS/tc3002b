from Lexer import *

class Parser:
	lexer = None
	token = None

	def __init__(self, filepath):
		self.lexer = Lexer(filepath)
		self.token = None

		self.firstPrimaryExpression = set((Tag.ID, Tag.NUMBER, Tag.TRUE, Tag.FALSE, ord('(')))

		self.firstUnaryExpression = self.firstPrimaryExpression.union( set((ord('-'), ord('!'))) )

		self.firstExtendedMultiplicativeExpression = set((ord('*'), ord('/'), Tag.MOD))

		self.firstMultiplicativeExpression = self.firstUnaryExpression

		self.firstExtendedAdditiveExpression = set((ord('+'), ord('-')))

		## ADD THE OTHER FIRST SETS WE WILL BE USING ##

		self.firstAdditiveExpression = self.firstMultiplicativeExpression

		self.firstExtendedRelationalExpression = set((ord('<'), ord('>'), Tag.LEQ, Tag.GEQ))

		self.firstRelationalExpression = self.firstAdditiveExpression

		self.firstExtendedEqualityExpression = set((ord('='), Tag.NEQ))

		self.firstEqualityExpression = self.firstRelationalExpression

		self.firstExtendedConditionalTerm = {Tag.AND}

		self.firstConditionalTerm = self.firstEqualityExpression

		self.firstExtendedConditionalExpression = {Tag.OR}
		
		self.firstConditionalExpression = self.firstConditionalTerm

		self.firstMovementStatement = set((Tag.FORWARD, Tag.BACKWARD, Tag.LEFT, Tag.RIGHT, Tag.SETX, Tag.SETY, Tag.SETXY,  Tag.HOME))

		self.firstDrawingStatement = set((Tag.CLEAR, Tag.CIRCLE, Tag.ARC, Tag.PENUP, Tag.PENDOWN, Tag.COLOR, Tag.PENWIDTH))

		self.firstSimpleStatement = set((Tag.VAR, Tag.ID, Tag.PRINT)).union(self.firstMovementStatement).union(self.firstDrawingStatement)

		self.firstStructuredStatement = set((Tag.WHILE, Tag.IF, Tag.IFELSE))

		self.firstConditionalStatement = set((Tag.IF, Tag.IFELSE))

		self.firstStatement = self.firstSimpleStatement.union(self.firstStructuredStatement)

		self.firstStatementSequence = self.firstStatement

		self.firstProgram = self.firstStatementSequence.union({Tag.EOF})
		
		self.firstExpression = self.firstConditionalExpression

		self.firstElement = set((Tag.STRING, Tag.ID, ord('(')))

	def error(self, extra = None):
		text = 'Line ' + str(self.lexer.line) + " - " 
		if extra == None:
			text = text + "."
		else:
			text = text + extra
		raise Exception(text)

	def check(self, tag):
		if self.token.tag == tag:
			self.token = self.lexer.scan()
		else:
			text = 'Line ' + str(self.lexer.line) + " - expected "
			if tag != Tag.ID:
				text = text + str(Token(tag)) + " before " + str(self.token) 
			else:
				text = text + "an identifier before " + str(self.token) 
			self.error(text)
	
	def analize(self):
		self.token = self.lexer.scan()
		self.program()

	def primaryExpression(self):
		if self.token.tag in self.firstPrimaryExpression:
			if self.token.tag == Tag.ID:
				self.check(Tag.ID)
			elif self.token.tag == Tag.NUMBER:
				self.check(Tag.NUMBER)
			elif self.token.tag == Tag.TRUE:
				self.check(Tag.TRUE)
			elif self.token.tag == Tag.FALSE:
				self.check(Tag.FALSE)
			elif self.token.tag == ord('('):
				self.check(ord('('))
				self.expression()
				self.check(ord(')'))
		else:
			self.error("expected a primary expression before " + str(self.token))
		
	def unaryExpression(self):
		if self.token.tag in self.firstUnaryExpression:
			if self.token.tag == ord('-'):
				self.check(ord('-'))
				self.unaryExpression()
			elif self.token.tag == ord('!'):
				self.check(ord('!'))
				self.unaryExpression()
			else:
				self.primaryExpression()
		else: 
			self.error("expected an unary expression before " + str(self.token))
		
	def extendedMultiplicativeExpression(self):
		if self.token.tag in self.firstExtendedMultiplicativeExpression:
			if self.token.tag == ord('*'):
				self.check(ord('*'))
				self.unaryExpression()
				self.extendedMultiplicativeExpression()
			elif self.token.tag == ord('/'):
				self.check(ord('/'))
				self.unaryExpression()
				self.extendedMultiplicativeExpression()
			elif self.token.tag == Tag.MOD:
				self.check(Tag.MOD)
				self.unaryExpression()
				self.extendedMultiplicativeExpression()
		else:
			pass

	def multiplicativeExpression(self):
		if self.token.tag in self.firstMultiplicativeExpression:
			self.unaryExpression()
			self.extendedMultiplicativeExpression()
		else:
			self.error("expected an multiplicative expression before " + str(self.token))
	
	def extendedAdditiveExpression(self):
		if self.token.tag in self.firstExtendedAdditiveExpression:
			if self.token.tag == ord('+'):
				self.check(ord('+'))
				self.multiplicativeExpression()
				self.extendedAdditiveExpression()
			elif self.token.tag == ord('-'):
				self.check(ord('-'))
				self.multiplicativeExpression()
				self.extendedAdditiveExpression()
		else:
			pass
	
	def additiveExpression(self):
		if self.token.tag in self.firstAdditiveExpression:
			self.multiplicativeExpression()
			self.extendedAdditiveExpression()
		else:
			self.error("expected an additive expression before " + str(self.token))
	
	def extendedRelationalExpression(self):
		if self.token.tag in self.firstExtendedRelationalExpression:
			if self.token.tag == ord('<'):
				self.check(ord('<'))
				self.additiveExpression()
				self.extendedRelationalExpression()
			elif self.token.tag == ord('>'):
				self.check(ord('>'))
				self.additiveExpression()
				self.extendedRelationalExpression()
			elif self.token.tag == Tag.LEQ:
				self.check(Tag.LEQ)
				self.additiveExpression()
				self.extendedRelationalExpression()
			elif self.token.tag == Tag.GEQ:
				self.check(Tag.GEQ)
				self.additiveExpression()
				self.extendedRelationalExpression()
		else:
			pass
	
	def relationalExpression(self):
		if self.token.tag in self.firstRelationalExpression:
			self.additiveExpression()
			self.extendedRelationalExpression()
		else:
			self.error("expected an relational expression before " + str(self.token))
	
	def extendedEqualityExpression(self):
		if self.token.tag in self.firstExtendedEqualityExpression:
			if self.token.tag == ord('='):
				self.check(ord('='))
				self.relationalExpression()
				self.extendedEqualityExpression()
			elif self.token.tag == Tag.NEQ:
				self.check(Tag.NEQ)
				self.relationalExpression()
				self.extendedEqualityExpression()
		else:
			pass
	
	def equalityExpression(self):
		if self.token.tag in self.firstEqualityExpression:
			self.relationalExpression()
			self.extendedEqualityExpression()
		else:
			self.error("expected an equality expression before " + str(self.token))
	
	def extendedConditionalTerm(self):
		if self.token.tag in self.firstExtendedConditionalTerm:
			if self.token.tag == Tag.AND:
				self.check(Tag.AND)
				self.equalityExpression()
				self.extendedConditionalTerm()
		else:
			pass
	
	def conditionalTerm(self):
		if self.token.tag in self.firstConditionalTerm:
			self.equalityExpression()
			self.extendedConditionalTerm()
		else:
			self.error("expected an conditional term before " + str(self.token))
	
	def extendedConditionalExpression(self):
		if self.token.tag in self.firstExtendedConditionalExpression:
			if self.token.tag == Tag.OR:
				self.check(Tag.OR)
				self.conditionalTerm()
				self.extendedConditionalExpression()
		else:
			pass
	
	def conditionalExpression(self):
		if self.token.tag in self.firstConditionalExpression:
			self.conditionalTerm()
			self.extendedConditionalExpression()
		else:
			self.error("expected an conditional expression before " + str(self.token))
	
	def expression(self):
		if self.token.tag in self.firstExpression:
			self.conditionalExpression()
		else:
			self.error("expected an expression before " + str(self.token))

	def ifElseStatement(self):
		if self.token.tag == Tag.IFELSE:
			self.check(Tag.IFELSE)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
			self.check(ord('['))
			self.statementSequence()
			self.check(ord(']'))
			self.check(ord('['))
			self.statementSequence()
			self.check(ord(']'))
		else:
			self.error("expected an IFELSE expression before " + str(self.token))

	def ifStatement(self):
		if self.token.tag == Tag.IF:
			self.check(Tag.IF)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
			self.check(ord('['))
			self.statementSequence()
			self.check(ord(']'))
		else:
			self.error("expected an IF expression before " + str(self.token))

	def conditionalStatement(self):
		if self.token.tag in self.firstConditionalStatement:
			if self.token.tag == Tag.IF:
				self.ifStatement()
			elif self.token.tag == Tag.IFELSE:
				self.ifElseStatement()
		else:
			self.error("expected an conditional expression before " + str(self.token))

	def repetitiveStatement(self):
		if self.token.tag == Tag.WHILE:
			self.check(Tag.WHILE)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
			self.check(ord('['))
			self.statementSequence()
			self.check(ord(']'))
		else:
			self.error("expected an repetitive expression before " + str(self.token))
		
	def structuredStatement(self):
		if self.token.tag in self.firstStructuredStatement:
			if self.token.tag in self.firstConditionalStatement:
				self.conditionalStatement()
			elif self.token.tag == Tag.WHILE:
				self.repetitiveStatement()
		else:
			self.error("expected an structured expression before " + str(self.token))

	def element(self):
		if self.token.tag in self.firstElement:
			if self.token.tag == Tag.STRING:
				self.check(Tag.STRING)
			elif self.token.tag in self.firstExpression:
				self.expression()
		else:
			self.error("expected an element expression before " + str(self.token))
		
	def elementList(self):
		if self.token.tag == ord(','):
			self.check(ord(','))
			self.element()
			self.elementList()
		else:
			pass
		
	def textStatement(self):
		if self.token.tag == Tag.PRINT:
			self.check(Tag.PRINT)
			self.check(ord('('))
			self.element()
			self.elementList()
			self.check(ord(')'))
		else:
			self.error("expected a PRINT statement before " + str(self.token))

	def penWidthStatement(self):
		if self.token.tag == Tag.PENWIDTH:
			self.check(Tag.PENWIDTH)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a PENWIDTH statement before " + str(self.token))

	def colorStatement(self):
		if self.token.tag == Tag.COLOR:
			self.check(Tag.COLOR)
			self.check(ord('('))
			self.expression()
			self.check(ord(','))
			self.expression()
			self.check(ord(','))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a COLOR statement before " + str(self.token))
		
	def penDownStatement(self):
		if self.token.tag == Tag.PENDOWN:
			self.check(Tag.PENDOWN)
			self.check(ord('('))
			self.check(ord(')'))
		else:
			self.error("expected a PENDOWN statement before " + str(self.token))
		
	def penUpStatement(self):
		if self.token.tag == Tag.PENUP:
			self.check(Tag.PENUP)
			self.check(ord('('))
			self.check(ord(')'))
		else:
			self.error("expected a PENUP statement before " + str(self.token))
		
	def arcStatement(self):
		if self.token.tag == Tag.ARC:
			self.check(Tag.ARC)
			self.check(ord('('))
			self.expression()
			self.check(ord(','))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a ARC statement before " + str(self.token))
		
	def circleStatement(self):
		if self.token.tag == Tag.CIRCLE:
			self.check(Tag.CIRCLE)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a CIRCLE statement before " + str(self.token))
		
	def clearStatement(self):
		if self.token.tag == Tag.CLEAR:
			self.check(Tag.CLEAR)
			self.check(ord('('))
			self.check(ord(')'))
		else:
			self.error("expected a CLEAR statement before " + str(self.token))

	def drawingStatement(self):
		if self.token.tag in self.firstDrawingStatement:
			if self.token.tag == Tag.CLEAR:
				self.clearStatement()
			elif self.token.tag == Tag.CIRCLE:
				self.circleStatement()
			elif self.token.tag == Tag.ARC:
				self.arcStatement()
			elif self.token.tag == Tag.PENUP:
				self.penUpStatement()
			elif self.token.tag == Tag.PENDOWN:
				self.penDownStatement()
			elif self.token.tag == Tag.COLOR:
				self.colorStatement()
			elif self.token.tag == Tag.PENWIDTH:
				self.penWidthStatement()
		else:
			self.error("expected a drawing statement before " + str(self.token))

	def setXYStatement(self):
		if self.token.tag == Tag.SETXY:
			self.check(Tag.SETXY)
			self.check(ord('('))
			self.expression()
			self.check(ord(','))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a SETXY statement before " + str(self.token))
	
	def setXStatement(self):
		if self.token.tag == Tag.SETX:
			self.check(Tag.SETX)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a SETX statement before " + str(self.token))
	
	def setYStatement(self):
		if self.token.tag == Tag.SETY:
			self.check(Tag.SETY)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a SETY statement before " + str(self.token))
	
	def leftStatement(self):
		if self.token.tag == Tag.LEFT:
			self.check(Tag.LEFT)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a LEFT statement before " + str(self.token))
	
	def rightStatement(self):
		if self.token.tag == Tag.RIGHT:
			self.check(Tag.RIGHT)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a RIGHT statement before " + str(self.token))
	
	def backwardStatement(self):
		if self.token.tag == Tag.BACKWARD:
			self.check(Tag.BACKWARD)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a BACKWARD statement before " + str(self.token))
	
	def forwardStatement(self):
		if self.token.tag == Tag.FORWARD:
			self.check(Tag.FORWARD)
			self.check(ord('('))
			self.expression()
			self.check(ord(')'))
		else:
			self.error("expected a FORWARD statement before " + str(self.token))
	
	def movementStatement(self):
		if self.token.tag in self.firstMovementStatement:
			if self.token.tag == Tag.FORWARD:
				self.forwardStatement()
			elif self.token.tag == Tag.BACKWARD:
				self.backwardStatement()
			elif self.token.tag == Tag.LEFT:
				self.leftStatement()
			elif self.token.tag == Tag.RIGHT:
				self.rightStatement()
			elif self.token.tag == Tag.SETX:
				self.setXStatement()
			elif self.token.tag == Tag.SETY:
				self.setYStatement()
			elif self.token.tag == Tag.SETXY:
				self.setXYStatement()
			elif self.token.tag == Tag.HOME:
				self.check(Tag.HOME)
				self.check(ord('('))
				self.check(ord(')'))
		else:
			self.error("expected a movement statement before " + str(self.token))

	def assigmentStatement(self):
		if self.token.tag == Tag.ID:
			self.check(Tag.ID)
			self.check(Tag.ASSIGN)
			self.expression()
		else:
			self.error("expected an ASSIGMENT statement before " + str(self.token))
		
	def identifierList(self):
		if self.token.tag == ord(','):
			self.check(ord(','))
			self.check(Tag.ID)
			self.identifierList()
		else:
			pass

	def declarationStatement(self):
		if self.token.tag == Tag.VAR:
			self.check(Tag.VAR)
			self.check(Tag.ID)
			self.identifierList()
		else:
			self.error("expected a DECLARATION statement before " + str(self.token))
		
	def simpleStatement(self):
		if self.token.tag in self.firstSimpleStatement:
			if self.token.tag == Tag.VAR:
				self.declarationStatement()
			elif self.token.tag == Tag.ID:
				self.assigmentStatement()
			elif self.token.tag in self.firstMovementStatement:
				self.movementStatement()
			elif self.token.tag in self.firstDrawingStatement:
				self.drawingStatement()
			elif self.token.tag == Tag.PRINT:
				self.textStatement()
		else:
			self.error("expected a simple statement statement before " + str(self.token))
		
	def statement(self):
		if self.token.tag in self.firstStatement:
			if self.token.tag in self.firstSimpleStatement:
				self.simpleStatement()
			elif self.token.tag in self.firstStructuredStatement:
				self.structuredStatement()
		else:
			self.error("expected a statement before " + str(self.token))
		
	def statementSequence(self):
		if self.token.tag in self.firstStatementSequence:
			self.statement()
			self.statementSequence()
		else:
			pass

	def program(self):
		if self.token.tag in self.firstProgram:
			self.statementSequence()
			if self.token.tag != Tag.EOF:
				print(str(self.token))
				self.error("ilegal start of a statement")
		else:
			self.error("expected a statement before " + str(self.token))