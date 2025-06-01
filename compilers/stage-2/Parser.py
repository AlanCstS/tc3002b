from Lexer import *
from Translator import *
from Type import *

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
		self.firstAdditiveExpression = self.firstMultiplicativeExpression
		self.firstExtendedRelationalExpression = set((ord('<'), ord('>'), Tag.LEQ, Tag.GEQ))
		self.firstRelationalExpression = self.firstAdditiveExpression
		self.firstExtendedEqualityExpression = set((ord('='), Tag.NEQ))
		self.firstEqualityExpression = self.firstRelationalExpression
		self.firstExtendedConditionalTerm = {Tag.AND}
		self.firstConditionalTerm = self.firstEqualityExpression
		self.firstExtendedConditionalExpression = {Tag.OR}
		self.firstConditionalExpression = self.firstConditionalTerm
		self.firstStatement = {Tag.VAR, Tag.ID, Tag.PRINT, Tag.IF, Tag.IFELSE, Tag.WHILE}
		self.firstProgram = self.firstStatement

		self.firstMovementStatement = {
			Tag.FORWARD, Tag.BACKWARD, Tag.LEFT, Tag.RIGHT,
			Tag.SETX, Tag.SETY, Tag.SETXY, Tag.HOME
		}

		self.firstDrawingStatement = {
			Tag.CLEAR, Tag.CIRCLE, Tag.ARC,
			Tag.PENUP, Tag.PENDOWN, Tag.COLOR, Tag.PENWIDTH
		}
		self.firstStatement.update(self.firstDrawingStatement)
		self.firstStatement.update(self.firstMovementStatement)
		self.firstProgram.update(self.firstStatement)

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
		return self.program()

	def program(self):
		if self.token.tag in self.firstProgram:
			seq = self.statementSequence()
			if self.token.tag != Tag.EOF:
				self.error("illegal content after program")
			return Program(seq)
		else:
			self.error("expected a statement")

	def statementSequence(self):
		stmt = self.statement()
		if self.token.tag in self.firstStatement:
			seq = self.statementSequence()
			return StatementSequence(stmt, seq)
		else:
			return StatementSequence(stmt)

	def movementStatement(self):
		tag = self.token.tag
		self.check(tag)
		expr = None
		if tag != Tag.HOME:
			self.check(ord('('))
			expr = self.expression()
			if tag == Tag.SETXY:
				self.check(ord(','))
				expr2 = self.expression()
				self.check(ord(')'))
				return SetXY(expr, expr2)
			self.check(ord(')'))
		if tag == Tag.FORWARD:
			return Forward(expr)
		elif tag == Tag.BACKWARD:
			return Backward(expr)
		elif tag == Tag.LEFT:
			return Left(expr)
		elif tag == Tag.RIGHT:
			return Right(expr)
		elif tag == Tag.SETX:
			return SetX(expr)
		elif tag == Tag.SETY:
			return SetY(expr)
		elif tag == Tag.HOME:
			self.check(ord('('))
			self.check(ord(')'))
			return Home()



	def drawingStatement(self):
		tag = self.token.tag
		self.check(tag)
		if tag in (Tag.CLEAR, Tag.PENUP, Tag.PENDOWN):
			self.check(ord('('))
			self.check(ord(')'))
			if tag == Tag.CLEAR:
				return Clear()
			elif tag == Tag.PENUP:
				return PenUp()
			elif tag == Tag.PENDOWN:
				return PenDown()
		elif tag == Tag.CIRCLE:
			self.check(ord('('))
			expr = self.expression()
			self.check(ord(')'))
			return Circle(expr)
		elif tag == Tag.ARC:
			self.check(ord('('))
			expr1 = self.expression()
			self.check(ord(','))
			expr2 = self.expression()
			self.check(ord(')'))
			return Arc(expr1, expr2)
		elif tag == Tag.COLOR:
			self.check(ord('('))
			r = self.expression()
			self.check(ord(','))
			g = self.expression()
			self.check(ord(','))
			b = self.expression()
			self.check(ord(')'))
			return Color(r, g, b, str(self.lexer.line))
		elif tag == Tag.PENWIDTH:
			self.check(ord('('))
			expr = self.expression()
			self.check(ord(')'))
			return PenWidth(expr)

	def statement(self):
		if self.token.tag == Tag.VAR:
			return self.declarationStatement()
		elif self.token.tag == Tag.PRINT:
			return self.printStatement()
		elif self.token.tag in self.firstMovementStatement:
			return self.movementStatement()
		elif self.token.tag in self.firstDrawingStatement:
			return self.drawingStatement()
		elif self.token.tag == Tag.IF:
			return self.ifStatement()
		elif self.token.tag == Tag.IFELSE:
			return self.ifElseStatement()
		elif self.token.tag == Tag.WHILE:
			return self.whileStatement()
		elif self.token.tag == Tag.ID:
			return self.assignmentStatement()
		else:
			self.error("invalid statement")


	def declarationStatement(self):
		self.check(Tag.VAR)
		id_token = self.token
		self.check(Tag.ID)
		first = IdDeclaration(id_token.value, str(self.lexer.line))
		rest = self.identifierList()
		return Declaration(first, rest)

	def identifierList(self):
		if self.token.tag == ord(','):
			self.check(ord(','))
			id_token = self.token
			self.check(Tag.ID)
			first = IdDeclaration(id_token.value, str(self.lexer.line))
			rest = self.identifierList()
			return idDeclarationList(first, rest)
		return None

	def assignmentStatement(self):
		id_token = self.token
		self.check(Tag.ID)
		self.check(Tag.ASSIGN)
		expr = self.expression()
		return Assigment(id_token.value, expr, str(self.lexer.line))

	def printStatement(self):
		self.check(Tag.PRINT)
		self.check(ord('('))
		elem = self.element()
		lst = self.elementList()
		self.check(ord(')'))
		return Print(elem, lst)

	def element(self):
		if self.token.tag == Tag.STRING:
			value = self.token.value
			self.check(Tag.STRING)
			return String(value)
		else:
			return self.expression()

	def elementList(self):
		if self.token.tag == ord(','):
			self.check(ord(','))
			elem = self.element()
			lst = self.elementList()
			return ElementList(elem, lst)
		return None

	def ifStatement(self):
		self.check(Tag.IF)
		self.check(ord('('))
		cond = self.expression()
		self.check(ord(')'))
		self.check(ord('['))
		seq = self.statementSequence()
		self.check(ord(']'))
		return If(cond, seq)

	def ifElseStatement(self):
		self.check(Tag.IFELSE)
		self.check(ord('('))
		cond = self.expression()
		self.check(ord(')'))
		self.check(ord('['))
		seq1 = self.statementSequence()
		self.check(ord(']'))
		self.check(ord('['))
		seq2 = self.statementSequence()
		self.check(ord(']'))
		return IfElse(cond, seq1, seq2)

	def whileStatement(self):
		self.check(Tag.WHILE)
		self.check(ord('('))
		cond = self.expression()
		self.check(ord(')'))
		self.check(ord('['))
		seq = self.statementSequence()
		self.check(ord(']'))
		return While(cond, seq)

	def expression(self):
		return self.conditionalExpression()

	def conditionalExpression(self):
		term = self.conditionalTerm()
		if self.token.tag == Tag.OR:
			self.check(Tag.OR)
			right = self.conditionalExpression()
			return Or(term, right)
		return term

	def conditionalTerm(self):
		eq = self.equalityExpression()
		if self.token.tag == Tag.AND:
			self.check(Tag.AND)
			right = self.conditionalTerm()
			return And(eq, right)
		return eq

	def equalityExpression(self):
		rel = self.relationalExpression()
		if self.token.tag == ord('='):
			self.check(ord('='))
			right = self.relationalExpression()
			return Equal(rel, right)
		elif self.token.tag == Tag.NEQ:
			self.check(Tag.NEQ)
			right = self.relationalExpression()
			return Different(rel, right)
		return rel

	def relationalExpression(self):
		add = self.additiveExpression()
		if self.token.tag == ord('<'):
			self.check(ord('<'))
			right = self.additiveExpression()
			return Lesser(add, right)
		elif self.token.tag == Tag.LEQ:
			self.check(Tag.LEQ)
			right = self.additiveExpression()
			return LesserOrEqual(add, right)
		elif self.token.tag == ord('>'):
			self.check(ord('>'))
			right = self.additiveExpression()
			return Greater(add, right)
		elif self.token.tag == Tag.GEQ:
			self.check(Tag.GEQ)
			right = self.additiveExpression()
			return GreaterOrEqual(add, right)
		return add

	def additiveExpression(self):
		mul = self.multiplicativeExpression()
		if self.token.tag == ord('+'):
			self.check(ord('+'))
			right = self.additiveExpression()
			return Add(mul, right)
		elif self.token.tag == ord('-'):
			self.check(ord('-'))
			right = self.additiveExpression()
			return Substract(mul, right)
		return mul

	def multiplicativeExpression(self):
		un = self.unaryExpression()
		if self.token.tag == ord('*'):
			self.check(ord('*'))
			right = self.multiplicativeExpression()
			return Multiply(un, right)
		elif self.token.tag == ord('/'):
			self.check(ord('/'))
			right = self.multiplicativeExpression()
			return Divide(un, right, str(self.lexer.line))
		elif self.token.tag == Tag.MOD:
			self.check(Tag.MOD)
			right = self.multiplicativeExpression()
			return Module(un, right, str(self.lexer.line))
		return un

	def unaryExpression(self):
		if self.token.tag == ord('-'):
			self.check(ord('-'))
			right = self.unaryExpression()
			return Minus(right)
		elif self.token.tag == ord('!'):
			self.check(ord('!'))
			right = self.unaryExpression()
			return Not(right)
		else:
			return self.primaryExpression()

	def primaryExpression(self):
		if self.token.tag == Tag.ID:
			name = self.token.value
			self.check(Tag.ID)
			return Identifier(name, str(self.lexer.line))
		elif self.token.tag == Tag.NUMBER:
			val = self.token.value
			self.check(Tag.NUMBER)
			return Number(val)
		elif self.token.tag == Tag.TRUE:
			self.check(Tag.TRUE)
			return Boolean(True)
		elif self.token.tag == Tag.FALSE:
			self.check(Tag.FALSE)
			return Boolean(False)
		elif self.token.tag == ord('('):
			self.check(ord('('))
			expr = self.expression()
			self.check(ord(')'))
			return expr
		else:
			self.error("expected a primary expression")
