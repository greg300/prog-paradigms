from prolog_structures import Rule, RuleBody, Term, Function, Variable, Atom, Number, Constant
from typing import List
from functools import reduce

import copy
import sys
import random

class Not_unifiable(Exception):
	pass

'''
Please read prolog_structures.py for data structures
that represent Prolog terms, rules, and goals.
'''
class Interpreter:
	def __init__(self):
		pass

	'''
	Example
	occurs_check (v, t) where v is of type Variable, t is of type Term.
	occurs_check (v, t) returns true if the Prolog Variable v occurs in t.
	Please see the lecture note Control in Prolog to revisit the concept of
	occurs-check.
	'''
	def occurs_check (self, v : Variable, t : Term) -> bool:
		if isinstance(t, Variable):
			return v == t
		elif isinstance(t, Function):
			for t in t.terms:
				if self.occurs_check(v, t):
					return True
			return False
		return False


	'''
	Problem 1
	variables_of_term (t) where t is of type Term.
	variables_of_clause (c) where c is of type Rule.

	The function should return the Variables contained in a term or a rule
	using Python set.

	The result must be saved in a Python set. The type of each element (a Prolog Variable)
	in the set is Variable.
	'''
	def variables_of_term (self, t : Term) -> set :
		# If t is a Variable:
		if isinstance(t, Variable):
			# Create a singleton set containing t and return it.
			return set([t])
		# If t is a Function:
		if isinstance(t, Function):
			s = set()
			for term in t.terms:
				if isinstance(term, Variable):
					s = s.union(set([term]))
				elif isinstance(term, Function):
					s = s.union(self.variables_of_term(term))
			return s
		# If t is neither a Term nor a Variable, it is a Constant.
		else:
			# Return the empty set.
			return set()

	def variables_of_clause (self, c : Rule) -> set :
		if c is None:
			return set()
		
		s = set()
		s = s.union(self.variables_of_term(c.head))
		for t in c.body.terms:
			s = s.union(self.variables_of_term(t))
		return s

	'''
	Problem 2
	substitute_in_term (s, t) where s is of type dictionary and t is of type Term
	substitute_in_clause (s, t) where s is of type dictionary and c is of type Rule,

	The value of type dict should be a Python dictionary whose keys are of type Variable
	and values are of type Term. It is a map from variables to terms.

	The function should return t_ obtained by applying substitution s to t.

	Please use Python dictionary to represent a subsititution map.
	'''
	def substitute_in_term (self, s : dict, t : Term) -> Term:
		keys = s.keys()
		# If t is a Function:
		if isinstance(t, Function):
			newTerms = []
			for term in t.terms:
				if isinstance(term, Variable):
					if term in keys:
						newTerms.append(s[term])
					else:
						newTerms.append(term)
				elif isinstance(term, Function):
					innerFunc = self.substitute_in_term(s, term)
					newTerms.append(innerFunc)
				else:
					newTerms.append(term)
			
			return Function(t.relation, newTerms)
		elif isinstance(t, Variable):
			if t in keys:
				if isinstance(s[t], Atom):
					return Atom(s[t].value)
				elif isinstance(s[t], Number):
					return Number(s[t].value)
				elif isinstance(s[t], Variable):
					return Variable(s[t].value)
				elif isinstance(s[t], Function):
					return Function(s[t].relation, s[t].terms)
			else:
				return t
		else:
			return t

	def substitute_in_clause (self, s : dict, c : Rule) -> Rule:
		newCHead = self.substitute_in_term(s, c.head)
		newCBodyTerms = []
		for t in c.body.terms:
			newCBodyTerms.append(self.substitute_in_term(s, t))
		return Rule(newCHead, RuleBody(newCBodyTerms))


	'''
	Problem 3
	unify (t1, t2) where t1 is of type term and t2 is of type Term.
	The function should return a substitution map of type dict,
	which is a unifier of the given terms. You may find the pseudocode
	of unify in the lecture note Control in Prolog useful.

	The function should raise the exception raise Not_unfifiable (),
	if the given terms are not unifiable.

	Please use Python dictionary to represent a subsititution map.
	'''
	def rec_unify (self, t1: Term, t2: Term, s: dict) -> dict:
		x = self.substitute_in_term(s, t1)
		y = self.substitute_in_term(s, t2)

		xVars = self.variables_of_term(x)
		yVars = self.variables_of_term(y)
		if isinstance(x, Variable) and x not in yVars:
			s[x] = y
			for k in s.keys():
				s[k] = self.substitute_in_term({x : y}, s[k])
			return s
		elif isinstance(y, Variable) and y not in xVars:
			s[y] = x
			for k in s.keys():
				s[k] = self.substitute_in_term({y : x}, s[k])
			return s
		elif (isinstance(x, Variable) or isinstance(x, Constant)) and x == y:
			return s
		elif isinstance(x, Function) and isinstance(y, Function) and x.relation == y.relation and len(x.terms) == len(y.terms):
			l = [(x.terms[i], y.terms[i]) for i in range(len(x.terms))]
			s = reduce(lambda theta, p: self.rec_unify(p[0], p[1], theta), l, s)

			return s

		else:
			raise Not_unifiable()

	def unify (self, t1: Term, t2: Term) -> dict:
		s = self.rec_unify(t1, t2, {})
		return s


	fresh_counter = 0
	def fresh(self) -> Variable:
		self.fresh_counter += 1
		return Variable("_G" + str(self.fresh_counter))
	def freshen(self, c: Rule) -> Rule:
		c_vars = self.variables_of_clause(c)
		theta = {}
		for c_var in c_vars:
			theta[c_var] = self.fresh()

		return self.substitute_in_clause(theta, c)


	'''
	Problem 4
	Following the Abstract interpreter pseudocode in the lecture note Control in Prolog to implement
	a nondeterministic Prolog interpreter.

	nondet_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of Terms (results), which is an instance of the original goal and is
	a logical consequence of the program. See the tests cases (in src/main.py) as examples.
	'''
	def exists_unifiable_pair(self, resolvent : List[Term], program : List[Rule]) -> bool:
		for i in range(len(resolvent)):
			for j in range(len(program)):
				try:
					ap = self.freshen(program[j])
					theta = self.unify(resolvent[i], ap.head)
					return True
				except Not_unifiable:
					continue
		return False


	def nondet_query (self, program : List[Rule], pgoal : List[Term]) -> List[Term]:
		while True:
			g = copy.copy(pgoal)
			resolvent = copy.copy(g)
			while len(resolvent) > 0:
				if not self.exists_unifiable_pair(resolvent, program):
					break

				i = random.randint(0, len(resolvent) - 1)
				a = resolvent[i]
				
				j = random.randint(0, len(program) - 1)
				ap = self.freshen(program[j])
				try:
					theta = self.unify(a, ap.head)
					resolvent.remove(a)
					for term in ap.body.terms:
						resolvent.append(term)
					for k in range(len(resolvent)):
						resolvent[k] = self.substitute_in_term(theta, resolvent[k])
					for k in range(len(g)):
						g[k] = self.substitute_in_term(theta, g[k])
				except Not_unifiable:
					pass
			if len(resolvent) == 0:
				return g
			else:
				continue



	'''
	Challenge Problem

	det_query (program, goal) where
		the first argument is a program which is a list of Rules.
		the second argument is a goal which is a list of Terms.

	The function returns a list of term lists (results). Each of these results is
	an instance of the original goal and is a logical consequence of the program.
	If the given goal is not a logical consequence of the program, then the result
	is an empty list. See the test cases (in src/main.py) as examples.
	'''
	def dfs_det_query(self, resolvent : List[Term], goal : Term, program : List[Rule], solutions : List[List[Term]]) -> bool:
		if len(resolvent) == 0:
			solutions.append([goal])
			return True
		
		while (len(resolvent) > 0):
			goalChoice = resolvent.pop(0)
			searched = False

			for rule in program:
				try:
					#self.unify(goalChoice, rule.head)
					rule = self.freshen(rule)
					theta = self.unify(goalChoice, rule.head)

					newResolvent = copy.copy(resolvent)
					newGoal = copy.copy(goal)

					for term in rule.body.terms:
						newResolvent.append(term)
					for k in range(len(newResolvent)):
						newResolvent[k] = self.substitute_in_term(theta, newResolvent[k])
					newGoal = self.substitute_in_term(theta, newGoal)

					result = self.dfs_det_query(newResolvent, newGoal, program, solutions)
					searched = result or searched
				except Not_unifiable:
					pass
			if not searched:
				return False

	def det_query (self, program : List[Rule], pgoal : List[Term]) -> List[List[Term]]:
		solutions = []
		goals = copy.copy(pgoal)
		firstGoal = goals[0]
		self.dfs_det_query(goals, firstGoal, program, solutions)

		return solutions
