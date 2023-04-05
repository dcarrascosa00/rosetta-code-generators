package com.regnosys.rosetta.generator.python.docrefs

import com.google.inject.Inject
import com.google.inject.Provider
import com.regnosys.rosetta.generator.python.PythonCodeGenerator
import com.regnosys.rosetta.rosetta.RosettaModel
import com.regnosys.rosetta.tests.RosettaInjectorProvider
import com.regnosys.rosetta.tests.util.ModelHelper
import org.eclipse.xtext.resource.XtextResourceSet
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.extensions.InjectionExtension
import org.eclipse.xtext.testing.util.ParseHelper
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.^extension.ExtendWith

import static org.junit.jupiter.api.Assertions.*

@ExtendWith(InjectionExtension)
@InjectWith(RosettaInjectorProvider)

class DocReferenceTest {
	
    @Inject extension ModelHelper
    @Inject PythonCodeGenerator generator;

    @Inject extension ParseHelper<RosettaModel>
    @Inject Provider<XtextResourceSet> resourceSetProvider;
	
	
	@Test
	def void declearCorpusWithBodyReference() {
		'''
			body Organisation Org1 <"some description 1">
			corpus Agreement Org1 "Agreement 1" Agr1 <"some description 2">
			
		'''.parseRosettaWithNoErrors
	}
	
	
	@Test
	def void declearCorpusWithoutBodyReference() {
		'''
			corpus Agreement "Agreement 1" Agr1 <"some description 2">
						
		'''.parseRosettaWithNoErrors
	}
	
	
	@Test
	def void corpusDisplaytNameIsOptional() {
		'''
			corpus Agreement Agr1
		'''.parseRosettaWithNoErrors
	}
	
	@Test
	def void typeCanHaveDocRef() {
		'''
			body Organisation Org1
			corpus Agreement Org1 "Agreement 1" Agr1
			
			segment name
			
			type Foo:
				[docReference Org1 Agr1 name "something" provision "some provision"]
				bar string (1..1)
			
		'''.parseRosettaWithNoErrors
	}
	
	@Test
	def void docRefProvisionIsOptional() {
		'''
			body Organisation Org1
			corpus Agreement Org1 "Agreement 1" Agr1
			
			segment name
			
			type Foo:
				[docReference Org1 Agr1 name "something"]
				bar string (1..1)
			
		'''.parseRosettaWithNoErrors
	}
	
	@Test
	def void attributeCanHaveDocRef() {
		'''
			body Organisation Org1
			corpus Agreement Org1 "Agreement 1" Agr1
			
			segment name
			
			type Foo:
				bar string (1..1)
					[docReference Org1 Agr1 name "something"]
			
		'''.parseRosettaWithNoErrors
	}
	
	@Test
	def void enumCanHaveDocRef() {
		'''
			body Organisation Org1
			corpus Agreement Org1 "Agreement 1" Agr1
			
			segment name
			
			enum Foo:
			[docReference Org1 Agr1 name "something"]
				bar
					
			
		'''.parseRosettaWithNoErrors
	}
	
	@Test
	def void enumValueCanHaveDocRef() {
		'''
			body Organisation Org1
			corpus Agreement Org1 "Agreement 1" Agr1
			
			segment name
			
			enum Foo:
				bar
					[docReference Org1 Agr1 name "something"]
		'''.parseRosettaWithNoErrors
	}
	
	@Test
	def void functionsCanHaveDocRef() {
		'''
			body Organisation Org1
			corpus Agreement Org1 "Agreement 1" Agr1
			
			segment name
			
			func Sum:
				[docReference Org1 Agr1 name "something"]
				inputs: x number (0..*)
				output: sum number (1..1)

		'''.parseRosetta
		
		
	}
	
	@Test
	def void conditionsCanHaveDocRef() {
		
		
		val python=
		'''
			body Organisation Org1
			corpus Agreement Org1 "Agreement 1" Agr1
			
			segment name
			
			type Foo:
				a int (1..1)
				
				condition:
					[docReference Org1 Agr1 name "something"]
					a > 0
			
			
		'''.parseRosettaWithNoErrors
	}
	
	
	def generatePython(CharSequence model) {
    	val eResource = model.parseRosettaWithNoErrors.eResource
    	generator.afterGenerateTest(eResource.contents.filter(RosettaModel).toList)
    	
    }
    
    

	
}