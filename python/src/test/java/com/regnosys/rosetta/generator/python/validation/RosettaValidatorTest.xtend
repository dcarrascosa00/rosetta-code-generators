package com.regnosys.rosetta.generator.python.validation

import com.google.inject.Inject
import com.regnosys.rosetta.RosettaRuntimeModule
import com.regnosys.rosetta.rosetta.simple.Data
import com.regnosys.rosetta.tests.RosettaInjectorProvider
import com.regnosys.rosetta.tests.util.ModelHelper
import com.regnosys.rosetta.validation.RosettaValidator
import org.eclipse.xtext.diagnostics.Diagnostic
import org.eclipse.xtext.service.SingletonBinding
import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.extensions.InjectionExtension
import org.eclipse.xtext.testing.validation.ValidationTestHelper
import org.eclipse.xtext.validation.Check
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.^extension.ExtendWith

import static com.regnosys.rosetta.rosetta.RosettaPackage.Literals.*
import static com.regnosys.rosetta.rosetta.expression.ExpressionPackage.Literals.*
import static com.regnosys.rosetta.rosetta.simple.SimplePackage.Literals.*

@ExtendWith(InjectionExtension)
@InjectWith(MyRosettaInjectorProvider)
class RosettaValidatorTest implements RosettaIssueCodes {

	@Inject extension ValidationTestHelper
	@Inject extension ModelHelper
	
	
	/*
	 * There are some tests which uses variables from RosettaIssueCodes which give a fail in the test even though they are correct.
	 */
	
	

	@Test
	@Disabled
	def void noDuplicateTypesInAnnotationSourceTest() {
		val model = '''
			type Foo:
				foo string (0..1)
			
			rule source TestA {
				Foo:
				+ foo
				
				Foo:
				+ foo
			}
		'''.parseRosetta
		model.assertError(ROSETTA_EXTERNAL_CLASS, null,
            "Duplicate type `Foo`.")
	}

	@Test
	@Disabled
	def void synonymNotAllowedInRuleSourceTest() {
		val model = '''
			type Foo:
				foo string (0..1)
			
			rule source TestA {
				Foo:
				[meta "bar"]
				+ foo
					[value "bar" path "baz"]
			}
		'''.parseRosetta
		model.assertError(ROSETTA_EXTERNAL_CLASS_SYNONYM, null,
            "You may not define synonyms in a rule source.")
        model.assertError(ROSETTA_EXTERNAL_SYNONYM, null,
            "You may not define synonyms in a rule source.")
	}


	@Test
	@Disabled
	def void cannotImplicitlyOverrideRuleReferenceFromSuperSourceTest() {
		val model = '''
			type Foo:
				foo string (0..1)
			
			reporting rule RA:
				return "A"
			
			reporting rule RB:
				return "B"
			
			rule source TestA {
				Foo:
				+ foo
					[ruleReference RA]
			}
			
			rule source TestB extends TestA {
				Foo:
				+ foo
					[ruleReference RB]
			}
		'''.parseRosetta
		model.assertError(ROSETTA_EXTERNAL_REGULAR_ATTRIBUTE, null,
            "There is already a mapping defined for `foo`. Try removing the mapping first with `- foo`.")
	}

	
	@Test
	@Disabled
	def void cannotRemoveNonExistingRuleReferenceFromExternalRuleSourceTest() {
		val model = '''
			type Foo:
				foo string (0..1)
			
			reporting rule RA:
				return "A"
			
			reporting rule RB:
				return "B"
			
			rule source TestA {
				Foo:
				- foo
			}
		'''.parseRosetta
		model.assertError(ROSETTA_EXTERNAL_REGULAR_ATTRIBUTE, null,
            "You cannot remove this mapping because `foo` did not have a mapping defined before.")
	}
	
	@Test
	def void mayNotUseAmbiguousOutputTest() {
		val model = '''
			type Foo:
				result int (1..1)
			
			func F:
			    inputs:
			        foo Foo (1..1)
			    output:
			        result int (1..1)
						
			    set result:
			        foo extract [ result ]
		'''.parseRosetta
		model.assertError(ROSETTA_SYMBOL_REFERENCE, null,
            "Ambiguous reference. `result` may either refer to `item -> result` or to the output variable.")
	}
	
	@Test
	def void dateMemberHasRightTypeTest() {
		'''
			func F:
				inputs:
					d date (1..1)
				output: result boolean (1..1)
				set result:
					d -> day > 15
		'''.parseRosettaWithNoIssues
	}
	
	@Test
	def void nameShadowingNotAllowed1() {
		val model =
		'''
			func F:
				inputs:
					a int (1..1)
				output: result int (1..1)
				set result:
					42 extract a [ a ]
		'''.parseRosetta
		model.assertError(CLOSURE_PARAMETER, null,
            "Duplicate name.")
	}
	
	@Test
	def void nameShadowingNotAllowed2() {
		val model =
		'''
			func F:
				output: result int (1..1)
				alias a: 10
				set result:
					42 extract a [ a ]
		'''.parseRosetta
		model.assertError(CLOSURE_PARAMETER, null,
            "Duplicate name.")
	}
	
	@Test
	def void nameShadowingNotAllowed3() {
		val model =
		'''
			func F:
				output: a int (1..1)
				set a:
					42 extract a [ a ]
		'''.parseRosetta
		model.assertError(CLOSURE_PARAMETER, null,
            "Duplicate name.")
	}
	
	@Test
	def void nameShadowingNotAllowed4() {
		val model =
		'''
			func F:
				output: result int (1..1)
				set result:
					42 extract a [ 10 extract a [ a ] ]
		'''.parseRosetta
		model.assertError(CLOSURE_PARAMETER, null,
            "Duplicate name.")
	}
	
	@Test
	def void mayDoRecursiveCalls() {
		'''
			func Rec:
				output: result int (1..1)
				alias test: Rec()
				set result: Rec()
		'''.parseRosettaWithNoIssues
	}
	
	@Test
	def void testCannotOmitParametersOfBinaryFunction() {
		val model =
		'''
			func Add:
				inputs:
					a int (1..1)
					b int (1..1)
				output: result int (1..1)
				set result:
					a + b
			
			func Foo:
				inputs: a int (0..*)
				output: b int (0..*)
				add b:
					a extract [Add]
		'''.parseRosetta
		model.assertError(ROSETTA_SYMBOL_REFERENCE, null,
            "Expected 2 arguments, but got 0 instead.")
	}
	
	@Test
	def void testCannotCallParameter() {
		val model =
		'''
			func Foo:
				inputs: a int (0..*)
				output: b int (0..*)
				add b:
					a()
		'''.parseRosetta
		model.assertError(ROSETTA_SYMBOL_REFERENCE, null,
            "A variable may not be called.")
	}
	
	@Test
	def void testGeneratedInputWithoutImplicitVariable() {
		val model =
		'''
			func Foo:
				inputs: a int (0..*)
				output: b int (0..*)
				add b:
					extract [item+1]
		'''.parseRosetta
		model.assertError(MAP_OPERATION, null,
            "There is no implicit variable in this context. This operator needs an explicit input in this context.")
	}
	
	@Test
	def void testImplicitVariableWhenItDoesNotExist() {
		val model =
		'''
			func Foo:
				inputs: a int (0..*)
				output: b int (0..*)
				add b:
					item
		'''.parseRosetta
		model.assertError(ROSETTA_IMPLICIT_VARIABLE, null,
            "There is no implicit variable in this context.")
	}
	
	@Test
	def void testGeneratedInputValidationRedirection() {
		val model =
		'''
			type Foo:
				a int (1..1)
				condition A:
					/42
		'''.parseRosetta
		model.assertError(ROSETTA_IMPLICIT_VARIABLE, null,
            "Expected type `number`, but got `Foo` instead.")
	}
	
	@Test
	@Disabled
	def void testLowerCaseClass() {
		val model =
		'''
			synonym source FIX
			synonym source FpML
			
			type partyIdentifier: <"">
				partyId string (1..1) <"">
					[synonym FIX value "PartyID" tag 448]
					[synonym FpML value "partyId"]
		'''.parseRosettaWithNoErrors
		model.assertWarning(DATA, INVALID_CASE,
            "Type name should start with a capital")
	}
	
	@Test
	@Disabled
	def void testLowerCaseEnumeration() {
		val model =
		'''
			enum quoteRejectReasonEnum: <"">
				UnknownSymbol
				Other
		'''.parseRosettaWithNoErrors
		model.assertWarning(ROSETTA_ENUMERATION, INVALID_CASE,
            "Enumeration name should start with a capital")
	}
	
	@Test
	@Disabled
	def void testUpperCaseAttribute() {
		val model =
		'''
			synonym source FIX
			synonym source FpML
			type PartyIdentifier: <"">
					PartyId string (1..1) <"">
						[synonym FIX value "PartyID" tag 448]
						[synonym FpML value "partyId"]
		'''.parseRosettaWithNoErrors
		model.assertWarning(ATTRIBUTE, INVALID_CASE,
            "Attribute name should start with a lower case")
	}
		
	@Test
	@Disabled
	def void testTypeExpectation() {
		val model =
		'''
			type Foo:
				id int (1..1)
			
				condition R: 
					if id = True
					then id < 1
		'''.parseRosetta
		model.assertError(ROSETTA_CONDITIONAL_EXPRESSION, TYPE_ERROR,
			"Incompatible types: cannot use operator '=' with int and boolean.")
	}
	
	@Test
	def void testTypeExpectationMagicType() {
		'''
			qualifiedType productType {}
			type Foo:
				id productType (1..1)
				val int (1..1)
			
			    condition R:
				    if  id = "Type"
				    then val < 1
		'''.parseRosettaWithNoErrors
	}
	
	@Test
	def void testTypeExpectationNoError() {
		val model =
		'''
			type Foo:
				id int (1..1)
			
			condition R:
				if id = 1
				then id < 1
		'''.parseRosettaWithNoErrors
		model.assertNoError(TYPE_ERROR)
	}
	
	@Test
	@Disabled
	def void testTypeExpectationError() {
		val model =
		'''
			type Foo:
				id boolean (1..1)
			condition R:
				if id = True
				then id < 1
		'''.parseRosetta
		model.assertError(ROSETTA_CONDITIONAL_EXPRESSION, TYPE_ERROR, "Incompatible types: cannot use operator '<' with boolean and int.")
	}
	
	@Test
	@Disabled
	def void testTypeErrorAssignment_01() {
		val model =
		'''
			namespace "test"
			version "test"
			
			type Foo:
				id boolean (1..1)
			
			func Test:
				inputs: in0 Foo (0..1)
				output: out Foo (0..1)
				set out:
					"not a Foo"
		'''.parseRosetta
		model.assertError(OPERATION, TYPE_ERROR, "Expected type 'Foo' but was 'string'")
	}
	
	
	@Test
	@Disabled
	def void testTypeErrorAssignment_02() {
		val model =
		'''
			type Foo:
				id boolean (1..1)
			
			func Test:
				inputs: in0 Foo (0..1)
				output: out Foo (0..1)
				set out -> id:
					"not a boolean"
		'''.parseRosetta
		model.assertError(OPERATION, TYPE_ERROR, "Expected type 'boolean' but was 'string'")
	}
	
	@Test
	@Disabled
	def void testTypeErrorAssignment_03() {
		val model =
		'''
			type WithKey:
				[metadata key]
			
			type TypeToUse:
				attr WithKey (0..1)
				[metadata reference]
			
			func Bar:
			  inputs:
			    in1 TypeToUse (1..1)
			  output: result TypeToUse (1..1)
			  set result -> attr:
			     in1 as-key
		'''.parseRosetta
		model.assertError(OPERATION, TYPE_ERROR, "Expected type 'WithKey' but was 'TypeToUse'")
	}
	
	@Test
	def void testTypeErrorAssignment_04() {
		val model =
		'''
			enum Enumerate : X Y Z
			type Type:
				other Enumerate (0..1)
			func Funcy:
				inputs: in0 Type (0..1)
				output: out string (0..1)
				alias Ali : in0 -> other = Enumerate -> X
		'''.parseRosetta
		model.assertNoErrors
	}
	
	@Test
	@Disabled
	def void testTypeErrorAssignment_05() {
		val model =
		'''
			type Type:
				other int (0..1)
			func Funcy:
				inputs: in0 Type (0..1)
				output: out string (0..1)
				set out: in0->other
		'''.parseRosetta
		model.assertError(OPERATION, TYPE_ERROR, "Expected type 'string' but was 'int'")
	}
	
	@Test
	def void testAttributesWithLocationBadTarget() {
		'''
			metaType scheme string
			metaType reference string
			
			type Bar:
				bar string (1..1)
					[metadata address "pointsTo"=Foo->foo]
			
		'''.parseRosetta
		//TODO work out how to assert linking error
		//model.assertError(ROSETTA_CALLABLE_CALL, null, "Couldn't resolve reference to RosettaCallable 'Foo' on RosettaCallableCall")
	}
	
	@Test
	def void testAttributesWithLocationAndNoAddress() {
		val model ='''
			metaType scheme string
			metaType reference string
			
			type Foo:
				foo string (1..1) 
			
			type Bar:
				bar string (1..1)
					[metadata address "pointsTo"=Foo->foo]
			
		'''.parseRosetta
		model.assertError(ANNOTATION_QUALIFIER, null, "Target of address must be annotated with metadata location")
	}
	
	@Test
	@Disabled
	def void testAttributesWithLocationAndAddressWrongType() {
		val model ='''
			metaType scheme string
			metaType reference string
			type Foo:
				foo int (1..1) 
					[metadata location]
			
			type Bar:
				bar string (1..1)
					[metadata address "pointsTo"=Foo->foo]
			
		'''.parseRosetta
		model.assertError(ANNOTATION_QUALIFIER, TYPE_ERROR, "Expected address target type of 'string' but was 'int'")
	}
	

	@Test
	def void testDuplicateAttribute() {
		val model = '''
			type Foo:
				i int (1..1)
			
			type Bar extends Foo:
				i int (1..1)
		'''.parseRosetta
		model.assertNoErrors
	}
	
	@Test
	@Disabled
	def void testDuplicateAttributeNotAllowedWithDiffCard1() {
		val model = '''
			type Foo:
				i int (1..1)
			
			type Bar extends Foo:
				i int (0..1)
		'''.parseRosetta
		model.assertError(ATTRIBUTE, CARDINALITY_ERROR, "Overriding attribute 'i' with cardinality (0..1) must match the cardinality of the attribute it overrides (1..1)")
	}
	
	@Test
	@Disabled
	def void testDuplicateAttributeNotAllowedWithDiffCard2() {
		val model = '''
			type Foo:
				i int (1..1)
			
			type Bar extends Foo:
				i int (1..*)
		'''.parseRosetta
		model.assertError(ATTRIBUTE, CARDINALITY_ERROR, "Overriding attribute 'i' with cardinality (1..*) must match the cardinality of the attribute it overrides (1..1)")
	}
	
	@Test
	@Disabled
	def void testDuplicateAttributeNotAllowedWithDiffType() {
		val model = '''
			type Foo:
				i int (1..1)
			
			type Bar extends Foo:
				i string (1..1)
		'''.parseRosetta
		model.assertError(ATTRIBUTE, DUPLICATE_ATTRIBUTE, "Overriding attribute 'i' with type (string) must match the type of the attribute it overrides (int)")
	}
	

	@Test
	def void testDuplicateAttributeWithOverride() {
		val model = '''
			type A1 :
				i int (1..1)
			
			
			type A2 extends A1 :
				j int (1..1)
			
			
			
			type Foo:
				f A1 (1..1)
			
			type Bar extends Foo:
				override f A2 (1..1)
		'''.parseRosetta
		model.assertNoErrors
	}
	
	@Test
	@Disabled
	def void testDuplicateAttributeWithOverrideBadTypes() {
		val model = '''
			type A1 :
				i int (1..1)
			
			
			type A2 extends A1 :
				j int (1..1)
			
			type A3 :
				j int (1..1)
			
			type Foo:
				f A1 (1..1)
			
			type Bar extends Foo:
				override f A3 (1..1)
		'''.parseRosetta
		model.assertError(ATTRIBUTE, DUPLICATE_ATTRIBUTE, '''Overriding attribute 'f' must have a type that overrides its parent attribute type of A1''')
	}
	
	@Test
	@Disabled
	def void testDuplicateBasicTypeAttributeWithOverrideBadTypes() {
		val model = '''
			type Foo:
				i int (1..1)
			
			type Bar extends Foo:
				override i string (1..1)
		'''.parseRosetta
		
		model.assertError(ATTRIBUTE, DUPLICATE_ATTRIBUTE, '''Overriding attribute 'i' must have a type that overrides its parent attribute type of int''')
	}

	@Test
	@Disabled
	def void testDuplicateAttributeWithOverrideWithDifferentCardinality() {
		val model = '''
			type A1 :
				i int (1..1)
			
			type A2 extends A1 :
				j int (1..1)
			
			type Foo:
				f A1 (1..1)
			
			type Bar extends Foo:
				override f A2 (0..1)
		'''.parseRosetta
			
		model.assertError(ATTRIBUTE, CARDINALITY_ERROR, '''Overriding attribute 'f' with cardinality (0..1) must match the cardinality of the attribute it overrides (1..1)''')
	}

		
	@Test
	@Disabled
	def void testDuplicateAttributeWithOverrideWithUnboundedCardinality() {
		val model = '''
			type A1 :
				i int (1..1)
			
			type A2 extends A1 :
				j int (1..1)
			
			type Foo:
				f A1 (1..*)
			
			type Bar extends Foo:
				override f A2 (1..1)
		'''.parseRosetta
			
		model.assertError(ATTRIBUTE, CARDINALITY_ERROR, '''Overriding attribute 'f' with cardinality (1..1) must match the cardinality of the attribute it overrides (1..*)''')
	}
		
	@Test
	@Disabled
	def void testDuplicateEnumLiteral() {
		val model = '''
			enum Foo:
				BAR BAZ BAR
		'''.parseRosetta
		model.assertError(ROSETTA_ENUM_VALUE, DUPLICATE_ENUM_VALUE, 'Duplicate enum value')
	}
	
	@Test 
	@Disabled
	def void testDuplicateType() {
		val model = '''
			type Bar:
			
			type Foo:
			
			enum Foo: BAR
		'''.parseRosetta
		model.assertError(ROSETTA_TYPE, DUPLICATE_ELEMENT_NAME, 'Duplicate element name')
	}
		
	@Test
	def void testDuplicateChoiceRuleAttribute_thisOne() {
		val model = '''
			type Bar:
				attribute1 string (0..1)
				attribute2 string (0..1)
				attribute3 string (0..1)
			
				condition Foo:
					required choice
					attribute1, attribute1
		'''.parseRosetta
		model.assertError(CHOICE_OPERATION, null, 'Duplicate attribute.')
	}
	
	@Test
	def void testDuplicateChoiceRuleAttribute_thatOne() {
		val model = '''
			type Bar:
				attribute1 string (0..1)
				attribute2 string (0..1)
				attribute3 string (0..1)
			
			condition Foo:
				required choice attribute1 , attribute2 , attribute2
		'''.parseRosetta
		model.assertError(CHOICE_OPERATION, null, 'Duplicate attribute.')
	}

	@Test
	@Disabled
	def void shouldGenerateNoConditionNameWarning() {
		val model = '''
			type Foo:
				x string (0..1)
				
				condition:
					x exists
		'''.parseRosetta
		model.assertWarning(CONDITION, INVALID_NAME,
			"Condition name should be specified")
	}
	
	@Test
	@Disabled
	def void shouldGenerateConditionNameInvalidCaseWarning() {
		val model = '''
			type Foo:
				x string (0..1)
				
				condition xExists:
					x exists
		'''.parseRosetta
		model.assertWarning(CONDITION, INVALID_CASE,
			"Condition name should start with a capital")
	}

	@Test
	def void shouldNoGenerateErrorsForConditionWithInheritedAttributeExists() {
		val model = '''
			type Foo:
				x string (0..1)
			
			type Bar extends Foo:
				y string (0..1)
				
				condition XExists:
					x exists
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}

	@Test
	def checkMergeSynonymErrorOnSingleCardinality() {
		val model = '''
			synonym source FpML
			
			type Foo:
				attr int (0..1)
					[synonym FpML merge "bar"]
		'''.parseRosetta
		model.assertError(ROSETTA_SYNONYM_BODY, null, "Merge synonym can only be specified on an attribute with multiple cardinality.")
	}
	
	@Test
	def checkMergeSynonymNoErrorOnMultiCardinality() {
		val model = '''
			synonym source FpML
						
			type Foo:
				attr int (0..*)
					[synonym FpML merge "bar"]
		'''.parseRosetta
		model.assertNoErrors()
	}

 	@Test
	def checkMappingMultipleSetToWithoutWhenCases() {
		val model = '''
			type Quote:
				attr int (1..1)
					[synonym FIX 
							set to 1,
							set to 2]
		'''.parseRosetta
		model.assertError(ROSETTA_MAPPING, null, "Only one set to with no when clause allowed.")
	}
	
	@Test
	def checkMappingMultipleSetToOrdering() {
		val model = '''
			type Quote:
				attr int (1..1)
					[synonym FIX 
							set to 1,
							set to 2 when "a.b.c" exists]
		'''.parseRosetta
		model.assertError(ROSETTA_MAPPING, null, "Set to without when case must be ordered last.")
	}
	
	@Test
	def checkMappingSetToTypeCheck() {
		val model = '''
			type Foo:
				value0 string (1..1)
			
			type Quote:
				attr Foo (1..1)
					[synonym FIX 
							set to "hello"]
		'''.parseRosetta
		model.assertError(ROSETTA_MAPPING, null, "Set to constant type does not match type of field.")
	}
	
	@Test
	def checkMappingSetToEnumTypeCheck() {
		val model = '''
			enum Foo: ONE
			
			enum Bar: BAR
			
			type Quote:
				attr Foo (1..1)
					[synonym FIX 
							set to Bar.BAR]
		'''.parseRosetta
		model.assertError(ROSETTA_MAPPING, null, "Set to constant type does not match type of field.")
	}
	
	@Test
	def checkMappingSetToWhenTypeCheck() {
		val model = '''
			synonym source FpML
			type Foo:
				stringVal string (1..1)
			
			type Quote:
				attr Foo (1..1)
					[synonym FpML value "foo" set when "foo->bar" exists]
		'''.parseRosetta
		model.assertNoErrors
	}
	
	@Test
	def checkOperationTypes() {
		val model = '''
			type Clazz:
				test boolean (0..1)
			
				condition DataRule:
					if test = True 
						or False <> False
						or 1 > 0
						or 1 < 0
						or 1 >= 0
						or 1 <= 0
						or 1 <> 0
						or 1 = 0
					then 1.1 = .0
						and 0.2 <> 0.1
						and 0.2 > 0.1
						and 0.2 < 0.1
						and 0.2 <= 0.1
						and 0.2 >= 0.1
		'''.parseRosetta
		model.assertNoErrors
	}	
	
	@Test
	@Disabled
	def checkDateZonedDateTypes() {
		val model = '''
			recordType date{}
			recordType zonedDateTime{}
			
			func Foo:
			  inputs:
			    timestamp zonedDateTime (1..1)
			  output: result date (1..1)
			
			func Bar:
			  inputs:
			    timestamp date (1..1)
			  output: result boolean (1..1)
			  set result:
			     Foo(timestamp) = timestamp
			
		'''.parseRosetta
		model.assertError(ROSETTA_SYMBOL_REFERENCE, TYPE_ERROR, 
			"Expected type 'zonedDateTime' but was 'date'")
	}
	
	@Test
	def checkAsKeyUsage_01() {
		val model = '''
			type WithKey:
				[metadata key]
			
			type TypeToUse:
				attr WithKey (0..1)
				[metadata reference]
			
			func Bar:
			  inputs:
			    in0 WithKey (1..1)
			  output: result TypeToUse (1..1)
			  set result -> attr:
			     in0 as-key
		'''.parseRosetta
		model.assertNoErrors
	}
	
	@Test
	def checkAsKeyUsage_02() {
		val model = '''
			type WithKey:
				[metadata key]
			
			type TypeToUse:
				attr WithKey (0..1)
				[metadata reference]
				attr2 TypeToUse (0..1)
			
			func Bar:
			  inputs:
			    in0 WithKey (1..1)
			    in1 TypeToUse (1..1)
			  output: result TypeToUse (1..1)
			  set result -> attr2:
			     in1 as-key
		'''.parseRosetta
		model.assertError(SEGMENT, null,
			"'as-key' can only be used with attributes annotated with [metadata reference] annotation.")
	}
	
	@Test
	def checkAsKeyUsage_03() {
		val model = '''
			type WithKey:
			
			type TypeToUse:
				attr WithKey (0..1)
				[metadata reference]
		'''.parseRosetta
		model.assertWarning(ATTRIBUTE, null,
			"WithKey must be annotated with [metadata key] as reference annotation is used")
	}
	
	@Test
	def checkAsKeyUsage_04() {
		val model = '''
			type WithKey:
				[metadata key]
			
			type TypeToUse:
				attr WithKey (0..1)
				[metadata reference]
			
			func Bar:
			  inputs:
			    in0 WithKey (1..1)
			  output: result WithKey (1..1)
			  set result:
			     in0 as-key
		'''.parseRosetta
		model.assertError(AS_KEY_OPERATION, null,
			"'as-key' can only be used when assigning an attribute. Example: \"assign-output out -> attribute: value as-key\"")
	}
	
	@Test
	def checkSynonymPathSyntax_01() {
		val model = '''
			type TypeToUse:
				attr string (0..1)
				[synonym FpML value "adjustedDate" path "relative.date" meta id]
		'''.parseRosetta
		model.assertError(ROSETTA_SYNONYM_VALUE_BASE, null,
			"Character '.' is not allowed in paths. Use '->' to separate path segments.")
	}

	@Test
	def checkSynonymPathSyntax_02() {
		val model = '''
			type TypeToUse:
				attr string (0..1)
				[synonym FpML set to "Custom" when "Pty+Src" = "D"]
		'''.parseRosetta
		model.assertError(ROSETTA_MAP_PATH_VALUE, null,
			"Character '+' is not allowed in paths. Use '->' to separate path segments.")
	}

	@Test
	def checkChoiceConditionAttributes() {
		val model = '''
			type Bar:
				attribute1 string (0..1)
				attribute2 string (0..1)
				attribute3 string (0..1)
			
				condition:
					required choice
					attribute1
		'''.parseRosetta
		model.assertError(CHOICE_OPERATION, null,
			"At least two attributes must be passed to a choice rule.")
	}
	
	
	
	@Test
	def void externalSynonymWithFormatShouldOnlyOnDate() {
	val model='''
			type Foo:
				foo int (0..1)
			
			synonym source TEST_Base
			
			synonym source TEST extends TEST_Base {
				
				Foo:
					+ foo
						[value "bar" path "baz" dateFormat "MM/dd/yy"]
			}
		'''.parseRosetta
		model.assertError(ROSETTA_SYNONYM_BODY, null,
			"Format can only be applied to date/time types")
	}
	
	@Test
	@Disabled
	def void externalSynonymWithFormatValid() {
	val model='''
			type Foo:
				foo time (0..1)
			
			synonym source TEST_Base
			
			synonym source TEST extends TEST_Base {
				
				Foo:
					+ foo
						[value "bar" path "baz" dateFormat "MMB/dd/yy"]
			}
		'''.parseRosetta
		model.assertError(ROSETTA_SYNONYM_BODY, null,
			"Format must be a valid date/time format - Unknown pattern letter: B")
	}
	
	@Test
	def void internalSynonymWithFormatShouldOnlyBeOnDate() {
	val model='''
			type Foo:
				foo int (0..1)
				[synonym TEST_Base value "bar" path "baz" dateFormat "MM/dd/yy"]
			synonym source TEST_Base
		'''.parseRosetta
		model.assertError(ROSETTA_SYNONYM_BODY, null,
			"Format can only be applied to date/time types")
	}
	
	@Test
	def void externalSynonymCanExtendMultipleParents() {
	val model='''
			type Foo:
				foo time (0..1)
			
			synonym source TEST_Base1
			synonym source TEST_Base2
			synonym source TEST_Base3
			
			synonym source TEST extends TEST_Base1, TEST_Base2, TEST_Base3 {
			
			}
		'''.parseRosetta
		model.assertNoErrors
	}
	
	@Test
	def void internalSynonymWithPatternShouldBeValid() {
	val model='''
			type Foo:
				foo int (0..1)
				[synonym TEST_Base value "bar" path "baz" pattern "([A-Z)" "$1"]
			synonym source TEST_Base
		'''.parseRosetta
		model.assertError(ROSETTA_SYNONYM_BODY, null,
			"Pattern to match must be a valid regular expression")
	}
	
	@Disabled
	@Test
	def void testFishIsAShark() {//This test tests that when a check throws an exception it is translated into a validation error - see ExceptionValidator below
		val model='''
			type MyFish:
				foo int (0..1)
				[synonym TEST_Base value "bar" path "baz" pattern "([A-Z)" "$1"]
			synonym source TEST_Base
			
		'''.parseRosetta
		model.assertError(ROSETTA_TYPE, null,
			"checkForSharks")
	}
	
	@Test
	def void enumSynonymWithPatternShouldBeValid() {
	val model='''
			enum Enumerate : X Y Z
			
			synonym source TEST_Base
			synonym source TEST extends TEST_Base {
				
				enums
				
				Enumerate:
					+ X
						[value "bar" pattern "([A-Z)" "$1"]
			}
		'''.parseRosetta
		model.assertError(ROSETTA_ENUM_SYNONYM, null,
			"Pattern to match must be a valid regular expression")
	}
	
	@Test
	def shouldGenerateRuleCardinalityWarning() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule Aa:
				extract Bar->bar1 as "A"
			
			type Bar:
				bar1 string (0..*)
			
			type BarReport:
				aa string (1..1)
					[ruleReference Aa]
		'''.parseRosetta
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field aa has single cardinality whereas the reporting rule Aa has multiple cardinality.")
	}

	@Test
	def shouldGenerateRuleTypeError() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->barA exists
			
			reporting rule Aa:
				extract Bar->barA as "A"
			reporting rule Bb:
				extract Bar->barB as "B"
				
			reporting rule Cc:
				extract Bar->barC as "C"
			reporting rule Dd:
				extract Bar->barD as "D"
			reporting rule Ee:
				extract Bar->barE as "E"
				
			reporting rule Ff:
				extract Bar->barF as "F"
			
			type Bar:
				barA date (0..1)
				barB time (0..1)
				barC zonedDateTime (0..1)
				barD int (0..1)
				barE number (0..1)
				barF BazEnum (0..1)
			enum BazEnum:
				X
				Y
				Z
			
			type BarReport:
				aa string (1..1)
					[ruleReference Aa]
				bb string (1..1)
					[ruleReference Bb]
				cc string (1..1)
					[ruleReference Cc]
				dd string (1..1)
					[ruleReference Dd]
				ee string (1..1)
					[ruleReference Ee]
				ff string (1..1)
					[ruleReference Ff]
			
		'''.parseRosetta
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field aa has type string whereas the reporting rule Aa has type date.")
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field bb has type string whereas the reporting rule Bb has type time.")
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field cc has type string whereas the reporting rule Cc has type zonedDateTime.")
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field dd has type string whereas the reporting rule Dd has type int.")
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field ee has type string whereas the reporting rule Ee has type number.")
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field ff has type string whereas the reporting rule Ff has type BazEnum.")
	}
	
	@Test
	def shouldGenerateRuleTypeError2() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->barA exists
			
			reporting rule Aa:
			(
				extract Bar->barA as "A",
				extract Bar->barB as "B"
			)
			
			type Bar:
				barA string (0..1)
				barB number (0..1)
			
			type BarReport:
				aa string (1..1)
					[ruleReference Aa]
			
		'''.parseRosetta
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field aa has single cardinality whereas the reporting rule Aa has multiple cardinality.")
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field aa has type string whereas the reporting rule Aa has type Object.")
	}
	
	@Test
	def shouldNotGenerateRuleTypeErrorUsingReturn() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule A:
				return "Not Modelled" 
					as "A"
			
			type Bar:
				bar1 string (0..1)
			
			type BarReport:
				a string (1..1)
					[ruleReference A]
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}

	@Test
	def void shouldNotGenerateTypeValidationError() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule BarBarOne:
				(
					filter when Bar->test = True then extract Bar->bar1,
					filter when Bar->test = False then extract Bar->bar2
				)  as "1 BarOne"
			
			type Bar:
				test boolean (1..1)
				bar1 string (0..1)
				bar2 string (1..1)
			
			type BarReport:
				barBarOne string (1..1)
					[ruleReference BarBarOne]
			
		'''.parseRosetta
		model.assertNoErrors
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field barBarOne has single cardinality whereas the reporting rule BarBarOne has multiple cardinality.")
	}
	
	@Test
	def void shouldNotGenerateTypeValidationError2() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule BarBarOne:
				(
					filter when Bar->test = True then extract Bar->bar1 + Bar->bar2,
					filter when Bar->test = False then extract Bar->bar2
				)  as "1 BarOne"
			
			type Bar:
				test boolean (1..1)
				bar1 string (0..1)
				bar2 string (1..1)
			
			type BarReport:
				barBarOne string (1..1)
					[ruleReference BarBarOne]
			
		'''.parseRosetta
		model.assertNoErrors
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field barBarOne has single cardinality whereas the reporting rule BarBarOne has multiple cardinality.")
	}
	
	@Test
	def void shouldNotGenerateTypeValidationError3() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule BarBarOne:
				(
					filter when Bar->test = True then extract Bar->bar1 * Bar->bar2,
					filter when Bar->test = False then extract Bar->bar2
				)  as "1 BarOne"
			
			type Bar:
				test boolean (1..1)
				bar1 number (0..1)
				bar2 number (1..1)
			
			type BarReport:
				barBarOne number (1..1)
					[ruleReference BarBarOne]
			
		'''.parseRosetta
		model.assertNoErrors
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field barBarOne has single cardinality whereas the reporting rule BarBarOne has multiple cardinality.")
	}
	
	@Test
	def void shouldNotGenerateTypeValidationError4() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule BarBarOne:
				(
					filter when Bar->test = True then extract Bar->bar1,
					filter when Bar->test = False then extract Bar->bar2
				)  as "1 BarOne"
			
			type Bar:
				test boolean (1..1)
				bar1 number (0..1)
				bar2 int (1..1)
			
			type BarReport:
				barBarOne number (1..1)
					[ruleReference BarBarOne]
			
		'''.parseRosetta
		model.assertNoErrors
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field barBarOne has single cardinality whereas the reporting rule BarBarOne has multiple cardinality.")
	}

	@Test
	def void shouldNotGenerateTypeValidationError5() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule BarBarOne:
				(
					filter when Bar->test = True then extract Bar->bar1,
					filter when Bar->test = False then extract Bar->bar2
				)  as "1 BarOne"
			
			type Bar:
				test boolean (1..1)
				bar1 Baz (0..1)
				bar2 Baz (1..1)
			
			type Baz:
				baz1 number (1..1)
			
			type BarReport:
				barBarOne Baz (1..1)
					[ruleReference BarBarOne]
			
		'''.parseRosetta
		model.assertNoErrors
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field barBarOne has single cardinality whereas the reporting rule BarBarOne has multiple cardinality.")
	}
	
	@Test
	def void shouldGenerateTypeValidationErrorDifferentDataType() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule BarBarOne:
				(
					filter when Bar->test = True then extract Bar->bar1,
					filter when Bar->test = False then extract Bar->bar2
				)  as "1 BarOne"
			
			type Bar:
				test boolean (1..1)
				bar1 Baz (1..1)
				bar2 Qux (1..1)
			
			type Baz:
				baz1 number (1..1)
			
			type Qux:
				qux1 int (1..1)
			
			type BarReport:
				barBarOne Baz (1..1)
					[ruleReference BarBarOne]
			
		'''.parseRosetta
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Type mismatch - report field barBarOne has type Baz whereas the reporting rule BarBarOne has type Object.")
		model.assertWarning(ROSETTA_RULE_REFERENCE, null, "Cardinality mismatch - report field barBarOne has single cardinality whereas the reporting rule BarBarOne has multiple cardinality.")
	}


	@Test
	def shouldGenerateDuplicateRuleError() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule A:
				return "Not Modelled" 
					as "A"
			
			type Bar:
				bar1 string (0..1)
			
			type BarReport:
				a string (1..1)
					[ruleReference A]
				b string (1..1)
					[ruleReference A]
		'''.parseRosetta
		model.assertError(ROSETTA_RULE_REFERENCE, null, "Duplicate reporting rule A")
	}
	
	@Test
	def shouldGenerateUnsupportedCardinalityError() {
		val model = '''
			body Authority TEST_REG
			corpus TEST_REG MiFIR
			
			report TEST_REG MiFIR in T+1
			when FooRule
			with type BarReport
			
			eligibility rule FooRule:
				filter when Bar->bar1 exists
			
			reporting rule A:
				extract Bar->bar1 as "A"
			
			type Bar:
				bar1 string (0..*)
			
			type BarReport:
				a string (0..*)
					[ruleReference A]
		'''.parseRosetta
		model.assertError(ATTRIBUTE, null, "Report attributes with basic type (string) and multiple cardinality is not supported.")
	}
	
	@Test
	def shouldNotGenerateCountCardinalityErrorForMap() {
		val model = '''
			type Bar:
				foos Foo (0..*)
			type Foo:
				attr string (1..1)
			
			func FuncFoo:
			 	inputs:
			 		bars Bar (0..*)
				output:
					fooCounts int (0..*)
				
				add fooCounts:
					bars 
						map bar [ bar -> foos ]
						map foosItem [ foosItem count ]
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def shouldNotGenerateCountCardinalityErrorDefaultParameterForMap() {
		val model = '''
			type Bar:
				foos Foo (0..*)
			type Foo:
				attr string (1..1)
			
			func FuncFoo:
			 	inputs:
			 		bars Bar (0..*)
				output:
					fooCounts int (0..*)
				
				add fooCounts:
					bars 
						map [ item -> foos ]
						map [ item count ]
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def shouldNotGenerateCountCardinalityErrorForNestedMap() {
		val model = '''
			type Bar:
				foos Foo (0..*)
			type Foo:
				amount number (1..1)
			
			func FuncFoo:
			 	inputs:
			 		bars Bar (0..*)
				output:
					result boolean (1..1)
				
				alias results:
					bars -> foos
						map [ item -> amount > 0 ]
				
				set result:
					results all = True
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def shouldNotGenerateCountCardinalityErrorDefaultParameterForNestedMap() {
		val model = '''
			type Bar:
				foos Foo (0..*)
			type Foo:
				amount number (1..1)
			
			func FuncFoo:
			 	inputs:
			 		bars Bar (0..*)
				output:
					result boolean (1..1)
				
				alias results:
					bars -> foos
						map foo [ foo -> amount > 0 ]
				
				set result:
					results all = True
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}

	@Test
	def shouldNotGenerateErrorForMapListOperation() {
		val model = '''
			type Bar:
				foo Foo (1..1)
			type Foo:
				amount number (1..1)
			
			func FuncFoo:
			 	inputs:
			 		bars Bar (0..*)
				output:
					result number (1..1)
				
				set result:
					bars 
						map [ item -> foo ]
						map [ item -> amount ]
						distinct 
						only-element
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def shouldGenerateNoErrorForFeatureCallAfterListOperation() {
		val model = '''
			type Bar:
				foo Foo (1..1)
			type Foo:
				amount number (1..1)
			
			func FuncFoo:
			 	inputs:
			 		bars Bar (0..*)
				output:
					result number (1..1)
				
				set result:
					bars map [ item -> foo ] distinct only-element -> amount
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	@Disabled
	def shouldGenerateErrorForFeatureCallAfterListOperation2() {
		val model = '''
			type Bar:
				foo Foo (1..1)
			type Foo:
				amount number (1..1)
			
			func FuncFoo:
			 	inputs:
			 		bars Bar (0..*)
				output:
					result number (1..1)
				
				set result:
					if bars exists
					then bars map [ item -> foo ] distinct only-element -> amount
		'''.parseRosetta
		// then clause should generate syntax error (see test above shouldGenerateErrorForFeatureCallAfterListOperation)
		model.assertError(ROSETTA_MODEL, Diagnostic.SYNTAX_DIAGNOSTIC, "missing EOF at '->'")
	}

	@Test
	def void shouldNotGenerateCardinalityWarning() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		n1 number (0..1)
			 		n2 number (0..1)
			 		n3 number (0..1)
				output:
					result boolean (0..1)
				
				set result:
					if n1 exists and n2 exists and n3 exists
					then n1 + n2 = n3
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def void shouldNotGenerateCardinalityWarning2() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		n1 number (0..1)
			 		n2 number (0..1)
			 		n3 number (0..1)
				output:
					result boolean (0..1)
				
				alias n3Alias:
					GetNumberList( n3 ) only-element
				
				set result:
					n1 + n2 = n3Alias
					
			func GetNumberList:
				inputs:
					x number (1..1)
				output:
					xs number (0..*)
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def void shouldGenerateListFilterNoExpressionError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					filteredFoo Foo (0..*)
				
				add filteredFoo:
					foos
						filter
			
			type Foo:
				x string (1..1)
		'''.parseRosetta
		model.assertError(FILTER_OPERATION, null, "Missing a function reference.")
	}
	
	@Test
	def void shouldGenerateListFilterParametersError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					filteredFoo Foo (0..*)
				
				add filteredFoo:
					foos
						filter a, b [ a -> attr ]
			
			type Foo:
				attr boolean (1..1)
		'''.parseRosetta
		model.assertError(INLINE_FUNCTION, null, "Function must have 1 named parameter.")
	}
	
	@Test
	def void shouldGenerateListFilterExpressionTypeError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					filteredFoo Foo (0..*)
				
				add filteredFoo:
					foos
						filter [ item -> x ]
			
			type Foo:
				x string (1..1)
		'''.parseRosetta
		model.assertError(INLINE_FUNCTION, null, "Expression must evaluate to a boolean.")
	}
	
	@Test
	def void shouldGenerateListMapNoExpressionError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map
			
			type Foo:
				x string (1..1)
		'''.parseRosetta
		model.assertError(MAP_OPERATION, null, "Missing a function reference.")
	}
	
	@Test
	def void shouldGenerateListMapParametersError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map a, b [ a -> x ]
			
			type Foo:
				x string (1..1)
		'''.parseRosetta
		model.assertError(INLINE_FUNCTION, null, "Function must have 1 named parameter.")
	}
	
	@Test
	def void mapWithNamedFunctionReferenceShouldGenerateNoError() {
		val model = '''
			func DoSomething:
				inputs:
					a Foo (1..1)
				output:
					result string (1..1)
					
				set result:
					a -> x
			
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map DoSomething
			
			type Foo:
				x string (1..1)
		'''.parseRosetta
		model.assertNoIssues
	}
	
	@Test
	def void shouldGenerateListMapParametersErrorNamedFunctionReference() {
		val model = '''
			func DoSomething:
				inputs:
					a Foo (1..1)
					b boolean (1..1)
				output:
					result string (1..1)
					
				set result:
					a -> x
			
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map DoSomething
			
			type Foo:
				x string (1..1)
		'''.parseRosetta
		model.assertError(NAMED_FUNCTION_REFERENCE, null, "Function must have 1 parameter.")
	}
	
	@Test
	def void shouldNotGenerateListMapExpressionCardinalityError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map a [ a -> xs ] // list of lists
						flatten
			
			type Foo:
				xs string (0..*)
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def void shouldNotGenerateListMapExpressionCardinalityError2() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map a [ a -> bars ] // list of list<bar>
						map bars [ bars -> x ] // list of list<string> (maintain same list cardinality)
						flatten // list<string>
			
			type Foo:
				bars Bar (0..*)
			
			type Bar:
				x string (0..1)
		'''.parseRosetta
		model.assertNoIssues
	}
	
	@Test
	def void shouldNotGenerateListMapExpressionCardinalityError3() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map a [ a -> bars ] // list of list<bar>
						map bars [ bars -> bazs ] // list of list<baz>
						map bazs [ bazs -> x ] // list of list<string>
						flatten // list<string>
			
			type Foo:
				bars Bar (0..*)
			
			type Bar:
				bazs Baz (0..*)
				
			type Baz:
				x string (0..1)
		'''.parseRosetta
		model.assertNoIssues
	}
	
	@Test
	def void shouldGenerateListFlattenCardinalityError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map a [ a -> x ] // not a list of lists
						flatten
			
			type Foo:
				x string (0..1) // single cardinality
		'''.parseRosetta
		model.assertError(FLATTEN_OPERATION, null, "List flatten only allowed for list of lists.")
	}
	
	@Test
	def void shouldGenerateListFlattenCardinalityError2() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					updatedFoos Foo (0..*)
				
				add updatedFoos:
					foos
						flatten
			
			type Foo:
				x string (0..1)
		'''.parseRosetta
		model.assertError(FLATTEN_OPERATION, null, "List flatten only allowed for list of lists.")
	}
	
	@Test
	def void shouldNotGenerateListSingleCardinalityError() {
		'''
			func FuncFoo:
			 	inputs:
			 		foo Foo (1..1)
				output:
					s string (1..1)
				
				set s:
					foo
						map [ item -> x ]
			
			type Foo:
				x string (0..1)
		'''.parseRosettaWithNoIssues
	}
	
	@Test
	def void shouldGenerateListSingleCardinalityError2() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foo Foo (1..1)
				output:
					onlyFoo Foo (1..1)
				
				set onlyFoo:
					foo
						only-element
			
			type Foo:
				x string (0..1)
		'''.parseRosetta
		model.assertWarning(ROSETTA_ONLY_ELEMENT, null, "List only-element operation cannot be used for single cardinality expressions.")
	}
	
	@Test
	def void shouldGenerateListSingleCardinalityError3() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foo Foo (1..1)
				output:
					s string (1..1)
				
				set s:
					foo -> x
						only-element
			
			type Foo:
				x string (0..1)
		'''.parseRosetta
		model.assertWarning(ROSETTA_ONLY_ELEMENT, null, "List only-element operation cannot be used for single cardinality expressions.")
	}
	
	@Test
	def void shouldNotGenerateListSingleCardinalityError4() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					s string (1..1)
				
				set s:
					foos
						only-element
						map [ item -> x ]
			
			type Foo:
				x string (0..1)
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	def void shouldGenerateListUnflattenedAssignOutputError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map a [ a -> xs ] // list of lists
			
			type Foo:
				xs string (0..*)
		'''.parseRosetta
		model.assertError(OPERATION, null, "Assign expression contains a list of lists, use flatten to create a list.")
	}
	
	@Test
	def void shouldGenerateListUnflattenedSetError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				add strings:
					foos
						map a [ a -> xs ] // list of lists
			
			type Foo:
				xs string (0..*)
		'''.parseRosetta
		model.assertError(OPERATION, null, "Assign expression contains a list of lists, use flatten to create a list.")
	}
	
	@Test
	def void shouldGenerateListUnflattenedAliasError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					strings string (0..*)
				
				alias stringsAlias:
					foos
						map a [ a -> xs ] // list of lists
				
				add strings:
					stringsAlias
			
			type Foo:
				xs string (0..*)
		'''.parseRosetta
		model.assertError(SHORTCUT_DECLARATION, null, "Alias expression contains a list of lists, use flatten to create a list.")
	}
	
	@Test
	def void shouldGenerateListOnlyElementUnflattenedError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					res string (0..1)
				
				set res:
					foos
						map a [ a -> xs ] // list of lists
						only-element
			
			type Foo:
				xs string (0..*)
		'''.parseRosetta
		model.assertError(ROSETTA_ONLY_ELEMENT, null, "List must be flattened before only-element operation.")
	}
	
	@Test
	def void shouldGenerateListDistinctUnflattenedError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					res string (0..*)
				
				add res:
					foos
						map a [ a -> xs ] // list of lists
						distinct
			
			type Foo:
				xs string (0..*)
		'''.parseRosetta
		model.assertError(DISTINCT_OPERATION, null, "List must be flattened before distinct operation.")
	}
	
	@Test
	def void shouldNotGenerateTypeErrorForExpressionInBrackets() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foo Foo (1..1)
				output:
					result boolean (1..1)
				
				set result:
					( foo -> x1 and foo -> x2 ) 
					and ( foo -> x4 < 5.0 
						and ( foo -> x3 is absent or foo -> x6 exists ) )
			
			type Foo:
				x1 boolean (1..1)
				x2 boolean (1..1)
				x3 number (0..1)
				x4 number (1..1)
				x5 int (1..1)
				x6 string (0..1)
		'''.parseRosetta
		model.assertNoErrors
		model.assertNoIssues
	}
	
	@Test
	@Disabled
	def void shouldGenerateTypeErrorForExpressionInBrackets() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foo Foo (1..1)
				output:
					result boolean (1..1)
				
				set result:
					( foo -> x1 and foo -> x2 ) 
					and ( foo -> x4 // number
						and ( foo -> x3 is absent or foo -> x6 exists ) )
			
			type Foo:
				x1 boolean (1..1)
				x2 boolean (1..1)
				x3 number (1..1)
				x4 number (1..1)
				x5 int (1..1)
				x6 string (1..1)
		'''.parseRosetta
		model.assertError(ROSETTA_BINARY_OPERATION, TYPE_ERROR, "Left hand side of 'and' expression must be boolean")
	}
	
	@Test
	@Disabled
	def void shouldNotGenerateTypeErrorForExpressionInBrackets3() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foo Foo (1..1)
				output:
					result boolean (1..1)
				
				set result:
					( foo -> x3 and foo -> x4 ) exists
			
			type Foo:
				x3 number (1..1)
				x4 number (1..1)
		'''.parseRosetta
		model.assertError(ROSETTA_EXISTS_EXPRESSION, TYPE_ERROR, "Left hand side of 'and' expression must be boolean")
	}
	
	@Test
	def void shouldGenerateReduceParametersError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					res string (1..1)
				
				set res:
					foos
						reduce a [ a -> x ]
			
			type Foo:
				x string (0..1)
		'''.parseRosetta
		model.assertError(INLINE_FUNCTION, null, "Function must have 2 named parameters.")
	}
	
	@Test
	def void shouldGenerateReduceTypeError() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					res string (1..1)
				
				set res:
					foos
						reduce a, b [ a -> x ]
			
			type Foo:
				x string (0..1)
		'''.parseRosetta
		model.assertError(REDUCE_OPERATION, null, "List reduce expression must evaluate to the same type as the input. Found types Foo and String.")
	}
	
	@Test
	def void shouldGenerateReduceCardinalityError() {
		val model = '''
			type Foo:
				x string (0..1)
			
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					res Foo (1..1)
				
				set res:
					foos
						reduce a, b [ GetFooList( a, b ) ]
			
			func GetFooList:
			 	inputs:
			 		foo1 Foo (1..1)
			 		foo2 Foo (1..1)
				output:
					foos Foo (0..*)
		'''.parseRosetta
		model.assertError(INLINE_FUNCTION, null, "Operation only supports single cardinality expressions.")
	}
	
	@Test
	def void shouldGenerateListSortCardinalityError() {
		val model = '''
			type Foo:
				attrList string (1..*) // list
			
			func SortFooOnAttr:
				inputs:
					foos Foo (0..*)
				output:
					sortedFoos Foo (0..*)
			
				add sortedFoos:
					foos sort [item -> attrList] // sort based on multi-cardinality
		'''.parseRosetta
		model.assertError(INLINE_FUNCTION, null, "Operation only supports single cardinality expressions.")
	}
	
	@Test
	def void shouldGenerateListSortTypeError() {
		val model = '''
			type Foo:
				attrList string (1..*) // list
			
			func SortFooOnAttr:
				inputs:
					foos Foo (0..*)
				output:
					sortedFoos Foo (0..*)
			
				add sortedFoos:
					foos sort // sort based on Foo
		'''.parseRosetta
		model.assertError(SORT_OPERATION, null, "Operation sort only supports comparable types (string, int, string, date). Found type Foo.")
	}
	
	@Test
	def void shouldGenerateListSortTypeError2() {
		val model = '''
			type Bar:
				foo Foo (1..1)
			
			type Foo:
				attr string (1..1)
			
			func SortBarOnFoo:
				inputs:
					bars Bar (0..*)
				output:
					sortedBars Bar (0..*)
			
				add sortedBars:
					bars 
						sort x [ x -> foo ] // sort based on Foo
		'''.parseRosetta
		model.assertError(INLINE_FUNCTION, null, "Operation sort only supports comparable types (string, int, string, date). Found type Foo.")
	}
	
	@Test
	@Disabled
	def void shouldGenerateListIndexNoItemExpression() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					indexFoo Foo (0..1)
				
				set indexFoo:
					foos
						get-item [ item -> attr ]
			
			type Foo:
				attr int (1..1)
		'''.parseRosetta
		model.assertError(null, null, "List get-item does not allow expressions using an item or named parameter.")
	}
	
	@Test
	@Disabled
	def void shouldGenerateListIndexNoNamedExpression() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
				output:
					indexFoo Foo (0..1)
				
				set indexFoo:
					foos
						get-item x [ x -> attr ]
			
			type Foo:
				attr int (1..1)
		'''.parseRosetta
		model.assertError(null, null, "List get-item does not allow expressions using an item or named parameter.")
	}
	
	@Test
	@Disabled
	def void shouldGenerateListIndexNoItemExpression2() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
			 		index int (1..1)
				output:
					indexFoo Foo (0..1)
				
				set indexFoo:
					foos
						get-item [ index ]
			
			type Foo:
				attr int (1..1)
		'''.parseRosetta
		model.assertNoErrors()
	}
	
	@Test
	@Disabled
	def void shouldGenerateListIndexNoItemExpression3() {
		val model = '''
			func FuncFoo:
			 	inputs:
			 		foos Foo (0..*)
			 		index int (1..1)
				output:
					removeLast Foo (0..1)
				
				set removeLast:
					foos
						remove-index [ foos count - 1 ]
			
			type Foo:
				attr int (1..1)
		'''.parseRosetta
		model.assertNoErrors()
	}
	
	@Test
	def void joinShouldAllowExpressions() {
		'''
			func FuncFoo:
			 	inputs:
			 		stringList string (0..*)
				output:
					joined string (1..1)
				
				set joined:
					stringList
						join ("a" + "b")
			
			type Foo:
				attr int (1..1)
		'''.parseRosettaWithNoIssues
	}
	
	@Test
	@Disabled
	def void shouldWarnNonUsedImportsForData() {
		val model = '''
			import foo.bar.*
			
			
			type Foo:
				attr int (1..1)
		'''.parseRosetta
		model.assertWarning(IMPORT, UNUSED_IMPORT, "Unused import foo.bar.*")
	}
	
	
	@Test
	def void shouldNotWarnForValidDataImports() {
		val models = newArrayList('''
			namespace test.one
			
			type Foo:
				attr int (1..1)
		''',
		'''
			namespace test.two
			import test.one.*
			
			
			type Bar:
				attr Foo (1..1)
		''').parseRosetta
		
		models.forEach[assertNoIssues]
	}
	
	@Test
	def void shouldNotWarnForValidEnumImports() {
		val models = newArrayList('''
			namespace test.one
			
			enum Foo:
				A B C
		''',
		'''
			namespace test.two
			import test.one.*
			
			
			type Bar:
				attr Foo (1..1)
		''').parseRosetta
		
		models.forEach[assertNoIssues]
	}
	
	@Test
	def void shouldNotWarnForValidFuncImports() {
		val models = newArrayList('''
			namespace test.one
			
			type Foo1:
				attr int (1..1)
		''',
		'''
			namespace test.two
			
			type Foo2:
				attr int (1..1)
		''',
		'''
			namespace test.three
			import test.one.*
			import test.two.*
			
			func Bar:
				inputs:
					foo1 Foo1 (1..1)
				output:
					foo2 Foo2 (1..1)
		''').parseRosetta
		
		models.forEach[assertNoIssues]
	}
	
	@Test
	def void shouldNotWarnForValidFuncAlias() {
		val models = newArrayList('''
			namespace test.one
			
			type Foo1:
				attr int (1..1)
		''',
		'''
			namespace test.two
			import test.one.*
			
			type Foo2:
				attr Foo1 (1..1)
		''',
		'''
			namespace test.three
			import test.one.*
			
			func Bar:
				inputs:
					foo1 Foo1 (1..1)
				output:
					foo1x Foo1 (1..1)
				
				alias a: foo1 -> attr
		''').parseRosetta
		
		models.forEach[assertNoIssues]
	}
}
	
class MyRosettaInjectorProvider extends RosettaInjectorProvider {
	override createRuntimeModule() {
		return new RosettaRuntimeModule(){
			override bindClassLoaderToInstance() {
				return MyRosettaInjectorProvider
						.getClassLoader();
			}
			
			@SingletonBinding(eager=true)
			override Class<? extends RosettaValidator> bindRosettaValidator() {
				return ExceptionValidator
			}
		}
	}
}

class ExceptionValidator extends RosettaValidator{
	@Check
	def checkForSharks(Data ele) {
		if (ele.name.contains("Fish")) throw new Exception("SHARK!")
		
	}
}