package com.regnosys.rosetta.generator.python.func

import com.google.inject.Inject
import com.regnosys.rosetta.tests.RosettaInjectorProvider
import com.regnosys.rosetta.tests.util.ModelHelper

import org.eclipse.xtext.testing.InjectWith
import org.eclipse.xtext.testing.extensions.InjectionExtension
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.^extension.ExtendWith
import com.regnosys.rosetta.generator.python.PythonCodeGenerator
import static org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Disabled

/*
 * Test Principal
 */
@ExtendWith(InjectionExtension)
@InjectWith(RosettaInjectorProvider)
class PythonFunctionsTest {
	
	 @Inject extension ModelHelper
     @Inject PythonCodeGenerator generator;
	
	
	@Test
    def void testSimpleSet() {
    	val python = 
    	'''
    	func Abs: <"Returns the absolute value of a number. If the argument is not negative, the argument is returned. If the argument is negative, the negation of the argument is returned.">
    		inputs:
    		    arg number (1..1)
    		output:
    		    result number (1..1)
    		set result:
    		    if arg < 0 then -1 * arg else arg
    		set result: 
    			arg
    	'''.generatePython
    	
		val expected = 
		'''
		class Abs(ABC):
		"""
		Returns the absolute value of a number. If the argument is not negative, the argument is returned. If the argument is negative, the negation of the argument is returned.
		"""
			def evaluate(self,arg):
				result = self.doEvaluate(arg)
				return result
			
			@abstractmethod
			def doEvaluate(self,arg):
				pass
			
			
		
		class AbsDefault(Abs):
			def doEvaluate(self,arg):
				result=None
				return self.assignOutput(result,arg)
							
			def assignOutput(self,result,arg):
				def returnResult_0():
					def _then_fn0():
						return (-1 * _resolve_rosetta_attr(self, "arg"))
					def _else_fn0():
						return _resolve_rosetta_attr(self, "arg")	
					return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "arg"), "<", 0), _then_fn0, _else_fn0)
				result = returnResult_0()
				
				def returnResult_1():
					return _resolve_rosetta_attr(self, "arg")
				result = returnResult_1()
				
				return result
				
		'''
		assertTrue(python.toString.contains(expected))
	
    	
    }
    
    
	@Test
    def void testSimpleAdd() {
    	val python = 
    	'''
    	func AppendToVector: <"Append a single value to a vector (list of numbers).">
    	    inputs:
    	    	vector number (0..*) <"Input vector.">
    	    	value number (1..1) <"Value to add to the vector.">
    	    output:
    	        resultVector number (0..*) <"Resulting vector.">
    	
    	    add resultVector: vector
    	    add resultVector: value
    	'''.generatePython
    	
		val expected =
		'''
		class AppendToVector(ABC):
		"""
		Append a single value to a vector (list of numbers).
		"""
			def evaluate(self,value, vector=None):
				resultVector = self.doEvaluate(value, vector)
				return resultVector
			
			@abstractmethod
			def doEvaluate(self,value, vector=None):
				pass
			
			
		
		class AppendToVectorDefault(AppendToVector):
			def doEvaluate(self,value, vector=None):
				resultVector=[]
				return self.assignOutput(resultVector,value, vector)
							
			def assignOutput(self,resultVector,value, vector=None):
				def returnResult_0():
					return _resolve_rosetta_attr(self, "vector")
				addVar0 = returnResult_0()
				resultVector.extend(addVar0)
				
				def returnResult_1():
					return _resolve_rosetta_attr(self, "value")
				addVar1 = returnResult_1()
				resultVector.extend(addVar1)
				
				return resultVector
		'''
		assertTrue(python.toString.contains(expected))
	
    	
    }
    
    @Test
    def void testAddWithComplexType() {
    	val python = 
    	'''
    	func AppendToVector: <"Append a single value to a vector (list of numbers).">
    	    	    inputs:
    	    	    	vector A (0..*) <"Input vector.">
    	    	    	value number (1..1) <"Value to add to the vector.">
    	    	    output:
    	    	        resultVector A (0..*) <"Resulting vector.">
    	    	
    	    	    add resultVector->value1->value2: value
    	    	    add resultVector: vector
    	    	    
    	type A:
    		value1 B(0..*)
    		value2 int(1..1)
    		
    	type B:
    		value2 int(1..1)
    	'''.generatePython
    	
		val expected =
		'''
		class AppendToVectorDefault(AppendToVector):
			def doEvaluate(self,value, vector=None):
				resultVector=[]
				return self.assignOutput(resultVector,value, vector)
							
			def assignOutput(self,resultVector,value, vector=None):
				def returnResult_0():
					return _resolve_rosetta_attr(self, "value")
				addVar0 = returnResult_0()
				_set_attr("resultVector",_set_attr("value1",_set_attr("value2",returnResult_0)))
				
				def returnResult_1():
					return _resolve_rosetta_attr(self, "vector")
				addVar1 = returnResult_1()
				resultVector.extend(addVar1)
				
				return resultVector
		'''
		assertTrue(python.toString.contains(expected))
	
    	
    }
    
    @Test
    def void testSetWithComplexType() {
    	val python =  
    	'''
    	func Create_UnitType: <"Create UnitType with given currency or financial unit.">
    		inputs:
    			currency string (0..1)
    				[metadata scheme]
    			financialUnit FinancialUnitEnum (0..1)
    		output:
    			unitType UnitType (1..1)
    	
    		condition CurrencyOrFinancialUnitExists:
    			currency exists or financialUnit exists
    	
    		set unitType -> currency: currency
    		set unitType -> financialUnit: financialUnit
    	enum FinancialUnitEnum: <"Provides enumerated values for financial units, generally used in the context of defining quantities for securities.">
    		Contract <"Denotes financial contracts, such as listed futures and options.">
    		ContractualProduct <"Denotes a Contractual Product as defined in the CDM.  This unit type would be used when the price applies to the whole product, for example, in the case of a premium expressed as a cash amount.">
    		IndexUnit <"Denotes a price expressed in index points, e.g. for a stock index.">
    		LogNormalVolatility <"Denotes a log normal volatility, expressed in %/month, where the percentage is represented as a decimal. For example, 0.15 means a log-normal volatility of 15% per month.">
    		Share <"Denotes the number of units of financial stock shares.">
    		ValuePerDay <"Denotes a value (expressed in currency units) for a one day change in a valuation date, which is typically used for expressing sensitivity to the passage of time, also known as theta risk, or carry, or other names.">
    		ValuePerPercent <"Denotes a value (expressed in currency units) per percent change in the underlying rate which is typically used for expressing sensitivity to volatility changes, also known as vega risk.">
    		Weight <"Denotes a quantity (expressed as a decimal value) represented the weight of a component in a basket.">
    	type UnitType: <"Defines the unit to be used for price, quantity, or other purposes">
    		capacityUnit CapacityUnitEnum (0..1) <"Provides an enumerated value for a capacity unit, generally used in the context of defining quantities for commodities.">
    		weatherUnit WeatherUnitEnum (0..1) <"Provides an enumerated values for a weather unit, generally used in the context of defining quantities for commodities.">
    		financialUnit FinancialUnitEnum (0..1) <"Provides an enumerated value for financial units, generally used in the context of defining quantities for securities.">
    		currency string (0..1) <"Defines the currency to be used as a unit for a price, quantity, or other purpose.">
    			[metadata scheme]
    	
    		condition UnitType: <"Requires that a unit type must be set.">
    			one-of
    	enum CapacityUnitEnum: <"Provides enumerated values for capacity units, generally used in the context of defining quantities for commodities.">
    	    ALW <"Denotes Allowances as standard unit.">
    	    BBL <"Denotes a Barrel as a standard unit.">
    	    BCF <"Denotes Billion Cubic Feet as a standard unit.">
    	    BDFT <"Denotes Board Feet as a standard unit.">
    	    CBM <"Denotes Cubic Meters as a standard unit.">
    	    CER <"Denotes Certified Emissions Reduction as a standard unit.">
    	    CRT <"Denotes Climate Reserve Tonnes as a standard unit.">
    	    DAG <"Denotes 10 grams as a standard unit used in precious metals contracts (e.g MCX).">
    	    DAY <"Denotes a single day as a standard unit used in time charter trades.">
    	    DMTU <"Denotes Dry Metric Ton (Tonne) Units - Consists of a metric ton of mass excluding moisture.">
    	    ENVCRD <"Denotes Environmental Credit as a standard unit.">
    	    ENVOFST <"Denotes Environmental Offset as a standard unit.">
    	    FEU <"Denotes a 40 ft. Equivalent Unit container as a standard unit.">
    	    G <"Denotes a Gram as a standard unit.">
    	    GBBSH <"Denotes a GB Bushel as a standard unit.">
    	    GBBTU <"Denotes a GB British Thermal Unit as a standard unit.">
    	    GBCWT <"Denotes a GB Hundredweight unit as standard unit.">
    	    GBGAL <"Denotes a GB Gallon unit as standard unit.">
    	    GBMBTU <"Denotes a Thousand GB British Thermal Units as a standard unit.">
    	    GBMMBTU <"Denotes a Million GB British Thermal Units as a standard unit.">
    	    GBT <"Denotes a GB Ton as a standard unit.">
    	    GBTHM <"Denotes a GB Thermal Unit as a standard unit.">
    	    GJ <"Denotes a Gigajoule as a standard unit.">
    	    GW <"Denotes a Gigawatt as a standard unit.">
    	    GWH <"Denotes a Gigawatt-hour as a standard unit.">
    	    HL <"Denotes a Hectolitre as a standard unit.">
    	    HOGB <"Denotes a 100-troy ounces Gold Bar as a standard unit.">
    	    ISOBTU <"Denotes an ISO British Thermal Unit as a standard unit.">
    	    ISOMBTU <"Denotes a Thousand ISO British Thermal Unit as a standard unit.">
    	    ISOMMBTU <"Denotes a Million ISO British Thermal Unit as a standard unit.">
    	    ISOTHM <"Denotes an ISO Thermal Unit as a standard unit.">
    	    KG <"Denotes a Kilogram as a standard unit.">
    	    KL <"Denotes a Kilolitre as a standard unit.">
    	    KW <"Denotes a Kilowatt as a standard unit.">
    	    KWD <"Denotes a Kilowatt-day as a standard unit.">
    	    KWH <"Denotes a Kilowatt-hour as a standard unit.">
    	    KWM <"Denotes a Kilowatt-month as a standard unit.">
    	    KWMIN <"Denotes a Kilowatt-minute as a standard unit.">
    	    KWY <"Denotes a Kilowatt-year as a standard unit.">
    	    L <"Denotes a Litre as a standard unit.">
    	    LB <"Denotes a Pound as a standard unit.">
    	    MB <"Denotes a Thousand Barrels as a standard unit.">
    	    MBF <"Denotes a Thousand board feet, which are used in contracts on forestry underlyers as a standard unit.">
    	    MJ <"Denotes a Megajoule as a standard unit.">
    	    MMBF <"Denotes a Million board feet, which are used in contracts on forestry underlyers as a standard unit.">
    	    MMBBL <"Denotes a Million Barrels as a standard unit.">
    	    MSF <"Denotes a Thousand square feet as a standard unit.">
    	    MT <"Denotes a Metric Ton as a standard unit.">
    	    MW <"Denotes a Megawatt as a standard unit.">
    	    MWD <"Denotes a Megawatt-day as a standard unit.">
    	    MWH <"Denotes a Megawatt-hour as a standard unit.">
    	    MWM <"Denotes a Megawatt-month as a standard unit.">
    	    MWMIN <"Denotes a Megawatt-minute as a standard unit.">
    	    MWY <"Denotes a Megawatt-year as a standard unit.">
    	    OZT <"Denotes a Troy Ounce as a standard unit.">
    	    SGB <"Denotes a Standard Gold Bar as a standard unit.">
    	    TEU <"Denotes a 20 ft. Equivalent Unit container as a standard unit.">
    	    USBSH <"Denotes a US Bushel as a standard unit.">
    	    USBTU <"Denotes a US British Thermal Unit as a standard unit.">
    	    USCWT <"Denotes US Hundredweight unit as a standard unit.">
    	    USGAL <"Denotes a US Gallon unit as a standard unit.">
    	    USMBTU <"Denotes a Thousand US British Thermal Units as a standard unit.">
    	    USMMBTU <"Denotes a Million US British Thermal Units as a standard unit.">
    	    UST <"Denotes a US Ton as a standard unit.">
    	    USTHM <"Denotes a US Thermal Unit as a standard unit.">
    	enum WeatherUnitEnum: <"Provides enumerated values for weather units, generally used in the context of defining quantities for commodities.">
    		CDD <"Denotes Cooling Degree Days as a standard unit.">
    		CPD <"Denotes Critical Precipitation Day as a standard unit.">
    		HDD <"Heating Degree Day as a standard unit.">
    	'''.generatePython
    	
		val expected =
		'''
		class Create_UnitTypeDefault(Create_UnitType):
			def doEvaluate(self,currency=None, financialUnit=None):
				unitType=UnitType()
				return self.assignOutput(unitType,currency, financialUnit)
							
			def assignOutput(self,unitType,currency=None, financialUnit=None):
				def returnResult_0():
					return _resolve_rosetta_attr(self, "currency")
				_set_attr("unitType",_set_attr("currency",returnResult_0)) 
				
				def returnResult_1():
					return _resolve_rosetta_attr(self, "financialUnit")
				_set_attr("unitType",_set_attr("financialUnit",returnResult_1)) 
				
				return unitType
		'''
		assertTrue(python.toString.contains(expected))
    }
    
    @Test
    def void testAddAndSet() {
    	val python = 
    	'''
    	func ResolvePerformanceReset: <"Defines how to resolve the reset value for a performance payout.">
    		inputs:
    			observation Observation (1..1) <"Represents the observation that will be used to compute the reset value.">
    			date date (1..1) <"Specifies the date of the reset.">
    		output:
    			reset Reset (1..1)
    		set reset -> resetValue: <"Assigns the observed value to the reset value.">
    			observation -> observedValue
    		set reset -> resetDate:
    			date
    		add reset -> observations: <"Assigns the observation required to compute the rest value as audit.">
    			observation
    	type Reset: <"Defines the reset value or fixing value produced in cashflow calculations, during the life-cycle of a financial instrument. The reset process defined in Create_Reset function joins product definition details with observations to compute the reset value.">
    		[metadata key]
    		resetValue Price (1..1) <"Specifies the reset or fixing value. The fixing value could be a cash price, interest rate, or other value.">
    		resetDate date (1..1) <"Specifies the date on which the reset occurred.">
    		observations Observation (1..*)
    	type Price : 
    		price int(1..1)
    	
    	type Observation: <"Defines a single, numerical value that was observed in the marketplace. Observations of market data are made independently to business events or trade life-cycle events, so data instances of Observation can be created independently of any other model type, hence it is annotated as a root type. Observations will be broadly reused in many situations, so references to Observation are supported via the 'key' annotation.">
    		[rootType]
    		[metadata key]
    		observedValue Price (1..1) <"Specifies the observed value as a number.">
    	'''.generatePython
    	
		val expected = 
		'''
		class ResolvePerformanceReset(ABC):
		"""
		Defines how to resolve the reset value for a performance payout.
		"""
			def evaluate(self,observation, date):
				reset = self.doEvaluate(observation, date)
				return reset
			
			@abstractmethod
			def doEvaluate(self,observation, date):
				pass
			
			
		
		class ResolvePerformanceResetDefault(ResolvePerformanceReset):
			def doEvaluate(self,observation, date):
				reset=Reset()
				return self.assignOutput(reset,observation, date)
							
			def assignOutput(self,reset,observation, date):
				def returnResult_0():
				"""
				Assigns the observed value to the reset value.
				"""
					return _resolve_rosetta_attr(_resolve_rosetta_attr(self, "observation"), "observedValue")
				_set_attr("reset",_set_attr("resetValue",returnResult_0)) 
				
				def returnResult_1():
					return _resolve_rosetta_attr(self, "date")
				_set_attr("reset",_set_attr("resetDate",returnResult_1)) 
				
				def returnResult_2():
				"""
				Assigns the observation required to compute the rest value as audit.
				"""
					return _resolve_rosetta_attr(self, "observation")
				addVar2 = returnResult_2()
				_set_attr("reset",_set_attr("observations",returnResult_2))
				
				return reset
		'''
		assertTrue(python.toString.contains(expected))
		
    }
    
    @Test
    def void testFilterOperation() {
    	val python =
    	'''
    	func FilterQuantity: <"Filter list of quantities based on unit type.">
    	    	    inputs:
    	    	        quantities Quantity (0..*) <"List of quantities to filter.">
    	    	        unit UnitType (1..1) <"Currency unit type.">
    	    	    output:
    	    	        filteredQuantities Quantity (0..*)
    	    	
    	    	    add filteredQuantities:
    	    	        quantities
    	    	            filter item -> unit = unit
    	    	type Quantity: <"Specifies a quantity as a single value to be associated to a financial product, for example a transfer amount resulting from a trade. This data type extends QuantitySchedule and requires that only the single amount value exists.">
    	    		value number (0..1) <"Specifies the value of the measure as a number. Optional because in a measure vector or schedule, this single value may be omitted.">
    	    		unit UnitType (0..1) <"Qualifies the unit by which the amount is measured. Optional because a measure may be unit-less (e.g. when representing a ratio between amounts in the same unit).">
    	  		type UnitType: <"Defines the unit to be used for price, quantity, or other purposes">
    	  			value int (1..1)
    	'''.generatePython
    	
		val expected =
		'''
		class FilterQuantity(ABC):
		"""
		Filter list of quantities based on unit type.
		"""
			def evaluate(self,unit, quantities=None):
				filteredQuantities = self.doEvaluate(unit, quantities)
				return filteredQuantities
			
			@abstractmethod
			def doEvaluate(self,unit, quantities=None):
				pass
			
			
		
		class FilterQuantityDefault(FilterQuantity):
			def doEvaluate(self,unit, quantities=None):
				filteredQuantities=[]
				return self.assignOutput(filteredQuantities,unit, quantities)
							
			def assignOutput(self,filteredQuantities,unit, quantities=None):
				def returnResult_0():
					return list(filter(lambda _resolve_rosetta_attr(self, "quantities"):all_elements(unit, "=", _resolve_rosetta_attr(self, "unit")),_resolve_rosetta_attr(self, "quantities")))
				addVar0 = returnResult_0()
				filteredQuantities.extend(addVar0)
				
				return filteredQuantities
		'''
		assertTrue(python.toString.contains(expected))
		
    }
    
    @Test
    def void testWithEnumAttr() {
    	//Test a set output constructor having attributes as enums
    	
    	val python =
    	'''
    	enum ArithmeticOperationEnum: <"An arithmetic operator that can be passed to a function">
    	    Add <"Addition">
    	    Subtract <"Subtraction">
    	    Multiply <"Multiplication">
    	    Divide <"Division">
    	    Max <"Max of 2 values">
    	    Min <"Min of 2 values">
    	
    	func ArithmeticOperation:
    		inputs:
    	        n1 number (1..1)
    	        op ArithmeticOperationEnum (1..1)
    	        n2 number (1..1)
    		output:
    	        result number (1..1)
    	
    		set result:
    	        if op = ArithmeticOperationEnum -> Add then
    	            n1 + n2
    	        else if op = ArithmeticOperationEnum -> Subtract then
    	            n1 - n2
    	        else if op = ArithmeticOperationEnum -> Multiply then
    	            n1 * n2
    	        else if op = ArithmeticOperationEnum -> Divide then
    	            n1 / n2
    	        else if op = ArithmeticOperationEnum -> Max then
    	            Max( n1, n2 )
    	        else if op = ArithmeticOperationEnum -> Min then
    	            Min( n1, n2 )
    	'''.generatePython
    	
		val expected =
		'''
		class ArithmeticOperationDefault(ArithmeticOperation):
			def doEvaluate(self,n1, op, n2):
				result=None
				return self.assignOutput(result,n1, op, n2)
							
			def assignOutput(self,result,n1, op, n2):
				def returnResult_0():
					def _then_fn5():
						return Min(_resolve_rosetta_attr(self, "n1"), _resolve_rosetta_attr(self, "n2"))
					def _else_fn5():
						return True	
					def _then_fn4():
						return Max(_resolve_rosetta_attr(self, "n1"), _resolve_rosetta_attr(self, "n2"))
					def _else_fn4():
						return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "op"), "=", _resolve_rosetta_attr(ArithmeticOperationEnum, "MIN")), _then_fn5, _else_fn5)	
					def _then_fn3():
						return (_resolve_rosetta_attr(self, "n1") / _resolve_rosetta_attr(self, "n2"))
					def _else_fn3():
						return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "op"), "=", _resolve_rosetta_attr(ArithmeticOperationEnum, "MAX")), _then_fn4, _else_fn4)	
					def _then_fn2():
						return (_resolve_rosetta_attr(self, "n1") * _resolve_rosetta_attr(self, "n2"))
					def _else_fn2():
						return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "op"), "=", _resolve_rosetta_attr(ArithmeticOperationEnum, "DIVIDE")), _then_fn3, _else_fn3)	
					def _then_fn1():
						return (_resolve_rosetta_attr(self, "n1") - _resolve_rosetta_attr(self, "n2"))
					def _else_fn1():
						return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "op"), "=", _resolve_rosetta_attr(ArithmeticOperationEnum, "MULTIPLY")), _then_fn2, _else_fn2)	
					def _then_fn0():
						return (_resolve_rosetta_attr(self, "n1") + _resolve_rosetta_attr(self, "n2"))
					def _else_fn0():
						return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "op"), "=", _resolve_rosetta_attr(ArithmeticOperationEnum, "SUBTRACT")), _then_fn1, _else_fn1)	
					return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "op"), "=", _resolve_rosetta_attr(ArithmeticOperationEnum, "ADD")), _then_fn0, _else_fn0)
				result = returnResult_0()
				
				return result
		'''
    	assertTrue(python.toString.contains(expected))
    }
     
    @Disabled
    def void testFilterOperation2() {
    	val python =
    	'''
    	func FilterQuantityByCurrencyExists: <"Filter list of quantities based on unit type.">
    	    inputs:
    	        quantities QuantitySchedule (0..*) <"List of quantities to filter.">
    	    output:
    	        filteredQuantities QuantitySchedule (0..*)
    	
    	    add filteredQuantities:
    	        quantities
    	            filter item -> unit -> currency exists
    	type QuantitySchedule: <"Specifies a quantity as a single value to be associated to a financial product, for example a transfer amount resulting from a trade. This data type extends QuantitySchedule and requires that only the single amount value exists.">
    	    	value number (0..1) <"Specifies the value of the measure as a number. Optional because in a measure vector or schedule, this single value may be omitted.">
    	    	unit UnitType (0..1) <"Qualifies the unit by which the amount is measured. Optional because a measure may be unit-less (e.g. when representing a ratio between amounts in the same unit).">
    	type UnitType: <"Defines the unit to be used for price, quantity, or other purposes">
    	  		currency string(0..1)
    	'''.generatePython
    }
    
    @Test 
    def void testAlias1() {
    	//Test alias with simple types
    	
    	val python =
    	'''
    	func testAlias:
    		inputs:
    			inp1 number(1..1)
    			inp2 number(1..1)
    		output:
    			result number(1..1)
    		alias Alias:
    			if inp1 < 0 then inp1
    			
    		set result:
    			Alias
    	'''.generatePython
    	
		val expected =
		'''
		lass testAlias(ABC):
			def evaluate(self,inp1, inp2):
				result = self.doEvaluate(inp1, inp2)
				return result
			
			@abstractmethod
			def doEvaluate(self,inp1, inp2):
				pass
			
			@abstractmethod
			def Alias(self,inp1, inp2):
				pass
			
		
		class testAliasDefault(testAlias):
			def doEvaluate(self,inp1, inp2):
				result=None
				return self.assignOutput(result,inp1, inp2)
							
			def assignOutput(self,result,inp1, inp2):
				def returnResult_0():
					return self.Alias(inp1, inp2)
				result = returnResult_0()
				
				return result
				
			def Alias(self,inp1, inp2):
				def _then_fn0():
					return _resolve_rosetta_attr(self, "inp1")
				def _else_fn0():
					return True	
				return if_cond_fn(all_elements(_resolve_rosetta_attr(self, "inp1"), "<", 0), _then_fn0, _else_fn0)
		'''
		assertTrue(python.toString.contains(expected))
    	
    }
    
    @Test
    def void testAlias2() {
    	//Test alias with complex types
    	val python =
    	'''
    	type A:
    		valueA number(1..1)
    	type B:
        	valueB number(1..1)
    	type C:
    	    valueC number(1..1)
    	func testAlias:
       		inputs:
 	  			a A (1..1)
    	    	b B (1..1)
    	    output:
    	    	c C (1..1)
    	    alias Alias1:
    	    	a->valueA
    	    alias Alias2:
    	    	b->valueB
    	    set c->valueC:
    	    	Alias1*Alias2
    	'''.generatePython
    	
    	val expected = 
    	'''
    	class testAliasDefault(testAlias):
    		def doEvaluate(self,a, b):
    			c=C()
    			return self.assignOutput(c,a, b)
    						
    		def assignOutput(self,c,a, b):
    			def returnResult_0():
    				return (self.Alias1(a, b) * self.Alias2(a, b))
    			_set_attr("c",_set_attr("valueC",returnResult_0)) 
    			
    			return c
    			
    		def Alias1(self,a, b):
    			return _resolve_rosetta_attr(_resolve_rosetta_attr(self, "a"), "valueA")
    		
    		def Alias2(self,a, b):
    			return _resolve_rosetta_attr(_resolve_rosetta_attr(self, "b"), "valueB")
    	'''
    	
    	assertTrue(python.toString.contains(expected))
    	
    }
    
    @Test
    def void testComplexSetConstructors() {
    	//Test function with complex attributes in the set constructors, that means, sets that 
    	//goes from attribute to attributes (attribute1->attribute2)
    	val python =
    	'''
    	type InterestRatePayout: <" A class to specify all of the terms necessary to define and calculate a cash flow based on a fixed, a floating or an inflation index rate. The interest rate payout can be applied to interest rate swaps and FRA (which both have two associated interest rate payouts), credit default swaps (to represent the fee leg when subject to periodic payments) and equity swaps (to represent the funding leg). The associated globalKey denotes the ability to associate a hash value to the InterestRatePayout instantiations for the purpose of model cross-referencing, in support of functionality such as the event effect and the lineage.">
    		[metadata key]
    		rateSpecification RateSpecification (0..1) <"The specification of the rate value(s) applicable to the contract using either a floating rate calculation, a single fixed rate, a fixed rate schedule, or an inflation rate calculation.">
    	
    	type RateSpecification: <" A class to specify the fixed interest rate, floating interest rate or inflation rate.">
    		floatingRate FloatingRateSpecification (0..1) <"The floating interest rate specification, which includes the definition of the floating rate index. the tenor, the initial value, and, when applicable, the spread, the rounding convention, the averaging method and the negative interest rate treatment.">
    	
    	type FloatingRateSpecification: <"A class defining a floating interest rate through the specification of the floating rate index, the tenor, the multiplier schedule, the spread, the qualification of whether a specific rate treatment and/or a cap or floor apply.">
    		[metadata key]
    	
    	    rateOption FloatingRateOption (0..1)
    	
    	type FloatingRateOption: <"Specification of a floating rate option as a floating rate index and tenor.">
    		value int(1..1)
    	
    	type ObservationIdentifier:	<"Defines the parameters needed to uniquely identify a piece of data among the population of all available market data.">
    		observable Observable (1..1) <"Represents the asset or rate to which the observation relates.">
    		observationDate date (1..1) <"Specifies the date value to use when resolving the market data.">
    	
    	type Observable: <"Specifies the object to be observed for a price, it could be an asset or a reference.">
    		[metadata key]
    	
    		rateOption FloatingRateOption (0..1) <"Specifies a floating rate index and tenor.">
    		
    	func ResolveInterestRateObservationIdentifiers: <"Defines which attributes on the InterestRatePayout should be used to locate and resolve the underlier's price, for example for the reset process.">
    		inputs:
    			payout InterestRatePayout (1..1)
    			date date (1..1)
    		output:
    			identifiers ObservationIdentifier (1..1)
    	
    		set identifiers -> observable -> rateOption:
    			payout -> rateSpecification -> floatingRate -> rateOption
    		set identifiers -> observationDate:
    			date
    	'''.generatePython
    	
		val expected = 
		'''
		class ResolveInterestRateObservationIdentifiersDefault(ResolveInterestRateObservationIdentifiers):
			def doEvaluate(self,payout, date):
				identifiers=ObservationIdentifier()
				return self.assignOutput(identifiers,payout, date)
							
			def assignOutput(self,identifiers,payout, date):
				def returnResult_0():
					return _resolve_rosetta_attr(_resolve_rosetta_attr(_resolve_rosetta_attr(_resolve_rosetta_attr(self, "payout"), "rateSpecification"), "floatingRate"), "rateOption")
				_set_attr("identifiers",_set_attr("observable",_set_attr("rateOption",returnResult_0))) 
				
				def returnResult_1():
					return _resolve_rosetta_attr(self, "date")
				_set_attr("identifiers",_set_attr("observationDate",returnResult_1)) 
				
				return identifiers
		'''
		assertTrue(python.toString.contains(expected))
    	
    }
    
    @Test
    def void testOneCondition() {
    	val python =
    	'''
    	func RoundToNearest:
    		inputs:
    			value number (1..1)
    			nearest number (1..1)
    			roundingMode RoundingModeEnum (1..1)
    		output:
    			roundedValue number (1..1)
    		condition PositiveNearest:
    			nearest > 0
    	enum RoundingModeEnum:
    		Down
    		Up
    	'''.generatePython
    	
    	val expected = 
    	'''
    	class RoundToNearest(ABC):
    		def evaluate(self,value, nearest, roundingMode):
    			def PositiveNearest():
    				return all_elements(_resolve_rosetta_attr(self, "nearest"), ">", 0)
    			if not PositiveNearest():
    				raise Exception("Error")
    			roundedValue = self.doEvaluate(value, nearest, roundingMode)
    			return roundedValue
    	'''
    	assertTrue(python.toString.contains(expected))
    }
    
    @Test
    def void testMultipleConditions() {
    	val python =
    	'''
    	func RoundToNearest:
    		inputs:
    			value number (1..1)
    			nearest number (1..1)
    			roundingMode RoundingModeEnum (1..1)
    		output:
    			roundedValue number (1..1)
    		condition PositiveNearest:
    			nearest > 0
    		condition valueNegative:
    		    value < 0
    	enum RoundingModeEnum:
    		Down
    		Up
    	'''.generatePython
    	
    	val expected =
    	'''
    	class RoundToNearest(ABC):
    		def evaluate(self,value, nearest, roundingMode):
    			def PositiveNearest():
    				return all_elements(_resolve_rosetta_attr(self, "nearest"), ">", 0)
    			def valueNegative():
    				return all_elements(_resolve_rosetta_attr(self, "value"), "<", 0)
    			if not PositiveNearest():
    				raise Exception("Error")
    			if not valueNegative():
    				raise Exception("Error")
    			roundedValue = self.doEvaluate(value, nearest, roundingMode)
    			return roundedValue
    	'''
    	assertTrue(python.toString.contains(expected))
    }
    
    @Test
    def void testPostCondition() {
    	val python =
    	'''
    	func NewFloatingPayout: <"Function specification to create the interest rate (floating) payout part of an Equity Swap according to the 2018 ISDA CDM Equity Confirmation template.">
    		inputs: masterConfirmation EquitySwapMasterConfirmation2018 (0..1)
    		output: interestRatePayout InterestRatePayout (1..1)
    	
    		post-condition InterestRatePayoutTerms: <"Interest rate payout must inherit terms from the Master Confirmation Agreement when it exists.">
    			if masterConfirmation exists then 
    			//interestRatePayout -> calculationPeriodDates = masterConfirmation -> equityCalculationPeriod and 
    			interestRatePayout -> paymentDates = masterConfirmation -> equityCashSettlementDates
    	type EquitySwapMasterConfirmation2018:
    		equityCashSettlementDates PaymentDates (1..1) 
    	type PaymentDates:
    		date date(0..1)
    	type InterestRatePayout:
    		paymentDates PaymentDates(0..1)
    	'''.generatePython
 
    	val expected =
    	'''
    	class NewFloatingPayout(ABC):
    	"""
    	Function specification to create the interest rate (floating) payout part of an Equity Swap according to the 2018 ISDA CDM Equity Confirmation template.
    	"""
    		def evaluate(self,masterConfirmation=None):
    			interestRatePayout = self.doEvaluate(masterConfirmation)
    			def InterestRatePayoutTerms():
    				def _then_fn0():
    					return all_elements(_resolve_rosetta_attr(_resolve_rosetta_attr(self, "interestRatePayout"), "paymentDates"), "=", _resolve_rosetta_attr(_resolve_rosetta_attr(self, "masterConfirmation"), "equityCashSettlementDates"))
    				def _else_fn0():
    					return True	
    				return if_cond_fn(((_resolve_rosetta_attr(self, "masterConfirmation")) is not None), _then_fn0, _else_fn0)
    			if not InterestRatePayoutTerms():
    				raise Exception("Interest rate payout must inherit terms from the Master Confirmation Agreement when it exists.")
    			return interestRatePayout
    	'''
    	assertTrue(python.toString.contains(expected))
    	
    }
    

	def generatePython(CharSequence model) {
		val m = model.parseRosetta
        val resourceSet = m.eResource.resourceSet
        val version = m.version
        
        val result = newHashMap
        result.putAll(generator.beforeAllGenerate(resourceSet, #{m}, version))
        result.putAll(generator.beforeGenerate(m.eResource, m, version))
        result.putAll(generator.generate(m.eResource, m, version))
        result.putAll(generator.afterGenerate(m.eResource, m, version))
        result.putAll(generator.afterAllGenerate(resourceSet, #{m}, version))
        
        result
    }
    
    
    
	
	
	
	
}
