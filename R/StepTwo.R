HTML("<h3> Profit Estimation Overview </h3>
      <p> If the mapping component runs successfully, a total estimated revenue value will be calculated for use in the 'Profit Estimation' 
      section. This portion of the tool contains the same calculations provided by the FFED Extension Excel tool for estimating the pre-tax profit
      of opening up a grocery store in the designated area.</p>
      
      <h4> Calculations </h4>
      <p> The user should initially select one of the two scenario options which provide different estimations and user inputs.</p>
      
      <h5> Scenario One - Owned Building </h5> 
      <p> The building is owned by the business that is operating the store. In this case the building would generally be 
      depreciated over 39 years, unless the building's useful life is believed to be less than 39 years.
      Values which are specific to scenario one are;
      
      <ul>
      <li>Building Remodel Costs - cost of remodeling purchased grocery store</li>
      <li>Parking Lot Improvements - cost for parking lot improvements</li>
      </ul>
      
      </p>
      
      <h5> Scenario Two - Leased Building </h5> 
      <p> The building is owned by a third party and periodic rent is paid, typically monthly. 
      In this case there would be no depreciation. Values which are specific to scenario two are;
      
      <ul> 
      <li>Monthly Rent - rent cost per month</li>
      <li>Leasehold Improvements - cost of improving rented grocery store</li>
      <li>Leasehold Improvements Use Life - use life of leasehold improvements in years</li>
      </ul>
      
      </p>
      
      <h5> Shared Inputs </h5>
      <p> The following are a list of inputs and short definitions which are shared between the two 
      presented scenarios available in the calculation tool. </p> 
      
      <ul> 
      <li>Loan Amount - amount taken for loan</li>
      <li>Interest Rate - interest on the loan as a percentage</li> 
      <li>Shelving Check Out Counters - shelving and register infrastructure</li>
      <li>Computer Equipment POS - computer equipment costs</li>
      <li>Vehicles - vehicle purchase costs</li>
      <li>Display Cases - display cases purchase and setup costs</li>
      <li>Refrigeration - refrigeration purchase and setup costs</li>
      <li>Freezers - freezer purchase and setup costs</li>
      <li>Meat Cutting Equipment - meat cutting equipment purchase and setup costs</li>
      <li>Miscellaneous Assets - an optional asset to evaluate</li>
      <li>Miscellaneous Asset Life - use life of asset in years</li>
      </ul>
      </p>
      
      <h5> Percentages </h5>
      <p> 
      
      <ul> 
      <li>Gross Margin Percentage - percentage of total estimated revenue after subtracting cost of goods</li>
      <li>Employee Wages Percentage - percentage of the total estimated revenue spent on employee wages</li> 
      <li>Officer Compensation Percentage - percentage of the total estimated revenue spent on officer compensation</li>
      <li>Other Operating Expense Percentage - percentage of the total estimated revenue spent on operating expenses</li>
      <li>Other Income Percentage - percentage of other income constituting secondary income</li>
      <li>Interest Income Percentage - percentage of interest income constituting secondary income</li>
      </ul>
      
      </p>
      
      <h5> Informational Value Boxes </h5>
      <p> As the user inputs values in each of the sections and adjusts the sliders for percentages,
      the value boxes on the right hand side should adjust dynamically, allowing the user to select values
      and percentages based on their needs. The value box names and their definitions are;
      
      <ul> 
      <li>Pre-Tax Profit - Total Estimated Revenue + Secondary Income - Expenses</li>
      <li>Total Estimated Revenue - Determined by metro, town, rural populations and market size</li> 
      <li>Gross Margin - Total Estimated Revenue - Cost of Goods Sold</li>
      <li>Secondary Income - Other Income + Interest Income</li>
      <li>Depreciation Costs - Varies based on scenario but the sum of all asset costs</li>
      <li>Expenses - The sum of Cost of Goods Sold, Wages, Operating and Interest Expenses, and Depreciation costs</li>
      </ul>
      </p>
      
      <h5> Potential Errors </h5>
      <p> There are usually two things that can go wrong with this section. The first is that the values don't load up in the 
      value boxes and the second is values which come out as infinities in the value boxes. The following are potential troubleshoots.
      
      
      <ol> 
      <li>Refresh the 'Profit Estimation' tab if values don't update</li>
      <li>Remove any 0's in the use life inputs since these values get divided and therefore cause the infinity errors</li>
      </ol>
      </p>")