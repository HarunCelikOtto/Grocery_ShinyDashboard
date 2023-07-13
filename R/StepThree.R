HTML("<h3> Plot Overview </h3>
                  
      <h4> Census Plots </h4>
      <p> The plots using the American Community Survey (2021) and the Decennial Census (2020) were pulled in order to display data 
      visualizations relevant to food desert status and the understanding the market of a given location. 
      This data is displayed in three bar plots and one data table inside the `Census Income` and `Census Demographics` tabs.
      The data is automatically retrieved based on the market size generated in the first step of the tool. </p>
      
      <p> The following are the variables displayed in these plots 
      
      <ul> 
      <li>Median Household Income (ACS): this plot correlates with access to food, provides important information about the store’s potential customer base</li>
      <li>Employment Status (ACS): an indicator of poverty, highlighting which areas may benefit most from the additional jobs provided by a potential store</li>
      <li>Languages Spoken other than English (ACS): highlighting the cultural makeup of a market area</li>
      <li>Race/Hispanic Origin (Decennial): displaying areas with higher proportions of minority groups most likely to be living in food deserts</li>
      <li>Total Population (Decennial):  helping understand a potential market size (included alongside Race/Hispanic Origin table)</li>
      </ul>
      </P>
      
      <h5> Hover Boxes </h5>
      <p>  Since these plots were rendered interactive with the Plotly package, users are able to see exact values of estimates for the variables in the plot 
      along with their margins of error by hovering their mouse over the plot.</p>
      
      <h4> Rural Grocery Store Analysis </h4>
      <p> The rural grocery store analysis uses the Salesgenie 2020 grocery sales data. The dashboard reviews the number and type of grocery stores in small 
      communities in Iowa with a population of less than 2,500. Cities are categorized into groups based on population size and if the city is within a metro 
      or non-metro county as well as adjacency to a metro area. The dashboard also shows the sales volume distribution for the non-chain grocery stores in Iowa 
      cities with populations under 2,500.</p>")