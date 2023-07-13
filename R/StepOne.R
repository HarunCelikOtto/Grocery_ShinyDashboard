HTML("<h3> Map Overview </h3>
                 
      <p> The 'Store Map' navigation tab contains the mapping tool which generates a visual
      representation of a market area. This calculation is <b> not the same as the 
      one indicated in the Excel tool provided by ISU's FFED Extension </b> and
      should therefore be used only as a visual reference. 
      The calculation for the market area circle takes a user provided 
      address as its center and buffers the circle as far as half the distance of the furthest grocery
      store in four quadrants of the circle (NE, NW, SE, SW). </p>
      
      <h4> Using the Map </h4>
      <p> The mapping tool is split into two operations which are calculated after the user has
      defined their address and store type search. The tool should be used in the following order.</p>
      
      <h5> Address </h5>
      <p>The user should provide a potential store address which follows the format of <b>Street, City, State (abbreviation)</b>. 
      The tool parses the address by commas so a comma should be inserted between each component of the address.</p>
      
      <h5> Type of Store </h5>
      <p> The type of store field is a keyword that is provided to the Google API to pull in store information.
      By default this value is set to 'grocery' and should ideally be kept that way for most purposes. Exceptions may
      include trying to map out the dollar stores in an area, though the closest ones will already be displayed with the
      default value.</p>
      
      <h5> Calculate Location </h5>
      <p> Once the address and store type inputs are defined, the user should click the 'Calculate Location' button.
      This button will call the necessary functions to generate a circle buffer. On average this will take 20 seconds or so,
      but internet connections may change that load time. Refer to the progress bar at the bottom right of the screen after
      clicking to check if run time is complete.</p>
      
      <h5> Map Retrieved Data </h5>
      <p> Once the run time is completed after the 'Calculate Location' button is clicked, the user should click 
      'Map Retrieved Data' button. Like the previous button, this will generate a progress bar that displays the run time 
      of the mapping process. Once complete, the user should see that the map has generated some points which display
      the area size outlined in red, cities in blue, and nearby stores in green. If a market size is successfully generated
      then the 'Map Data' section should be populated with all of the cities and counties intersecting the area size.</p>
      
      <h5> Potential Errors </h5>
      <p> In this process there are a lot of external calls to data sources that may generate some issues. Usually the application
      will time out if there is an error, and the user can try the following to fix the errors.
      
      <ol> 
      <li>Checking that the address is formatted properly</li>
      <li>Entering an address in a more general area (ie, 'Main Street' instead of '100 Main Street') </li>
      <li>Keeping the store type value to 'grocery' as its default</li>
      <li>Refreshing the app</li>
      <li>Restarting the app by closing it and reopening it</li>
      </ol>
      
      If none of these fixes seem to work, it may likely be that one of our external data sources is under maintenance or has
      changed rendering the tool temporarily unuseable. Common problems are addresses which can't be geocoded, one alternative way to check
      is to see if the address shows up on a Google Maps entry.
      </p>")