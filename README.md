# hotspots-initiative

![hotspots-initiative-app](https://github.com/user-attachments/assets/6c37ae1c-0bb9-4ae7-afba-9dc86f5d6873)

### About this Project
Bycatch, the unintentional capture of species fishers don’t want, can’t sell, or can’t keep, can be a significant challenge in commercial fisheries. Beyond its environmental impact, bycatch creates economic inefficiencies and regulatory hurdles for the fishing industry. Bycatch issues became particularly pressing after the 2010 Deepwater Horizon oil spill, which disrupted Gulf Coast ecosystems and fisheries.

To restore impacted fisheries, the Communications Networks and Mapping Tools to Reduce Bycatch Project was initiated by the Deepwater Horizon Open Ocean Trustees in 2019. Mote Marine Laboratory is collaborating with the National Fish and Wildlife Foundation (NFWF) and National Oceanic and Atmospheric Administration (NOAA), who manage this project, with the goal of exploring how real-time communication networks can help reduce bycatch. By enabling fishermen to share and act on spatial data in near real-time, bycatch hotspots can be identified and avoided by a fleet of participants. As part of this initiative, a Shiny for R application has been developed to prototype a streamlined data reporting and visualization tool that can benefit commercial and charter fishers in the Gulf of Mexico.

Using Shiny, this prototype for an interactive web app tailored to the needs of the Charter Fisherman’s Association (CFA) was designed to facilitate real-time data sharing and visualization based on fisher-provided feedback. The CFA members have reported that depredation can be intense enough to make an area unfishable, as can high currents. The app reflects these requests to enable fishers to know to avoid the locations before they leave port or once they are out on the water. 

#### User-Submitted Reports
The app allows participating fishermen to submit real-time reports about depredation, bycatch, and current speed. This feature is user-friendly, with dropdown menus for species identification, geolocation services, options to attach additional notes, and automated user identification and time stamp appending. The goal was to minimize the time and effort required to submit accurate reports.

#### Automated Quality Assurance
To ensure the integrity of the data, the app includes automated checks for common errors, such as missing values or out-of-bounds GPS coordinates. This feature helps maintain the reliability of the information being shared across the network.

#### Real-Time Mapping Solutions
Once data is submitted, the app sends it to a secure Google Sheet for storage and generates interactive maps that visualize depredation hotspots in near real-time. Users can visualize their own data or the group’s collective data in a 1 mi by 1 mi grid or a heat map and filter the entries by species type and time frame, enabling them to identify patterns and make informed decisions quickly. For example, fishermen can avoid areas with high rates of depredation, reducing unwanted interactions with predators such as sharks and dolphins and reducing bycatch mortality of damaged fish.

### Visit the App
Visit the app [here](https://cfemm.shinyapps.io/destin-app) using credentials (username: test-user; password: hotspots).
