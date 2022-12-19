# parkmybike

My latest passion project is a routing applet which tells you where you can park your bike. The motivation behind this project is the lack of routing applications which have built in options to locate your nearest cycle parking and add it to your route (neither Google Maps nor Citymapper have this function!).

The current versions uses data from the Cycle Infrastructure Database (CiD). I wrote an R script which will take in an origin and destination postcode from the user. The script will find the nearest cycle parking rack within a 5 minute walking radius of your destination, and return a  multimodal route (cycle and walking), with a cycling segment between your origin and parking space, and a walking segment between your parking space and final destination. 

At the moment, it does not have a user interface, but I intend on building one in RShiny. Future iterations of this project can include accounting for different cycling route options (e.g., quiet route, flatter route etc.) and the implementation of a Google Streetview Image for each bike parking location.
