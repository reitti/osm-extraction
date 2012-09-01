# osm-extraction

A command-line app for extracting names and coordinates of ways and nodes in OpenStreetMap XML data.
For use primarily in the data update process of our search index.

## Usage

Build the uberjar: `lein uberjar`, and then run it with `java -jar`. It'll be in the `target` directory.

When run, the program expects OSM XML data from stdin, and prints the extracted information to stdout.
Each line in stdout will be a placename and its coordinates (lon,lat) separated by a pipe.

    Vuosaaren uimahalli|25.1416744,60.2088469
    
The coordinates are only included for node data. For way data they are blank.
