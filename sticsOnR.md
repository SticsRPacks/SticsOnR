# sticsOnR

## There are three ways to use STICS with R with `sticsOnR`, and they are listed below.

### 1. Using JavaStics through R

It is possible to use JavaStics through command line using specific R functions. It is the most advanced use yet waiting for the development of the second way (using XML files). It allows the user to be sure to work with the same version (and capabilities) as he/she is used to.

Advantage:
- Same workflow than JavaStics (for users that are used to it)
- State of the art for STICS/R communication (yet)

Drawbacks:
- Have to install JavaStics + JAVA
- Less explicit concerning errors
- No parameter value change (have to open JavaStics, or in file modification)

### 2. Working using XML files from R

Read the parameters directly in the XML files, change their values, and create the text files input for STICS.

Advantage:
- No installation of JavaStics + JAVA needed.
- More explicit concerning errors
- Prescribe parameter values from R
- STICS files management according to the proper version (compared to direct text files management)

Drawbacks:
- Need more development
- Slower for intensive file usage compared to text files

### 3. Working using STICS inputs directly (text files)

Read the parameters directly from the text files input of STICS, and change their values.

Advantage:
- Fast
- Work directly on the inputs of STICS (STICS uses those text files, not the XMLs)

Drawbacks:
- Need to handle the requirement of the model version (particularly for tec file, *e.g.* irrigation).
- Need to generate the files beforehand (using XML way, or JavaStics way)


## To do

1. JavaStics way

Nothing. Everything is working.

2. XML and text ways

* Finalize the read and write functions for XML.
* Several USMs:
  - Generate each USM sequentially, run them, and import the outputs (including rapport) before running the next USM.
  - Generate different sets of text files for each USM in usms.xml and put them in a different repertory
* Read `mod_rapport*.sti`
* Adapt `stics_eval()` considering the way of simulation (sequential USMs, each USM in a separate directory,...)  
