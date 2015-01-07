*----------------------------------------------------------------------------------------------------------------------*
*                                                                          
*                SPSS SETUP FILE FOR RESIDENT INDICATORS DATA
*         UNIVERSITY OF MASSACHUSETTS AMHERST GAMBLING STUDY
*                               (DATASET AT004_01)
* 
*
*  SPSS setup sections are provided for the ASCII version of this data
*  collection.  These sections are listed below:
*
*  GET DATA:  reads in the text data file and assigns the name, type, and  
*  length for each variable in the data file. Users must replace "filepath"
*  with a filename specifying the directory on the user's computer
*  system in which the downloaded and unzipped data file is
*  physically located (e.g., "c:\temp\AT004_01.txt").
*
*  VARIABLE LABELS:  assigns descriptive labels to the variables in this data file
*
*  NOTE:  Users should modify this setup file to suit their specific needs
*
*  CREATING A PERMANENT SPSS DATA FILE: If users wish to create and 
*  save an SPSS data file for further analysis using SPSS for Windows, the
*  necessary "SAVE OUTFILE" command is provided in the last line of
*  this file.  To activate the command, users must delete the leading
*  asterisk (*) and replace "spss-filename" with a filename specifying
*  the location on the user's computer system to which the new data file
*  will be saved (e.g., SAVE OUTFILE="c:\spsswin\data\AT004_01.sav").
*
*------------------------------------------------------------------------------------------------------------------------*.

GET DATA  /TYPE=TXT
  /FILE="FILEPATH\AT004_01.txt"
  /ENCODING='UTF8'
  /DELCASE=LINE
  /DELIMITERS="\t"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  Municipal A21
  County A18
  State A3
  Region A60
  C_year F4.0
  AT004_001 A14
  AT004_002 A60
  AT004_003 F4.1
  AT004_004 F6.0
  AT004_005 F7.0
  AT004_006 F7.0
  AT004_007 A90
.

VARIABLE LABELS
 Municipal  'Municipal'/
 County 'County'/
	State 'State'/
	Region 'Region'/
	c_year 'Calender Year'/
	AT004_001 'AT004_001: Period'/
	AT004_002 'AT004_002: Geographic Label'/
	AT004_003 'AT004_003: Unemployment Rate'/
	AT004_004 'AT004_004: Number of Unemployed'/
	AT004_005 'AT004_005: Number of Employed'/
	AT004_006 'AT004_006: Number in Labor Force'/
	AT004_007 'AT004_007: Footnote Label'/
.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.


*SAVE OUTFILE="filepath.sav".
