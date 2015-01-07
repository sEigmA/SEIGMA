/****************************************************************************
 *                                                                          *
 *             SAS SETUP FILE FOR RESIDENT INDICATORS DATA					*
 *        UNIVERSITY OF MASSACHUSETTS AMHERST GAMBLING STUDY FILES		    *
 *                  	(DATASET AT004_01)								    *
 *																		    *
 *																		    *
 * SAS setup sections are provided for the ASCII version of this data	    *
 * collection.  These sections are listed below:						    *
 *																		    *
 *																		    *
 * DATA:  begins a SAS data step and names the output SAS data set.		    *
 *																		    *
 * INFILE:  identifies the file to be read with the INPUT statement below.  *
 * Users must replace "filepath" with a filename specifying the		    	*
 * directory on the user's computer system in which the downloaded and	    *
 * unzipped data file is physically located (e.g.,"c:\temp\AT004_01.txt").  * 
 *																		    *
 * INFORMAT:  defines lengths for the new variables in this data file		*			
 *																			*
 * FORMAT:  specifies a temporary default format for displaying the			*
 * values of the variables in this data file								*
 *																			*
 * INPUT:  reads in the raw data from the external file source/path			*
 * specified above in the INFILE statement									*
 *																		    *
 * LABEL:  assigns permanent descriptive labels to the variables in this  	*
 * data file. Variable labels and variable names may be identical in 		*
 * certain instances														*
 *																		    *
 * NOTE:  Users should modify this setup file to suit their specific needs. *
 *																			*
******************************************************************************/
title1 'PROGRAM:      AT004_01.SAS';

**IMPORT TXT FILE**;
**INPUT VARIABLES**;
**ASSIGN LABELS**;
data WORK.AT004_01;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile 'FILEPATH\AT004_01.txt' delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
informat Municipal $21. ;
informat County $18.;
informat State $3. ;
informat Region $60. ;
informat C_year $4. ;
informat AT004_001 $14. ;
informat AT004_002 $59. ;
informat AT004_003 best32. ;
informat AT004_004 best32. ;
informat AT004_005 best32. ;
informat AT004_006 best32. ;
informat AT004_007 $86. ;
format Municipal $21. ;
format County $18.;
format State $3. ;
format Region $60. ;
format C_year $4. ;
format AT004_001 $14. ;
format AT004_002 $59. ;
format AT004_003 best12. ;
format AT004_004 best12. ;
format AT004_005 best12. ;
format AT004_006 best12. ;
format AT004_007 $86. ;
input Municipal $ County $State $ Region $ C_year $ AT004_001 $ AT004_002 $ AT004_003 AT004_004 AT004_005 AT004_006 AT004_007 $ ;
if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
label Municipal		= 'Municipal'
	County			= 'County'
	State			= 'State'
	Region			= 'Region'
	new_year 		= 'Calender Year'
	AT004_001		= 'AT004_001: Period'
	AT004_002		= 'AT004_002: Geographic Label'
	AT004_003		= 'AT004_003: Unemployment Rate'
	AT004_004		= 'AT004_004: Number of Unemployed'
	AT004_005		= 'AT004_005: Number of Employed'
	AT004_006		= 'AT004_006: Number in Labor Force'
	AT004_007		= 'AT004_007: Footnote Label'  ;
run;

