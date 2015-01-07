/****************************************************************************
 *                                                                          *
 *                 SAS SETUP FILE FOR SOCIAL DATA							*
 *        UNIVERSITY OF MASSACHUSETTS AMHERST GAMBLING STUDY FILES		    *
 *                  	(DATASET BA003_01_5yr)								    *
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
 * unzipped data file is physically located (e.g.,"c:\temp\BA003_01_5yr.txt").  * 
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
title1 'PROGRAM:      BA003_01_5yr.SAS';

*************************************************************************************************************************************************************;
data WORK.BA003_01_5yr    ;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile 'FILEPATH\BA003_01_5yr.txt' delimiter='09'x MISSOVER DSD lrecl=32767 firstobs=2 ;
informat	Municipal	$21.00 	;
informat	County	$18.00 	;
informat	State	$3.00 	;
informat	Region	$60.00 	;
informat	BA003_001	$9.00 	;
informat	BA003_002	$19.00 	;
informat	BA003_003	$10.00 	;
informat	BA003_004	$67.00 	;
informat	BA003_005	$48.00 	;
informat	BA003_006	$55.00 	;
informat	BA003_007	$56.00 	;
informat	BA003_008	$55.00 	;
informat	BA003_009	$62.00 	;
informat	BA003_010	$56.00 	;
informat	BA003_011	$63.00 	;
informat	BA003_012	$57.00 	;
informat	BA003_013	$64.00 	;
informat	BA003_014	$58.00 	;
informat	BA003_015	$65.00 	;
informat	BA003_016	$64.00 	;
informat	BA003_017	$71.00 	;
informat	BA003_018	$65.00 	;
informat	BA003_019	$72.00 	;
informat	BA003_020	$63.00 	;
informat	BA003_021	$70.00 	;
informat	BA003_022	$64.00 	;
informat	BA003_023	$71.00 	;
informat	BA003_024	$65.00 	;
informat	BA003_025	$72.00 	;
informat	BA003_026	$66.00 	;
informat	BA003_027	$73.00 	;
informat	BA003_028	$65.00 	;
informat	BA003_029	$72.00 	;
informat	BA003_030	$66.00 	;
informat	BA003_031	$73.00 	;
informat	BA003_032	$65.00 	;
informat	BA003_033	$72.00 	;
informat	BA003_034	$66.00 	;
informat	BA003_035	$73.00 	;
informat	BA003_036	$65.00 	;
informat	BA003_037	$72.00 	;
informat	BA003_038	$66.00 	;
informat	BA003_039	$73.00 	;
informat	BA003_040	$65.00 	;
informat	BA003_041	$72.00 	;
informat	BA003_042	$66.00 	;
informat	BA003_043	$73.00 	;
informat	BA003_044	$65.00 	;
informat	BA003_045	$72.00 	;
informat	BA003_046	$66.00 	;
informat	BA003_047	$73.00 	;
informat	BA003_048	$65.00 	;
informat	BA003_049	$72.00 	;
informat	BA003_050	$66.00 	;
informat	BA003_051	$73.00 	;
informat	BA003_052	$65.00 	;
informat	BA003_053	$72.00 	;
informat	BA003_054	$66.00 	;
informat	BA003_055	$73.00 	;
informat	BA003_056	$65.00 	;
informat	BA003_057	$72.00 	;
informat	BA003_058	$66.00 	;
informat	BA003_059	$73.00 	;
informat	BA003_060	$65.00 	;
informat	BA003_061	$72.00 	;
informat	BA003_062	$66.00 	;
informat	BA003_063	$73.00 	;
informat	BA003_064	$68.00 	;
informat	BA003_065	$75.00 	;
informat	BA003_066	$69.00 	;
informat	BA003_067	$76.00 	;
informat	BA003_068	$69.00 	;
informat	BA003_069	$76.00 	;
informat	BA003_070	$70.00 	;
informat	BA003_071	$77.00 	;
informat	BA003_072	$68.00 	;
informat	BA003_073	$75.00 	;
informat	BA003_074	$76.00 	;
informat	BA003_075	$75.00 	;
informat	BA003_076	$82.00 	;
informat	BA003_077	$76.00 	;
informat	BA003_078	$83.00 	;
informat	BA003_079	$77.00 	;
informat	BA003_080	$84.00 	;
informat	BA003_081	$78.00 	;
informat	BA003_082	$85.00 	;
informat	BA003_083	$68.00 	;
informat	BA003_084	$75.00 	;
informat	BA003_085	$76.00 	;
informat	BA003_086	$75.00 	;
informat	BA003_087	$82.00 	;
informat	BA003_088	$76.00 	;
informat	BA003_089	$83.00 	;
informat	BA003_090	$77.00 	;
informat	BA003_091	$84.00 	;
informat	BA003_092	$78.00 	;
informat	BA003_093	$85.00 	;
informat	BA003_094	$102.00 	;
informat	BA003_095	$109.00 	;
informat	BA003_096	$103.00 	;
informat	BA003_097	$110.00 	;
informat	BA003_098	$122.00 	;
informat	BA003_099	$129.00 	;
informat	BA003_100	$123.00 	;
informat	BA003_101	$130.00 	;
informat	BA003_102	$130.00 	;
informat	BA003_103	$137.00 	;
informat	BA003_104	$131.00 	;
informat	BA003_105	$138.00 	;
informat	BA003_106	$102.00 	;
informat	BA003_107	$109.00 	;
informat	BA003_108	$103.00 	;
informat	BA003_109	$110.00 	;
informat	BA003_110	$139.00 	;
informat	BA003_111	$146.00 	;
informat	BA003_112	$140.00 	;
informat	BA003_113	$147.00 	;
informat	BA003_114	$112.00 	;
informat	BA003_115	$119.00 	;
informat	BA003_116	$113.00 	;
informat	BA003_117	$120.00 	;
informat	BA003_118	$99.00 	;
informat	BA003_119	$106.00 	;
informat	BA003_120	$100.00 	;
informat	BA003_121	$107.00 	;
informat	BA003_122	$89.00 	;
informat	BA003_123	$96.00 	;
informat	BA003_124	$90.00 	;
informat	BA003_125	$97.00 	;
informat	BA003_126	$109.00 	;
informat	BA003_127	$116.00 	;
informat	BA003_128	$110.00 	;
informat	BA003_129	$117.00 	;
format	Municipal	$21.00 	;
format	County	$18.00 	;
format	State	$3.00 	;
format	Region	$60.00 	;
format	BA003_001	$9.00 	;
format	BA003_002	$19.00 	;
format	BA003_003	$10.00 	;
format	BA003_004	$67.00 	;
format	BA003_005	$48.00 	;
format	BA003_006	$55.00 	;
format	BA003_007	$56.00 	;
format	BA003_008	$55.00 	;
format	BA003_009	$62.00 	;
format	BA003_010	$56.00 	;
format	BA003_011	$63.00 	;
format	BA003_012	$57.00 	;
format	BA003_013	$64.00 	;
format	BA003_014	$58.00 	;
format	BA003_015	$65.00 	;
format	BA003_016	$64.00 	;
format	BA003_017	$71.00 	;
format	BA003_018	$65.00 	;
format	BA003_019	$72.00 	;
format	BA003_020	$63.00 	;
format	BA003_021	$70.00 	;
format	BA003_022	$64.00 	;
format	BA003_023	$71.00 	;
format	BA003_024	$65.00 	;
format	BA003_025	$72.00 	;
format	BA003_026	$66.00 	;
format	BA003_027	$73.00 	;
format	BA003_028	$65.00 	;
format	BA003_029	$72.00 	;
format	BA003_030	$66.00 	;
format	BA003_031	$73.00 	;
format	BA003_032	$65.00 	;
format	BA003_033	$72.00 	;
format	BA003_034	$66.00 	;
format	BA003_035	$73.00 	;
format	BA003_036	$65.00 	;
format	BA003_037	$72.00 	;
format	BA003_038	$66.00 	;
format	BA003_039	$73.00 	;
format	BA003_040	$65.00 	;
format	BA003_041	$72.00 	;
format	BA003_042	$66.00 	;
format	BA003_043	$73.00 	;
format	BA003_044	$65.00 	;
format	BA003_045	$72.00 	;
format	BA003_046	$66.00 	;
format	BA003_047	$73.00 	;
format	BA003_048	$65.00 	;
format	BA003_049	$72.00 	;
format	BA003_050	$66.00 	;
format	BA003_051	$73.00 	;
format	BA003_052	$65.00 	;
format	BA003_053	$72.00 	;
format	BA003_054	$66.00 	;
format	BA003_055	$73.00 	;
format	BA003_056	$65.00 	;
format	BA003_057	$72.00 	;
format	BA003_058	$66.00 	;
format	BA003_059	$73.00 	;
format	BA003_060	$65.00 	;
format	BA003_061	$72.00 	;
format	BA003_062	$66.00 	;
format	BA003_063	$73.00 	;
format	BA003_064	$68.00 	;
format	BA003_065	$75.00 	;
format	BA003_066	$69.00 	;
format	BA003_067	$76.00 	;
format	BA003_068	$69.00 	;
format	BA003_069	$76.00 	;
format	BA003_070	$70.00 	;
format	BA003_071	$77.00 	;
format	BA003_072	$68.00 	;
format	BA003_073	$75.00 	;
format	BA003_074	$76.00 	;
format	BA003_075	$75.00 	;
format	BA003_076	$82.00 	;
format	BA003_077	$76.00 	;
format	BA003_078	$83.00 	;
format	BA003_079	$77.00 	;
format	BA003_080	$84.00 	;
format	BA003_081	$78.00 	;
format	BA003_082	$85.00 	;
format	BA003_083	$68.00 	;
format	BA003_084	$75.00 	;
format	BA003_085	$76.00 	;
format	BA003_086	$75.00 	;
format	BA003_087	$82.00 	;
format	BA003_088	$76.00 	;
format	BA003_089	$83.00 	;
format	BA003_090	$77.00 	;
format	BA003_091	$84.00 	;
format	BA003_092	$78.00 	;
format	BA003_093	$85.00 	;
format	BA003_094	$102.00 	;
format	BA003_095	$109.00 	;
format	BA003_096	$103.00 	;
format	BA003_097	$110.00 	;
format	BA003_098	$122.00 	;
format	BA003_099	$129.00 	;
format	BA003_100	$123.00 	;
format	BA003_101	$130.00 	;
format	BA003_102	$130.00 	;
format	BA003_103	$137.00 	;
format	BA003_104	$131.00 	;
format	BA003_105	$138.00 	;
format	BA003_106	$102.00 	;
format	BA003_107	$109.00 	;
format	BA003_108	$103.00 	;
format	BA003_109	$110.00 	;
format	BA003_110	$139.00 	;
format	BA003_111	$146.00 	;
format	BA003_112	$140.00 	;
format	BA003_113	$147.00 	;
format	BA003_114	$112.00 	;
format	BA003_115	$119.00 	;
format	BA003_116	$113.00 	;
format	BA003_117	$120.00 	;
format	BA003_118	$99.00 	;
format	BA003_119	$106.00 	;
format	BA003_120	$100.00 	;
format	BA003_121	$107.00 	;
format	BA003_122	$89.00 	;
format	BA003_123	$96.00 	;
format	BA003_124	$90.00 	;
format	BA003_125	$97.00 	;
format	BA003_126	$109.00 	;
format	BA003_127	$116.00 	;
format	BA003_128	$110.00 	;
format	BA003_129	$117.00 	;
input	Municipal	$	
	County	$	
	State	$	
	Region	$	
	BA003_001	$	
	BA003_002	$	
	BA003_003	$	
	BA003_004	$	
	BA003_005	$	
	BA003_006	$	
	BA003_007	$	
	BA003_008	$	
	BA003_009	$	
	BA003_010	$	
	BA003_011	$	
	BA003_012	$	
	BA003_013	$	
	BA003_014	$	
	BA003_015	$	
	BA003_016	$	
	BA003_017	$	
	BA003_018	$	
	BA003_019	$	
	BA003_020	$	
	BA003_021	$	
	BA003_022	$	
	BA003_023	$	
	BA003_024	$	
	BA003_025	$	
	BA003_026	$	
	BA003_027	$	
	BA003_028	$	
	BA003_029	$	
	BA003_030	$	
	BA003_031	$	
	BA003_032	$	
	BA003_033	$	
	BA003_034	$	
	BA003_035	$	
	BA003_036	$	
	BA003_037	$	
	BA003_038	$	
	BA003_039	$	
	BA003_040	$	
	BA003_041	$	
	BA003_042	$	
	BA003_043	$	
	BA003_044	$	
	BA003_045	$	
	BA003_046	$	
	BA003_047	$	
	BA003_048	$	
	BA003_049	$	
	BA003_050	$	
	BA003_051	$	
	BA003_052	$	
	BA003_053	$	
	BA003_054	$	
	BA003_055	$	
	BA003_056	$	
	BA003_057	$	
	BA003_058	$	
	BA003_059	$	
	BA003_060	$	
	BA003_061	$	
	BA003_062	$	
	BA003_063	$	
	BA003_064	$	
	BA003_065	$	
	BA003_066	$	
	BA003_067	$	
	BA003_068	$	
	BA003_069	$	
	BA003_070	$	
	BA003_071	$	
	BA003_072	$	
	BA003_073	$	
	BA003_074	$	
	BA003_075	$	
	BA003_076	$	
	BA003_077	$	
	BA003_078	$	
	BA003_079	$	
	BA003_080	$	
	BA003_081	$	
	BA003_082	$	
	BA003_083	$	
	BA003_084	$	
	BA003_085	$	
	BA003_086	$	
	BA003_087	$	
	BA003_088	$	
	BA003_089	$	
	BA003_090	$	
	BA003_091	$	
	BA003_092	$	
	BA003_093	$	
	BA003_094	$	
	BA003_095	$	
	BA003_096	$	
	BA003_097	$	
	BA003_098	$	
	BA003_099	$	
	BA003_100	$	
	BA003_101	$	
	BA003_102	$	
	BA003_103	$	
	BA003_104	$	
	BA003_105	$	
	BA003_106	$	
	BA003_107	$	
	BA003_108	$	
	BA003_109	$	
	BA003_110	$	
	BA003_111	$	
	BA003_112	$	
	BA003_113	$	
	BA003_114	$	
	BA003_115	$	
	BA003_116	$	
	BA003_117	$	
	BA003_118	$	
	BA003_119	$	
	BA003_120	$	
	BA003_121	$	
	BA003_122	$	
	BA003_123	$	
	BA003_124	$	
	BA003_125	$	
	BA003_126	$	
	BA003_127	$	
	BA003_128	$	
	BA003_129	$	
;
label Municipal	=	"	Municipal			"
County	=	"	County			"
State	=	"	State			"
Region	=	"	Region			"
BA003_001	=	"	BA003_001	:	Five Year Average	"
BA003_002	=	"	BA003_002	:	Id	"
BA003_003	=	"	BA003_003	:	Id2	"
BA003_004	=	"	BA003_004	:	Geography	"
BA003_005	=	"	BA003_005	:	Estimate; SEX AND AGE - Total population	"
BA003_006	=	"	BA003_006	:	Estimate Margin of Error; SEX AND AGE - Total population	"
BA003_007	=	"	BA003_007	:	Percent Margin of Error; SEX AND AGE - Total population	"
BA003_008	=	"	BA003_008	:	Estimate; SEX AND AGE - Male	"
BA003_009	=	"	BA003_009	:	Estimate Margin of Error; SEX AND AGE - Male	"
BA003_010	=	"	BA003_010	:	Percent; SEX AND AGE - Male	"
BA003_011	=	"	BA003_011	:	Percent Margin of Error; SEX AND AGE - Male	"
BA003_012	=	"	BA003_012	:	Estimate; SEX AND AGE - Female	"
BA003_013	=	"	BA003_013	:	Estimate Margin of Error; SEX AND AGE - Female	"
BA003_014	=	"	BA003_014	:	Percent; SEX AND AGE - Female	"
BA003_015	=	"	BA003_015	:	Percent Margin of Error; SEX AND AGE - Female	"
BA003_016	=	"	BA003_016	:	Estimate; SEX AND AGE - Under 5 years	"
BA003_017	=	"	BA003_017	:	Estimate Margin of Error; SEX AND AGE - Under 5 years	"
BA003_018	=	"	BA003_018	:	Percent; SEX AND AGE - Under 5 years	"
BA003_019	=	"	BA003_019	:	Percent Margin of Error; SEX AND AGE - Under 5 years	"
BA003_020	=	"	BA003_020	:	Estimate; SEX AND AGE - 5 to 9 years	"
BA003_021	=	"	BA003_021	:	Estimate Margin of Error; SEX AND AGE - 5 to 9 years	"
BA003_022	=	"	BA003_022	:	Percent; SEX AND AGE - 5 to 9 years	"
BA003_023	=	"	BA003_023	:	Percent Margin of Error; SEX AND AGE - 5 to 9 years	"
BA003_024	=	"	BA003_024	:	Estimate; SEX AND AGE - 10 to 14 years	"
BA003_025	=	"	BA003_025	:	Estimate Margin of Error; SEX AND AGE - 10 to 14 years	"
BA003_026	=	"	BA003_026	:	Percent; SEX AND AGE - 10 to 14 years	"
BA003_027	=	"	BA003_027	:	Percent Margin of Error; SEX AND AGE - 10 to 14 years	"
BA003_028	=	"	BA003_028	:	Estimate; SEX AND AGE - 15 to 19 years	"
BA003_029	=	"	BA003_029	:	Estimate Margin of Error; SEX AND AGE - 15 to 19 years	"
BA003_030	=	"	BA003_030	:	Percent; SEX AND AGE - 15 to 19 years	"
BA003_031	=	"	BA003_031	:	Percent Margin of Error; SEX AND AGE - 15 to 19 years	"
BA003_032	=	"	BA003_032	:	Estimate; SEX AND AGE - 20 to 24 years	"
BA003_033	=	"	BA003_033	:	Estimate Margin of Error; SEX AND AGE - 20 to 24 years	"
BA003_034	=	"	BA003_034	:	Percent; SEX AND AGE - 20 to 24 years	"
BA003_035	=	"	BA003_035	:	Percent Margin of Error; SEX AND AGE - 20 to 24 years	"
BA003_036	=	"	BA003_036	:	Estimate; SEX AND AGE - 25 to 34 years	"
BA003_037	=	"	BA003_037	:	Estimate Margin of Error; SEX AND AGE - 25 to 34 years	"
BA003_038	=	"	BA003_038	:	Percent; SEX AND AGE - 25 to 34 years	"
BA003_039	=	"	BA003_039	:	Percent Margin of Error; SEX AND AGE - 25 to 34 years	"
BA003_040	=	"	BA003_040	:	Estimate; SEX AND AGE - 35 to 44 years	"
BA003_041	=	"	BA003_041	:	Estimate Margin of Error; SEX AND AGE - 35 to 44 years	"
BA003_042	=	"	BA003_042	:	Percent; SEX AND AGE - 35 to 44 years	"
BA003_043	=	"	BA003_043	:	Percent Margin of Error; SEX AND AGE - 35 to 44 years	"
BA003_044	=	"	BA003_044	:	Estimate; SEX AND AGE - 45 to 54 years	"
BA003_045	=	"	BA003_045	:	Estimate Margin of Error; SEX AND AGE - 45 to 54 years	"
BA003_046	=	"	BA003_046	:	Percent; SEX AND AGE - 45 to 54 years	"
BA003_047	=	"	BA003_047	:	Percent Margin of Error; SEX AND AGE - 45 to 54 years	"
BA003_048	=	"	BA003_048	:	Estimate; SEX AND AGE - 55 to 59 years	"
BA003_049	=	"	BA003_049	:	Estimate Margin of Error; SEX AND AGE - 55 to 59 years	"
BA003_050	=	"	BA003_050	:	Percent; SEX AND AGE - 55 to 59 years	"
BA003_051	=	"	BA003_051	:	Percent Margin of Error; SEX AND AGE - 55 to 59 years	"
BA003_052	=	"	BA003_052	:	Estimate; SEX AND AGE - 60 to 64 years	"
BA003_053	=	"	BA003_053	:	Estimate Margin of Error; SEX AND AGE - 60 to 64 years	"
BA003_054	=	"	BA003_054	:	Percent; SEX AND AGE - 60 to 64 years	"
BA003_055	=	"	BA003_055	:	Percent Margin of Error; SEX AND AGE - 60 to 64 years	"
BA003_056	=	"	BA003_056	:	Estimate; SEX AND AGE - 65 to 74 years	"
BA003_057	=	"	BA003_057	:	Estimate Margin of Error; SEX AND AGE - 65 to 74 years	"
BA003_058	=	"	BA003_058	:	Percent; SEX AND AGE - 65 to 74 years	"
BA003_059	=	"	BA003_059	:	Percent Margin of Error; SEX AND AGE - 65 to 74 years	"
BA003_060	=	"	BA003_060	:	Estimate; SEX AND AGE - 75 to 84 years	"
BA003_061	=	"	BA003_061	:	Estimate Margin of Error; SEX AND AGE - 75 to 84 years	"
BA003_062	=	"	BA003_062	:	Percent; SEX AND AGE - 75 to 84 years	"
BA003_063	=	"	BA003_063	:	Percent Margin of Error; SEX AND AGE - 75 to 84 years	"
BA003_064	=	"	BA003_064	:	Estimate; SEX AND AGE - 85 years and over	"
BA003_065	=	"	BA003_065	:	Estimate Margin of Error; SEX AND AGE - 85 years and over	"
BA003_066	=	"	BA003_066	:	Percent; SEX AND AGE - 85 years and over	"
BA003_067	=	"	BA003_067	:	Percent Margin of Error; SEX AND AGE - 85 years and over	"
BA003_068	=	"	BA003_068	:	Estimate; SEX AND AGE - Median age (years)	"
BA003_069	=	"	BA003_069	:	Estimate Margin of Error; SEX AND AGE - Median age (years)	"
BA003_070	=	"	BA003_070	:	Percent; SEX AND AGE - Median age (years)	"
BA003_071	=	"	BA003_071	:	Percent Margin of Error; SEX AND AGE - Median age (years)	"
BA003_072	=	"	BA003_072	:	Estimate; SEX AND AGE - 18 years and over	"
BA003_073	=	"	BA003_073	:	Estimate Margin of Error; SEX AND AGE - 18 years and over	"
BA003_074	=	"	BA003_074	:	Percent Margin of Error; SEX AND AGE - 18 years and over	"
BA003_075	=	"	BA003_075	:	Estimate; SEX AND AGE - 18 years and over - Male	"
BA003_076	=	"	BA003_076	:	Estimate Margin of Error; SEX AND AGE - 18 years and over - Male	"
BA003_077	=	"	BA003_077	:	Percent; SEX AND AGE - 18 years and over - Male	"
BA003_078	=	"	BA003_078	:	Percent Margin of Error; SEX AND AGE - 18 years and over - Male	"
BA003_079	=	"	BA003_079	:	Estimate; SEX AND AGE - 18 years and over - Female	"
BA003_080	=	"	BA003_080	:	Estimate Margin of Error; SEX AND AGE - 18 years and over - Female	"
BA003_081	=	"	BA003_081	:	Percent; SEX AND AGE - 18 years and over - Female	"
BA003_082	=	"	BA003_082	:	Percent Margin of Error; SEX AND AGE - 18 years and over - Female	"
BA003_083	=	"	BA003_083	:	Estimate; SEX AND AGE - 65 years and over	"
BA003_084	=	"	BA003_084	:	Estimate Margin of Error; SEX AND AGE - 65 years and over	"
BA003_085	=	"	BA003_085	:	Percent Margin of Error; SEX AND AGE - 65 years and over	"
BA003_086	=	"	BA003_086	:	Estimate; SEX AND AGE - 65 years and over - Male	"
BA003_087	=	"	BA003_087	:	Estimate Margin of Error; SEX AND AGE- 65 years and over  - Male	"
BA003_088	=	"	BA003_088	:	Percent; SEX AND AGE- 65 years and over  - Male	"
BA003_089	=	"	BA003_089	:	Percent Margin of Error; SEX AND AGE - 65 years and over - Male	"
BA003_090	=	"	BA003_090	:	Estimate; SEX AND AGE- 65 years and over  - Female	"
BA003_091	=	"	BA003_091	:	Estimate Margin of Error; SEX AND AGE - 65 years and over - Female	"
BA003_092	=	"	BA003_092	:	Percent; SEX AND AGE - 65 years and over - Female	"
BA003_093	=	"	BA003_093	:	Percent Margin of Error; SEX AND AGE - 65 years and over - Female	"
BA003_094	=	"	BA003_094	:	Estimate; RACE - White	"
BA003_095	=	"	BA003_095	:	Estimate Margin of Error; RACE - White	"
BA003_096	=	"	BA003_096	:	Percent; RACE - White	"
BA003_097	=	"	BA003_097	:	Percent Margin of Error; RACE - White	"
BA003_098	=	"	BA003_098	:	Estimate; RACE - Black or African American	"
BA003_099	=	"	BA003_099	:	Estimate Margin of Error; RACE - Black or African American	"
BA003_100	=	"	BA003_100	:	Percent; RACE - Black or African American	"
BA003_101	=	"	BA003_101	:	Percent Margin of Error; RACE - Black or African American	"
BA003_102	=	"	BA003_102	:	Estimate; RACE - American Indian and Alaska Native	"
BA003_103	=	"	BA003_103	:	Estimate Margin of Error; RACE - American Indian and Alaska Native	"
BA003_104	=	"	BA003_104	:	Percent; RACE - American Indian and Alaska Native	"
BA003_105	=	"	BA003_105	:	Percent Margin of Error; RACE - American Indian and Alaska Native	"
BA003_106	=	"	BA003_106	:	Estimate; RACE - Asian	"
BA003_107	=	"	BA003_107	:	Estimate Margin of Error; RACE - Asian	"
BA003_108	=	"	BA003_108	:	Percent; RACE - Asian	"
BA003_109	=	"	BA003_109	:	Percent Margin of Error; RACE - Asian	"
BA003_110	=	"	BA003_110	:	Estimate; RACE - Native Hawaiian and Other Pacific Islander	"
BA003_111	=	"	BA003_111	:	Estimate Margin of Error; RACE - Native Hawaiian and Other Pacific Islander	"
BA003_112	=	"	BA003_112	:	Percent; RACE - Native Hawaiian and Other Pacific Islander	"
BA003_113	=	"	BA003_113	:	Percent Margin of Error; RACE - Native Hawaiian and Other Pacific Islander	"
BA003_114	=	"	BA003_114	:	Estimate; RACE - Some other race	"
BA003_115	=	"	BA003_115	:	Estimate Margin of Error; RACE - Some other race	"
BA003_116	=	"	BA003_116	:	Percent; RACE - Some other race	"
BA003_117	=	"	BA003_117	:	Percent Margin of Error; RACE - Some other race	"
BA003_118	=	"	BA003_118	:	Estimate; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"
BA003_119	=	"	BA003_119	:	Estimate Margin of Error; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"
BA003_120	=	"	BA003_120	:	Percent; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"
BA003_121	=	"	BA003_121	:	Percent Margin of Error; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"
BA003_122	=	"	BA003_122	:	Estimate; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"
BA003_123	=	"	BA003_123	:	Estimate Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"
BA003_124	=	"	BA003_124	:	Percent; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"
BA003_125	=	"	BA003_125	:	Percent Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"
BA003_126	=	"	BA003_126	:	Estimate; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"
BA003_127	=	"	BA003_127	:	Estimate Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"
BA003_128	=	"	BA003_128	:	Percent; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"
BA003_129	=	"	BA003_129	:	Percent Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"
;
run;





