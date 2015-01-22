*---------------------------------------------------------------------------------------------------------------------------*
*                                                                                                                                
*                     SPSS SETUP FILE FOR SOCIAL DATA                                             
*         UNIVERSITY OF MASSACHUSETTS AMHERST GAMBLING STUDY                         
*                               (DATASET BA003_01_5yr)                                                              
* 
*
*  SPSS setup sections are provided for the ASCII version of this data
*  collection.  These sections are listed below:
*
*  GET DATA:  reads in the text data file and assigns the name, type, and  
*  length for each variable in the data file. Users must replace "filepath"
*  with a filename specifying the directory on the user's computer
*  system in which the downloaded and unzipped data file is
*  physically located (e.g., "c:\temp\BA003_01_5yr.txt").
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
*  will be saved (e.g., SAVE OUTFILE="c:\spsswin\data\BA003_01_5yr.sav").
*
*------------------------------------------------------------------------------------------------------------------------*.

GET DATA  /TYPE=TXT
  /FILE="FILEPATH\BA003_01_5yr.txt"
  /ENCODING='UTF8'
  /DELCASE=LINE
  /DELIMITERS="\t"
  /ARRANGEMENT=DELIMITED
  /FIRSTCASE=2
  /IMPORTCASE=ALL
  /VARIABLES=
  Municipal A21
  County A10
  State A3
  Region A13
  BA003_001 A9
  BA003_002 A19
  BA003_003 F10.0
  BA003_004 A55
  BA003_005 F9.0
  BA003_006 A5
  BA003_007 A3
  BA003_008 F9.0
  BA003_009 F4.0
  BA003_010 F4.1
  BA003_011 F4.1
  BA003_012 F9.0
  BA003_013 F4.0
  BA003_014 F4.1
  BA003_015 F4.1
  BA003_016 F8.0
  BA003_017 A5
  BA003_018 F3.1
  BA003_019 A5
  BA003_020 F8.0
  BA003_021 F5.0
  BA003_022 F3.1
  BA003_023 F4.1
  BA003_024 F8.0
  BA003_025 F5.0
  BA003_026 F4.1
  BA003_027 F3.1
  BA003_028 F8.0
  BA003_029 F5.0
  BA003_030 F4.1
  BA003_031 F4.1
  BA003_032 F8.0
  BA003_033 F5.0
  BA003_034 F4.1
  BA003_035 F4.1
  BA003_036 F8.0
  BA003_037 F4.0
  BA003_038 F4.1
  BA003_039 F4.1
  BA003_040 F8.0
  BA003_041 F4.0
  BA003_042 F4.1
  BA003_043 F4.1
  BA003_044 F8.0
  BA003_045 F4.0
  BA003_046 F4.1
  BA003_047 F4.1
  BA003_048 F8.0
  BA003_049 F5.0
  BA003_050 F4.1
  BA003_051 F4.1
  BA003_052 F8.0
  BA003_053 F5.0
  BA003_054 F4.1
  BA003_055 F3.1
  BA003_056 F8.0
  BA003_057 F4.0
  BA003_058 F4.1
  BA003_059 F4.1
  BA003_060 F8.0
  BA003_061 F5.0
  BA003_062 F4.1
  BA003_063 F4.1
  BA003_064 F7.0
  BA003_065 F5.0
  BA003_066 F3.1
  BA003_067 F3.1
  BA003_068 F4.1
  BA003_069 F4.1
  BA003_070 A3
  BA003_071 A3
  BA003_072 F9.0
  BA003_073 A5
  BA003_074 A3
  BA003_075 F9.0
  BA003_076 A5
  BA003_077 F4.1
  BA003_078 A5
  BA003_079 F9.0
  BA003_080 A5
  BA003_081 F4.1
  BA003_082 A5
  BA003_083 F8.0
  BA003_084 F4.0
  BA003_085 A3
  BA003_086 F8.0
  BA003_087 F4.0
  BA003_088 F4.1
  BA003_089 F4.1
  BA003_090 F8.0
  BA003_091 A5
  BA003_092 F4.1
  BA003_093 F4.1
  BA003_094 F9.0
  BA003_095 F5.0
  BA003_096 F4.1
  BA003_097 F4.1
  BA003_098 F8.0
  BA003_099 F5.0
  BA003_100 F4.1
  BA003_101 F4.1
  BA003_102 F7.0
  BA003_103 F5.0
  BA003_104 F4.1
  BA003_105 F4.1
  BA003_106 F8.0
  BA003_107 F5.0
  BA003_108 F4.1
  BA003_109 F4.1
  BA003_110 F6.0
  BA003_111 F4.0
  BA003_112 F3.1
  BA003_113 F4.1
  BA003_114 F8.0
  BA003_115 F5.0
  BA003_116 F4.1
  BA003_117 F4.1
  BA003_118 F8.0
  BA003_119 A5
  BA003_120 F4.1
  BA003_121 A5
  BA003_122 F9.0
  BA003_123 A5
  BA003_124 F4.1
  BA003_125 A5
  BA003_126 F7.0
  BA003_127 F5.0
  BA003_128 F3.1
  BA003_129 F4.1.


VARIABLE LABELS
Municipal	"	Municipal			"	/
County	"	County			"	/
State	"	State			"	/
Region	"	Region			"	/
BA003_001	"	BA003_001	:	Five Year Average	"	/
BA003_002	"	BA003_002	:	Id	"	/
BA003_003	"	BA003_003	:	Id2	"	/
BA003_004	"	BA003_004	:	Geography	"	/
BA003_005	"	BA003_005	:	Estimate; SEX AND AGE - Total population	"	/
BA003_006	"	BA003_006	:	Estimate Margin of Error; SEX AND AGE - Total population	"	/
BA003_007	"	BA003_007	:	Percent Margin of Error; SEX AND AGE - Total population	"	/
BA003_008	"	BA003_008	:	Estimate; SEX AND AGE - Male	"	/
BA003_009	"	BA003_009	:	Estimate Margin of Error; SEX AND AGE - Male	"	/
BA003_010	"	BA003_010	:	Percent; SEX AND AGE - Male	"	/
BA003_011	"	BA003_011	:	Percent Margin of Error; SEX AND AGE - Male	"	/
BA003_012	"	BA003_012	:	Estimate; SEX AND AGE - Female	"	/
BA003_013	"	BA003_013	:	Estimate Margin of Error; SEX AND AGE - Female	"	/
BA003_014	"	BA003_014	:	Percent; SEX AND AGE - Female	"	/
BA003_015	"	BA003_015	:	Percent Margin of Error; SEX AND AGE - Female	"	/
BA003_016	"	BA003_016	:	Estimate; SEX AND AGE - Under 5 years	"	/
BA003_017	"	BA003_017	:	Estimate Margin of Error; SEX AND AGE - Under 5 years	"	/
BA003_018	"	BA003_018	:	Percent; SEX AND AGE - Under 5 years	"	/
BA003_019	"	BA003_019	:	Percent Margin of Error; SEX AND AGE - Under 5 years	"	/
BA003_020	"	BA003_020	:	Estimate; SEX AND AGE - 5 to 9 years	"	/
BA003_021	"	BA003_021	:	Estimate Margin of Error; SEX AND AGE - 5 to 9 years	"	/
BA003_022	"	BA003_022	:	Percent; SEX AND AGE - 5 to 9 years	"	/
BA003_023	"	BA003_023	:	Percent Margin of Error; SEX AND AGE - 5 to 9 years	"	/
BA003_024	"	BA003_024	:	Estimate; SEX AND AGE - 10 to 14 years	"	/
BA003_025	"	BA003_025	:	Estimate Margin of Error; SEX AND AGE - 10 to 14 years	"	/
BA003_026	"	BA003_026	:	Percent; SEX AND AGE - 10 to 14 years	"	/
BA003_027	"	BA003_027	:	Percent Margin of Error; SEX AND AGE - 10 to 14 years	"	/
BA003_028	"	BA003_028	:	Estimate; SEX AND AGE - 15 to 19 years	"	/
BA003_029	"	BA003_029	:	Estimate Margin of Error; SEX AND AGE - 15 to 19 years	"	/
BA003_030	"	BA003_030	:	Percent; SEX AND AGE - 15 to 19 years	"	/
BA003_031	"	BA003_031	:	Percent Margin of Error; SEX AND AGE - 15 to 19 years	"	/
BA003_032	"	BA003_032	:	Estimate; SEX AND AGE - 20 to 24 years	"	/
BA003_033	"	BA003_033	:	Estimate Margin of Error; SEX AND AGE - 20 to 24 years	"	/
BA003_034	"	BA003_034	:	Percent; SEX AND AGE - 20 to 24 years	"	/
BA003_035	"	BA003_035	:	Percent Margin of Error; SEX AND AGE - 20 to 24 years	"	/
BA003_036	"	BA003_036	:	Estimate; SEX AND AGE - 25 to 34 years	"	/
BA003_037	"	BA003_037	:	Estimate Margin of Error; SEX AND AGE - 25 to 34 years	"	/
BA003_038	"	BA003_038	:	Percent; SEX AND AGE - 25 to 34 years	"	/
BA003_039	"	BA003_039	:	Percent Margin of Error; SEX AND AGE - 25 to 34 years	"	/
BA003_040	"	BA003_040	:	Estimate; SEX AND AGE - 35 to 44 years	"	/
BA003_041	"	BA003_041	:	Estimate Margin of Error; SEX AND AGE - 35 to 44 years	"	/
BA003_042	"	BA003_042	:	Percent; SEX AND AGE - 35 to 44 years	"	/
BA003_043	"	BA003_043	:	Percent Margin of Error; SEX AND AGE - 35 to 44 years	"	/
BA003_044	"	BA003_044	:	Estimate; SEX AND AGE - 45 to 54 years	"	/
BA003_045	"	BA003_045	:	Estimate Margin of Error; SEX AND AGE - 45 to 54 years	"	/
BA003_046	"	BA003_046	:	Percent; SEX AND AGE - 45 to 54 years	"	/
BA003_047	"	BA003_047	:	Percent Margin of Error; SEX AND AGE - 45 to 54 years	"	/
BA003_048	"	BA003_048	:	Estimate; SEX AND AGE - 55 to 59 years	"	/
BA003_049	"	BA003_049	:	Estimate Margin of Error; SEX AND AGE - 55 to 59 years	"	/
BA003_050	"	BA003_050	:	Percent; SEX AND AGE - 55 to 59 years	"	/
BA003_051	"	BA003_051	:	Percent Margin of Error; SEX AND AGE - 55 to 59 years	"	/
BA003_052	"	BA003_052	:	Estimate; SEX AND AGE - 60 to 64 years	"	/
BA003_053	"	BA003_053	:	Estimate Margin of Error; SEX AND AGE - 60 to 64 years	"	/
BA003_054	"	BA003_054	:	Percent; SEX AND AGE - 60 to 64 years	"	/
BA003_055	"	BA003_055	:	Percent Margin of Error; SEX AND AGE - 60 to 64 years	"	/
BA003_056	"	BA003_056	:	Estimate; SEX AND AGE - 65 to 74 years	"	/
BA003_057	"	BA003_057	:	Estimate Margin of Error; SEX AND AGE - 65 to 74 years	"	/
BA003_058	"	BA003_058	:	Percent; SEX AND AGE - 65 to 74 years	"	/
BA003_059	"	BA003_059	:	Percent Margin of Error; SEX AND AGE - 65 to 74 years	"	/
BA003_060	"	BA003_060	:	Estimate; SEX AND AGE - 75 to 84 years	"	/
BA003_061	"	BA003_061	:	Estimate Margin of Error; SEX AND AGE - 75 to 84 years	"	/
BA003_062	"	BA003_062	:	Percent; SEX AND AGE - 75 to 84 years	"	/
BA003_063	"	BA003_063	:	Percent Margin of Error; SEX AND AGE - 75 to 84 years	"	/
BA003_064	"	BA003_064	:	Estimate; SEX AND AGE - 85 years and over	"	/
BA003_065	"	BA003_065	:	Estimate Margin of Error; SEX AND AGE - 85 years and over	"	/
BA003_066	"	BA003_066	:	Percent; SEX AND AGE - 85 years and over	"	/
BA003_067	"	BA003_067	:	Percent Margin of Error; SEX AND AGE - 85 years and over	"	/
BA003_068	"	BA003_068	:	Estimate; SEX AND AGE - Median age (years)	"	/
BA003_069	"	BA003_069	:	Estimate Margin of Error; SEX AND AGE - Median age (years)	"	/
BA003_070	"	BA003_070	:	Percent; SEX AND AGE - Median age (years)	"	/
BA003_071	"	BA003_071	:	Percent Margin of Error; SEX AND AGE - Median age (years)	"	/
BA003_072	"	BA003_072	:	Estimate; SEX AND AGE - 18 years and over	"	/
BA003_073	"	BA003_073	:	Estimate Margin of Error; SEX AND AGE - 18 years and over	"	/
BA003_074	"	BA003_074	:	Percent Margin of Error; SEX AND AGE - 18 years and over	"	/
BA003_075	"	BA003_075	:	Estimate; SEX AND AGE - 18 years and over - Male	"	/
BA003_076	"	BA003_076	:	Estimate Margin of Error; SEX AND AGE - 18 years and over - Male	"	/
BA003_077	"	BA003_077	:	Percent; SEX AND AGE - 18 years and over - Male	"	/
BA003_078	"	BA003_078	:	Percent Margin of Error; SEX AND AGE - 18 years and over - Male	"	/
BA003_079	"	BA003_079	:	Estimate; SEX AND AGE - 18 years and over - Female	"	/
BA003_080	"	BA003_080	:	Estimate Margin of Error; SEX AND AGE - 18 years and over - Female	"	/
BA003_081	"	BA003_081	:	Percent; SEX AND AGE - 18 years and over - Female	"	/
BA003_082	"	BA003_082	:	Percent Margin of Error; SEX AND AGE - 18 years and over - Female	"	/
BA003_083	"	BA003_083	:	Estimate; SEX AND AGE - 65 years and over	"	/
BA003_084	"	BA003_084	:	Estimate Margin of Error; SEX AND AGE - 65 years and over	"	/
BA003_085	"	BA003_085	:	Percent Margin of Error; SEX AND AGE - 65 years and over	"	/
BA003_086	"	BA003_086	:	Estimate; SEX AND AGE - 65 years and over - Male	"	/
BA003_087	"	BA003_087	:	Estimate Margin of Error; SEX AND AGE- 65 years and over  - Male	"	/
BA003_088	"	BA003_088	:	Percent; SEX AND AGE- 65 years and over  - Male	"	/
BA003_089	"	BA003_089	:	Percent Margin of Error; SEX AND AGE - 65 years and over - Male	"	/
BA003_090	"	BA003_090	:	Estimate; SEX AND AGE- 65 years and over  - Female	"	/
BA003_091	"	BA003_091	:	Estimate Margin of Error; SEX AND AGE - 65 years and over - Female	"	/
BA003_092	"	BA003_092	:	Percent; SEX AND AGE - 65 years and over - Female	"	/
BA003_093	"	BA003_093	:	Percent Margin of Error; SEX AND AGE - 65 years and over - Female	"	/
BA003_094	"	BA003_094	:	Estimate; RACE - White	"	/
BA003_095	"	BA003_095	:	Estimate Margin of Error; RACE - White	"	/
BA003_096	"	BA003_096	:	Percent; RACE - White	"	/
BA003_097	"	BA003_097	:	Percent Margin of Error; RACE - White	"	/
BA003_098	"	BA003_098	:	Estimate; RACE - Black or African American	"	/
BA003_099	"	BA003_099	:	Estimate Margin of Error; RACE - Black or African American	"	/
BA003_100	"	BA003_100	:	Percent; RACE - Black or African American	"	/
BA003_101	"	BA003_101	:	Percent Margin of Error; RACE - Black or African American	"	/
BA003_102	"	BA003_102	:	Estimate; RACE - American Indian and Alaska Native	"	/
BA003_103	"	BA003_103	:	Estimate Margin of Error; RACE - American Indian and Alaska Native	"	/
BA003_104	"	BA003_104	:	Percent; RACE - American Indian and Alaska Native	"	/
BA003_105	"	BA003_105	:	Percent Margin of Error; RACE - American Indian and Alaska Native	"	/
BA003_106	"	BA003_106	:	Estimate; RACE - Asian	"	/
BA003_107	"	BA003_107	:	Estimate Margin of Error; RACE - Asian	"	/
BA003_108	"	BA003_108	:	Percent; RACE - Asian	"	/
BA003_109	"	BA003_109	:	Percent Margin of Error; RACE - Asian	"	/
BA003_110	"	BA003_110	:	Estimate; RACE - Native Hawaiian and Other Pacific Islander	"	/
BA003_111	"	BA003_111	:	Estimate Margin of Error; RACE - Native Hawaiian and Other Pacific Islander	"	/
BA003_112	"	BA003_112	:	Percent; RACE - Native Hawaiian and Other Pacific Islander	"	/
BA003_113	"	BA003_113	:	Percent Margin of Error; RACE - Native Hawaiian and Other Pacific Islander	"	/
BA003_114	"	BA003_114	:	Estimate; RACE - Some other race	"	/
BA003_115	"	BA003_115	:	Estimate Margin of Error; RACE - Some other race	"	/
BA003_116	"	BA003_116	:	Percent; RACE - Some other race	"	/
BA003_117	"	BA003_117	:	Percent Margin of Error; RACE - Some other race	"	/
BA003_118	"	BA003_118	:	Estimate; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"	/
BA003_119	"	BA003_119	:	Estimate Margin of Error; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"	/
BA003_120	"	BA003_120	:	Percent; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"	/
BA003_121	"	BA003_121	:	Percent Margin of Error; HISPANIC OR LATINO AND RACE - Hispanic or Latino (of any race)	"	/
BA003_122	"	BA003_122	:	Estimate; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"	/
BA003_123	"	BA003_123	:	Estimate Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"	/
BA003_124	"	BA003_124	:	Percent; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"	/
BA003_125	"	BA003_125	:	Percent Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino	"	/
BA003_126	"	BA003_126	:	Estimate; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"	/
BA003_127	"	BA003_127	:	Estimate Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"	/
BA003_128	"	BA003_128	:	Percent; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"	/
BA003_129	"	BA003_129	:	Percent Margin of Error; HISPANIC OR LATINO AND RACE - Not Hispanic or Latino - Two or more races	"	/
.
CACHE.
EXECUTE.
DATASET NAME DataSet1 WINDOW=FRONT.


*SAVE OUTFILE="filepath.sav".
