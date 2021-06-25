*          DATA SET CPREP3201  AT LEVEL 016 AS OF 09/01/00                      
*PHASE CP3201A                                                                  
         TITLE 'CPREP3201-COMBINED AGENCY REPORT'                               
         PRINT NOGEN                                                            
CP3201   CSECT                                                                  
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,91,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,91,AGYADD                                                     
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,91,REPORT                                                     
         SSPEC H4,117,PAGE                                                      
         SPROG 0,THRU,1                                                         
         SSPEC H07,22,C'       ----COMBINED--- ---AGENCY---      '              
         SSPEC H07,86,C'       ----COMBINED--- ---AGENCY---      '              
         SSPEC H08,22,C'HOMES  AVG                            CPP'              
         SSPEC H08,86,C'HOMES  AVG                            CPP'              
         SSPEC H09,01,C'MARKET RANK/NAME'                                       
         SSPEC H10,01,C'----------------'                                       
         SSPEC H09,22,C'UNIV.  RTG SPOTS  CPP  SPOTS    CPP  INDEX'             
         SSPEC H10,22,C'-----  --- -----  ---  -----    ---  -----'             
         SSPEC H09,65,C'MARKET RANK/NAME'                                       
         SSPEC H10,65,C'----------------'                                       
         SSPEC H09,86,C'UNIV.  RTG SPOTS  CPP  SPOTS    CPP  INDEX'             
         SSPEC H10,86,C'-----  --- -----  ---  -----    ---  -----'             
         SPROG 2                                                                
         SSPEC H09,01,C'MARKET RANK/NAME'                                       
         SSPEC H10,01,C'----------------'                                       
         SSPEC H09,22,C'TV HH   CPP   (000)   CPM   SPOTS  DOLLARS'             
         SSPEC H10,22,C'-----   ---   -----   ---   -----  -------'             
         SSPEC H09,65,C'MARKET RANK/NAME'                                       
         SSPEC H10,65,C'----------------'                                       
         SSPEC H09,86,C'TV HH   CPP   (000)   CPM   SPOTS  DOLLARS'             
         SSPEC H10,86,C'-----   ---   -----   ---   -----  -------'             
         SPROG 3                                                                
         SSPEC H09,01,C'PROGRAM/DAYPART '                                       
         SSPEC H10,01,C'--------------- '                                       
         SSPEC H09,22,C'TV HH   CPP   (000)   CPM   SPOTS  DOLLARS'             
         SSPEC H10,22,C'-----   ---   -----   ---   -----  -------'             
         SSPEC H09,65,C'PROGRAM/DAYPART '                                       
         SSPEC H10,65,C'----------------'                                       
         SSPEC H09,86,C'TV HH   CPP   (000)   CPM   SPOTS  DOLLARS'             
         SSPEC H10,86,C'-----   ---   -----   ---   -----  -------'             
         SPROG 4                                                                
         SSPEC H07,22,C'       ----COMBINED--- ---AGENCY---      '              
         SSPEC H07,86,C'       ----COMBINED--- ---AGENCY---      '              
         SSPEC H08,22,C' DEMO  AVG                            CPP'              
         SSPEC H08,86,C' DEMO  AVG                            CPP'              
         SSPEC H09,01,C'PROGRAM/DAYPART '                                       
         SSPEC H10,01,C'--------------- '                                       
         SSPEC H09,22,C'UNIV.  RTG SPOTS  CPP  SPOTS    CPP  INDEX'             
         SSPEC H10,22,C'-----  --- -----  ---  -----    ---  -----'             
         SSPEC H09,65,C'PROGRAM/DAYPART '                                       
         SSPEC H10,65,C'----------------'                                       
         SSPEC H09,86,C'UNIV.  RTG SPOTS  CPP  SPOTS    CPP  INDEX'             
         SSPEC H10,86,C'-----  --- -----  ---  -----    ---  -----'             
         SPROG 5                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016CPREP3201 09/01/00'                                      
         END                                                                    
