*          DATA SET ACREPFR01S AT LEVEL 010 AS OF 09/16/02                      
*PHASE ACFR01A                                                                  
         TITLE 'ACFR01 - SPECS FOR TOYOTA FEE REPORT'                           
ACFR01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         RSPEC MAXLINES,55                                                      
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         SPROG 0,1,2,3,4,5,6,7,8,9                                              
         ACDEF F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 0,1,2,3,4,5,8,9                                                  
         ACDEF H1,1,RUN                                                         
         ACDEF H3,1,COMPANY                                                     
         ACDEF H1,102,REPORT                                                    
         ACDEF H1,115,PAGE                                                      
         ACDEF H2,102,REQUESTOR                                                 
         ACDEF H11,1,CL5'MONTH'                                                 
         ACDEF H10,11,CL41'-------------------M O N T H-------------'           
         ACDEF H11,11,CL41'SALARY           PROD       TOTAL        '           
         ACDEF H12,11,CL41'                 HOURS      HOURS        '           
         ACDEF H10,52,CL6'------'                                               
         ACDEF H11,52,CL6'CLIENT'                                               
         ACDEF H12,52,CL6'HOURS '                                               
*                                                                               
         ACDEF H10,63,CL37'--------Y E A R  T O  D A T E--------'               
         ACDEF H11,63,CL37'PROD      ACTUAL    HOURLY     SALARY'               
         ACDEF H12,63,CL37'HOURS     --%---    RATE       COSTS '               
*                                                                               
         ACDEF H10,105,CL28'---------M O N T H----------'                       
         ACDEF H11,107,CL27'SALARY       FINAL CHARGE '                         
         ACDEF H12,107,CL27'COSTS          FACTOR A   '                         
*                                                                               
         SPROG 8,9                                                              
         ACDEF H10,63,CL37'--------Y E A R  T O  D A T E--------'               
         ACDEF H11,63,CL37'PRD HRS   PRD HRS   ACTUAL     SALARY'               
         ACDEF H12,63,CL37'(GROUP)   (TOTAL)   --%---     COSTS '               
         ACDEF H10,105,CL28'-----MONTHLY ALLOCATION-----'                       
*                                                                               
         SPROG 9                                                                
         ACDEF H12,63,CL37'(*IND*)   (*ALL*)   --%---     COSTS '               
*                                                                               
         SPROG 6                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,112,PAGE                                                      
         ACDEF H2,46,CL39'ANALYSIS OF PERSONNEL TIME && COST RECAP'             
         ACDEF H3,46,39C'_'                                                     
         ACDEF H6,21,CL43' ACCOUNT        PROMOTIONS                 '          
         ACDEF H7,21,CL43' MANAGEMENT                      CREATIVE  '          
         ACDEF H8,21,CL43'_____________  _____________  _____________'          
         ACDEF H6,66,CL28'  PRINT        B''CAST PROD./'                        
         ACDEF H7,66,CL28'  SERVICES     B''CAST TRAFF.'                        
         ACDEF H8,66,CL28'_____________  _____________'                         
*        ACDEF H6,96,C'  ACCOUNT         MEDIA     '                            
*        ACDEF H6,96,C'                            '                            
         ACDEF H7,96,CL28'  PLANNING        PLANNING  '                         
         ACDEF H8,96,CL28'_____________  _____________'                         
         ACDEF H6,126,CL14'  BUSINESS   '                                       
         ACDEF H7,126,CL14'  && LEGAL   '                                       
         ACDEF H8,126,CL14'_____________'                                       
         ACDEF H6,141,CL14'              '                                      
         ACDEF H7,141,CL14'    TOTAL     '                                      
         ACDEF H8,141,CL14'______________'                                      
         EJECT                                                                  
         SPROG 7                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,112,PAGE                                                      
         ACDEF H2,49,CL33'ANALYSIS OF PERSONNEL TIME && COST'                   
         ACDEF H3,49,33C'_'                                                     
         ACDEF H7,5,C'DEPARTMENT/'                                              
         ACDEF H8,5,C'  TITLE AND NAME'                                         
         ACDEF H9,5,C'________________'                                         
         ACDEF H7,41,C'1ST QUARTER    2ND QUARTER    3RD QUARTER'               
         ACDEF H8,41,C'   HOURS          HOURS          HOURS   '               
         ACDEF H9,41,C'___________    ___________    ___________'               
         ACDEF H7,86,C'4TH QUARTER                      12 MONTH'               
         ACDEF H8,86,C'   HOURS               %           TOTAL '               
         ACDEF H9,86,C'___________            __________________'               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPFR01S09/16/02'                                      
         END                                                                    
