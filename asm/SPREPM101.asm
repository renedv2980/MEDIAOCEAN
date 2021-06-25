*          DATA SET SPREPM101  AT LEVEL 016 AS OF 08/29/00                      
*PHASE SPM101A                                                                  
         TITLE 'SPREPM101-CPP EXTRACT'                                          
         PRINT NOGEN                                                            
SPM101   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,STATION                                                      
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,97,AGYADD                                                     
         SSPEC H3,1,PAGE                                                        
         SSPEC H3,97,REPORT                                                     
         SSPEC H3,50,PERIOD                                                     
         SSPEC H5,1,CLIENT                                                      
         SPROG 1                                                                
         SSPEC H1,50,C'CPP EXTRACT RULES'                                       
         SSPEC H2,50,C'-----------------'                                       
         SSPEC H07,1,C'CLT  PRD  EST  START    END'                             
         SSPEC H08,1,C'---  ---  ---  -----    ---'                             
         SSPEC H07,35,C'DEMOS'                                                  
         SSPEC H08,35,C'-----'                                                  
         SPROG 2                                                                
         SSPEC H1,50,C'CPP EXTRACT AUDIT REPORT'                                
         SSPEC H2,50,C'------------------------'                                
         SSPEC H07,1,C'RTG'                                                     
         SSPEC H08,1,C'SRV'                                                     
         SSPEC H09,1,C'---'                                                     
         SSPEC H07,5,C'        RPT         PRG NET/                  '          
         SSPEC H08,5,C'TARGET  DEM     DPT TYP IND  SPOTS     DOLLARS'          
         SSPEC H09,5,C'------  ---     --- --- ---  -----     -------'          
         SSPEC H07,52,C'                                  BONUS'                
         SSPEC H08,52,C' RATINGS IMPRESSIONS     SPECIAL  SPOTS'                
         SSPEC H09,52,C' ------- -----------     -------  -----'                
         SPROG 3                                                                
         SSPEC H1,50,C'CPP AFFILIATE ASSIGNMENT REPORT'                         
         SSPEC H2,50,C'------------------------'                                
         SSPEC H7,2,C'NSI'                                                      
         SSPEC H8,1,C'AFFIL'                                                    
         SSPEC H9,1,C'-----'                                                    
         SSPEC H7,2,C'NSI'                                                      
         SSPEC H8,9,C'STATION'                                                  
         SSPEC H9,9,C'-------'                                                  
         SSPEC H7,19,C'CPPRS'                                                   
         SSPEC H8,19,C'AFFIL'                                                   
         SSPEC H9,19,C'-----'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SPREPM101 08/29/00'                                      
         END                                                                    
