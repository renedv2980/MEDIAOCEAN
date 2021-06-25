*          DATA SET SPREPXD01  AT LEVEL 009 AS OF 08/29/00                      
*PHASE SPXD01A                                                                  
         TITLE 'SPREPXD01-DANCER PROGRAM/AFFILIATION REPORT SPECS'              
         PRINT NOGEN                                                            
SPMP01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,STATION                                                      
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 1,THRU,8                                                         
*                                                                               
         SSPEC H1,2,MEDIA                                                       
*                                                                               
*                                                                               
         SSPEC H1,83,AGYNAME                                                    
*                                                                               
         SSPEC H2,2,REQUESTOR                                                   
*                                                                               
*                                                                               
         SSPEC H2,83,AGYADD                                                     
*                                                                               
         SSPEC H3,83,C'RATING SERVICE - NSI'                                    
*                                                                               
         SSPEC H4,43,PERIOD                                                     
*                                                                               
         SSPEC H5,2,CLIENT                                                      
*                                                                               
         SSPEC H6,2,PRODUCT                                                     
*                                                                               
         SSPEC H6,43,MARKET                                                     
*                                                                               
         SSPEC H7,2,ESTIMATE                                                    
*                                                                               
         SSPEC H7,83,PAGE                                                       
*                                                                               
         SSPEC H7,93,REPORT                                                     
         SPROG 1                                                                
         SSPEC H01,43,C'PROGRAM ANALYSIS'                                       
         SSPEC H02,43,C'----------------'                                       
         SSPEC H10,1,C'         STATION             '                           
         SSPEC H11,1,C'STATION    TYPE  PROGRAM TYPE'                           
         SSPEC H12,1,C'-------  ------- ------------'                           
         SSPEC H10,42,C'         PERCENT'                                       
         SSPEC H11,42,C'DOLLARS  DOLLARS'                                       
         SSPEC H12,42,C'-------  -------'                                       
         SSPEC H10,62,C' IMPS   PERCENT'                                        
         SSPEC H11,62,C'(000)     IMPS '                                        
         SSPEC H12,62,C'-----   -------'                                        
         SPROG 2                                                                
         SSPEC H01,43,C'STATION ANALYSIS'                                       
         SSPEC H02,43,C'----------------'                                       
         SSPEC H10,1,C'                         PERCENT'                        
         SSPEC H11,1,C'STATION TYPE     DOLLARS DOLLARS'                        
         SSPEC H12,1,C'------------     ------- -------'                        
         SSPEC H10,39,C' IMP   PERCENT'                                         
         SSPEC H11,39,C'(000)    IMPS '                                         
         SSPEC H12,39,C'-----  -------'                                         
         SPROG 3                                                                
         SSPEC H01,43,C'TIME PERIOD ANALYSIS'                                   
         SSPEC H02,43,C'--------------------'                                   
         SSPEC H11,1,C'DAY'                                                     
         SSPEC H12,1,C'---'                                                     
         SSPEC H11,10,C'TIME'                                                   
         SSPEC H12,10,C'----'                                                   
         SSPEC H10,23,C'STATION   AVERAGE  PERCENT  PERCENT'                    
         SSPEC H11,23,C'  TYPE    RATING   DOLLARS   IMPS  '                    
         SSPEC H12,23,C'-------   -------  -------  -------'                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPXD01 08/29/00'                                      
         END                                                                    
