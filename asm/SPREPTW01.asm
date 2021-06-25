*          DATA SET SPREPTW01  AT LEVEL 004 AS OF 04/29/87                      
*PHASE SPTW01A,*                                                                
         TITLE 'SPTW01 - CREATE TRAFFIC TWX RECS ON CONTROL FILE'               
*        PRINT NOGEN                                                            
SPTW01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         SPROG 0,THRU,8                                                         
         SSPEC H1,44,C'TRAFFIC STATION TWX AND ADDRESS'                         
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,44,C'--------------------------'                              
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,100,PAGE                                                      
*                                                                               
         SSPEC H6,3,C'MEDIA'                                                    
         SSPEC H6,10,C'STATION'                                                 
         SSPEC H6,20,C'TWX NUMBER'                                              
         SSPEC H6,40,C'TWX ANSWERBACK'                                          
         SSPEC H6,65,C'CODE'                                                    
         SSPEC H6,75,C'STATION ADDRESS'                                         
         SSPEC H7,3,C'-----'                                                    
         SSPEC H7,10,C'-------'                                                 
         SSPEC H7,20,C'----------'                                              
         SSPEC H7,40,C'--------------'                                          
         SSPEC H7,65,C'----'                                                    
         SSPEC H7,75,C'---------------'                                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPTW01 04/29/87'                                      
         END                                                                    
