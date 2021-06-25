*          DATA SET TACKTRST   AT LEVEL 006 AS OF 03/02/01                      
*PHASE TACKTRSA TACKTRST                                                        
*                                                                               
         TITLE 'TACKTRST- TRSTAB FOR TALENT CHECK PROGRAM'                      
TACKTRST CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF W4 TRUSTEES                                                   
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(TRSTLEN)        RECORD LENGTH                                
         DC    AL4(TRSTLKEY)       DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXW4TS)        MAX. NUMBER OF RECORDS                       
         DC    AL1(0)              NUMBER OF BUCKETS                            
         DC    AL1(0)              DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXW4TS*TRSTLEN)X'00'  THE TABLE                                
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TACKTRST  03/02/01'                                      
         END                                                                    
