*          DATA SET TACKACC    AT LEVEL 003 AS OF 03/02/01                      
*PHASE TACKACCA                                                                 
*                                                                               
         TITLE 'TACKACC - ACCTAB FOR TALENT CHECK PROGRAM'                      
ACCTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF ACCOUNT ACCUMULATORS                                          
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(ACCLEN)         RECORD LENGTH                                
         DC    AL4(ACCLKEY)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXACCS)        MAX. NUMBER OF RECORDS                       
         DC    AL1((ACCLEN-ACCDSP)/4)   NUMBER OF BUCKETS                       
         DC    AL1(ACCDSP)         DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXACCS*ACCLEN)X'00'  THE TABLE                                 
*                                                                               
*TACKREPD                                                                       
*TAGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TACKACC   03/02/01'                                      
         END                                                                    
