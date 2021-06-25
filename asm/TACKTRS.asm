*          DATA SET TACKTRS    AT LEVEL 001 AS OF 09/18/08                      
*PHASE TACKTRSA                                                                 
*                                                                               
         TITLE 'TACKTRS- TRSTAB FOR TALENT CHECK PROGRAM'                       
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
*DDDLCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE TACKREPD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TACKTRS   09/18/08'                                      
         END                                                                    
