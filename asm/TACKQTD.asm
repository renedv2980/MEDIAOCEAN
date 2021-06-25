*          DATA SET TACKQTD    AT LEVEL 004 AS OF 02/09/12                      
*PHASE TACKQTDA                                                                 
*                                                                               
         TITLE 'TACKPQT - PQTDTAB FOR TALENT CHECK PROGRAM'                     
QTDTAB   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*        TABLE OF PERFORMER QTD AMOUNTS                                         
*                                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(QTDLEN)         RECORD LENGTH                                
         DC    AL4(QTDLKEY)        DISP. TO KEY / KEY LENGTH                    
         DC    AL4(MAXQTDS)        MAX. NUMBER OF RECORDS                       
         DC    AL1((QTDLEN-QTDDSP)/4)   NUMBER OF BUCKETS                       
         DC    AL1(QTDDSP)         DISP TO BUCKETS                              
         DC    X'80'               BINARY                                       
         DC    AL1(0)                                                           
         DC    (MAXQTDS*QTDLEN)X'00'  THE TABLE                                 
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
**PAN#1  DC    CL21'004TACKQTD   02/09/12'                                      
         END                                                                    
