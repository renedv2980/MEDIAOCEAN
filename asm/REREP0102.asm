*          DATA SET REREP0102  AT LEVEL 005 AS OF 08/31/00                      
*          DATA SET REREP0102  AT LEVEL 004 AS OF 03/05/96                      
*PHASE RE0102A                                                                  
         TITLE 'REREP0102 - TEST FOR MEL'                                       
RE0102   CSECT                                                                  
         DS    5000C                                                            
         ORG   *-5000                                                           
         PRINT NOGEN                                                            
         NMOD1 0,**RE0102,RR=RE                                                 
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         EJECT                                                                  
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXXMOD              DO ALL IN REQFRST                            
*                                                                               
* READ A STATION RECORD TO MAKE SURE CAN READ DIR AND FILE                      
*                                                                               
MAIN     DS    0H                                                               
         LA    RF,RSTAREC                                                       
         ST    RF,AIOAREA                                                       
         XC    KEY,KEY             GET FIRST COMBO STATION REC                  
         MVI   KEY,X'02'                                                        
         GOTO1 HIGH                                                             
MAIN05   CLI   KEY,X'02'           MAKE SURE STILL STA RECS                     
         BE    *+6                                                              
         DC    H'0'                NO COMBO RECS FOUND                          
**NOP    CLI   KEY+26,C'C'                                                      
**NOP    BE    MAIN07                                                           
**NOP    GOTO1 SEQ                                                              
**NOP    B     MAIN05                                                           
&                                                                               
MAIN07   DS    0H                                                               
         GOTO1 GREC                                                             
*                                                                               
EXXMOD   XMOD1                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
* DATAMGR INTERFACE                                                             
       ++INCLUDE RGENIO                                                         
*                                                                               
* WORKING STORAGE                                                               
LASTCM   DS    CL27                KEY OF LAST COMBO REC CHANGED                
RELO     DS    F                                                                
COMMAND  DS    CL8                                                              
AIOAREA  DS    A                                                                
NEWEL    DS    CL7                                                              
AM       DS    CL4                                                              
FM       DS    CL4                                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP0102 08/31/00'                                      
         END                                                                    
