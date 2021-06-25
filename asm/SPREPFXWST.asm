*          DATA SET SPREPFXWST AT LEVEL 111 AS OF 04/10/96                      
*PHASE SPFX02Z                                                                  
         TITLE 'SPFX02 - MAYBE GET RECORDS BACK TO BEFORE OTO EXTERN'           
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
         CLI   MODE,REQLAST                                                     
         BE    FX200                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
FX       DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVAGYMD                                                   
FX05     GOTO1 HIGH                                                             
         MVC   MYCLT,KEY+1                                                      
         B     FX20                                                             
*                                                                               
FX10     GOTO1 SEQ                                                              
*                                                                               
FX20     CLC   KEY(1),SVAGYMD                                                   
         BNE   EXIT                                                             
         CLC   MYCLT,KEY+1         IF CLIENT IS DIFFERENT                       
         BE    *+12                                                             
         MVI   KEY+3,X'FF'         BUMP TO NEXT CLIENT                          
         B     FX05                                                             
*                                                                               
         AP    KEYCNT,=P'1'                                                     
         L     R1,ADBUY                                                         
         ST    R1,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC                                                          
         LA    R6,24(R6)           POINT TO BDELEM                              
FX25     CLI   0(R6),0                                                          
         BE    FX10                                                             
         CLI   0(R6),X'0C'                                                      
         BE    FX50                                                             
         ST    R6,SVADR            SAVE ADRESS OF PREVIOUS ELEM                 
FX30     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     FX25                                                             
*                                                                               
FX50     DS    0H                                                               
         TM    6(R6),X'80'         MINUS?                                       
         BNZ   *+12                                                             
         ST    R6,SVADR            NO SO SAVE ADRESS...                         
         B     FX30                AND GET NEXT ELEM                            
*                                                                               
         L     R1,SVADR            POINT TO PREVIOUS ELEMENT                    
         CLI   0(R1),X'0B'                                                      
         BE    FX60                                                             
         CLI   0(R1),X'0C'                                                      
         BNE   FX75                                                             
*                                                                               
FX60     TM    6(R1),X'40'                                                      
         BNZ   FX100                                                            
*                                                                               
FX75     DS    0H                  PREV ELEM NOT 0B OR OC SO ITS WRONG          
         AP    RECFND,=P'1'                                                     
         MVC   P+1(20),=C'RECORDS DO NOT MATCH'                                 
         GOTO1 REPORT                                                           
*                                                                               
         L     R6,AREC                                                          
         SR    R3,R3                                                            
         ICM   R3,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'TAPEREC',(R6),C'DUMP',(R3),=C'1D00'               
         GOTO1 REPORT                                                           
*                                                                               
FX100    B     FX10                                                             
*                                                                               
FX200    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,COUNTERS                                                      
         LA    R4,24                                                            
         LA    R5,COUNTERX                                                      
FX210    DS    0H                                                               
         OI    3(R3),X'0F'                                                      
         UNPK  P(8),0(4,R3)                                                     
         MVC   P+10(20),4(R3)                                                   
         GOTO1 REPORT                                                           
         BXLE  R3,R4,FX210                                                      
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
COUNTERS DS    0D                                                               
RECFND   DC    PL4'0',CL20'ODD RECORDS FOUND'                                   
KEYCNT   DC    PL4'0',CL20'KEYS READ'                                           
COUNTERX EQU   *-1                                                              
*                                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
ELCODE   DS    X                                                                
MYCLT    DS    XL2                                                              
SVADR    DS    F                                                                
RECORD   DS    CL50                                                             
SPACE    DS    XL2000                                                           
                                                                                
*                                                                               
AGENCYD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'111SPREPFXWST04/10/96'                                      
         END                                                                    
