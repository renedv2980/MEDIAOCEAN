*          DATA SET UNLDEXT    AT LEVEL 016 AS OF 05/01/02                      
*PHASE UNLDEXTA                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'UNLDEXT - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
UNLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,*UNLDEXT,RR=R5                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
UNXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         ST    R5,RELO                                                          
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,VPRNTBL                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BNE   *+14                                                             
         XC    COUNT,COUNT                                                      
         B     UNXIT               INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    UNXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    UNXEOF              END-OF-FILE                                  
         B     UNXIT                                                            
         SPACE 2                                                                
UNXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     UNXIT                                                            
         SPACE 2                                                                
UNXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     UNXIT                                                            
         SPACE 2                                                                
UNXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                                          
* UNIT, PACKAGE, HISTORY RECORDS                                  *             
*******************************************************************             
         SPACE 1                                                                
UNXREC   DS    0H                                                               
*                                                                               
         USING NURECD,R3                                                        
         L     R3,AREC                                                          
         CLI   0(R3),X'04'         UNIT                                         
         BNE   UNXKEEP                                                          
*        CLI   1(R3),X'93'         DNCH                                         
*        BNE   UNXKEEP             (USE UNIT DSECT FOR HIST REC)                
         LR    R4,R3                                                            
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         BNE   UNXKEEP                                                          
UNX04    MVC   BYTE,0(R4)                                                       
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),0                                                          
         BE    UNXKEEP                                                          
         CLC   0(1,R4),BYTE                                                     
         BH    UNX04                                                            
         BE    UNX04                                                            
         GOTO1 VPRNTBL,DMCB,=C'REC',AREC,C'DUMP',40,=C'1D'                      
         B     UNXKEEP                                                          
*                                                                               
UNX05    MVC   BYTE2,NUKCLT        PASS CLIENT                                  
         BAS   RE,CHKCLT           CHECK CLIENT                                 
         BE    UNXPURGE                                                         
         B     UNXKEEP                                                          
UNX10    CLI   0(R3),X'02'           PACKAGE RECORD                             
         BNE   UNX20                                                            
         DROP  R3                                                               
         USING NPRECD,R3                                                        
         MVC   BYTE2,NPKCLT        PASS CLIENT                                  
         BAS   RE,CHKCLT                                                        
         BE    UNXPURGE                                                         
         B     UNXKEEP                                                          
         DROP  R3                                                               
*                                                                               
UNX20    DS    0H                                                               
         B     UNXKEEP                                                          
*                                                                               
*****    GOTO1 VPRNTBL,DMCB,=C'REC2',AREC,C'DUMP',60,=C'1D'                     
                                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
* END-OF-FILE PROCESSING                                                        
*                                                                               
UNXEOF   DS    0H                                                               
*                                                                               
*        MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
*        GOTO1 VPRINTER                                                         
*        MVI   SPACING+3,C'1'                                                   
*        MVC   P+10(26),=C'SUMMARY OF RECORDS CHANGED'                          
*        L     R4,COUNT                                                         
*        EDIT  (R4),(10,P+37)                                                   
**       L     R4,DOLLARS                                                       
**       EDIT  (R4),(12,P+50)                                                   
**       GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
*                                                                               
         PRINT GEN                                                              
         GETEL R4,27,ELCODE                                                     
*                                                                               
CHKCLT   NTR1                                                                   
         LA    R1,CLIENTBL                                                      
CHKCLT5  CLC   BYTE2,3(R1)                                                      
         BE    CHKX                                                             
         LA    R1,5(R1)                                                         
         CLI   0(R1),0             EOF                                          
         BNE   CHKCLT5                                                          
         LTR   R1,R1                                                            
CHKX     XIT1                                                                   
*                                                                               
CLIENTBL DS    0H                                                               
         DC    CL3'BVT',XL2'86B3'                                               
         DC    CL3'CHE',XL2'88E4'                                               
         DC    CL3'CJR',XL2'8931'                                               
         DC    CL3'CTS',XL2'8A72'                                               
         DC    CL3'ELA',XL2'9160'                                               
         DC    CL3'EUR',XL2'9291'                                               
         DC    CL3'GTW',XL2'9A76'                                               
         DC    CL3'HDE',XL2'9C64'                                               
         DC    CL3'LGE',XL2'ACC4'                                               
         DC    CL3'MBL',XL2'B02B'                                               
         DC    CL3'MRX',XL2'B237'                                               
         DC    CL3'PKB',XL2'BD41'                                               
         DC    CL3'WDP',XL2'D86F'                                               
         DC    CL3'WDW',XL2'D876'                                               
         DC    CL3'WHV',XL2'D8F5'                                               
         DC    X'00'                                                            
*                                                                               
         SPACE 2                                                                
* LITERAL POOL                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0F                                                               
COUNT    DC    F'0'                                                             
DOLLARS  DC    F'0'                                                             
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
RELO     DS    A                                                                
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
VPRNTBL  DS    A                                                                
MFULL    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    CL1                                                              
BYTE2    DS    CL2                                                              
ELCODE   DS    CL1                                                              
NEW      DS    CL20                                                             
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE NEGENPACK                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPLAN                                                      
       ++INCLUDE SPTRNREV                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016UNLDEXT   05/01/02'                                      
         END                                                                    
