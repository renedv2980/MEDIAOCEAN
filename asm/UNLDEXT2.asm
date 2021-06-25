*          DATA SET UNLDEXT2   AT LEVEL 091 AS OF 05/01/02                      
*PHASE UNLDEX2A,+0                                                              
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
         TM    NUPACKST,X'20'      LOCKED?                                      
         BNO   UNXKEEP                                                          
         TM    NURSTAT,X'80'       DELETED                                      
         BNO   UNXKEEP                                                          
         MVI   ELCODE,X'10'        BILLED?                                      
         BAS   RE,GETEL                                                         
         BNE   UNXKEEP                                                          
         GOTO1 VPRNTBL,DMCB,=C'REC',AREC,C'DUMP',70,=C'1D'                      
         B     UNXKEEP                                                          
*                                                                               
*                                                                               
                                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
* END-OF-FILE PROCESSING                                                        
*                                                                               
UNXEOF   DS    0H                                                               
*                                                                               
**       MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
**       GOTO1 VPRINTER                                                         
**       MVI   SPACING+3,C'1'                                                   
**       MVC   P+10(26),=C'SUMMARY OF RECORDS CHANGED'                          
*8       L     R4,COUNT                                                         
**       EDIT  (R4),(10,P+37)                                                   
**       L     R4,DOLLARS                                                       
**       EDIT  (R4),(12,P+50)                                                   
**       GOTO1 VPRINTER                                                         
         B     UNXIT                                                            
*                                                                               
         PRINT GEN                                                              
         GETEL R3,27,ELCODE                                                     
*                                                                               
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
**PAN#1  DC    CL21'091UNLDEXT2  05/01/02'                                      
         END                                                                    
