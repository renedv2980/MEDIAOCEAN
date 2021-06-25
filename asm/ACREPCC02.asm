*          DATA SET ACREPCC02  AT LEVEL 193 AS OF 08/16/00                      
*PHASE ACCC02A,*                                                                
         TITLE 'CLIENT BILLING / TMS CHECK'                                     
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACCC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCC**,R8,R9,RR=RE                                           
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=GENERAL W/S                               
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=LOCAL W/S                                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    EXIT                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,ACCLAST                                                     
         BE    EXIT                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC=HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC=LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC=EQUAL                                 
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
         SPACE 1                                                                
REQL     DS    0H                                                               
*                                                                               
         PUSH  USING                                                            
         USING TIMRECD,IOKEY                                                    
         MVC   TIMKEY,BCSPACES                                                  
         MVI   TIMKCPY,X'94'                                                    
         MVC   TIMKUNT(L'UL1R),UL1R                                             
         MVC   TIMKACT,QACCOUNT                                                 
*                                                                               
         LA    RF,DMRDHI                                                        
         B     *+8                                                              
REQL02   LA    RF,DMRSEQ                                                        
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 DATAMGR,BCPARM,(RF),ACCDIR,IOKEYSAV,IOKEY,IOWORK,0               
         BNE   REQL10                                                           
         CLC   TIMKEY(TIMKACT-TIMKEY),IOKEYSAV                                  
         BNE   REQL10                                                           
         CLC   QACCOUNT,BCSPACES                                                
         BE    *+14                                                             
         CLC   TIMKACT,QACCOUNT                                                 
         BNE   REQL10                                                           
         CLC   UL1C,TIMKCUNT                                                    
         BNE   REQL02                                                           
         CLC   TIMKREF,=C'*TIME*'                                               
         BNE   REQL02                                                           
         GOTO1 DATAMGR,BCPARM,DMGET,ACCMST,TIMKDA,IOREC,IOWORK,0                
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 CHKTRX,BCPARM,IOREC                                              
         BE    REQL02                                                           
         GOTO1 PRTXN,BCPARM,IOKEY                                               
         GOTO1 ACREPORT                                                         
         GOTO1 PRTXN,BCPARM,IOKEYSAV2                                           
         GOTO1 ACREPORT                                                         
         B     REQL02                                                           
*                                                                               
REQL10   B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK TIMELS                                             *         
*                                                                     *         
* P1 = A(RECORD)                                                      *         
***********************************************************************         
         SPACE 1                                                                
CHKTIM   NTR1  ,                                                                
         MVI   INDS,0                                                           
CTIM00   LA    R3,IOREC                                                         
         LA    R3,TIMRFST-TIMRECD(R3)                                           
*                                                                               
         MVI   STIMSEQ,0                                                        
         MVI   STIMETYP,0                                                       
         USING TIMELD,R3                                                        
         XR    RF,RF                                                            
CTIM02   CLI   TIMEL,0                                                          
         BE    CHKTIMY                                                          
         CLI   TIMEL,TIMELQ                                                     
         BNE   CTIM08                                                           
         CLC   TIMSEQ,STIMSEQ                                                   
         BL    CHKTIMN             SEQUENCE DECREASED - PROBLEM                 
         BH    CTIM06              SEQUENCE INCREASED - NO PROBLEM              
*                                  SEQUENCE THE SAME                            
         CLC   TIMETYP,STIMETYP                                                 
         BNH   CHKTIMN             TYPE NOT INCREASED - PROBLEM                 
*                                                                               
CTIM06   MVC   STIMSEQ,TIMSEQ                                                   
         MVC   STIMETYP,TIMETYP                                                 
*                                                                               
CTIM08   IC    RF,TIMLN                                                         
         BXH   R3,RF,CTIM02                                                     
         DROP  R3                                                               
*                                                                               
CHKTIMY  B     EXITY                                                            
*                                                                               
CHKTIMN  OI    INDS,INO                                                         
         GOTO1 CURCLU,(R3)                                                      
         B     CTIM00                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CORRECT CLUSTER                                          *         
***********************************************************************         
         SPACE 1                                                                
CURCLU   NTR1  ,                                                                
         LR    R3,R1                                                            
         USING TIMELD,R3                                                        
         MVC   CTIMSEQ,TIMSEQ                                                   
         MVI   CTIMETYP,0                                                       
         CLI   TIMETYP,TIMEINP                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R4,STIMSEQ                                                       
         LA    R4,1(R4)                                                         
         XR    RF,RF                                                            
CCLU02   CLI   TIMEL,TIMELQ                                                     
         BNE   EXIT                                                             
         CLC   TIMSEQ,CTIMSEQ                                                   
         BNE   CCLU10                                                           
         CLC   TIMETYP,CTIMETYP                                                 
         BNH   CCLU10                                                           
         STC   R4,TIMSEQ                                                        
         MVC   CTIMETYP,TIMETYP                                                 
         IC    RF,TIMLN                                                         
         BXH   R3,RF,CCLU02                                                     
         DROP  R3                                                               
*                                                                               
CCLU10   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK TRANSACTIONS                                       *         
*                                                                     *         
* P1 = A(RECORD)                                                      *         
***********************************************************************         
         SPACE 1                                                                
CHKTRX   NTR1  ,                                                                
         MVI   INDS,0                                                           
         L     R2,0(R1)                                                         
         USING TIMRECD,R2                                                       
         LA    R3,TIMRFST                                                       
*                                                                               
         USING TIMELD,R3                                                        
CTRX02   CLI   TIMEL,0                                                          
         BE    CHKTRXY                                                          
         CLI   TIMEL,TIMELQ                                                     
         BNE   CTRX08                                                           
         CLI   TIMETYP,TIMEINP                                                  
         BNE   CTRX08                                                           
*        CLC   TIMADAT,=X'960304'                                               
         CLC   TIMADAT,=X'960101'                                               
         BL    CTRX08                                                           
*        CLC   TIMADAT,=X'960310'                                               
*        BH    CTRX08                                                           
         GOTO1 SRCTRX,BCPARM,TIMRECD,TIMELD                                     
         BNE   CHKTRXN                                                          
*                                                                               
CTRX08   XR    RF,RF                                                            
         IC    RF,TIMLN                                                         
         BXH   R3,RF,CTRX02                                                     
         DROP  R3                                                               
*                                                                               
CHKTRXY  GOTO1 DATAMGR,BCPARM,DMREAD,ACCDIR,IOKEY,IOKEY,IOWORK,0                
         B     EXITY                                                            
*                                                                               
CHKTRXN  GOTO1 DATAMGR,BCPARM,DMREAD,ACCDIR,IOKEY,IOKEY,IOWORK,0                
         OI    INDS,INO                                                         
         B     EXITN                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SEARCH FOR THE CORRECT TRANSACTION RECORD                *         
*                                                                     *         
* NTRY: P1 = A(TIMRECD)                                               *         
*       P2 = A(TIMELD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SRCTRX   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING TIMRECD,R2                                                       
         USING TIMELD,R3                                                        
*                                                                               
         USING TRNRECD,IOKEY2                                                   
         MVC   TRNKEY,BCSPACES                                                  
         MVC   TRNKCPY,QCOMPANY                                                 
         MVC   TRNKUNT(L'TIMACC),TIMACC                                         
         MVC   TRNKWORK,TIMTSK                                                  
         MVC   TRNKCULC,TIMKCULA                                                
         MVC   TRNKDATE,TIMKPEDT                                                
         MVC   IOKEYSAV2,TRNKEY                                                 
         GOTO1 DATAMGR,BCPARM,DMRDHI,ACCDIR,IOKEYSAV2,IOKEY2,IOWORK,0           
         BNE   SRCTRXN                                                          
         CLC   TRNKEY(TRNKREF-TRNKEY),IOKEYSAV2                                 
         BNE   SRCTRXN                                                          
*                                                                               
SRCTRXY  B     EXITY                                                            
*                                                                               
SRCTRXN  B     EXITN                                                            
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT TRANSACTION DETAILS                                *         
*                                                                     *         
* NTRY: P1 = A (TRANSACTION RECORD)                                   *         
***********************************************************************         
         SPACE 1                                                                
PRTXN    NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING TRNRECD,R2                                                       
         PUSH  USING                                                            
         USING PLINED,P                                                         
         MVC   PACT,TRNKUNT                                                     
         MVC   PWC,TRNKWORK                                                     
         MVC   PCAC,TRNKCUNT                                                    
*        GOTO1 HEXOUT,BCPARM,TRNKDATE,PDATE,L'TRNKDATE,0                        
         GOTO1 DATCON,BCPARM,(1,TRNKDATE),(X'20',PDATE)                         
         MVC   PREF,TRNKREF                                                     
         GOTO1 HEXOUT,BCPARM,TRNKSBR,PSUB,L'TRNKSBR,0                           
         GOTO1 HEXOUT,BCPARM,TRNKDA,PDA,L'TRNKDA,0                              
         POP   USING                                                            
PRTXNX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
PUTREC   DC    C'PUTREC  '                                                      
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
BCSPACES DC    CL256' '                                                         
UL1C     DC    C'1C'                                                            
UL1R     DC    C'1R'                                                            
ULSJ     DC    C'SJ'                                                            
ACCMST   DC    C'ACCMST '                                                       
ACCDIR   DC    C'ACCDIR '                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
MTOTALS  DS    0CL40                                                            
         DC    CL40'NUMBER OF TRANSACTIONS'                                     
         DC    CL40'NUMBER OF TIME TRANSACTIONS'                                
         DC    CL40'NUMBER OF SUCCESSFUL GETTIMS'                               
         DC    CL40'NUMBER OF UNSUCCESSFUL GETTIMS'                             
         DC    CL40'NUMBER OF 1C ACCOUNTS ON PROFILE'                           
         DC    CL40'NUMBER OF 1C ACCOUNTS ON SPAEL'                             
         DC    CL40'NUMBER OF ALTERNATIVE 1CS FOUND'                            
         DC    CL40'NUMBER OF ALTERNATIVE DATES FOUND'                          
         EJECT                                                                  
***********************************************************************         
* LOCAL W/S                                                           *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
BCPARM   DS    6A                                                               
TIMEKEY  DS    XL64                                                             
IOKEY    DS    XL64                                                             
IOKEYSAV DS    XL64                                                             
IOKEY2   DS    XL64                                                             
IOKEYSAV2 DS    XL64                                                            
*                                                                               
INDS     DS    XL1                                                              
INO      EQU   X'80'                                                            
STIMSEQ  DS    XL(L'TIMSEQ)                                                     
STIMETYP DS    XL(L'TIMETYP)                                                    
CTIMSEQ  DS    XL(L'TIMSEQ)                                                     
CTIMETYP DS    XL(L'TIMETYP)                                                    
*                                                                               
TOTALS   DS    0PL8                                                             
TTRANS   DS    PL8                 NUMBER OF TRANSACTIONS                       
TTIME    DS    PL8                 NUMBER OF TIME TRANSACTIONS                  
TTIMYES  DS    PL8                 NUMBER OF SUCCESSFUL GETTIMS                 
TTIMNO   DS    PL8                 NUMBER OF UNSUCCESSFUL GETTIMS               
T1CPRO   DS    PL8                 NUMBER OF 1C ACCOUNTS ON PROFILE             
T1CSPA   DS    PL8                 NUMBER OF 1C ACCOUNTS ON SPAEL               
TFIND1C  DS    PL8                 NUMBER OF ALTERNATIVE 1CS FOUND              
TFINDTIM DS    PL8                 NUMBER OF ALTERNATIVE DATES FOUND            
TOTALSN  EQU   (*-TOTALS)/L'TOTALS                                              
*                                                                               
IOWORK   DS    XL64                                                             
IOREC    DS    XL2048                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT LINE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PACT     DS    CL14                ACCOUNT                                      
         DS    CL1                                                              
PWC      DS    CL2                 WORK-CODE                                    
         DS    CL1                                                              
PCAC     DS    CL14                CONTRA-ACCOUNT                               
         DS    CL1                                                              
PDATE    DS    CL6                 DATE                                         
         DS    CL1                                                              
PREF     DS    CL6                                                              
         DS    CL1                                                              
PSUB     DS    CL2                                                              
         DS    CL1                                                              
PDA      DS    CL8                                                              
*                                                                               
         DS    (L'P-(*-PLINED))C                                                
         EJECT                                                                  
*        ACREPWORKD                                                             
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*        ACGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*        DDREPXTRAD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*        DDMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*        DDEBLOCK                                                               
         PRINT OFF                                                              
       ++INCLUDE DDEBLOCK                                                       
         PRINT ON                                                               
*        ACLANGEQU                                                              
         PRINT OFF                                                              
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
*        ACDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
*        ACGOBLOCK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*        ACPRORATAD                                                             
         PRINT OFF                                                              
PRORATAD DSECT                                                                  
       ++INCLUDE ACPRORATAD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'193ACREPCC02 08/16/00'                                      
         END                                                                    
