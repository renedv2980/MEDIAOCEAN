*          DATA SET PPREP0302G AT LEVEL 003 AS OF 05/01/02                      
*PHASE PP0302G,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
*INCLUDE BINSRCH2                                                               
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL COPY SELECTED CONTRACTS FROM AGENCY JW TO                 
*   AGENCY FR                                                                   
*                                                                               
*      QOPT5     Y= TEST RUN (DON'T MARK FILE)                                  
*      QOPT6     Y= DUMP FIRST 10 RECORDS (BEFORE AND AFTER)                    
*                                                                               
PP0302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP0302                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP02WRKD,R8                                                      
**                                                                              
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'         CONTRACTS READ                               
         ZAP   CHGCNT,=P'0'        CONTRACTS COPIED                             
         ZAP   COMCNT,=P'0'        CONTRACTS WITH STANDARD COMMENTS             
         ZAP   DUMPCNT,=P'0'                                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         LA    R0,PCONREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'10'          CONTRACTS                                   
         MVC   KEY+4(3),=C'FAR'    MASTER CLIENT WANTED                         
*****    MVC   KEY+4(3),QCLIENT    MASTER CLIENT WANTED                         
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
PROC3    GOTO1 SEQ                                                              
PROC5    CLC   KEY(7),KEYSAVE      CHECK THRU CLIENT                            
         BNE   PROC80              GO DO NEXT MEDIA                             
         AP    INCNT,=P'1'                                                      
         GOTO1 GETPRT              GET THE CONTRACT REC                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROC10   DS    0H                                                               
         LA    R2,PCONREC                                                       
         USING PCONREC,R2                                                       
         CLI   PCONELEM,X'10'      CONTRACT DESCRIPTION ELEMENT ?               
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         CLC   PCONSDT,=X'5F0000'  START AFTER 12/31/94 ?                       
         BNH   PROC3               NO - NEXT SEQ REC                            
*                                                                               
PROC20   DS    0H                  TEST FOR "FORD" PUB                          
         MVC   SKEY,KEY            SAVE CONTRACT KEYS                           
         LA    R4,KEY                                                           
         USING PUBKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   PUBKMED,PCONKMED                                                 
         MVC   PUBKPUB(6),PCONKPUB                                              
         MVC   PUBKAGY,=C'FR'      FORD AGENCY                                  
         MVI   PUBKCOD,X'81'                                                    
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     PUB FOUND ?                                  
         BE    PROC30              YES - COPY CONTRACT                          
         MVC   KEY(64),SKEY        RESTORE KEYS                                 
         B     PROC3               NEXT SEQ REC - NO CONVERSION                 
         DROP  R2                                                               
*                                                                               
PROC30   DS    0H              *****  CONVERT CONTRACT                          
*                                                                               
         LA    R2,PCONREC+33                                                    
         LA    R7,P+90                                                          
         MVI   ELCODE,X'30'        STANDARD COMMENT ELEMENT                     
PROC30B  BAS   RE,NEXTEL                                                        
         BNE   PROC30D             NO MORE                                      
         AP    COMCNT,=P'1'        HAVE STANDARD COMMENT                        
         MVC   0(6,R7),2(R2)       STD COMMENT                                  
         LA    R7,8(R7)            BUMP OVER ON PRINT LINE                      
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),PCONKMED    CONTRACT MEDIA CODE                          
         MVC   WORK+1(6),2(R2)     ADD STANDARD CONTRACT CODE TO TABLE          
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         B     PROC30B             LOOK FOR ANOTHER STD COMMENT                 
         DROP  R4                                                               
*                                                                               
PROC30D  DS    0H                                                               
         LA    R2,PCONREC          RESET TO BEGINNING OF REC                    
         USING PCONREC,R2                                                       
         MVC   P(3),PCONKAGY       AGENCY AND MEDIA                             
         MVC   P+4(3),PCONKCLT                                                  
         GOTO1 PUBEDIT,DMCB,PCONKPUB,P+10                                       
         MVC   HALF,PCONNUM                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+27(4),DUB                                                      
         GOTO1 DATCON,DMCB,(3,PCONSDT),(5,P+34)                                 
         GOTO1 (RF),(R1),(3,PCONEDT),(5,P+43)                                   
         MVI   P+42,C'-'                                                        
         MVC   P+55(33),=C'** CONTRACT COPIED TO AGENCY FR **'                  
         BAS   RE,RPRT                                                          
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   CLEARREC            NO                                           
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    CLEARREC            YES                                          
*                                                                               
PROC35   DS    0H                                                               
         MVC   P+55(12),=C'** BEFORE **'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
CLEARREC SR    R5,R5             CLEAR END OF RECORD                            
         IC    R5,PCONREC+25                                                    
         SLL   R5,8                                                             
         IC    R5,PCONREC+26                                                    
         SR    RE,RE                                                            
         LA    RE,PCONREC                                                       
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PCONREC                                                       
         LA    RF,2000(RF)                                                      
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         MVC   PCONKAGY,=C'FR'     CHANGE TO FORD AGENCY                        
         XC    KEY,KEY                                                          
         MVC   KEY(25),PCONREC                                                  
         AP    CHGCNT,=P'1'                                                     
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC40              NO                                           
*                                                                               
         GOTO1 ADDPRT                                                           
*                                                                               
PROC40   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC50              NO                                           
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC50              YES                                          
*                                                                               
PROC42   MVC   P+55(12),=C'** AFTER ***'                                        
         BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC50   DS    0H                                                               
         MVC   KEY(64),SKEY        RESTORE CONTRACT KEYS                        
         GOTO1 HIGH                                                             
*                                                                               
         B     PROC3               NEXT SEQ REC                                 
*                                                                               
         DROP  R2                                                               
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'          END OF FILE                                   
         BE    EXIT                                                             
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+3,X'FF'             SKIP TO NEXT AGY/MED                     
         XC    KEY+4(28),KEY+4                                                  
         GOTO1 HIGH                                                             
         CLI   KEY,X'FF'               END OF FILE                              
         BE    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'10'         CONTRACTS                                    
         MVC   KEY+4(3),=C'FAR'    MASTER CLIENT WANTED                         
*****    MVC   KEY+4(3),QCLIENT    MASTER CLIENT WANTED                         
         B     PROC2                                                            
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         MVC   WORK(7),=7X'FF'      END OF TABLE                                
         GOTO1 =V(BINSRCH),BINPARMS,(X'01',WORK)                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                          PRINT STD COMMENT CODES FOUND                        
         XC    WORK(7),WORK                                                     
         GOTO1 =V(BINSRCH),BINPARMS,(X'02',WORK)                                
         CLI   0(R1),1                                                          
         BNE   *+6                                                              
         DC    H'0'                MUST BE FOUND                                
         ZICM  R3,1(R1),3          POINT R3 TO TABLE - A(FOUND RECORD)          
*                                                                               
         MVC   P+10(28),=C'STANDARD COMMENT CODES FOUND'                        
         GOTO1 REPORT                                                           
         MVC   P+10(28),=C'----------------------------'                        
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+19(3),=C'MED'                                                  
         MVC   P+23(6),=C'*CODE*'                                               
         GOTO1 REPORT                                                           
         MVC   P+19(3),=C'---'                                                  
         MVC   P+23(6),=C'------'                                               
         GOTO1 REPORT                                                           
*                                                                               
RUNL10   CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    RUNL40              YES - FINISH UP                              
*                                                                               
         MVC   P+20(1),0(R3)       MEDIA                                        
         MVC   P+23(6),1(R3)       STD CONTRACT CODE                            
         GOTO1 REPORT                                                           
         LA    R3,7(R3)                                                         
         B     RUNL10                                                           
*                                                                               
RUNL40   GOTO1 REPORT                                                           
         LA    R4,COUNTS                                                        
RUNL50   CLI   0(R4),X'FF'                                                      
         BE    RUNL90                                                           
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL50                                                           
*                                                                               
RUNL90   DS    0H                                                               
*                                                                               
RUNLX    DS    0H                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
         MVI   RCSUBPRG,10                                                      
         MVC   HEAD5+62(8),=C'WRITE=NO'                                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+68(3),=C'YES'                                              
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1                                                                   
         SPACE 2                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),LASTPUB                                                 
         IC    R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(2),PAGYKAGY                                                
         GOTO1 HIGHPUB                                                          
         B     NP2B                                                             
*                                                                               
NP2      DS    0H                                                               
         GOTO1 SEQPUB                                                           
NP2B     DS    0H                                                               
         MVI   PUBKMED,X'FF'       SET EOF                                      
         CLC   KEY(1),KEYSAVE                                                   
         BNE   NPX                                                              
         CLC   KEY+7(2),KEYSAVE+7                                               
         BNE   NP2                                                              
         CLI   KEY+9,X'81'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LASTPUB,KEY+1                                                    
         GOTO1 GETNAME                                                          
*****    L     RF,ALTLREC                                                       
*****    XC    0(50,RF),0(RF)                                                   
*****    GOTO1 SEQPUB                                                           
*****    CLC   KEY(9),PUBKEY                                                    
*****    BNE   NPX                                                              
*****    GOTO1 GETLTL                                                           
*                                                                               
NPX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BNE   NEXTEL+2                                                         
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
DMPKEY   NTR1                                                                   
         SPACE 2                                                                
         LA    R5,KEY                                                           
         LA    R2,25                                                            
         GOTO1 HEXOUT,DMCB,(R5),P+01,(R2),=C'N'                                 
*                                                                               
         MVC   WORK(25),0(R5)                                                   
         TR    WORK(25),TRTAB                                                   
         MVC   P+75(25),WORK                                                    
         B     EXIT                                                             
         SPACE 2                                                                
DMPREC   NTR1                                                                   
         SPACE 1                                                                
         L     R5,AREC                                                          
         MVC   HALF,25(R5)        RECORD LENGTH                                 
         LH    R2,HALF                                                          
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   EXIT                                                             
         CH    R4,=H'32'                                                        
         BNH   *+8                                                              
         LA    R4,32                                                            
         XC    WORK,WORK                                                        
         GOTO1 HEXOUT,DMCB,(R5),WORK,(R4),=C'N'                                 
*                                                                               
         MVC   P+01(8),WORK+00                                                  
         MVC   P+10(8),WORK+08                                                  
         MVC   P+19(8),WORK+16                                                  
         MVC   P+28(8),WORK+24                                                  
         MVC   P+37(8),WORK+32                                                  
         MVC   P+46(8),WORK+40                                                  
         MVC   P+55(8),WORK+48                                                  
         MVC   P+64(8),WORK+56                                                  
*                                                                               
         MVC   WORK(32),0(R5)                                                   
         TR    WORK(32),TRTAB                                                   
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+75(0),WORK                                                     
         LA    R4,1(R4)                                                         
         BAS   RE,RPRT                                                          
         LA    R5,0(R5,R4)                                                      
         B     DMPREC2                                                          
         SPACE 3                                                                
TRTAB    DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     00-0F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     10-1F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     20-2F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     30-3F                    
         DC    X'404B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     40-4F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B5B5C5D4B4B'     50-5F                    
         DC    X'60614B4B4B4B4B4B4B4B4B6B6C6D4B6F'     60-6F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B7B4B7D7E4B'     70-7F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     80-8F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     90-9F                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     A0-AF                    
         DC    X'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B'     B0-BF                    
         DC    X'4BC1C2C3C4C5C6C7C8C94B4B4B4B4B4B'     C0-CF                    
         DC    X'4BD1D2D3D4D5D6D7D8D94B4B4B4B4B4B'     D0-DF                    
         DC    X'4B4BE2E3E4E5E6E7E8E94B4B4B4B4B4B'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'CONT RECS READ'                                             
CHGCNT   DS    PL8                                                              
         DC    CL15'RECORDS COPIED'                                             
COMCNT   DS    PL8                                                              
         DC    CL15'STD COMMENTS'                                               
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
SAVKEY   DS    CL32                                                             
CNT24    DS    PL4                                                              
CNT70    DS    PL4                                                              
OLDDATE  DS    CL3                                                              
ERRSW    DS    CL1                                                              
PUBSW    DS    CL1                                                              
CHKGST   DS    CL1                                                              
DDSBLD   DS    CL1                                                              
DATETYP  DS    CL1                                                              
KEYPSW   DS    CL1                                                              
*                                                                               
PUBNET   DS    PL8                                                              
PUBCASH  DS    PL8                                                              
PUBMYTAX DS    PL8                                                              
PUBBASIS DS    PL8                                                              
PUBGSTT  DS    PL8                                                              
PUBGSTTP DS    PL8                                                              
*                                                                               
OUTSPC   DS    CL40                                                             
*                                                                               
BINPARMS DC    A(0)                                                             
BINATAB  DC    A(TABLE)                                                         
BINCOUNT DC    A(0)                RECORD COUNT                                 
         DC    A(7)                LENGTH                                       
         DC    AL1(0),AL3(7)                                                    
BINMAX   DC    A(30)               MAX NUMBER OF RECORDS                        
*                                                                               
TABLE    DS    CL210                                                            
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    XL30                                                             
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PPGKEY   DS    CL64                                                             
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP0302G05/01/02'                                      
         END                                                                    
