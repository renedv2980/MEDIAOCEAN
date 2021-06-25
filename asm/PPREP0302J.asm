*          DATA SET PPREP0302J AT LEVEL 005 AS OF 05/01/02                      
*PHASE PP0302J,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM WILL COPY SELECTED CONTRACTS FROM ADVERTISER PUB               
*   NUMBER TO AGENCY PUB NUMBER                                                 
*   OUTPUT CAN BE EITHER TO TAPE OR THE PRINT FILE                              
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
         ZAP   CHGCNT,=P'0'        CONTRACTS TESTED                             
         ZAP   OUTCNT,=P'0'        RECORDS ACTUALLY WRITTEN                     
         ZAP   DUPCNT,=P'0'        DUPLICATE CONTRACTS                          
         ZAP   C30CNT,=P'0'        STANDARD COMMENTS DELETED                    
         ZAP   C40CNT,=P'0'        SPECIAL COMMENTS DELETED                     
         ZAP   DUMPCNT,=P'0'                                                    
*                                                                               
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                  FIRST BUY FOR REQUEST                        
         MVI   RCWRITE,C'Y'                                                     
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
*                                                                               
         OPEN  (OUT,OUTPUT)                                                     
*                                                                               
*        OPEN THE FILES FOR PRNT8 (SWITCH)                                      
*                                                                               
         L     RF,UTL                                                           
         MVI   4(RF),X'74'      SWITCH TO PRNT8 FILE (YNR AGENCY HERE)          
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'PRINT',PRTFILES,PPBYOWRK                
*                                                                               
         L     RF,UTL                                                           
         MVI   4(RF),X'54'      SWITCH TO PRNT6 FILE FOR CONTRACTS              
*                                      (BDNY AGENCY HERE)                       
         LA    R0,PCONREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'BDM'       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'10'          CONTRACTS                                   
         MVC   KEY+4(3),=C'DP '     CLIENT WANTED                               
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    CLC   KEY(2),=C'BD'       SAME AGENCY ?                                
         BE    PROC10              YES                                          
         MVI   KEY,X'FF'           NO - END OF RUN                              
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
PROC10   CLC   KEY(7),KEYSAVE      CHECK THRU CLIENT                            
         BNE   PROC80              GO DO NEXT MEDIA                             
         AP    INCNT,=P'1'                                                      
         GOTO1 GETPRT              GET THE CONTRACT REC                         
*                                                                               
         LA    R5,PCONREC                                                       
         USING PCONREC,R5                                                       
*                                                                               
         CLC   PCONEDT,=X'600101'  END DATE EARLIER THAN 1/1/96 ?               
         BL    PROC3               YES - IGNORE                                 
         CLC   PCONSDT,=X'610C1F'  START DATE AFTER 12/31/97 ?                  
         BH    PROC3               YES - IGNORE                                 
*                                                                               
PROC20   DS    0H                  TEST FOR ADV PUB PASSIVE POINTER             
         L     RF,UTL                                                           
         MVI   4(RF),X'74'         SWITCH TO PRNT8 FILE                         
*                                                                               
         MVC   SKEY,KEY            SAVE CONTRACT KEYS                           
         LA    R4,KEY                                                           
         USING PUBVKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   PUBVID,X'FD'        RECORD ID                                    
         MVC   PUBVMED,PCONKMED    MEDIA                                        
         MVC   PUBVADV,=C'DP '     ADVERTISER CODE                              
         MVC   PUBVAOR,=C'BD'      AOR CODE           (BDNY)                    
         MVC   PUBVAGY,=C'YN'      AGENCY CODE        (YNR)                     
         MVC   PUBVVPUB(6),PCONKPUB                                             
         GOTO1 HIGHPUB                                                          
*                                                                               
         L     RF,UTL                                                           
         MVI   4(RF),X'54'      SWITCH BACK TO PRNT6                            
*                                                                               
         CLC   KEY(15),KEYSAVE     PUB FOUND ?                                  
         BE    PROC30              YES - COPY CONTRACT                          
         MVC   KEY(64),SKEY        RESTORE CONTRACT KEYS                        
         GOTO1 HIGH                AND SEQUENCE                                 
*                                                                               
         B     PROC3               NEXT SEQ REC - NO COPY                       
*                                                                               
PROC30   DS    0H                  *****  COPY CONTRACT RECORD                  
*                                                                               
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
         GOTO1 PUBEDIT,DMCB,PUBVAPUB,P+60                                       
         BAS   RE,RPRT                                                          
*                                                                               
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   PROC35D             CHECK FOR STD COMMENT ELEM'S                 
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC35D             YES - CHECK FOR STD COMMENT ELEM'S           
*                                                                               
PROC35   DS    0H                                                               
         MVC   P+55(12),=C'** BEFORE **'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC35D  DS    0H                                                               
         XC    PCONREQ,PCONREQ     CLEAR AGENCY SIGNER                          
*                                                                               
PROC35E  DS    0H                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'30'        STD COMMENT ELEM                             
         BAS   RE,NEXTEL                                                        
         BNE   PROC35G             NO (MORE) - CHECK SPECIAL COMMENTS           
*                                                                               
         GOTO1 RECUP,DMCB,(1,PCONREC),(R2),0       DELETE ELEMENT               
         AP    C30CNT,=P'1'                                                     
         B     PROC35E             LOOK FOR MORE                                
*                                                                               
PROC35G  DS    0H                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCODE,X'40'        SPECIAL COMMENT ELEM                         
         BAS   RE,NEXTEL                                                        
         BNE   CLEARREC            NO (MORE) - FINISH COPY                      
*                                                                               
         GOTO1 RECUP,DMCB,(1,PCONREC),(R2),0       DELETE ELEMENT               
         AP    C40CNT,=P'1'                                                     
         B     PROC35G             LOOK FOR MORE                                
*                                                                               
CLEARREC DS    0H                CLEAR END OF RECORD                            
         MVC   HALF,PCONREC+25                                                  
         LH    R1,HALF                                                          
         LA    RE,PCONREC                                                       
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,4000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
         AP    CHGCNT,=P'1'        COUNT BEFORE DUP TEST                        
         L     RF,UTL                                                           
         MVI   4(RF),X'74'      SWITCH TO PRNT8 FILE FOR DUP TEST               
*                                                                               
         MVC   PCONKAGY,=C'YN'             CHANGE AGENCY (YNR)                  
         MVC   PCONKPUB(6),PUBVAPUB        CHANGE TO AGENCY PUB                 
         XC    KEY,KEY                                                          
         MVC   KEY(25),PCONREC                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     DOES RECORD EXIST ?                          
         BNE   PROC38              NO - OK TO COPY                              
         AP    DUPCNT,=P'1'                                                     
         MVC   P+55(15),=C'** DUPLICATE **'                                     
         BAS   RE,RPRT                                                          
         BAS   RE,DMPKEY           PRINT DUPLICATE KEY                          
         BAS   RE,RPRT                                                          
*                                                                               
         L     RF,UTL                                                           
         MVI   4(RF),X'54'         SWITCH BACK TO PRNT6 FILE (BDNY)             
*                                                                               
         B     PROC50              NEXT RECORD                                  
*                                                                               
PROC38   XC    KEY,KEY                                                          
*****    MVC   KEY(25),PCONREC                                                  
*                                                                               
*****    GOTO1 ADDPRT                                                           
*                                                                               
         L     RF,UTL                                                           
         MVI   4(RF),X'54'         SWITCH BACK TO PRNT6 FILE (BDNY)             
*                                                                               
         XCEFL REC-4,4004          CLEAR OUTPUT AREA                            
         MVC   HALF,PCONREC+25     REC LENGTH                                   
         LH    R1,HALF                                                          
         LA    RF,REC                                                           
         LA    RE,PCONREC                                                       
         MOVE  ((RF),(R1)),(RE)    MOVE PCONREC TO REC                          
         LH    R1,HALF                                                          
         LA    R1,4(R1)            TOTAL REC LENGTH                             
         STH   R1,REC-4                                                         
*                                                                               
         CLI   RCWRITE,C'N'        WRITE RECORD ?                               
         BE    PROC40              NO                                           
         PUT   OUT,REC-4                                                        
         AP    OUTCNT,=P'1'                                                     
*                                                                               
PROC40   DS    0H                                                               
         CLI   QOPT6,C'Y'          DUMP RECORDS ?                               
         BNE   PROC50              NO                                           
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC50              YES                                          
*                                                                               
PROC42   MVC   P+55(12),=C'** AFTER ***'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         MVC   P+55(12),=C'** REC-4 ***'                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPRECA                                                       
*                                                                               
PROC50   DS    0H                                                               
         MVC   KEY(64),SKEY        RESTORE CONTRACT KEYS                        
         GOTO1 HIGH                AND SEQUENCE                                 
*                                                                               
         B     PROC3               NEXT SEQ REC                                 
*                                                                               
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
         XC    KEY+4(28),KEY+4                                                  
         MVI   KEY+3,X'10'         CONTRACTS                                    
         MVC   KEY+4(3),=C'DP '     CLIENT WANTED                               
         B     PROC2                                                            
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         CLOSE OUT                                                              
*                                                                               
RUNL10   DS    0H                                                               
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
*                                                                               
DMPRECA  NTR1                                                                   
         LA    R5,REC-4                                                         
         MVC   HALF,29(R5)        PCONREC RECORD LENGTH                         
         LH    R2,HALF                                                          
         LA    R2,4(R2)           REC-4   RECORD LENGTH                         
         LA    R3,0(R5,R2)                                                      
         B     DMPREC2                                                          
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
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
OUT      DCB   DDNAME=TAPEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         SPACE 2                                                                
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL15'CONT RECS READ'                                             
CHGCNT   DS    PL8                                                              
         DC    CL15'RECORDS TESTED'                                             
OUTCNT   DS    PL8                                                              
         DC    CL15'RECORDS WRITTEN'                                            
DUPCNT   DS    PL8                                                              
         DC    CL15'DUPLICATE RECS'                                             
C30CNT   DS    PL8                                                              
         DC    CL15'DELETED STD COM'                                            
C40CNT   DS    PL8                                                              
         DC    CL15'DELETED SPC COM'                                            
         DC    X'FF'                                                            
DUMPCNT  DS    PL8                                                              
*                                                                               
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
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
         DS    F                                                                
REC      DS    4000C                                                            
*                                                                               
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
PPBYOWRK DS    600C                                                             
*                                                                               
PUBVKEY  DSECT                                                                  
       ++INCLUDE PPADVPUBPP                                                     
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREP0302J05/01/02'                                      
         END                                                                    
