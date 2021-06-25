*          DATA SET PPREP0302K AT LEVEL 002 AS OF 05/01/02                      
*PHASE PP0302K,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0302 - PRTFIX PROGRAM'                                        
*                                                                               
*   THIS PROGRAM TESTS BUYS FOR PUBS WITH ZONE NUMBERS TO SEE IF                
*   IDENTICAL BUYS EXIST FOR THE BASE PUB                                       
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
         ZAP   INCNT,=P'0'         BUYS READ                                    
         ZAP   CHGCNT,=P'0'        BUYS COPIED                                  
         ZAP   ZONCNT,=P'0'        BUYS WITH "ZONED" PUBS                       
         ZAP   DUPCNT,=P'0'        DUPLICATE BUYS                               
         ZAP   PAYCNT,=P'0'        BUYS WITH "PAID" ELEMS                       
         ZAP   BASCNT,=P'0'        "BASE" PUB BUYS EXIST                        
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
         OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),=C'WIN'       START AT AGENCY/MEDIA                       
         MVI   KEY+3,X'21'          BUYS (PASSIVE KEY)                          
         LA    R6,CLTTAB                                                        
         MVC   KEY+4(3),0(R6)       CLIENT WANTED                               
*                                                                               
PROC2    GOTO1 HIGH                                                             
         B     PROC5                                                            
*                                                                               
CLTTAB   DC    C'BMOBMDLA2',X'FF'                                               
*                                                                               
PROC3    GOTO1 SEQ                                                              
*                                                                               
PROC5    CLC   KEY(3),=C'WIN'      SAME AGENCY/MEDIA ?                          
         BE    PROC10              YES                                          
         MVI   KEY,X'FF'           NO - END OF RUN                              
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
PROC10   CLC   KEY(7),KEYSAVE      CHECK THRU CLIENT                            
         BNE   PROC80              GO DO NEXT CLIENT                            
         AP    INCNT,=P'1'                                                      
*                                                                               
         CLI   KEY+11,40           PUB ZONE NUMBER > 40 ?                       
         BNH   PROC3               NO - IGNORE                                  
         CLC   KEY+16(3),=X'620000'    DATE BEFORE 1998 ?                       
         BNH   PROC3               YES - IGNORE                                 
*                                                                               
         AP    ZONCNT,=P'1'                                                     
*                                                                               
         GOTO1 GETPRT              GET THE BUY REC                              
*                                                                               
         LA    R5,PBUYREC                                                       
         USING PBUYREC,R5                                                       
*                                                                               
PROC20   DS    0H                                                               
*                                                                               
         MVC   SKEY,KEY            SAVE BUY KEYS                                
         XC    KEY+25(7),KEY+25                                                 
         MVI   COPYSW,1            1=COPY BUY                                   
         MVI   KEY+11,0     TO READ FOR BUY AGAINST BASE PUB (NO ZONE)          
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE     BUY FOUND ?                                  
         BNE   PROC30              NO - OK TO COPY                              
         MVI   COPYSW,0            DO NOT COPY                                  
         AP    BASCNT,=P'1'                                                     
*                                                                               
PROC30   DS    0H                  *****  PRINT BUY RECORD                      
*                                                                               
         MVC   P(3),PBUYKAGY       AGENCY AND MEDIA                             
         MVC   P+4(3),PBUYKCLT                                                  
         MVC   P+8(3),PBUYKPRD                                                  
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,P+12                                       
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,P+28)                                
         MVC   HALF,PBUYKEST       ESTIMATE                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+40(4),DUB                                                      
         CLI   PBUYKLIN,1          LINE NUMBER > 1 ?                            
         BNH   PROC30B             NO                                           
         MVI   P+36,C'-'                                                        
         ZIC   R0,PBUYKLIN         LINE NUMBER                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+37(2),DUB                                                      
         TM    PBUYCNTL,X'80'      BUY DELETED ?                                
         BNO   PROC30B             NO                                           
         MVC   P+50(5),=C'*DEL*'                                                
*                                                                               
PROC30B  CLI   COPYSW,1                                                         
         BE    PROC30D                                                          
         MVC   P+80(21),=C'** BASE BUY EXISTS **'                               
         TM    KEY+25,X'80'   BASE BUY DELETED ?                                
         BNO   PROC30D             NO                                           
         MVC   P+105(5),=C'*DEL*'                                               
PROC30D  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'        PAY ELEMENT                                  
*                                                                               
PROC30E  BAS   RE,NEXTEL                                                        
         BNE   PROC30P             NO (MORE) - PRINT THE BUY                    
         OC    2(3,R2),2(R2)       PAID?                                        
         BZ    PROC30E             NO                                           
         AP    PAYCNT,=P'1'        YES                                          
         MVC   P+60(15),=C'** HAS PAIDS **'                                     
*                                                                               
*****    GOTO1 RECUP,DMCB,(1,PCONREC),(R2),0       DELETE ELEMENT               
*****    B     PROC35E             LOOK FOR MORE                                
*                                                                               
PROC30P  BAS   RE,RPRT                                                          
*                                                                               
         CLI   COPYSW,1            COPY RECORD ?                                
         BNE   PROC50              NO - GET NEXT BUY                            
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   PROC35D             NO - COPY BUY REC                            
         AP    DUMPCNT,=P'1'                                                    
         CP    DUMPCNT,=P'10'      10 RECORDS DUMPED ?                          
         BH    PROC35D             YES - COPY BUY REC                           
*                                                                               
PROC35   DS    0H                                                               
         MVC   P+55(12),=C'** BEFORE **'                                        
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC35D  DS    0H                                                               
         MVI   PBUYREC+14,0      CHANGE TO BASE PUB                             
*                                                                               
CLEARREC DS    0H                CLEAR END OF RECORD                            
         MVC   HALF,PBUYREC+25                                                  
         LH    R1,HALF                                                          
         LA    RE,PBUYREC                                                       
         LR    RF,RE                                                            
         AR    RE,R1                                                            
         LA    RF,4000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
*                                                                               
*                                                                               
PROC38   XC    KEY,KEY                                                          
         MVC   KEY(25),PBUYREC                                                  
         AP    CHGCNT,=P'1'                                                     
*                                                                               
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
*****    BAS   RE,DMPKEY                                                        
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
*                                                                               
PROC50   DS    0H                                                               
         MVC   KEY(64),SKEY        RESTORE BUY KEYS                             
         GOTO1 HIGH                AND SEQUENCE                                 
*                                                                               
         B     PROC3               NEXT SEQ REC                                 
*                                                                               
*                                                                               
PROC80   DS    0H                                                               
         CLI   KEY,X'FF'           END OF FILE ?                                
         BE    EXIT                YES                                          
         LA    R6,3(R6)            BUMP TO NEXT CLIENT                          
         CLI   0(R6),X'FF'         MORE CLIENTS ?                               
         BNE   PROC80D             YES                                          
         MVI   KEY,X'FF'           NO - END RUN                                 
         GOTO1 HIGH                                                             
         B     EXIT                                                             
*                                                                               
PROC80D  MVC   KEY+4(3),0(R6)      CLIENT WANTED                                
         MVI   KEY+3,X'21'         BUYS (PASSIVE)                               
         XC    KEY+7(25),KEY+7                                                  
         B     PROC2                                                            
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
*****    CLOSE OUT                                                              
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
         MVI   RCSUBPRG,30                                                      
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
         DC    CL15'BUY RECS READ'                                              
ZONCNT   DS    PL8                                                              
         DC    CL15'  "ZONED" PUBS'                                             
BASCNT   DS    PL8                                                              
         DC    CL15'BASE BUYS EXIST'                                            
PAYCNT   DS    PL8                                                              
         DC    CL15'  "PAID" ELEMS'                                             
CHGCNT   DS    PL8                                                              
         DC    CL15'BUY RECS COPIED'                                            
DUPCNT   DS    PL8                                                              
         DC    CL15'DUPLICATE RECS'                                             
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
COPYSW   DS    CL1                                                              
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
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREP0302K05/01/02'                                      
         END                                                                    
