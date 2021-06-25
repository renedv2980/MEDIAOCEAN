*          DATA SET PPREP0302T AT LEVEL 042 AS OF 05/01/02                      
*PHASE PP0302T,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL READ PUBS AND CHECK PUBSATE FOR                      
*        "NF" - NEWFOUNDLAND                                                    
*        IT WILL ADD A TAX ENTRY IN THE TAX ELEMENT                             
*        FOR 0.0 TAX PCT EFFECTIVE APR01/97 (IF NOT ALREADY THERE)              
*                                                                               
*        TO THOSE OF THE BUYRECS FOR THAT PUB                                   
*        SEE IF NOT BILLED OR PAID IF SO CLEAR PBDTAX                           
*                                                                               
*        IF BILLED OR PAID FLAG THE BUY (LEAVE TAX ALONE)                       
*                                                                               
*        QOPT5     Y= TEST RUN (DON'T MARK FILE)                                
*        QOPT6     Y= DUMP MODIFIED BUYREC                                      
*                                                                               
         PRINT NOGEN                                                            
PP0202   CSECT                                                                  
         NMOD1 0,PP0202                                                         
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
         CLI   MODE,PROCPUB                                                     
         BE    PROC                                                             
         CLI   MODE,FPUBREQ                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   NFCNT,=P'0'                                                      
         ZAP   NFPCNT,=P'0'                                                     
         ZAP   ERRCNT,=P'0'                                                     
         ZAP   BUYRCNT,=P'0'                                                    
         ZAP   BUYACNT,=P'0'                                                    
         ZAP   BUYTCNT,=P'0'                                                    
         ZAP   BUYCNT,=P'0'                                                     
         ZAP   PTAXCNT,=P'0'       PUBS WITH TAX ELEMENTS                       
         ZAP   CTAXCNT,=P'0'       PUB TAX ELEMENTS CHANGED                     
         B     EXIT                                                             
*                                                                               
REQF     DS    0H                  FIRST PUB FOR REQUEST                        
         MVC   SKEY,KEY            SAVE PPG'S KEY                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY      STARTING AGY                                 
         MVC   KEY+2(1),QMEDIA     STARTING MED                                 
         MVI   KEY+3,X'01'         AGENCY RECORD                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         MVC   KEY(64),SKEY        RESTORE PPG'S KEY                            
         GOTO1 HIGH                                                             
*                                                                               
*******  NO-OP CANADIAN TEST                                                    
*                                                                               
*******  CLI   PAGYNAT,C'C'        CANADIAN AGENCY ?                            
*******  BE    REQF10              YES                                          
*******  MVI   MODE,LBUYREQ        NO - SET FOR NEXT AGY/MED                    
*******  B     EXIT                                                             
*                                                                               
REQF10   MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=X'0001'       RESET PAGE NUM                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
PROC     DS    0H                                                               
         MVI   PTAXSW,0                                                         
*                                                                               
         CLC   PUBKAGY,=C'ZZ'      SEE IF AGENCY PUB                            
         BE    EXIT                NO - IGNORE                                  
*                                                                               
         MVC   PPGKEY,KEY         SAVE PPG'S KEY                                
         XC    KEY,KEY            TO BE SURE I MUST REREAD RECORD               
         MVC   KEY(25),PUBREC                                                   
         GOTO1 HIGHPUB                                                          
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 MUST FIND                                   
         GOTO1 GETNAME            READS PUB INTO PUBREC                         
*                                                                               
         AP    INCNT,=P'1'                                                      
         CLC   PUBSTATE,=C'NF'     SEE IF NEWFOUNDLAND                          
         BE    PROC3                                                            
*                 ADDITIONAL CHECK FOR NEWFOUNDLAND                             
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'90'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         CLI   11(R2),C' '         CHECK FOR PST NF CODE                        
         BNH   EXIT                                                             
         AP    NFPCNT,=P'1'       NEWFNDLND PST COUNT                           
*                                                                               
PROC3    DS    0H                                                               
         AP    NFCNT,=P'1'                                                      
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'22'         LOOK FOR TAX ELEMENT                        
         BAS   RE,NEXTEL                                                        
         BNE   PROCP                NOT FOUND - STILL CHECK BUYS                
         AP    PTAXCNT,=P'1'                                                    
*                                                                               
         USING PUBTAXEL,R2                                                      
         LA    RF,PUBTAX1                                                       
         OC    0(6,RF),0(RF)       SEE IF  ENTRY USED                           
         BZ    PROC8                                                            
         CLC   0(6,RF),=X'000000610401'    SEE IF 0.0 4/1/97                    
         BE    PROC8X               ALREADY ENTERED - CHECK BUYS                
         LA    RF,PUBTAX2                                                       
         OC    0(6,RF),0(RF)       SEE IF  ENTRY USED                           
         BZ    PROC8                                                            
         CLC   0(6,RF),=X'000000610401'    SEE IF 0.0 4/1/97                    
         BE    PROC8X               ALREADY ENTERD - CHECK BUYS                 
         LA    RF,PUBTAX3                                                       
         CLC   0(6,RF),=X'000000610401'    SEE IF 0.0 4/1/97                    
         BE    PROC8X                ALREADY ENTERED - CHECK BUYS               
         OC    0(6,RF),0(RF)       SEE IF  ENTRY USED                           
         BNZ   PROCERR                                                          
*                                                                               
PROC8    MVC   0(6,RF),=X'000000610401'   ADD ENTRY                             
         MVI   PTAXSW,1           SET TAX DATA ADDED                            
         AP    CTAXCNT,=P'1'                                                    
*                                                                               
         LA    R0,PUBREC          SET FOR PUTPUB AND DMPREC                     
         ST    R0,AREC                                                          
*                                                                               
         CLI   RCWRITE,C'N'        MEANS DON'T MARK FILE                        
         BE    PROC8X             GO CHECK BUY                                  
         GOTO1 PUTPUB                                                           
*                                                                               
PROC8X   DS    0H                                                               
         B     PROCP                     GO CHECK BUYS                          
*                                                                               
PROCERR  MVC   P+1(20),=C'TOO MANY TAX ENTRIES'                                 
         MVC   P+25(2),PUBKAGY                                                  
         GOTO1 PUBEDIT,DMCB,PUBKPUB,P+30                                        
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     PROCPB                                                           
*                                                                               
PROCP    DS    0H                                                               
         MVC   P+1(8),=C'NF  PUB='                                              
         MVC   P+10(2),PUBKAGY                                                  
         GOTO1 PUBEDIT,DMCB,PUBKPUB,P+15                                        
         CLI   PTAXSW,1                                                         
         BNE   *+10                                                             
         MVC   P+30(20),=C'- NEW TAX DATA ADDED'                                
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
*                                                                               
         CLI   PTAXSW,1            SEE IF TAX CHANGED                           
         BNE   PROCPB                                                           
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   *+8                 NO                                           
         BAS   RE,DMPREC                                                        
*                                                                               
PROCPB   DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKAGY      STARTING AGY                                 
         MVC   KEY+2(1),PUBKMED    STARTING MED                                 
         MVI   KEY+3,X'21'         BUYS (PASSIVE KEY)                           
         MVC   KEY+7(6),PUBKPUB                                                 
*                                                                               
PROC20   GOTO1 HIGH                                                             
         B     PROC40                                                           
*                                                                               
PROC30   DS    0H                                                               
         GOTO1 SEQ                                                              
PROC40   DS    0H                                                               
         CLC   KEY(4),KEYSAVE        AGY/MED/RECORD                             
         BNE   PROCX                                                            
         CLC   KEY+7(6),PUBKPUB      CHECK RIGHT PUB                            
         BH    NEXTCLT                SKIP TO NEXT CLIENT                       
         BE    PROC60                                                           
         MVC   KEY+7(6),PUBKPUB       SEARCH FOR THIS PUB                       
         XC    KEY+13(12),KEY+13                                                
         B     PROC20                                                           
*                                                                               
PROC60   DS    0H                                                               
         AP    BUYRCNT,=P'1'                                                    
         CLC   KEY+16(3),=X'610401'    CHK INS BEFORE APR01/97                  
         BL    PROC30              NEXT BUY REC                                 
*                                                                               
PROC60B  LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         AP    BUYACNT,=P'1'          AFTER MAR31/97 COUNTER                    
         OC    PBDTAX,PBDTAX          CHECK FOR TAX                             
         BZ    PROC30                  SKIP                                     
         AP    BUYTCNT,=P'1'          WITH TAX?                                 
*                                                                               
*****    TM    PBDCNDA,X'80'           SEE IF CANADIAN                          
*****    BZ    PROC30                                                           
*                                                                               
         MVI   PAIDSW,C'N'                                                      
         MVI   BILLSW,C'N'                                                      
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
PROC60C  BAS   RE,NEXTEL           CHK FOR DATED PAY ELEM                       
         BNE   PROC60F                                                          
         OC    2(3,R2),2(R2)       SEE IF IT HAS A DATE                         
         BZ    PROC60C             ALREADY PAID-THEN SKIP                       
         MVI   PAIDSW,C'Y'                                                      
*                                                                               
PROC60F  LA    R2,PBUYREC+33       SEE IF BUYREC HAS BILL ELEM                  
         MVI   ELCODE,X'26'                                                     
PROC60H  BAS   RE,NEXTEL                                                        
         BNE   PROC70                NO- THEN ADD ONE                           
         OC    5(3,R2),5(R2)          CHECK FOR DATE                            
         BZ    PROC60H                                                          
         MVI   BILLSW,C'Y'                                                      
*                                                                               
PROC70   CLI   PAIDSW,C'N'                                                      
         BNE   PROC70B                                                          
         CLI   BILLSW,C'N'                                                      
         BNE   PROC70B                                                          
         XC    PBDTAX,PBDTAX                                                    
         AP    BUYCNT,=P'1'                                                     
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   *+8                 NO                                           
         BAS   RE,DMPREC                                                        
*                                                                               
         MVC   P+70(13),=C'- TAX REMOVED'                                       
         CLI   RCWRITE,C'N'        MEANS DON'T MARK FILE                        
         BE    PROC70D            GO CHECK NEXT BUY                             
         GOTO1 PUTPRT                                                           
         B     PROC70D            GO CHECK NEXT BUY                             
*                                                                               
PROC70B  DS    0H                                                               
         AP    ERRCNT,=P'1'                                                     
PROC70D  MVC   PSECOND+1(5),=C'ADD ='                                           
         GOTO1 HEXOUT,DMCB,KEY+27,PSECOND+7,4,=C'N'                             
*                                                                               
         DS    0H                                                               
         MVC   P+1(3),PBUYKCLT                                                  
         MVC   P+5(3),PBUYKPRD                                                  
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+10(3),DUB                                                      
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,P+16)                                
         CLI   PBUYKLIN,1                                                       
         BE    PROC70C                                                          
         MVI   P+24,C'-'                                                        
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+25(2),DUB                                                      
PROC70C  DS    0H                                                               
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,P+30                                       
*                                                                               
         EDIT  (B3,PBDTAX),(7,P+50),4                                           
         MVC   P+60(1),PAIDSW                                                   
         MVC   P+65(1),BILLSW                                                   
         CLC   PAIDSW(2),=C'NN'                                                 
         BE    *+10                                                             
         MVC   P+70(22),=C'*** PAID OR BILLED ***'                              
*                                                                               
PROC90   DS    0H                                                               
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     PROC30            SEQ READ                                       
*                                                                               
NEXTCLT  DS    0H                                                               
         ZIC   R1,KEY+6                                                         
         LA    R1,1(R1)                                                         
         STC   R1,KEY+6                                                         
         MVC   KEY+7(6),PUBKPUB                                                 
         MVI   KEY+3,X'21'                                                      
         XC    KEY+13(12),KEY+13                                                
         B     PROC20                                                           
*                                                                               
*                                                                               
PROCX    DS    0H                                                               
         MVC   KEY(64),PPGKEY      RESTORE PPG'S KEY                            
         GOTO1 HIGHPUB                                                          
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
RUNL     DS    0H                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    RUNLX                                                            
         MVC   P(15),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,23(R4)                                                        
         B     RUNL5                                                            
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
         MVI   RCSUBPRG,0                                                       
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
         L     RF,ALTLREC                                                       
         XC    0(50,RF),0(RF)                                                   
         GOTO1 SEQPUB                                                           
         CLC   KEY(9),PUBKEY                                                    
         BNE   NPX                                                              
         GOTO1 GETLTL                                                           
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
         SPACE 3                                                                
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
         MVC   HALF,25(R5)      RECORD LENGHT                                   
         LH    R2,HALF                                                          
****     LA    R2,880                                                           
         LA    R3,0(R5,R2)                                                      
DMPREC2  DS    0H                                                               
         LR    R4,R3                                                            
         SR    R4,R5                                                            
         BNP   DMPRECX                                                          
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
*                                                                               
DMPRECX  BAS   RE,RPRT      SKIP BETWEEN RECORDS                                
         B     EXIT                                                             
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
         DC    CL15'RECORDS READ'                                               
NFCNT    DS    PL8                                                              
         DC    CL15'NEWFLND PUBS'                                               
NFPCNT   DS    PL8                                                              
         DC    CL15'NEWF PST PUBS'                                              
PTAXCNT  DS    PL8                                                              
         DC    CL15'TAX PUBS'                                                   
CTAXCNT  DS    PL8                                                              
         DC    CL15'TAX PUBS CHGED'                                             
BUYRCNT  DS    PL8                                                              
         DC    CL15'BUYS READ'                                                  
BUYACNT  DS    PL8                                                              
         DC    CL15'AFTER MAR31/97'                                             
BUYTCNT  DS    PL8                                                              
         DC    CL15'BUYS WITH TAX'                                              
BUYCNT   DS    PL8                                                              
         DC    CL15'BUY TAX CLEARED'                                            
ERRCNT   DS    PL8                                                              
         DC    CL15'BILLED OR PAID'                                             
         DC    X'FF'                                                            
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
PAIDSW   DS    CL1                                                              
BILLSW   DS    CL1                                                              
PTAXSW   DS    CL1                                                              
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
         BUFF  LINES=2000,ROWS=1,COLUMNS=6,FLAVOR=PACKED,KEYLIST=(10,A)X        
               ,COMMENT=10                                                      
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
FRSTSW   DS    XL1                                                              
TYPE     DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ITOT     DS    F                                                                
SKEY     DS    CL64                                                             
PPGKEY   DS    CL64                                                             
*                                                                               
ELEM     DS    CL25                                                             
*                                                                               
BUFREC   DS    0CL68                                                            
BUFKEY   DS    0CL10                                                            
BUFTYPE  DS    CL1                                                              
BUFCLT   DS    CL3                                                              
BUFPUB   DS    CL6                                                              
BUFCOM   DS    CL10                                                             
*                                                                               
BUFNET   DS    PL8                                                              
BUFCD    DS    PL8                                                              
BUFTAX   DS    PL8                                                              
BUFBASIS DS    PL8                                                              
BUFGST   DS    PL8                                                              
BUFGSTPD DS    PL8                                                              
*                                                                               
*                                                                               
       ++INCLUDE GVALUES                                                        
*                                                                               
       ++INCLUDE PBILPROF                                                       
         EJECT                                                                  
       ++INCLUDE PUBTAXEL                                                       
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042PPREP0302T05/01/02'                                      
         END                                                                    
