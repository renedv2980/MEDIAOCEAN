*          DATA SET PPREP0202L AT LEVEL 005 AS OF 05/01/02                      
*PHASE PP0202L,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202 - PRTFIX PROGRAM'                                        
*                                                                               
*        THIS PROGRAM WILL READ PUBS AND CHECK  PST ELEMS                       
*          TO THOSE OF THE BUYRECS FOR THAT PUB                                 
*        IF THE PST ELEM IS MISSING FROM THE BUY THEN IT IS ADDED               
*                                                                               
*        INSERT DATE MUST BE 06/01/94 OR LATER FOR "PQ" ENTRIES                 
*        AND 04/01/97 OR LATER FOR OTHER PROVINCES (SEE PROC60)                 
*                                                                               
*        QOPT1     Q= PQ PROVINCE SELECTION (PST POS'N 6 NOT NULL)              
*                     (PUBS WITH NULL IN PST POS'N 6 ARE BYPASSED)              
*                  H= "OTHER" PROVINCES (PST POS'N 7,8 OR 10 NOT NULL)          
*                     (PUBS NOT NULL IN PST POS'N 6 ARE BYPASSED)               
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
         ZAP   ERRCNT,=P'0'                                                     
         ZAP   ADDCNT,=P'0'                                                     
         ZAP   PPSTCNT,=P'0'       USED TO COUNT PUBS WITH PST ELEMS            
         ZAP   MATCNT,=P'0'        USED TO COUNT MATCHES                        
         ZAP   MORCNT,=P'0'        PUB PST ELEMS WITH MORE THAN 1 ENTRY         
         ZAP   PAIDCNT,=P'0'       BUYS WITH DATED PAID ELEMS                   
         ZAP   BILLCNT,=P'0'       BUYS WITH DATED BILLED ELEMS                 
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
*                                                                               
         CLI   PAGYNAT,C'C'        CANADIAN AGENCY ?                            
         BE    REQF10              YES                                          
         MVI   MODE,LBUYREQ        NO - SET FOR NEXT AGY/MED                    
         B     EXIT                                                             
*                                                                               
REQF10   MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'          RESET PAGE NUM                               
         CLI   QOPT5,C'Y'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         B     EXIT                                                             
*                                                                               
*                                                                               
PROC     DS    0H                                                               
         CLC   QAGENCY,PUBKAGY     SAME AGENCY ?                                
         BNE   EXIT                NO - IGNORE                                  
         AP    INCNT,=P'1'                                                      
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'90'                                                     
         BAS   RE,NEXTEL           PUB HAS PST ELEMENT ?                        
         BNE   EXIT                NO                                           
         AP    PPSTCNT,=P'1'                                                    
         MVC   ELEM(12),0(R2)          SAVE PST ELEM                            
         MVI   ELEM,X'84'      SINCE BUYRECS ELEM IS X'84'                      
*                                                                               
         MVC   P+1(2),PUBKAGY                                                   
         GOTO1 PUBEDIT,DMCB,PUBKPUB,P+18                                        
         GOTO1 HEXOUT,DMCB,0(R2),P+60,12,=C'N'      PUB PST ELEMENT             
*                                                                               
*                            *****  TEST PST FOR MULTIPLE ENTRIES               
         LA    R0,10               LOOP COUNTER                                 
         LA    R3,ELEM+2                                                        
         SR    R4,R4                                                            
PROCD    CLI   0(R3),0                                                          
         BE    *+8                                                              
         LA    R4,1(R4)            ENTRY COUNTER                                
         LA    R3,1(R3)            NEXT PST ENTRY                               
         BCT   R0,PROCD                                                         
         CH    R4,=H'2'            MORE THAN ONE ENTRY ?                        
         BL    PROCF               NO                                           
*                                  YES                                          
         MVC   P+86(27),=C'** MORE THAN 1 PST ENTRY **'                         
         AP    MORCNT,=P'1'                                                     
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     EXIT                SKIP THIS PUB                                
*                                                                               
PROCF    CLI   QOPT1,C'Q'          LOKING FOR PQ PROVINCE ?                     
         BNE   PROCJ               NO                                           
         CLI   ELEM+7,0            ANY PQ ENTRY                                 
         BNE   PROCP               YES                                          
         B     EXIT                NO - SKIP THIS PUB                           
PROCJ    DS    0H                                                               
         CLI   ELEM+7,0            ANY PQ ENTRY                                 
         BNE   EXIT                YES - SKIP - NOT LOOKING FOR PQ              
PROCP    MVI   SPACING,2                                                        
         MVC   P+86(19),=C'****  PST PUB  ****'                                 
         BAS   RE,RPRT                                                          
*                                                                               
         MVC   SKEY,KEY            SAVE PPG'S KEY                               
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
         CLC   KEY+16(3),=X'5E0601'    CHK INS BEFORE JUN01/94                  
         BL    PROC30              NEXT BUY REC                                 
         CLI   ELEM+7,0            ANY ENTRY IN PUB PST POS'N 6 ?               
         BNE   PROC60B             YES - TEST THIS BUY                          
         CLC   KEY+16(3),=X'610401'    CHK INS BEFORE APR01/97                  
         BL    PROC30              NEXT BUY REC                                 
*                                                                               
PROC60B  LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         TM    PBDCNDA,X'80'           SEE IF CANADIAN                          
         BZ    PROC30                                                           
*                                                                               
PROC60D  LA    R2,PBUYREC+33       SEE IF BUYREC HAS PST ELEM                   
         MVI   ELCODE,X'84'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PROC80                NO - THEN TEST FOR BILL/PAID               
*                                                                               
         CLC   ELEM(12),0(R2)         SEE IF DATA MATCHES                       
         BNE   PROC60F                                                          
         AP    MATCNT,=P'1'                                                     
         B     PROC30                                                           
*                                                                               
PROC60F  DS    0H                                                               
         AP    ERRCNT,=P'1'                                                     
         MVC   P+86(18),=C'** PST MISMATCH **'                                  
         BAS   RE,OUTBUY           FILL IN BUY INFO ON P1                       
         GOTO1 HEXOUT,DMCB,0(R2),P+60,12,=C'N'   BUY PST ELEMENT                
*****    MVC   PSECOND+51(8),=C'PUB PST='                                       
*****    GOTO1 HEXOUT,DMCB,ELEM,PSECOND+60,12,=C'N'   PUB PST ELEMENT           
*****    MVC   PSECOND+60(2),=C'90'       PUB PST ELEMENT CODE                  
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     PROC30            SEQ READ                                       
*                                                                               
PROC80   LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'25'                                                     
PROC80A  BAS   RE,NEXTEL           CHK FOR DATED PAY ELEM                       
         BNE   PROC84              NO PAY ELEM - GO TEST FOR BILL               
         OC    2(3,R2),2(R2)       SEE IF IT HAS A DATE                         
         BZ    PROC80A             NO - LOOK FOR ANOTHER PAY ELEM               
         AP    PAIDCNT,=P'1'                                                    
         MVI   P+49,C'Y'           ALREADY PAID                                 
         MVC   P+86(24),=C'*****  BUY PAID    *****'                            
         BAS   RE,OUTBUY           FILL IN BUY INFO ON P1                       
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         B     PROC30              SKIP TO NEXT BUY                             
*                                                                               
PROC84   LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
PROC84A  BAS   RE,NEXTEL           CHK FOR DATED BILL ELEM                      
         BNE   PROC100             NO BILL - GO ADD PST ELEM                    
         OC    5(3,R2),5(R2)       SEE IF IT HAS A DATE                         
         BZ    PROC84A             NO - LOOK FOR ANOTHER BILL ELEM              
         AP    BILLCNT,=P'1'                                                    
         MVI   P+55,C'Y'           ALREADY BILLED                               
         MVC   P+86(24),=C'*****  BUY BILLED  *****'                            
         BAS   RE,OUTBUY           FILL IN BUY INFO ON P1                       
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         MVI   P+55,C'Y'           ALREADY BILLED                               
*****    B     PROC30              SKIP TO NEXT BUY                             
*****                              ADD ELEMENT EVEN IF BILLED                   
*                                                                               
PROC100  DS    0H                 ADD MISSING PST ELEM                          
         MVC   P+86(30),=C'** X''84'' PST ELEMENT ADDED  **'                    
         BAS   RE,OUTBUY           FILL IN BUY INFO ON P1                       
         GOTO1 HEXOUT,DMCB,ELEM,P+60,12,=C'N'   BUY PST ELEMENT                 
         MVI   SPACING,2                                                        
         BAS   RE,RPRT                                                          
         AP    ADDCNT,=P'1'                                                     
         GOTO1 RECUP,DMCB,(1,PBUYREC),ELEM,0(R2)                                
         CLI   QOPT6,C'Y'          DUMP RECORD ?                                
         BNE   *+8                 NO                                           
         BAS   RE,DMPREC                                                        
         CLI   RCWRITE,C'N'        MEANS DON'T MARK FILE                        
         BE    PROC30             GO CHECK NEXT BUY                             
         GOTO1 PUTPRT                                                           
         B     PROC30             GO CHECK NEXT BUY                             
*                                                                               
PROCX    DS    0H                                                               
         MVC   KEY(64),SKEY        RESTORE PPG'S KEY                            
         B     EXIT                                                             
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
         EJECT                                                                  
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
         XC    P,P                                                              
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
*                                                                               
OUTBUY   NTR1                                                                   
*****    MVC   PSECOND+86(5),=C'ADD ='                                          
*****    GOTO1 HEXOUT,DMCB,KEY+27,PSECOND+92,4,=C'N'                            
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(5,P+36)                                
         CLI   PBUYKLIN,1                                                       
         BE    OUTB10                                                           
         MVI   P+44,C'-'                                                        
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+45(2),DUB                                                      
*                                                                               
OUTB10   DS    0H                                                               
         MVC   P+1(2),PBUYKAGY                                                  
         MVC   P+4(3),PBUYKCLT                                                  
         MVC   P+8(3),PBUYKPRD                                                  
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB                                                      
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,P+18                                       
         XIT1                                                                   
         EJECT                                                                  
*                             GET FIRST/NEXT PUB                                
NXTPUB   NTR1          ***  NOT USED IN THIS PROGRAM  ***                       
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
         MVC   HALF,25(R5)      RECORD LENGTH                                   
         LH    R2,HALF                                                          
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
ERRCNT   DS    PL8                                                              
         DC    CL15'PST MISMATCH'                                               
PPSTCNT  DS    PL8                                                              
         DC    CL15'PST PUBS'                                                   
ADDCNT   DS    PL8                                                              
         DC    CL15'PST ELEMS ADDED'                                            
MATCNT   DS    PL8                                                              
         DC    CL15'PUB-BUY MATCHES'                                            
MORCNT   DS    PL8                                                              
         DC    CL15'MULTI-ENTRY PST'                                            
PAIDCNT  DS    PL8                                                              
         DC    CL15'BUYS PAID'                                                  
BILLCNT  DS    PL8                                                              
         DC    CL15'BILLED ONLY'                                                
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
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREP0202L05/01/02'                                      
         END                                                                    
