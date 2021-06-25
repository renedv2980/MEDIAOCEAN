*          DATA SET PPREP0202W AT LEVEL 119 AS OF 05/01/02                      
*PHASE PP0202W,+0,NOAUTO                                                        
*INCLUDE MININAM                                                                
         TITLE 'PP0202W - PRINTPAK RESTORE BILLING'                             
*                                                                               
*************  CHANGE LOG  ************                                         
*                                                                               
*  SMYE  12/18/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
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
         CLI   MODE,PROCREQ                                                     
         BE    PROC                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         B     EXIT                                                             
*                                                                               
RUNF     DS    0H                                                               
         ZAP   INCNT,=P'0'                                                      
         ZAP   OUTCNT,=P'0'                                                     
         ZAP   BUYCNT,=P'0'                                                     
         ZAP   BILLDOL,=P'0'                                                    
         ZAP   BUYDOL,=P'0'                                                     
         ZAP   MBUYDOL,=P'0'                                                    
         ZAP   MBUYCNT,=P'0'                                                    
         B     EXIT                                                             
*                                                                               
PROC     DS    0H                                                               
         CLI   QOPT1,C'N'          MEANS DON'T MARK FILE                        
         BNE   *+8                                                              
         MVI   RCWRITE,C'N'                                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   DMINBTS,X'08'        SET TO PASS DELETES                         
         MVI   DMOUTBTS,X'FD'       SET TO PASS DELETES                         
*                                                                               
         OPEN  (IN,(INPUT))                                                     
GET      DS    0H                                                               
         BAS   RE,TAPEGET                                                       
         AP    INCNT,=P'1'                                                      
         CLC   REC(3),=X'FFFFFF'     END OF FILE                                
         BE    EOF                                                              
         CLC   REC(2),=C'FC'       MUST BE FOOTE CONE                           
         BNE   GET                                                              
*                                  CHK MEDIA/CLIENT                             
         MVC   WORK(1),REC+2                                                    
         MVC   WORK+1(3),REC+4                                                  
         LA    R2,MEDCLT                                                        
GET5     CLI   0(R2),X'FF'         END OF TABLE                                 
         BE    GET                                                              
         CLC   WORK(4),0(R2)                                                    
         BE    GET10                                                            
         LA    R2,4(R2)                                                         
         B     GET5                                                             
*                                                                               
GET10    CLI   REC+3,X'08'         BILLREC                                      
         BNE   CHKBUY                                                           
*                                                                               
         LA    R6,REC                                                           
         USING PBILLREC,R6                                                      
         CLC   PBILLDAT,=C'900321'    BILLS FROM MAR21/90                       
         BNE   GET                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   *+6                 ALREADY ON FILE                              
         DC    H'0'                FATAL ERROR                                  
         AP    BILLDOL,PBILLGRS                                                 
         LA    R0,REC                                                           
         ST    R0,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         CLI   QOPT1,C'N'          SEE IF NOT MARKING FILE                      
         BE    GET15                                                            
         GOTO1 ADDPRT                                                           
GET15    BAS   RE,FMTBIL                                                        
         BAS   RE,RPRT                                                          
         AP    OUTCNT,=P'1'                                                     
*                                                                               
         B     GET                                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
CHKBUY   CLI   REC+3,X'20'                                                      
         BNE   GET                                                              
         LA    R6,REC                                                           
         USING PBUYREC,R6                                                       
         XC    ELEM,ELEM                                                        
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
CHKB2    BAS   RE,NEXTEL                                                        
         BNE   GET                                                              
         CLC   5(3,R2),=X'5A0315'     BILL ELEM FROM MAR21/90                   
         BNE   CHKB2                                                            
         MVC   ELEM(23),0(R2)         SAVE BILL ELEM TO BE ADDED                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),REC                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    CHKB4               FOUND                                        
         MVC   P(26),=C'** BILLED BUY NOT FOUND **'                             
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
         MVC   FULL,ELEM+11                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    MBUYDOL,DUB                                                      
         AP    MBUYCNT,=P'1'                                                    
         B     GET                                                              
*                                                                               
         DROP  R6                                                               
*                                                                               
CHKB4    AP    BUYCNT,=P'1'                                                     
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CP    BUYCNT,=P'50'                                                    
         BH    CHKB6                                                            
         MVC   P(10),=C'**BEFORE**'                                             
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
CHKB6    LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
CHKB7    BAS   RE,NEXTEL                                                        
         BNE   CHKB8                                                            
         OC    5(3,R2),5(R2)      SEE IF I CAN USE THIS ELEM                    
         BNZ   CHKB7                                                            
         MVC   0(23,R2),ELEM                                                    
         B     CHKB10                                                           
*                                                                               
CHKB8    GOTO1 RECUP,DMCB,(1,PBUYREC),ELEM,(R2)                                 
*                                                                               
CHKB10   DS    0H                                                               
         MVC   FULL,ELEM+11                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    BUYDOL,DUB                                                       
         BAS   RE,FMTBUY                                                        
         BAS   RE,RPRT                                                          
         CP    BUYCNT,=P'50'                                                    
         BH    CHKB15                                                           
         MVC   P(9),=C'**AFTER**'                                               
         BAS   RE,RPRT                                                          
         BAS   RE,DMPREC                                                        
CHKB15   CLI   QOPT1,C'N'         SEE IF NOT MARKING FILE                       
         BE    CHKB18                                                           
         GOTO1 PUTPRT                                                           
*                                                                               
CHKB18   BAS   RE,RPRT            SKIP A LINE                                   
         B     GET                                                              
*                                                                               
*                                                                               
TAPEGET  NTR1                                                                   
         GET   IN,REC-4                                                         
         MVC   HALF,REC+25                                                      
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0           END OF RECORD                                  
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
FMTBIL   NTR                                                                    
         LA    R6,REC                                                           
         USING PBILLREC,R6                                                      
         LA    R7,P                                                             
         USING UBLIND,R7                                                        
         MVC   P(11),=C'**READDED**'                                            
         MVC   UBSEP,=C'ADJ'                                                    
         CLI   PBILSEP,C'A'                                                     
         BE    FMTB5                                                            
         MVC   UBSEP,=C'CD '                                                    
         CLI   PBILSEP,C'C'                                                     
         BE    FMTB5                                                            
         MVC   UBSEP,=C'   '                                                    
FMTB5    DS    0H                                                               
         CLI   PBRETAIL,0                                                       
         BE    FMTB7                                                            
         MVC   UBRET,=C'COR'                                                    
         CLI   PBRETAIL,X'81'          CORPORATE CONTROL                        
         BE    FMTB7                                                            
         MVC   UBRET,=C'SUM'                                                    
         CLI   PBRETAIL,X'41'          RETAIL SUMMARY                           
         BE    FMTB7                                                            
         MVC   UBRET,=C'RET'                                                    
FMTB7    DS    0H                                                               
         TM    PBILCMSW,X'20'              SEE IF AOR BILL                      
         BNO   FMTB8                                                            
         MVC   UBRET,=C'AOR'              CAN'T BE RETAIL AND AOR               
*                                                                               
FMTB8    MVC   UBMED,PBILKMED                                                   
         MVC   UBCLT,PBILKCLT                                                   
         MVC   UBPRD,PBILKPRD                                                   
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
*        GOTO1 DTCNV,DMCB,(1,PBILKMOS),(5,UBMOS)                                
         GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,UBMOS)                               
*        GOTO1 (RF),(R1),(1,PBILKBMN),(5,UBBMN)                                 
         GOTO1 DATCON,(R1),(3,PBILKBMN),(9,UBBMN)                               
         MVC   HALF,PBILKBNO                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBINV,DUB                                                        
         ZAP   DUB,PBILLGRS                                                     
         CVB   R0,DUB                                                           
         ST    R0,DMCB                                                          
         ZAP   DUB,PBILLBIL                                                     
         CVB   R0,DUB                                                           
         ST    R0,DMCB+4                                                        
         ZAP   DUB,PBILLNET                                                     
         CVB   R0,DUB                                                           
         ST    R0,DMCB+8                                                        
         LA    R2,DMCB                                                          
         BAS   RE,EDIT3                                                         
         EDIT  (P5,PBILLRCV),(15,P+113),2,COMMAS=YES,CR=YES                     
         XIT                                                                    
*                                                                               
EDIT3    EQU   *                                                                
         LA    R3,3                                                             
         LA    R4,UGRS                                                          
EDIT3A   EQU   *                                                                
         L     R0,0(R2)                                                         
         EDIT  (R0),(15,0(R4)),2,COMMAS=YES,CR=YES                              
         LA    R2,4(R2)                                                         
         LA    R4,16(R4)                                                        
         BCT   R3,EDIT3A                                                        
         BR    RE                                                               
*                                                                               
         DROP  R7                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
FMTBUY   NTR                                                                    
         LA    R7,P                                                             
         USING UBLIND,R7                                                        
         MVC   P(12),=C'**REBILLED**'                                           
         CLI   ELEM,X'28'          FINANICAL OPEN BILLING ELEM                  
         BNE   *+10                                                             
         MVC   P(12),=C'**OPEN REB**'                                           
*                                                                               
         MVC   UBMED,PBUYKMED                                                   
         MVC   UBCLT,PBUYKCLT                                                   
         MVC   UBPRD,ELEM+2                                                     
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   *+8                                                              
         MVI   UBPRD+4,C'Z'                                                     
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  UBEST,DUB                                                        
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(4,UBYDAT)                               
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(7,UBYDAT)                              
         LA    R4,UBYDAT+5                                                      
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+14                                                             
         MVC   UBYDAT+3(2),SPACES                                               
         LA    R4,UBYDAT+3                                                      
         CLI   PBUYKLIN,1                                                       
         BE    PRBY8                                                            
         SR    R5,R5                                                            
         IC    R5,PBUYKLIN                                                      
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(2,R4),DUB                                                      
         MVI   0(R4),C'-'                                                       
PRBY8    EQU   *                                                                
         GOTO1 PUBEDIT,DMCB,PBUYKPUB,UBPUB                                      
*                                                                               
         MVC   DMCB(12),ELEM+11                                                 
         LA    R2,DMCB                                                          
         BAS   RE,EDIT3                                                         
         XIT                                                                    
*                                                                               
         DROP  R7                                                               
*                                                                               
COUNTS   DS    0C                                                               
INCNT    DS    PL8                                                              
         DC    CL20'TAPE RECS READ'                                             
OUTCNT   DS    PL8                                                              
         DC    CL20'BILLS ADDED'                                                
BUYCNT   DS    PL8                                                              
         DC    CL20'BILLING ELEMS ADDED'                                        
BILLDOL  DS    PL8                                                              
         DC    CL20'BILL $ ADDED'                                               
BUYDOL   DS    PL8                                                              
         DC    CL20'BUY $ ADDED'                                                
MBUYCNT  DS    PL8                                                              
         DC    CL20'MISSING BUYS'                                               
MBUYDOL  DS    PL8                                                              
         DC    CL20'MISSING BUYS $'                                             
         DC    X'FF'                                                            
*                                                                               
MEDCLT   DC    C'MCX '                                                          
         DC    C'MEA '                                                          
         DC    C'MGBL'                                                          
         DC    C'MNV '                                                          
         DC    C'MPA '                                                          
         DC    C'MPBD'                                                          
         DC    C'MPSY'                                                          
         DC    C'MPTY'                                                          
         DC    C'MPVA'                                                          
         DC    C'MTBL'                                                          
*********************                                                           
         DC    C'NCMG'                                                          
         DC    C'NCSA'                                                          
         DC    C'NCSC'                                                          
         DC    C'NCSN'                                                          
         DC    C'NCSU'                                                          
         DC    C'NEA '                                                          
         DC    C'NPA '                                                          
         DC    C'NPSY'                                                          
         DC    C'NPTY'                                                          
         DC    C'NPWL'                                                          
         DC    C'NTBL'                                                          
         DC    C'OCSA'                                                          
         DC    C'OPTY'                                                          
         DC    C'OTBL'                                                          
         DC    C'TEA '                                                          
         DC    C'TGBL'                                                          
         DC    C'TPBD'                                                          
         DC    C'TPBV'                                                          
         DC    C'TPNI'                                                          
         DC    C'TPNS'                                                          
         DC    C'TTAM'                                                          
         DC    X'FFFFFFFF'                                                      
*                                                                               
RUNL     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R4,COUNTS                                                        
RUNL5    CLI   0(R4),X'FF'                                                      
         BE    EXIT                                                             
         MVC   P(20),8(R4)                                                      
         OI    7(R4),X'0F'                                                      
         UNPK  P+20(10),0(8,R4)                                                 
         GOTO1 REPORT                                                           
         LA    R4,28(R4)                                                        
         B     RUNL5                                                            
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
EOF      CLOSE (IN,)                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                             LINK TO REPORT                                    
         SPACE 2                                                                
         DS    0F                                                               
RPRT     NTR1                                                                   
         SPACE 2                                                                
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
         MVC   HALF,25(R5)                                                      
         LH    R2,HALF              USE RECORD LENGHT                           
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
IN       DCB   DDNAME=IN,                                              X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         SPACE 2                                                                
PP02WRKD DSECT                                                                  
LASTPUB  DS    XL6                                                              
ELCODE   DS    X                                                                
ELEM     DS    CL50                                                             
FRSTSW   DS    XL1                                                              
PUBDMWRK DS    12D                                                              
LTLDMWRK DS    12D                                                              
ABUFFC   DS    A                                                                
ITOT     DS    F                                                                
*                                                                               
         EJECT                                                                  
UBLIND   DSECT                                                                  
         DS    CL18                                                             
UBMED    DS    CL1                                                              
         DS    CL1                                                              
UBCLT    DS    CL3                                                              
         DS    CL1                                                              
UBPRD    DS    CL3                                                              
         DS    CL3                                                              
UBEST    DS    CL3                                                              
         DS    CL1                                                              
UBMOS    DS    CL6                                                              
         DS    CL1                                                              
UBBMN    DS    CL6                                                              
         DS    CL1                                                              
UBINV    DS    CL4                                                              
         DS    CL2                                                              
UBSEP    DS    CL3              ADJ/CD                                          
         DS    CL2                                                              
UBRET    DS    CL3              RET/SUM/COR/AOR                                 
         DS    CL3                                                              
UGRS     DS    CL15                                                             
         DS    CL1                                                              
UBIL     DS    CL15                                                             
         DS    CL1                                                              
UNET     DS    CL15                                                             
         ORG   UBMOS                                                            
UBYDAT   DS    CL8                                                              
         DS    CL1                                                              
UBPUB    DS    CL15                                                             
         DS    CL1                                                              
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'119PPREP0202W05/01/02'                                      
         END                                                                    
