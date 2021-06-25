*          DATA SET PPREP9302  AT LEVEL 006 AS OF 04/22/03                      
*PHASE PP9302A                                                                  
*                                                                               
         TITLE 'PP9302  PRINT GREY BILLING INTERFACE TAPE'                      
*                                                                               
PP9302   CSECT                                                                  
         NMOD1 0,PP9302                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP93WRKD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    OPEN                                                             
         CLI   MODE,RUNLAST                                                     
         BE    CLOSE                                                            
*                                                                               
         CLI   MODE,UNSRTREC                                                    
         BNE   EXIT                                                             
         MVC   SAVPARS,DMCB+4                                                   
         LM    R2,R3,SAVPARS                                                    
         B     BRANCH(R2)                                                       
*                                                                               
BRANCH   B     INITIAL                                                          
         B     INPUT                                                            
         B     OUTPUT                                                           
         B     FINAL                                                            
*                                                                               
INITIAL  MVI   FORCEHED,C'Y'                                                    
         MVC   GREC1,SPACES                                                     
         MVC   GREC2,SPACES                                                     
         MVC   GRECCD,=C'H10'                                                   
         MVI   GBATCHMN,C'&&'                                                   
         CLC   QSTART+2(2),=C'12'                                               
         BE    HDR1                                                             
         MVI   GBATCHMN,C'-'                                                    
         CLC   QSTART+2(2),=C'11'                                               
         BE    HDR1                                                             
         MVC   GBATCHMN,QSTART+3                                                
*                                                                               
HDR1     LA    R4,MEDTAB                                                        
HDR2     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID MEDIA                                
         CLC   0(1,R4),QMEDIA                                                   
         BE    HDR3                                                             
         LA    R4,8(R4)                                                         
         B     HDR2                                                             
*                                                                               
HDR3     MVC   GBATCHSQ,1(R4)                                                   
         MVC   GBATCHTY,=C'BH'                                                  
         LA    R1,GRYFILE          OUT PUT BATCH HEADER RECORD                  
         LA    R0,GREC1                                                         
         PUT   (1),(0)                                                          
*                                                                               
         MVC   SAVPARS+4(2),=C'40'      SORT REC LEN                            
         MVC   SAVPARS+6(2),=C'14'      SORT KEY LEN                            
         XC    KEY,KEY                                                          
         XC    PBILLREC(256),PBILLREC                                           
         LA    R5,13        FOR BCT                                             
         LA    R4,INVTOTG                                                       
HDR4     ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R5,HDR4                                                          
*                                                                               
         XC    PRDPSF(14),PRDPSF                                                
         XC    SAVEBIL,SAVEBIL                                                  
         XC    INVCT,INVCT                                                      
         MVI   CLTACT,0                                                         
         B     EXIT                                                             
         EJECT                                                                  
INPUT    BAS   RE,GYIN                                                          
         B     EXIT                                                             
*                                                                               
OUTPUT   BAS   RE,GYOUT                                                         
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
EXIT     MVC   DMCB+4(8),SAVPARS                                                
         DS    0H                                                               
XIT      XIT1                                                                   
*                                                                               
OPEN     LA    R1,GRYFILE                                                       
*&&DO                                                                           
         OPENR (1)                                                              
*&&                                                                             
*&&OS                                                                           
         OPEN  (GRYFILE,(OUTPUT))                                               
*&&                                                                             
         B     XIT                                                              
*                                                                               
CLOSE    LA    R2,GRYFILE                                                       
*&&DO                                                                           
         CLOSER (2)                                                             
*&&                                                                             
*&&OS                                                                           
         CLOSE ((2),)                                                           
*&&                                                                             
         B     XIT                                                              
         EJECT                                                                  
GYIN     NTR1                                                                   
         OC    KEY,KEY             FIRST TIME                                   
         BNZ   GYIN5                                                            
*                                                                               
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'08'                                                      
         CLC   QCLIENT,SPACES                                                   
         BE    GYIN1                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BE    GYIN1                                                            
         MVC   KEY+4(3),QCLIENT                                                 
*                                                                               
GYIN1    BAS   RE,HIGH                                                          
         B     GYIN5A                                                           
*                                                                               
GYIN5    BAS   RE,SEQ                                                           
GYIN5A   CLC   KEY(4),KEYSAVE           AGY/MED/RECORD                          
         BNE   GYIN90                                                           
         CLC   QCLIENT,SPACES                                                   
         BE    GYIN6                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BE    GYIN6                                                            
         CLC   QCLIENT,KEY+4            CLIENT CHECK                            
         BNE   GYIN90                                                           
GYIN6    LA    R0,PBILLREC              GET BILL REC                            
         ST    R0,AREC                                                          
         BAS   RE,GETR                                                          
         CLI   PBILLMOD,C'E'                                                    
         BE    GYIN7                                                            
         OC    PBILKEST,PBILKEST   FOR PRD AND SERIES BILLS                     
         BNE   GYIN5               USE ONLY EST= 0 BILL                         
GYIN7    DS    0H                                                               
         CLC   PBILLDAT,QSTART                                                  
         BL    GYIN5                    NEXT REC                                
         CLC   PBILLDAT,QEND                                                    
         BH    GYIN5                                                            
         L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         XC    SORTREC,SORTREC                                                  
         CLC   QCLIENT,SPACES                                                   
         BE    *+10                                                             
         MVC   SRTCLT,PBILKCLT                                                  
         MVC   SRTINV,PBILKBNO                                                  
         MVC   SRTPRD,PBILKPRD                                                  
         MVC   SRTEST,PBILKEST                                                  
         MVC   SMOS,PBILKMOS                                                    
         MVC   SINVDATE(4),PBILLDAT+2                                           
         MVC   SINVDATE+4(1),PBILLDAT+1                                         
         MVC   SGROSS(20),PBILLGRS      GROSS/G-CD/G-AC-CD/REC                  
         B     GYINX                                                            
GYIN90   MVI   SAVPARS+3,8              END OF INPUT                            
*                                                                               
GYINX    XIT1                                                                   
         EJECT                                                                  
GYOUT    NTR1                                                                   
         USING SORTRECD,R3                                                      
         CLC   SAVEBIL(5),SRTCLT          TEST SAME CLT/INV                     
         BE    GRY6                                                             
         MVC   G1PRDS,ZEROS                                                     
         MVC   G2PRDS,ZEROS                                                     
         MVC   GINVTOT,ZEROS                                                    
*                                                                               
GRY6     PACK  DUB,G1PRDS                                                       
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  G1PRDS,DUB                                                       
         PACK  DUB,GINVTOT                                                      
         AP    DUB,SREC                                                         
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         UNPK  GINVTOT,DUB                                                      
         MVC   GRECCD,=C'H11'                                                   
         MVC   GBATCHTY,=C'AD'                                                  
         CLC   SRTCLT,SAVEBIL                                                   
         BE    GRY6A                                                            
         BAS   RE,GETCLT                                                        
         BAS   RE,GETPRD                                                        
         B     GRY6B                                                            
*                                                                               
GRY6A    CLC   SRTPRD,SAVEBIL+5        SAME PRODUCT                             
         BE    *+8                                                              
         BAS   RE,GETPRD                                                        
GRY6B    MVC   GCLTBILL,PRDACNO                                                 
         MVC   GCLTPRD,PRDACNO                                                  
         MVC   GCLTALP,PCLTNUM                                                  
         OC    GCLTALP,SPACES                                                   
         MVC   GBRANCH,PCLTOFF                                                  
         MVC   GINVNO(2),SINVDATE                                               
         MVC   DUB(2),SRTINV                                                    
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GINVNO+2(4),DUB                                                  
         MVC   GINVDATE,SINVDATE                                                
         LA    R4,SMOS                                                          
         BAS   RE,CVD1                                                          
         MVC   GMONSERV+2(1),WORK+1          YEAR                               
         LA    R4,SMOS+1                                                        
         BAS   RE,CVD1                                                          
         MVC   GMONSERV(2),WORK                                                 
         B     GRY7                                                             
CVD1     SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB+6(2)                                                 
         BR    RE                                                               
GRY7     SR    R0,R0                                                            
         IC    R0,SRTEST                                                        
         SLL   R0,8                                                             
         IC    R0,SRTEST+1                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GEST(4),DUB+5(3)                                                 
         LA    R4,MEDTAB                                                        
GRY7A    CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                     INVALID MEDIA                           
         CLC   0(1,R4),QMEDIA                                                   
         BE    GRY7B                                                            
         LA    R4,8(R4)                                                         
         B     GRY7A                                                            
*                                                                               
GRY7B    MVC   GESTTYPE,4(R4)                                                   
         LA    R4,SMOS                                                          
         BAS   RE,CVD1                                                          
         MVC   GESTYR,WORK                                                      
         MVC   GESTREV,ZEROS                                                    
         LA    R4,MEDTAB                                                        
GRY8     CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                INVALID MEDIA                                
         CLC   0(1,R4),QMEDIA                                                   
         BE    GRY8A                                                            
         LA    R4,8(R4)                                                         
         B     GRY8                                                             
*                                                                               
GRY8A    PACK  DUB,6(2,R4)                                                      
         OC    PRDPSF,PRDPSF                                                    
         BZ    *+10                                                             
         AP    DUB,=P'1'                                                        
         OI    DUB+7,X'0F'                                                      
         UNPK  GMEDCD,DUB                                                       
         MVC   GREC2(42),GREC1                                                  
         MVC   GREC2ID,=C'**'                                                   
         MVC   G2PRDS,G1PRDS                                                    
         ZAP   DUB,SGROSS                                                       
         SP    DUB,SBILL           TO GET CASH DISC                             
         AP    DUB,SREC            ADD NET REC TO GET GROSS REC                 
         UNPK  GGROSS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    GGROSS+8,X'F0'                                                   
         AP    TINVTOT,SREC                                                     
         UNPK  GNET,SNET                                                        
         CP    SNET,=P'0'                                                       
         BL    *+8                                                              
         OI    GNET+8,X'F0'                                                     
         ZAP   DUB,SGROSS                                                       
         SP    DUB,SBILL                                                        
         UNPK  GDISC,DUB                CASH DISC                               
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    GDISC+6,X'F0'                                                    
         MVC   GPSF,ZEROS                                                       
         MVC   GCOMM,ZEROS                                                      
         ZAP   DUB,SREC                                                         
         SP    DUB,SNET                                                         
         BZ    BIR2                NO COMMISSION                                
         UNPK  GCOMM,DUB                                                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    GCOMM+7,X'F0'                                                    
         CLC   PRDPSF,=X'000001'        PSF NULL RATE                           
         BNH   BIR2                                                             
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PRDPSF                                                 
         L     R1,FULL                                                          
         ZAP   DUB,SGROSS                                                       
         SP    DUB,SBILL           TO GET CASH DISC                             
         AP    DUB,SREC            ADD NET REC TO GET GROSS REC                 
         CVB   R0,DUB                                                           
         MR    R0,R0                                                            
         D     R0,=F'10000000'             SCALE                                
         LR    R0,R1                                                            
         BAS   RE,CVD                                                           
         MVC   GPSF,WORK+1              GROSS X PRDPSF                          
         ZAP   DUB,SREC                                                         
         SP    DUB,SNET                                                         
         CVB   R1,DUB                   R1= COMM                                
         SR    R1,R0                    COMM - PSF= REAL COMM                   
         LR    R0,R1                                                            
         BAS   RE,CVD                                                           
         MVC   GCOMM,WORK+2                                                     
*                                                                               
BIR2     CLC   SAVEBIL(3),SRTCLT                                                
         BE    BIR3         SAME CLIENT                                         
         BAS   RE,ENDCLT                                                        
         B     BIR3A                                                            
*                                                                               
BIR3     CLC   SAVEBIL+3(2),SRTINV                                              
         BE    *+8                                                              
         BAS   RE,INVBK                 INVOICE BREAK                           
BIR3A    MVI   CLTACT,C'Y'                                                      
         MVC   P+1(5),PRDACNO                                                   
         MVC   P+7(3),SRTCLT                                                    
         MVC   P+19(3),SRTPRD                                                   
         OC    SRTEST,SRTEST                                                    
         BZ    BIR4                                                             
         EDIT  (2,SRTEST),(3,P+24)                                              
*                                                                               
BIR4     EDIT  (1,SMOS+1),(2,P+30)                                              
         MVI   P+32,C'/'                                                        
         EDIT  (1,SMOS),(2,P+33)                                                
         MVC   P+11(2),SINVDATE                                                 
         MVI   P+13,C'-'                                                        
         MVC   DUB(2),SRTINV                                                    
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+14(4),DUB                                                      
         ZAP   DUB2,SGROSS                                                      
         SP    DUB2,SBILL          TO GET CASH DISC                             
         AP    DUB2,SREC           ADD NET REC TO GET GROSS REC                 
         EDIT  (P8,DUB2),(16,P+37),2,COMMAS=YES,CR=YES                          
         AP    CLTTOTG,DUB              BUMP CLT TOTAL                          
         AP    RPTTOTG,DUB              BUMP RPT TOTAL                          
         AP    INVTOTG,DUB              BUMP INV TOTAL                          
         ZAP   DUB2,SGROSS                                                      
         SP    DUB2,SBILL               CASH DISC                               
         EDIT  (P8,DUB2),(16,P+69),2,COMMAS=YES,CR=YES                          
         AP    CLTTOTD,DUB                                                      
         AP    RPTTOTD,DUB                                                      
         AP    INVTOTD,DUB                                                      
         ZAP   DUB2,SREC                                                        
         SP    DUB2,SNET                AGY COMM                                
         EDIT  (P8,DUB2),(16,P+53),2,COMMAS=YES,CR=YES                          
         AP    CLTTOTC,DUB                                                      
         AP    RPTTOTC,DUB                                                      
         AP    INVTOTC,DUB                                                      
         EDIT  (P5,SNET),(16,P+85),2,COMMAS=YES,CR=YES                          
         AP    CLTTOTN,DUB                                                      
         AP    RPTTOTN,DUB                                                      
         AP    INVTOTN,DUB                                                      
         L     R1,INVCT                                                         
         LA    R1,1(R1)                                                         
         ST    R1,INVCT                                                         
         BAS   RE,PRINTIT                                                       
         MVC   SAVEBIL,SORTREC            SAVE KEY                              
GRY20    LA    R1,GRYFILE                                                       
         LA    R0,GREC1                                                         
         PUT   (1),(0)                                                          
         LA    R0,GREC2                                                         
         PUT   (1),(0)                                                          
         B     EXIT                                                             
         SPACE 2                                                                
CVD      CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
FINAL    BAS   RE,ENDCLT                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+2(20),=C'** REQUEST TOTALS **'                                 
         LA    R2,RPTTOTG                                                       
         BAS   RE,FMTTOT                                                        
         BAS   RE,PRINTIT                                                       
         MVI   SPACING,2                                                        
         MVC   P+2(80),QRECORD                                                  
         BAS   RE,PRINTIT                                                       
         MVC   GREC1+19(60),SPACES                                              
         MVC   GREC1(3),=C'H10'                                                 
         MVC   GREC1+7(2),=C'BT'                                                
         OI    TINVTOT+7,X'0F'                                                  
         UNPK  GREC1+9(10),TINVTOT                                              
         LA    R1,GRYFILE                                                       
         LA    R0,GREC1                                                         
         PUT   (1),(0)                                                          
         B     EXIT                                                             
         EJECT                                                                  
GETPRD   NTR1                                                                   
*                       GET PRODUCT RECORD                                      
         XC    KEY,KEY                                                          
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),SRTCLT                                                  
         MVC   KEY+7(3),SRTPRD                                                  
         BAS   RE,READ                                                          
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETR                                                          
         MVC   PRDACNO,PPRDACCT           SAVE ACCOUNT NUMBER                   
         MVC   PRDPSF,PPRDBILP+10               FEE                             
*                                                                               
         CLI   PRDACNO,X'FF'                                                    
         BE   GETP1                                                             
         MVC   PRDACNO,=5C'0'                                                   
         B     GETPX                                                            
*                                                                               
GETP1    MVC   FULL,PPRDACCT                                                    
         MVI   FULL,0                                                           
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PRDACNO,DUB                                                      
GETPX    XIT1                                                                   
         EJECT                                                                  
GETCLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),SRTCLT                                                  
         BAS   RE,READ                                                          
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         BAS   RE,GETR                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
ENDCLT   NTR1                                                                   
         BAS   RE,INVBK                                                         
         CLI   CLTACT,0                                                         
         BE    ENDC1                                                            
         BAS   RE,PRINTIT                                                       
         MVI   SPACING,2                                                        
         MVC   P+2(19),=C'** CLIENT TOTALS **'                                  
         LA    R2,CLTTOTG                                                       
         BAS   RE,FMTTOT                                                        
         BAS   RE,PRINTIT                                                       
         MVI   CLTACT,0                                                         
ENDC1    XIT1                                                                   
         EJECT                                                                  
*                                                                               
HIGH     LA    R0,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     DIR                                                              
*                                                                               
SEQ      LA    R0,DMRSEQ                                                        
         B     DIR                                                              
*                                                                               
READ     LA    R0,DMREAD                                                        
         MVC   KEYSAVE,KEY                                                      
*                                                                               
DIR      NTR1                                                                   
         ST    R0,DMCB                                                          
         MVC   DMCB(1),DMINBTS                                                  
         GOTO1 DATAMGR,DMCB,,PRTDIR,KEY,KEY                                     
         B     DMCHECK                                                          
*                                                                               
GETR     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,AREC,(0,DMWORK)               
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
INVBK    NTR1                                                                   
         CLC   INVCT,=F'1'                                                      
         BNH   IB2                                                              
         LA    R2,INVTOTG                                                       
         BAS   RE,FMTTOT                                                        
         MVC   P+5(20),=C'** INVOICE TOTALS **'                                 
         BAS   RE,PRINTIT                                                       
         B     IB4                                                              
*                                                                               
IB2      ZAP   INVTOTG,=P'0'                                                    
         ZAP   INVTOTC,=P'0'                                                    
         ZAP   INVTOTD,=P'0'                                                    
         ZAP   INVTOTN,=P'0'                                                    
IB4      XC    INVCT,INVCT                                                      
         XIT1                                                                   
*                                                                               
* ROUTINE TO PRINT TOTALS  INV/CLT/RPT                                          
*                                                                               
FMTTOT   EDIT  (P8,0(R2)),(16,P+37),2,COMMAS=YES,CR=YES                         
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         MVI   P+51,C'*'                                                        
         EDIT  (P8,24(R2)),(16,P+53),2,COMMAS=YES,CR=YES                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         MVI   P+67,C'*'                                                        
         EDIT  (P8,48(R2)),(16,P+69),2,COMMAS=YES,CR=YES                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         MVI   P+83,C'*'                                                        
         EDIT  (P8,72(R2)),(16,P+85),2,COMMAS=YES,CR=YES                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         MVI   P+99,C'*'                                                        
         ZAP   0(8,R2),=P'0'                                                    
         ZAP   24(8,R2),=P'0'                                                   
         ZAP   48(8,R2),=P'0'                                                   
         ZAP   72(8,R2),=P'0'                                                   
         BR    RE                       RETURN                                  
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
MEDTAB   DC    C'N',C'598',C'NR',C'52'         NEWSPAPERS                       
         DC    C'M',C'498',C'CM',C'42'           MAGS                           
         DC    C'T',C'499',C'TM',C'42'        TRADE                             
         DC    C'S',C'599',C'NS',C'52'        SUPPS                             
         DC    C'O',C'699',C'OB',C'60'         OUTDOOR                          
         DC    X'00'                                                            
*                                                                               
ZEROS    DC    15C'0'                                                           
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
GRYFILE  DTFMT BLKSIZE=1600,DEVADDR=SYS008,FILABL=NO,IOAREA1=GRYBUFF,  X        
               RECFORM=FIXBLK,RECSIZE=80,REWIND=UNLOAD,TPMARK=NO,      X        
               TYPEFLE=OUTPUT,WORKA=YES                                         
*&&                                                                             
*&&OS                                                                           
GRYFILE  DCB   DDNAME=GRYFILE,         DOS SYS008                      X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=00080,                                            X        
               BLKSIZE=01600,          DOS BLKSIZE=01600               X        
               MACRF=PM                                                         
*&&                                                                             
*                                                                               
*&&DO                                                                           
GRYBUFF  DS    200D                                                             
*&&                                                                             
         EJECT                                                                  
PP93WRKD DSECT                                                                  
INVTOTG  DS    D                                                                
CLTTOTG  DS    D                                                                
RPTTOTG  DS    D                                                                
INVTOTC  DS    D                                                                
CLTTOTC  DS    D                                                                
RPTTOTC  DS    D                                                                
INVTOTD  DS    D                                                                
CLTTOTD  DS    D                                                                
RPTTOTD  DS    D                                                                
INVTOTN  DS    D                                                                
CLTTOTN  DS    D                                                                
RPTTOTN  DS    D                                                                
*                                                                               
TINVTOT  DS    D                                                                
*                                                                               
DUB2     DS    D                                                                
PRDACNO  DS    CL5                                                              
PRDPSF   DS    CL3          FEE                                                 
BINVNO   DS    CL6                                                              
OLDPRD   DS    CL3                                                              
INVCT    DS    F                                                                
AREC     DS    A                                                                
SAVEBIL  DS    CL8                                                              
CLTACT   DS    CL1                                                              
*                                                                               
         DS    0F                                                               
SAVPARS  DS    CL24                                                             
         SPACE 2                                                                
GREC1    DS    0CL80                                                            
GRECCD   DC    CL3'H10'            1-3       RECORD CODE                        
GBATCHMN DS    CL1                 4         BATCH MTH   0-9/-/&/               
GBATCHSQ DS    CL3                 5-7       BATCH SEQ                          
GBATCHTY DS    CL2                 8-9       TYPE CODE                          
GBRANCH  DS    CL1                 10                                           
GCLTBILL DS    CL3                 11-13     CLT NUMBER CODE                    
         DS    CL2                 14-15     SPARE                              
GCLTALP  DS    CL3                 16-18     CLT APLHA                          
GINVNO   DS    CL6                 19-24     INVOICE NUMBER                     
GINVDATE DS    CL5                 25-29     MMDDY                              
GMONSERV DS    CL3                 30-32     MONTH OF SERVICE MMY               
GEST     DS    CL4                 33-36                                        
GESTTYPE DS    CL2                 37-38                                        
GESTYR   DS    CL2                 39-40                                        
GESTREV  DC    CL2'00'             41-42                                        
GMEDCD   DS    CL2                 43-44     MEDIA CODE                         
G1PRDS   DS    CL2                 45-46     NO. OF PRDS                        
GCLTPRD  DS    CL5                 47-51     CLT/PRD DIST CODE                  
GGROSS   DS    CL9                 52-60     GROSS                              
GDISC    DS    CL7                 61-67     CASH DISC                          
GCOMM    DS    CL8                 68-75     COMMISSION                         
         DS    CL5                 76-80     SPARE                              
*                                                                               
GREC2    DS    0CL80                                                            
         DS    CL42                1-42      SAME AS GREC1                      
GREC2ID  DC    C'**'                                                            
G2PRDS   DS    CL2                 45-46                                        
GPSF     DS    CL9                 47-55     PRODUCING SERVICE FEE              
GNET     DS    CL9                 56-64     NET PAYABLE                        
GINVTOT  DS    CL9                 65-73     NET RECEIVABLE                     
         DS    CL7                 74-80     SPARE                              
         SPACE 2                                                                
SORTRECD DSECT                                                                  
         DS    0D                                                               
SORTREC  DS    0CL40                                                            
SRTCLT   DS    CL3                                                              
SRTINV   DS    CL2        INV IN BINARY                                         
SRTPRD   DS    CL3                                                              
SRTEST   DS    CL2                                                              
SINVDATE DS    CL5               MMDDY                                          
SMOS     DS    CL2            YM                                                
*                                                                               
SGROSS   DS    PL5                                                              
SBILL    DS    PL5               G-CD                                           
SNET     DS    PL5              G-AC-CD                                         
SREC     DS    PL5              RECEIVABLE                                      
*                                                                               
         DS    CL3          SPARE                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPWORKD                                                        
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREP9302 04/22/03'                                      
         END                                                                    
