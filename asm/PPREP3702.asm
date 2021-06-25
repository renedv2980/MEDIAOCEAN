*          DATA SET PPREP3702  AT LEVEL 017 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044155.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PP3702A                                                                  
*INCLUDE PUBEDIT                                                                
*INCLUDE PUBFLOAT                                                               
*INCLUDE PPGETCG                                                                
*INCLUDE PRNTOFC                                                                
         PRINT NOGEN                                                            
         TITLE 'PP3702      VENDOR PAYERS LIST'                                 
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*  SMYE 02/13/09  RELINK FOR UPDATED PPGETCG                                    
*                                                                               
*  SMYE 05/04/06  INCREASE PRDTAB FROM CL23000 (1000 PROD'S) TO CL34500         
*                 (1000 PROD'S)                                                 
*                                                                               
*    BOBY 11/05    PRINT 2 CH MEDIA OFFICE CODE                                 
*                                                                               
*    SMYE 01/15/03 INCREASE PRDTAB FROM CL13800 (600 PROD'S) TO CL23000         
*                  (1000 PROD'S)                                                
*                                                                               
*    SMYE 04/30/01 INCREASE PRDTAB FROM CL11500 (500 PROD'S) TO CL13800         
*                  (600 PROD'S)                                                 
*                                                                               
*    SMYE 03/01    ADD ADDITIONAL CHARGES USING PPGETCG                         
*                                                                               
*    KWAN 03/16/00  FIX CLC BRANCHING BUG IN SV70ELM                            
*                   (BNL SHOULD BE BH)                                          
*                                                                               
*    KWAN 06/22/99  ALTER CD/NON-CD PUBS ONLY FEATURE TO IGNORE                 
*                   CD EFFECTIVE DATE IF IT 3 MONTHS OR MORE                    
*                   BEFORE THE REQUEST START DATE                               
*                                                                               
*    SMYE 01/12/96  MODIFIED MTHTAB HANDLING FOR YEARS AFTER 1999               
*                                                                               
*    SMYE 12/12/95 CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
*    BPLA 10/5/93  FIXES FOR QBPDATE = B                                        
*                                                                               
*    BPLA 7/12/93  DISPLAY PBYOBFD WITH COMMENTS                                
*                                                                               
*    LWEI 02/03/93 DISPLAY REF ELEMENT                                          
*                                                                               
*    BPLA 10/21/92 NEW PROFILE OPTIONS TO SUPPRESS "FREE" BUYS                  
*            PROGPROF+3  "Y" = SUPPRESS FREE BUYS ON UNAID REQS                 
*            PROGPROF+4  "Y" = SUPPRESS FREE BUYS ON PAID REQS                  
*            PROGPROF+5  "Y" = SUPPRESS FREE BUYS ON ALL ITEM REQS              
*                                                                               
*    BPLA 9/17/92  FREE BUY DATE CHECK CHANGED TO SEP01/92                      
*                                                                               
*    BPLA 7/21/92  DON'T REPORT FREE BUYS AS UNPAID IF INS DATE                 
*                  IS BEFORE AUG1/92                                            
*                                                                               
*    BPLA 6/9/92   CHANGES TO HANDLE "FREE" PAID INSERTIONS                     
*                  RPTEND MOVED TO IT'S OWN CSECT                               
*                                                                               
*    BPLA 7/16/91  ADD CODE FOR OFFICE LIST REQUESTS                            
*                                                                               
*    BPLA 7/1/91   ADD LOGIC FOR PROGPROF+2 'Y' MEANS SHOW INSERTION            
*                  TOTAL UNCLEARED/CLEARED WHEN USING QOPT1=U OR P              
*                  FOR THOSE AGENCIES THAT PAY BY INSERTION                     
*                                                                               
*              OPT1 P=PAID ITEMS ONLY                                           
*                   U=UPAID ITEMS ONLY                                          
*                   BLANK=ALL ITEMS                                             
*                                                                               
*              OPT2 Y=ONE PUB/PAGE                                              
*                                                                               
*              OPT3 C=CD PUBS ONLY                                              
*                   N=NON CD PUBS ONLY                                          
*                   BLANK=ALL PUBS                                              
*                                                                               
*              OPT4  =NO ADFILE INFO                                            
*                   A=AD NO. ONLY                                               
*                   B=AD NO. PLUS CAPTION                                       
*                   1=COPY NUMBER ONLY                                          
*                   2=COPY NUMBER PLUS CAPTION                                  
*                                                                               
*              OPT5 Y= FLAG BILLED/TRAFFICKED ITEMS                             
*                                                                               
*              OPT6 Y=SHOW PAYING ADDRESS/REP                                   
*                                                                               
*        REPORT PROFILE                                                         
*                                                                               
*        PROGPROF+0 I=INCH,L=LINE TOTALS (FOR NEWSPAPERS)                       
*****    PROGPROF SAVED IN RUNPROF AT FBUYREQ FOR THIS BYTE                     
*        PROGPROF+1 Y=SHOW LAST I/O                                             
*        PROGPROF+2 Y=SHOW INSERTION TOTAL UNCLEARED/CLEARED PORTIONS           
*                   WHEN USING QOPT1=U OR P                                     
*                                                                               
*                                                                               
PP3702   CSECT                                                                  
         NMOD1 0,PP3702,RR=R9                                                   
*                                                                               
         ST    R9,MRELO                                                         
         B     *+8                                                              
MRELO    DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,=V(VENWORK)                                                   
         A     R5,MRELO                                                         
         USING VENWORKD,R5,R6                                                   
         LA    R6,4095(R5)                                                      
         LA    R6,1(R6)                                                         
         MVC   RELO,MRELO           STORE RELO                                  
         LA    RF,ADCGWRK                                                       
         ST    RF,AADCGWRK                                                      
         DROP  R6                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   PROCESS                                                          
*                                                                               
         L     RF,=V(BLDMLST)                                                   
         A     RF,RELO                                                          
         ST    RF,VBLDMLST                                                      
         GOTO1 VBLDMLST                                                         
         B     EXT                                                              
*                                                                               
PROCESS  CLI   MODE,PROCBUY                                                     
         BNE   PP26A                                                            
         OC    KEY+21(3),KEY+21    IGNORE PASSIVE POINTERS                      
         BNZ   EXT                                                              
PROCB5   CLI   PUBSW,1       CHECKS IF PROCESSING THIS PUB                      
         BNE   EXT        NO - EXIT                                             
*                                                                               
         CLI   QBPDATE,C'B'                                                     
         BE    CKBDATE                                                          
         CLI   QBPDATE,C'P'                                                     
         BNE   PROCB8                                                           
         CLC   PBDPDATE,REQST                                                   
         BL    NEXTBUY                                                          
         CLC   PBDPDATE,REQEND                                                  
         BH    NEXTBUY                                                          
         B     PROCB8                                                           
*                                                                               
CKBDATE  CLC   BLBLDT,REQST                                                     
         BL    NEXTBUY                                                          
         CLC   BLBLDT,REQEND                                                    
         BH    NEXTBUY                                                          
*                                                                               
PROCB8   CLI   QBPDATE,C'B'        SEE IF DOING BILLABLE DATES                  
         BNE   PROCB10                                                          
         GOTO1 VMTHEND        MUST GO TO MTHEND FOR EACH INSERTION              
         B     CKQOPT1                                                          
*                                                                               
PROCB10  CLC   LASTYM,PBUYKDAT     CHK FOR CHG OF MTH                           
         BE    *+10                                                             
         GOTO1 VMTHEND                                                          
         MVC   LASTYM,PBUYKDAT                                                  
*                                                                               
CKQOPT1  DS    0H                                                               
*                                                                               
CKPAID   GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         CLI   QOPT1,C' '      SEE IF DOING PAID OR UNPAID REQ                  
         BNE   CKOPT1A                                                          
         CLI   PROGPROF+5,C'Y' SUPPRESSING FREE BUYS ON ALL ITEM REQ            
         BNE   CKOPT1A                                                          
         OC    GROSS(12),GROSS                                                  
         BZ    NEXTBUY                                                          
*                                                                               
CKOPT1A  MVI   PAIDSW,0       WILL BE SET TO X'01'                              
*                             IF DATED PAY ELEM IS FOUND                        
         LA    R3,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
CKQOPT1A BAS   RE,NEXTEL                                                        
         BNE   CKQOPT1D                                                         
         USING PPDUMD03,R3                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    CKQOPT1A                                                         
         CLI   ASOFDTE,0             SEE IF I HAVE AN AS OF DATE                
         BE    CKQOPT1C                                                         
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1A                                                         
*                                                                               
CKQOPT1C MVI   PAIDSW,X'01'       SET DATE PAY ELEM FOUND                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1E         GO PROCESS THIS ELEM                            
*                                                                               
CKQOPT1D DS    0H        I GET HERE IF NO VALID PAY ELEMS                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1X                                                         
*                                                                               
CKQOPT1E DS    0H                                                               
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1F                                                         
         LM    R7,R9,PGROSS                                                     
         A     R7,PPGROSS                                                       
         A     R8,PPAGYCOM                                                      
         A     R9,PPCSHDSC                                                      
         STM   R7,R9,PGROSS                                                     
         LM    R7,R9,PPGROSS                                                    
         SR    R7,R8                                                            
         SR    R7,R9                                                            
         L     R6,PAID                                                          
         AR    R6,R7                                                            
         ST    R6,PAID                                                          
CKQOPT1F BAS   RE,NEXTEL                                                        
         BE    CKQOPT1E                                                         
         B     CKQOPT1X                                                         
*                                                                               
*                                                                               
NEXTEL   DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
CKQOPT1X TM    PBUYCNTL,X'80'        SEE IF DELETED                             
         BZ    CKPD                                                             
         OC    PGROSS(12),PGROSS                                                
         BZ    NEXTBUY              NOT PAID SO IGNORE                          
         XC    GROSS(20),GROSS                                                  
CKPD     CLI   QOPT1,C'P'                                                       
         BNE   CKUNPD                                                           
         OC    PGROSS(12),PGROSS                                                
         BNZ   CKPD5                                                            
         CLI   PAIDSW,X'01'       SEE IF I FOUND A DATED PAY ELEM               
         BNE   NEXTBUY                                                          
*                                                                               
         OC    GROSS(12),GROSS    SEE IF FREE BUY                               
         BNZ   CKPD5                                                            
*                                                                               
         CLI   PROGPROF+4,C'Y'  SUPPRESSING "FREE" BUYS ON PAID REQ             
         BE    NEXTBUY                                                          
*                                                                               
*                                                                               
CKPD5    B     BUYOK                                                            
*                                                                               
CKUNPD   CLI   QOPT1,C'U'                                                       
         BNE   BUYOK        DOING ALL ITEMS                                     
         CLC   GROSS(12),PGROSS                                                 
         BNE   CKUNPD5                                                          
         OC    GROSS(12),GROSS     SEE IF "FREE" BUY                            
         BNE   NEXTBUY                                                          
         CLI   PAIDSW,X'01'        SEE IF I HAVE A DATED PAY ELEM               
         BE    NEXTBUY             YES THEN SKIP                                
*                                                                               
         CLC   PBUYKDAT,=X'5C0901'   SEE IF BEFORE SEP01/92                     
         BL    NEXTBUY               THEN DON'T REPORT AS UNPAID                
         CLI   PROGPROF+3,C'Y'  SEE IF SKIPPING "FREE" ON UNPAID REQ            
         BE    NEXTBUY                                                          
*                                                                               
CKUNPD5  B     BUYOK                                                            
*                                                                               
*                                                                               
BUYOK    MVC   MTHACT(5),=5C'Y'                                                 
         MVI   RPTACT,C'Y'                                                      
         CLC   SAVEPUB,PBUYKPUB                                                 
         BE    BUYOK5                                                           
         GOTO1 VPRNTPUB                                                         
*                                                                               
BUYOK5   CLC   SAVECLT,PBUYKCLT                                                 
         BE    BUYOK5A                                                          
         GOTO1 VCLTF                                                            
BUYOK5A  CLC   SAVEPRD,PBUYKPRD                                                 
         BE    BUYOK5C                                                          
         GOTO1 VPRDF                                                            
BUYOK5C  L     R2,ABUYOUTA                                                      
         USING PPBYOUTD,R2                                                      
         GOTO1 PPBYOUT,DMCB,(R2)                                                
         MVC   P+4(8),PBYOINS                                                   
         CLI   PBYOINS2,C' '                                                    
         BE    PPBY00                                                           
         MVI   PSECOND+3,C'+'                                                   
         MVC   PSECOND+4(8),PBYOINS2                                            
         B     PPBYX                                                            
PPBY00   DS    0H                                                               
*                                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    PPBYX                                                            
         MVC   PSECOND+5(11),PBYOISNM                                           
         B     PPBYX                                                            
PPBYX    DS    0H                                                               
         CLI   PBUYCNTL,X'80'           SEE IF DELETED                          
         BNE   PRTN1                                                            
         MVI   P+12,C'D'                                                        
         MVC   P+53(15),=C'CLEARED+DELETED'                                     
         B     PRTN2                                                            
PRTN1    MVC   P+53(14),PBYOGRS                                                 
PRTN2    DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMAG                                                           
         MVC   P+25(8),PBYOUR                                                   
         CLI   PBYOSPC,C' '                                                     
         BE    PRTN5                                                            
         MVC   P+34(10),PBYOSPC                                                 
         MVC   PSECOND+34(11),PBYOPRM                                           
         B     PRTN10                                                           
*                                                                               
PRTN5    MVC   P+34(11),PBYOPRM                                                 
*                                                                               
PRTN10   MVC   P+45(7),PBYOUNTS                                                 
         LA    R3,5                                                             
         LA    R4,P+45                                                          
PRTN20   CLI   P+51,C' '                                                        
         BNE   PRTEST                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),PBYOUNTS                                                 
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R3,PRTN20                                                        
         B     PRTEST                                                           
*                                                                               
PRTMAG   DS    0H                                                               
         MVC   P+28(20),PBYOSPC                                                 
         MVC   PSECOND+28(20),PBYOSPC2                                          
*                                                                               
PRTEST   CLI   QOPT5,C'Y'      SEE IF FLAGGING BILLED/TRAFFICED ITEMS           
         BNE   *+8                                                              
         BAS   RE,TSTBLTR                                                       
         SR    R0,R0                                                            
         IC    R0,PBUYKEST                                                      
         SLL   R0,8                                                             
         IC    R0,PBUYKEST+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(3),DUB+6(2)                                                 
*        GOTO1 DTCNV,DMCB,(1,PBDPDATE),(4,P+14)                                 
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(7,P+14)                                
         LM    R6,R9,GROSS                                                      
         STM   R6,R9,BUYGO                                                      
         SR    R6,R7                                                            
         ST    R6,BUYGLAC                                                       
         EDIT  BUYGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         EDIT  BUYCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         EDIT  BUYNP,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   PRTE20                                                           
         CLI   PBYOZZZ,C' '                                                     
         BE    PRTE20                                                           
         CLC   PSECOND+28(10),SPACES        SEE IF PSECOND IS USED              
         BNE   PRTE10              YES                                          
         MVC   PSECOND+25(50),PBYOZZZ                                           
         CLI   QMEDIA,C'N'                                                      
         BE    PRTE5                                                            
         MVC   PSECOND+28(50),PBYOZZZ                                           
         MVC   PSECOND+25(3),SPACES                                             
PRTE5    GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE10   GOTO1 VPRINTIT                                                         
         MVC   P+25(50),PBYOZZZ                                                 
         CLI   QMEDIA,C'N'                                                      
         BE    PRTE15                                                           
         MVC   P+28(50),PBYOZZZ                                                 
         MVC   P+25(3),SPACES                                                   
PRTE15   GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE20   GOTO1 VPRINTIT                                                         
PRTEX    OC    PGROSS(12),PGROSS                                                
         BZ    LTSTIO                   ADD TO MTH TOTALS                       
         CLI   QOPT1,C'P'                                                       
         BNE   PRTPPD                                                           
         CLC   GROSS(12),PGROSS                                                 
         BE    LTSTIO              TOTALLY PAID SO ADD TO MTH ACCUMS            
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
*                                                                               
*              SUBTRACT PGROSS FROM GROSS AND PUT IN PGROSS SO THAT             
*              PGROSS,PAGYCOM,PCSHDSC,PAID WILL REFLECT UNPAID AMTS             
*                                                                               
         L     R3,GROSS                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,PGROSS                                                        
         L     R3,AGYCOM                                                        
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         L     R3,CSHDSC                                                        
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,PCSHDSC                                                       
         L     R3,PYABLE                                                        
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,PAID                                                          
         B     PRTPPD1                                                          
PRTPPD   CLI   QOPT1,C'U'                                                       
         BNE   LTSTIO       IF DOING ALL ITEMS - NO ADJUSTMENT                  
         MVC   P+35(18),=C'PREVIOUSLY CLEARED'                                  
PRTPPD1  EDIT  PGROSS,(14,P+53),2,COMMAS=YES,MINUS=YES,FLOAT=$                  
         MVI   0(R1),C'('                                                       
         MVC   P+53(14),WORK+3                                                  
         MVI   P+66,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+66(2),=C'-)'                                                   
         LM    R3,R4,PGROSS                                                     
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$                    
         MVI   0(R1),C'('                                                       
         MVC   P+68(14),WORK+3                                                  
         MVI   P+81,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+81(2),=C'-)'                                                   
         EDIT  PCSHDSC,(12,P+82),2,COMMAS=YES,MINUS=YES,FLOAT=$                 
         MVI   0(R1),C'('                                                       
         MVC   P+82(12),WORK+5                                                  
         MVI   P+93,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+93(2),=C'-)'                                                   
         EDIT  PAID,(14,P+94),2,COMMAS=YES,MINUS=YES,FLOAT=$                    
         MVI   0(R1),C'('                                                       
         MVC   P+94(14),WORK+3                                                  
         MVI   P+107,C')'                                                       
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+107(2),=C'-)'                                                  
         GOTO1 VPRINTIT                                                         
         L     R3,BUYGO                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,BUYGO                                                         
         L     R3,BUYGLAC                                                       
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,BUYGLAC                                                       
         L     R3,BUYCD                                                         
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,BUYCD                                                         
         L     R3,BUYNP                                                         
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,BUYNP                                                         
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   LTSTIO                                                           
*                                                                               
*        NOW PRINT PORTION UNCLEARED/CLEARED                                    
*                                                                               
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
         CLI   QOPT1,C'U'                                                       
         BE    *+10                                                             
         MVC   P+35(2),SPACES     CHANGE TO "CLEARED"                           
*                                                                               
         EDIT  BUYGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         EDIT  BUYGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         EDIT  BUYCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R3,P+95                                             L01          
************************                                                        
****     GST CODE COPIED FROM PPREP2702                                         
****     NO-OP FOR NOW                                                          
****     CLI   PAGYNAT,C'C'                                        L01          
****     BNE   NTCAND3                                             L01          
****     EDIT  BUYGST,(12,(R3)),2,COMMAS=YES,MINUS=YES            L01X          
****     LA    R3,14(R3)                                           L01          
****                                                                            
****ND3  DS    0H                                                  L01          
*                                                                               
         EDIT  BUYNP,(14,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         GOTO1 VPRINTIT                                                         
*                                                                               
LTSTIO   DS    0H                  SHOW REF=                                    
         LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'83'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST2                                                            
         MVC   P+28(4),=C'REF='                                                 
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+32(0),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST2    LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'80'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST4                                                            
         MVC   P+28(5),=C'SREP='                                                
         MVC   P+34(4),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST4    CLI   PROGPROF+1,C'Y'                                                  
         BNE   LTST31                                                           
         XC    SV70ELM,SV70ELM                                                  
         LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
LTST5    BAS   RE,LNEXTEL                                                       
         BNE   LTST15                                                           
         OC    2(3,R6),2(R6)                                                    
         BZ    LTST5                                                            
         CLC   SV70ELM+2(3),2(R6)                                               
         BH    LTST5                                                            
         MVC   SV70ELM(11),0(R6)                                                
         B     LTST5                                                            
*                                                                               
*                                                                               
*              SAVE FIRST 11 BYTES OF 70 ELEM                                   
SV70ELM  DS    CL11                                                             
*                                                                               
LNEXTEL  DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R6),0                                                          
         BE    LNEXTELX                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                                                               
         B     LNEXTEL                                                          
LNEXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
LTST15   OC    SV70ELM,SV70ELM                                                  
         BZ    LTST31                                                           
*        GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(0,P+38)                                
         GOTO1 DATCON,DMCB,(3,SV70ELM+2),(0,P+38)                               
         MVC   P+28(9),=C'LAST I/O='                                            
         MVC   P+37(1),PBUYREC+2                                                
         MVI   P+38,C'-'                                                        
         MVI   P+44,C'-'                                                        
         MVC   HALF,SV70ELM+5                                                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+45(4),DUB                                                      
*                                                                               
         CLI   SV70ELM+10,C'N'                                                  
         BNE   LTST20                                                           
         MVC   P+50(3),=C'NEW'                                                  
         B     LTST30                                                           
*                                                                               
LTST20   CLI   SV70ELM+10,C'C'                                                  
         BNE   LTST25                                                           
         MVC   P+50(3),=C'CHA'                                                  
         B     LTST30                                                           
*                                                                               
LTST25   CLI   SV70ELM+10,C'D'                                                  
         BNE   LTST30                                                           
         MVC   P+50(3),=C'CAN'                                                  
*                                                                               
*LTST30   GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(3,P+54)                               
LTST30   GOTO1 DATCON,DMCB,(3,SV70ELM+2),(5,P+54)                               
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST31   LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'98'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   RLMTH                                                            
         MVC   P+28(5),=C'SITE='                                                
         MVC   P+34(20),2(R6)                                                   
         GOTO1 VPRINTIT                                                         
*                                                                               
RLMTH    DS    0H          PRINT UP TO 5 COMMENTS                               
         CLC   PBYOBFD(L'PBYOBFD),SPACES                                        
         BE    RL0                                                              
         MVC   P+28(L'PBYOBFD),PBYOBFD                                          
         GOTO1 VPRINTIT                                                         
RL0      DS    0H                                                               
         LA    R3,PBYOCOMS                                                      
         LA    R4,5                                                             
RL1      CLC   0(47,R3),SPACES                                                  
         BE    RL2X                                                             
         MVC   P+28(47),0(R3)                                                   
         GOTO1 VPRINTIT                                                         
         LA    R3,47(R3)                                                        
         BCT   R4,RL1                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
RL2X     DS    0H                                                               
         LA    R2,PPFILED+4095                                                  
         LA    R2,1(R2)                                                         
         USING PPFILED+4096,R2                                                  
         OC    PBDJOB,PBDJOB                                                    
         BZ    RLM2                                                             
         CLI   QOPT4,C' '                                                       
         BE    RLM2                                                             
         CLI   QOPT4,C'A'         AD NO ONLY                                    
         BE    RLM1A          NO NEED TO READ JOB REC                           
*                                                                               
*                                                                               
RLM1     DS    0H                                                               
         TM    QOPT4,X'30'                                                      
         BZ    RLM1A                                                            
         MVC   P+28(6),=C'COPY ='                                               
         MVC   P+28+7(17),PJOBCPY                                               
         B     RLM1B                                                            
*                                                                               
RLM1A    DS    0H                                                               
         MVC   P+28(8),=C'AD NO. ='                                             
         MVC   P+28+9(6),PBDJOB                                                 
*                                                                               
RLM1B    GOTO1 VPRINTIT                                                         
         TM    QOPT4,X'02'                                                      
         BZ    RLM2                                                             
         MVC   P+28(25),PJOBCAP1                                                
         MVC   PSECOND+28(25),PJOBCAP2                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLM2     DS    0H                                                               
         MVC   STKEY,KEY           SAVE CURRENT KEY                             
*                                  GET ADDITIONAL CHARGE DATA                   
         GOTO1 VPPGETCG,DMCB,(C'T',PBUYREC),DATAMGR,GETINS,PBUYKPRD             
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),X'00'         ANY ADD'L. CHARGES FOUND ?                   
         BE    GCLUPX              NO                                           
*                                                                               
         ZIC   R3,0(R1)         FOR LUP COUNTER - NO OF LINES IN BLOCK          
*                                                                               
         ICM   RE,15,0(R1)         RE POINTING TO DATA BLOCK                    
         BNZ   *+6                                                              
         DC    H'0'                ADDRESS IS NULLS                             
*                                                                               
         L     RF,AADCGWRK         ADDRESS OF AREA FOR ADD'L. CHGS.             
         LA    R1,ADCDLNTH         LENGTH OF BLOCK                              
         MOVE  ((RF),(R1)),(RE)    MOVE BLOCK TO ADCGWRK AREA IN W/S            
*                                                                               
         L     R4,AADCGWRK         WORK AREA FOR ADD'L. CHGS.                   
         USING ADDCHGD,R4                                                       
*                                                                               
         MVC   P+21(24),=C'** ADDITIONAL CHARGES **'                            
         GOTO1 VPRINTIT                                                         
*                                                                               
GCLUP    MVC   P+21(ADCLNLEN),ADCCOD1                                           
         GOTO1 VPRINTIT                                                         
         LA    R4,ADCLNLEN(R4)     BUMP TO NEXT BLOCK "LINE"                    
         BCT   R3,GCLUP                                                         
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   KEY,STKEY           RESTORE CURRENT KEY                          
         LA    R4,DMRDHI                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    GCLUPP                                                           
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    *+6                                                              
         DC    H'0'                                                             
GCLUPP   CLC   KEY(25),STKEY                                                    
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
GCLUPX   DS    0H                                                               
*                                                                               
         MVI   RPTACT,C'Y'                                                      
*                                                                               
         CLI   PBDSPACE,C'*'      SEE IF REAL INSERTION                         
         BE    RLMTH1              NO - BYPASS INS TOTALS                       
         AP    MTHINS,=P'1'                                                     
RLMTH1   CLI   QMEDIA,C'N'         IF MAG                                       
         BNE   RLMTH2              BYPASS LINES                                 
RLM2A    CLI   PROGPROF,C'I'                                                    
         BNE   RLM2C                                                            
         CLI   PBDUIND,X'89'                                                    
         BE    RLMTH1B                                                          
         CLI   PBDUIND,C'I'                                                     
         BE    RLM2B                                                            
         L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'1000'                                                     
         DP    DUB,=P'14'                                                       
         SRP   DUB(6),63,5                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2B    L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'100'                                                      
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2C    CLI   PBDUIND,C'I'        CONVERT INCHES TO LINES FOR ACCUMS           
         BE    RLMTH1A                                                          
         CLI   PBDUIND,X'89'       CONVERT INCHES TO LINES FOR ACCUMS           
         BNE   RLMTH1B             LOWER CASE I INCHES TO 2 DECIMALS            
RLMTH1A  SR    R6,R6                                                            
         L     R7,UNITS                                                         
         M     R6,=F'14'                                                        
         ST    R7,UNITS                                                         
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   RLMTH1B             NO                                           
         CVD   R7,DUB                                                           
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R7,DUB                                                           
         ST    R7,UNITS                                                         
*                                                                               
RLMTH1B  EQU   *                                                                
         L     R3,UNITS                                                         
         CVD   R3,DUB                                                           
         AP    MTHLINES,DUB                                                     
RLMTH2   L     R3,BUYGO                                                         
         CVD   R3,DUB                                                           
         AP    MTHGO,DUB                                                        
         L     R3,BUYGLAC                                                       
         CVD   R3,DUB                                                           
         AP    MTHGLAC,DUB                                                      
         L     R3,BUYCD                                                         
         CVD   R3,DUB                                                           
         AP    MTHCD,DUB                                                        
         L     R3,BUYNP                                                         
         CVD   R3,DUB                                                           
         AP    MTHNP,DUB                                                        
         MVC   SAVECLT,PBUYKCLT                                                 
         MVC   SAVEYMD,BLBLDT                                                   
         CLI   QBPDATE,C'B'                                                     
         BE    NEXTBUY                                                          
         MVC   SAVEYMD,PBUYKDAT                                                 
         B     NEXTBUY                                                          
NEXTBUY  B     EXT                                                              
         EJECT                                                                  
PP26A    CLI   MODE,LBUYCLI                                                     
         BNE   PP26B                                                            
         GOTO1 VCLTEND                                                          
         B     EXT                                                              
*                                                                               
PP26B    CLI   MODE,LBUYPUB                                                     
         BNE   PP26C                                                            
         GOTO1 VPUBEND                                                          
         B     EXT                                                              
*                                                                               
PP26C    CLI   MODE,FBUYPUB                                                     
         BNE   PP26E                                                            
         GOTO1 VPUBFRT                                                          
         B     EXT                                                              
*                                                                               
PP26E    CLI   MODE,LBUYPRO                                                     
         BNE   PP26F                                                            
         GOTO1 VPRDEND                                                          
         B     EXT                                                              
*                                                                               
PP26F    CLI   MODE,LBUYREP                                                     
         BNE   PP26G                                                            
         GOTO1 VREPEND                                                          
         B     EXT                                                              
*                                                                               
PP26G    CLI   MODE,LBUYREQ                                                     
         BNE   EXT                                                              
         GOTO1 VRPTEND                                                          
         B     EXT                                                              
*                                                                               
PPGEXT   MVC   KEY,PPGKEY                                                       
         GOTO1 HIGH                                                             
         B     EXT                                                              
         EJECT                                                                  
*                                                                               
*              ROUTINE TO FLAG BILLED/TRAFFICED ITEMS                           
TSTBLTR  NTR                                                                    
         LA    R3,PBDELEM                                                       
TSTB1    CLI   0(R3),X'26'                                                      
         BNE   TSTB4                                                            
         OC    5(3,R3),5(R3)       CK FOR ANY DATE                              
         BZ    TSTB4                                                            
         CLI   ASOFDTE,0                                                        
         BNE   TSTB2                                                            
         MVI   P+1,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB2    CLC   5(3,R3),ASOFDTE     CHK VS. AS OF DATE                           
         BH    NEXTBEL                                                          
         MVI   P+1,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB4    CLI   0(R3),X'70'                                                      
         BNE   NEXTBEL                                                          
         OC    2(3,R3),2(R3)                                                    
         BZ    NEXTBEL                                                          
         CLI   ASOFDTE,0                                                        
         BNE   TSTB6                                                            
         MVI   P+2,C'T'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB6    CLC   2(3,R3),ASOFDTE                                                  
         BH    NEXTBEL                                                          
         MVI   P+2,C'T'                                                         
*                                                                               
NEXTBEL  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0         END OF REC                                       
         BNE   TSTB1                                                            
*                                                                               
TSTBX    XIT                                                                    
EXT      XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
RPTEND   CSECT                     PRINT REPORT TOTALS                          
         NMOD1 0,RPTEND                                                         
         L     RC,PPFILEC                                                       
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         L     R9,VRPTCSCT                                                      
         USING RPTDSCT,R9                                                       
         CLI   RPTACT,C'Y'                                                      
         BNE   RPTEXT                                                           
RPTE     MVI   FORCEHED,C'Y'                                                    
         MVI   P,C' '             OLD PUB COULD STILL BE HERE                   
         MVC   P+1(70),P             IF NO ACTIVITY                             
         MVC   PSECOND+1(17),SPACES                                             
         MVI   PUBPSW,0     SO PRINTIT WON'T PRINT CONTINUED MESSAGE            
         MVC   P+1(20),=C' ** REPORT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
RPTE1    LA    R2,RPTINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,6                                                             
RPTE2    CP    0(8,R2),=P'0'                                                    
         BNE   RACTV                                                            
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,RPTE2                                                         
RPTE3    LA    R4,8(R4)                                                         
         BCT   R8,RPTE1                                                         
         B     RPTE5                                                            
*                                                                               
RACTV    LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         CLC   QSORT,=C'08'                 REP SORT                            
         BE    RACTV5                       NO PUB COUNTS                       
         CLC   QSORT,=C'09'                 ADDR SORT                           
         BE    RACTV5                       NO PUB COUNTS                       
*                                                                               
         LA    R1,RPTPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         EDIT  (R3),(4,P+12),0                                                  
         EDIT  (P8,(R1)),(4,P+12),0                                             
         MVC   P+17(4),=C'PUBS'                                                 
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
RACTV5   LA    R1,RPTINS                                                        
         LA    R1,0(R4,R1)                                                      
         CP    0(8,R1),=P'0'                                                    
         BE    NORINS                                                           
         EDIT  (P8,(R1)),(5,P+23),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NORINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   RACTIV1                                                          
         CP    0(8,R1),=P'0'                                                    
         BE    RACTIV1                                                          
         CLI   RUNPROF,C'I'                                                     
         BNE   RACTIVA                                                          
         EDIT  (P8,(R1)),(9,P+41),2                                             
         MVI   P+50,C'I'                                                        
         B     RACTIV1                                                          
RACTIVA  EDIT  (P8,(R1)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
RACTIV1  LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+94),2,COMMAS=YES,MINUS=YES                       
         GOTO1 VPRINTIT                                                         
         B     RPTE3                                                            
*                                                                               
RPTE5    MVC   P+5(5),=C'TOTALS'                                                
         CLC   QSORT,=C'08'           REP SORT                                  
         BE    RPTE5B                 NO PUB COUNTS                             
         CLC   QSORT,=C'09'           ADDR SORT                                 
         BE    RPTE5B                 NO PUB COUNTS                             
*                                                                               
         L     R2,RTOTPUBS                                                      
         EDIT  (R2),(4,P+12),0                                                  
         MVC   P+17(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
RPTE5B   ZAP   WKDUB,RPTINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTINS+8                                                      
RPTE6    AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,RPTE6                                                         
         CP    WKDUB,=P'0'                                                      
         BE    NORTINS                                                          
         EDIT  WKDUB,(6,P+22),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NORTINS  CLI   QMEDIA,C'N'                                                      
         BNE   RPTE8                                                            
         ZAP   WKDUB,RPTLINES                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTLINES+8                                                    
RPTE7    AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,RPTE7                                                         
         CP    WKDUB,=P'0'                                                      
         BE    RPTE8                                                            
         CLI   RUNPROF,C'I'                                                     
         BNE   RPTE7A                                                           
         EDIT  WKDUB,(10,P+40),2                                                
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     RPTE8                                                            
RPTE7A   EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
RPTE8    ZAP   WKDUB,RPTGO                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTGO+8                                                       
RPTE8C   AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,RPTE8C                                                        
         EDIT  WKDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,RPTGLAC                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTGLAC+8                                                     
RPTE8D   AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,RPTE8D                                                        
         EDIT  WKDUB,(15,P+67),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   DOUBLE,RPTCD                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTCD+8                                                       
RPTE8E   AP    DOUBLE,0(8,R4)                                                   
         LA    R4,8(R4)                                                         
         BCT   R3,RPTE8E                                                        
         CP    DOUBLE,=P'99999999'     IF OVER 999,999.99 NO COMMAS             
         BH    RPTE8E5                                                          
         EDIT  (P8,DOUBLE),(12,P+82),2,COMMAS=YES,MINUS=YES                     
         B     RPTE8E6                                                          
RPTE8E5  EDIT  (P8,DOUBLE),(12,P+82),2,MINUS=YES                                
RPTE8E6  CP    DOUBLE,=P'0'                                                     
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   DOUBLE,RPTNP                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTNP+8                                                       
RPTE8F   AP    DOUBLE,0(8,R4)                                                   
         LA    R4,8(R4)                                                         
         BCT   R3,RPTE8F                                                        
         CP    DOUBLE,=P'9999999999'   IF OVER 99,999,999.99 NO COMMAS          
         BH    RPTE8F5                                                          
         EDIT  (P8,DOUBLE),(14,P+94),2,COMMAS=YES,MINUS=YES                     
         B     RPTE8F6                                                          
RPTE8F5  EDIT  (P8,DOUBLE),(14,P+94),2,MINUS=YES                                
RPTE8F6  CP    DOUBLE,=P'0'                                                     
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         GOTO1 VPRINTIT                                                         
RPTEXT   XMOD1 1                                                                
*                                                                               
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
PUBFRT   CSECT                                                                  
         NMOD1 0,PUBF                                                           
         L     RC,PPFILEC                                                       
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         MVI   PUBSW,0                                                          
CKOPT34  CLI   QOPT3,C'C'          SEE IF DOING CD PUBS ONLY                    
         BE    GETCD                                                            
         CLI   QOPT3,C'N'          SEE IF DOING NO CD PUBS ONLY                 
         BNE   SETFLT   NO - GO SET FLOAT POSITION                              
*                                                                               
GETCD    EQU   *                   RESET BASE REGISTER FOR PUBREC               
         LA    R3,PUBREC+33                                                     
         USING PUBGENEL,R3                                                      
         CLI   0(R3),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'20'                                                      
         BE    HAVEL       HAVE PROD ELEMENT                                    
         CLI   QOPT3,C'C'                                                       
         BE    PFEXT     WILL BYPASS BUYS FOR THIS PUB SINCE PUBSW 0            
         B     SETFLT                                                           
*                                                                               
HAVEL    DS    0H                                                               
*                                                                               
         CLC   PUBCDDAT,CDPDATE    SEE IF I CAN IGNORE                          
         BNL   *+10                                                             
         XC    PUBCDDAT,PUBCDDAT   CLEAR IF LOW                                 
*                                                                               
         OC    PUBCDDAT,PUBCDDAT   CHK FOR CD EFF DATE                          
         BNZ   HAVEL5              IF PRESENT IS OR WAS A CD PUB                
         CP    PUBCD,=P'0'                                                      
         BE    NCHDIS              NO CASH DISCOUNT                             
HAVEL5   CLI   QOPT3,C'C'                                                       
         BE    SETFLT                                                           
         B     PFEXT     WILL BYPASS BUYS FOR THIS PUB SINCE PUBSW              
*                                                                               
NCHDIS   CLI   QOPT3,C'C'                                                       
         BE    PFEXT     WILL BYPASS BUYS FOR THIS PUB SINCE PUBSW 0            
         B     SETFLT                                                           
*                                                                               
*                                                                               
*                                                                               
SETFLT   MVI   PUBSW,1                                                          
         CLI   QOPT2,C'Y'          CHK ONE PUB PER PAGE                         
         BNE   *+8                 NO                                           
         MVI   FORCEHED,C'Y'                                                    
         DROP  R7                                                               
         DROP  R3                                                               
*                                                                               
PFEXT    XIT1                                                                   
         LTORG                                                                  
*                                                                               
MTHEND   CSECT                                                                  
         NMOD1 0,MTHEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         CLI   MTHACT,C'Y'                                                      
         BNE   MTHENDX             NO ACTIVITY                                  
         CLI   QBPDATE,C'B'            IF YES THEN DON'T PRINT MTH              
         BE    MTHEND2C                BREAKS                                   
*                                                                               
*MTHENDF  GOTO1 DTCNV,DMCB,(1,SAVEYMD),(5,P+5)                                  
MTHENDF  GOTO1 DATCON,DMCB,(3,SAVEYMD),(9,P+5)                                  
         MVC   P+11(6),=C'-TOTAL'                                               
MTHENDH  DS    0H                                                               
         CP    MTHINS,=P'0'                                                     
         BNH   MTHEND1                                                          
         EDIT  MTHINS,(3,P+28),0                                                
         MVC   P+32(10),=C'INSERTIONS'                                          
         CP    MTHINS,=P'1'                                                     
         BNE   MTHEND1                                                          
         MVI   P+41,C' '                                                        
MTHEND1  CLI   QMEDIA,C'N'                                                      
         BNE   MTHEND2             OMIT LINES FOR MAGS + SUPS                   
         CP    MTHLINES,=P'0'                                                   
         BZ    MTHEND2             NO LINES                                     
         CLI   RUNPROF,C'I'                                                     
         BNE   MTHEND1A                                                         
         EDIT  MTHLINES,(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     MTHEND2                                                          
MTHEND1A EDIT  MTHLINES,(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
MTHEND2  DS    0H                                                               
         EDIT  MTHGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    MTHGO,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  MTHGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         CP    MTHGLAC,=P'0'                                                    
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  MTHCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    MTHCD,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         EDIT  MTHNP,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CP    MTHNP,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES      RESTORE MAXLINES                                
         CLC   LINE,MAXLINES                                                    
         BNL   MTHEND2B         IF HIGH OR EQUAL DON'T SKIP                     
         GOTO1 VPRINTIT         SKIP A LINE                                     
*                                                                               
MTHEND2B DS    0H                                                               
         CLI   PRDSW,1           SEE IF DOING PRODUCTS SEPERATELY               
         BE    ROLLBUY                                                          
*    ROLL TO PRD TOTALS                                                         
*                                                                               
MTHEND2C L     R3,PRDMTHS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PRDMTHS                                                       
         LA    R3,PRDINS                                                        
         LA    R4,MTHINS                                                        
         LA    R6,6                                                             
MTHEND3  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,MTHEND3                                                       
*                                                                               
*    ROLL TO CLT TOTALS                                                         
*                                                                               
ROLLBUY  DS    0H                                                               
*                                                                               
MTHEND3C L     R3,CLTMTHS                                                       
         AH    R3,=H'1'                                                         
         ST    R3,CLTMTHS                                                       
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(0,WORK)                                 
         LA    R3,MTHTAB                                                        
         LA    R4,0                                                             
COMPDAT  CLC   WORK(4),0(R3)                                                    
         BE    MTHEND4                                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         CLC   0(4,R3),=4X'00'                                                  
         BNE   COMPDAT                                                          
         DC    H'0'                MTH NOT FOUND IN LIST                        
*                                                                               
MTHEND4  L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         LA    R3,6                                                             
         LA    R6,CLTINS                                                        
         LA    R7,MTHINS                                                        
         LA    R6,0(R4,R6)                                                      
MTHEND5  AP    0(8,R6),0(8,R7)                                                  
         LA    R6,ACCNUM*8(R6)                                                  
         LA    R7,8(R7)                                                         
         BCT   R3,MTHEND5                                                       
         LA    R3,6                                                             
         LA    R6,MTHTOTS               CLEAR MTH ACCUMS                        
MTHEND8  ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R3,MTHEND8                                                       
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
MTHENDX  XIT1                                                                   
*                                                                               
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
PRDEND   CSECT                                                                  
         NMOD1 0,PRDEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDENDX                                                          
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,1              SEE  IF DOING PRDS SEPERATELY               
         BE    PRDEND10                                                         
*                                                                               
         L     R3,PUBPRDS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PUBPRDS                                                       
         CLI   QBPDATE,C'B'        SEE IF DOING BILLABLE DATES                  
         BE    PRDEND0                                                          
         CLC   PRDMTHS,=F'1'                                                    
         BNH   CLRPRD                                                           
*                                                                               
PRDEND0  MVC   P+2(20),=C'** PRODUCT TOTALS **'                                 
         CP    PRDINS,=P'0'                                                     
         BE    NOINS               NO INSERTIONS                                
         EDIT  PRDINS,(3,P+28),0                                                
         MVC   P+32(10),=C'INSERTIONS'                                          
         CP    PRDINS,=P'1'                                                     
         BNE   *+8                                                              
         MVI   P+41,C' '                                                        
NOINS    CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND1                  SKIP LINES                              
         CP    PRDLINES,=P'0'                                                   
         BE    PRDEND1                                                          
         CLI   RUNPROF,C'I'                                                     
         BNE   PRDENDA                                                          
         EDIT  PRDLINES,(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND1                                                          
PRDENDA  EDIT  PRDLINES,(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
PRDEND1  DS    0H                                                               
         EDIT  PRDGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    PRDGO,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  PRDGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         CP    PRDGLAC,=P'0'                                                    
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  PRDCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    PRDCD,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         EDIT  PRDNP,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CP    PRDNP,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   PRDEND8                                                          
         GOTO1 VPRINTIT                                                         
PRDEND8  B     CLRPRD                                                           
*                                                                               
PRDEND10 DS    0H                  PRODUCTS SEPARATELY                          
         GOTO1 VPUBEND                                                          
         MVI   PUBPSW,0            SO I WON'T PRINT PUB CONTINUED MSG           
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(20),=C'** PRODUCT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
PRDEND15 LA    R2,PROINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,6                                                             
PRDEND17 CP    0(8,R2),=P'0'                                                    
         BNE   PACTV                                                            
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,PRDEND17                                                      
PRDEND18 LA    R4,8(R4)                                                         
         BCT   R8,PRDEND15                                                      
         B     PRDEND20                                                         
*                                                                               
PACTV    LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R1,PROINS                                                        
         LA    R1,0(R4,R1)                                                      
         CP    0(8,R1),=P'0'                                                    
         BE    NOPINS                                                           
         EDIT  (P8,(R1)),(5,P+24),0                                             
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PACTV5                                                           
         CP    0(8,R1),=P'0'                                                    
         BE    PACTV5                                                           
         CLI   RUNPROF,C'I'                                                     
         BNE   PACTV3                                                           
         EDIT  (P8,(R1)),(9,P+42),2                                             
         MVI   P+51,C'I'                                                        
         B     PACTV5                                                           
PACTV3   EDIT  (P8,(R1)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PACTV5   LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+94),2,COMMAS=YES,MINUS=YES                       
         GOTO1 VPRINTIT                                                         
         B     PRDEND18                                                         
*                                                                               
*                                                                               
PRDEND20 MVC   P+5(5),=C'TOTAL'                                                 
         ZAP   WKDUB,PROINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,PROINS+8                                                      
PRDEND22 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,PRDEND22                                                      
         CP    WKDUB,=P'0'                                                      
         BE    NOPTINS                                                          
         EDIT  WKDUB,(5,P+24),0                                                 
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPTINS  DS    0H                                                               
         LA    R6,PROLINES                                                      
         LA    R7,5                                                             
         LA    R8,TOTALS                                                        
PRDEND23 ZAP   DUB,0(8,R6)                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,8(R6)                                                         
PRDEND24 ZAP   DOUBLE,0(8,R4)                                                   
         AP    DUB,DOUBLE                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PRDEND24                                                      
         MVC   0(8,R8),DUB         SAVE RESULT IN TOTALS                        
         LA    R8,8(R8)                                                         
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PRDEND23                                                      
*                                                                               
         LA    R2,TOTALS                                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND26                                                         
* CHANGE 4/22                                                                   
         CP    0(8,R2),=P'0'                                                    
         BE    PRDEND26                                                         
*                                                                               
         CLI   RUNPROF,C'I'                                                     
         BNE   PRDEND25                                                         
         EDIT  (P8,0(R2)),(10,P+41),2                                           
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND26                                                         
PRDEND25 EDIT  (P8,0(R2)),(7,P+43),0                                            
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
*                                                                               
PRDEND26 LA    R2,8(R2)                                                         
         EDIT  (P8,00(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                     
         CP    00(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  (P8,08(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                     
         CP    08(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  (P8,16(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                     
         CP    16(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         EDIT  (P8,24(R2)),(14,P+94),2,COMMAS=YES,MINUS=YES                     
         CP    24(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         GOTO1 VPRINTIT                                                         
         LA    R3,PROTOTS                                                       
         LA    R4,ACCNUM*6                                                      
PRDEND30 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,PRDEND30                                                      
         MVI   PUBACT,0                                                         
*                                                                               
CLRPRD   DS    0H                                                               
         LA    R3,6                                                             
         LA    R4,PRDTOTS                                                       
CLRPRD5  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLRPRD5                                                       
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PRDACT,0                                                         
PRDENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRDF     CSECT                                                                  
         NMOD1 0,PRDF                                                           
         L     RC,PPFILEC                                                       
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    PRDFX               SKIP PRD PRINT                               
*                                                                               
PRDF00   DS    0H                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   PRDF3               YES- MUST READ PRODUCT                       
PRDF0    L     R6,APRDTAB                                                       
PRDF1    CLC   0(3,R6),=X'FFFFFF'                                               
         BNE   *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         CLC   0(3,R6),PBUYKPRD                                                 
         BE    PRDF4                                                            
         LA    R6,23(R6)                                                        
         B     PRDF1                                                            
*                                                                               
*                                                                               
PRDF3    DS    0H                                                               
         MVC   PPGKEY,KEY          SAVE PPG KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,PDIRRD                                                        
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         TM    KEY+25,X'80'                                                     
         BZ    *+6                                                              
         DC    H'0'                FATAL ERROR PRD IS DELETED                   
         LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,PFILERD                                                       
PRDF3C   DS    0H                                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,PDIRRD                                                        
         B     PRDF5                                                            
*                                                                               
PRDF4    MVC   PPRDNAME(20),3(R6)                                               
         MVC   PPRDKPRD(3),PBUYKPRD                                             
*                                                                               
PRDF5    DS    0H                                                               
         MVC   P+1(7),=C'PRODUCT'                                               
         MVC   P+9(3),PBUYKPRD                                                  
         MVC   P+14(20),PPRDNAME                                                
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
PRDF8    MVC   SAVEPRD,PBUYKPRD                                                 
*                                                                               
PRDFX    XIT1                                                                   
*                                                                               
PDIRRD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
PDIRX    XIT                                                                    
*                                                                               
PFILERD  NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    PFILX                                                            
         DC    H'0'                                                             
PFILX    XIT                                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PRNTPUB  CSECT                                                                  
         NMOD1 0,PRNTPUB                                                        
         L     RC,PPFILEC                                                       
         CLI   QOPT2,C'Y'     YES= SKIP ON NEW PUB                              
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'6'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         MVC   P+1(17),SPACES                                                   
         MVI   PUBPSW,0                                                         
         MVC   SAVPLIN1,SPACES                                                  
         MVC   SAVPLIN2,SPACES                                                  
         IC    R3,PAGYPROF+12                                                   
         GOTO1 VPUBEDIT,DMCB,((R3),PBUYKPUB),(0,SAVPLIN1+1)                     
         CLI   FORCEHED,C'Y'                                                    
         BE    SETR3               DONT SKIP A LINE                             
         GOTO1 VPRINTIT                                                         
SETR3    LA    R3,SAVPLIN1+1                                                    
         LA    R4,SAVPLIN2+1                                                    
         SR    R9,R9                                                            
SETR3A   CLI   0(R3),C' '                                                       
         BE    FLOAT                                                            
         MVI   0(R4),C'-'                                                       
         LA    R9,1(R9)                                                         
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     SETR3A                                                           
*                                                                               
FLOAT    LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R9,1(R9)            SAVE DISPLACEMENT                            
         GOTO1 VPUBFLT,DMCB,PUBREC,(R3)                                         
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         MVC   0(11,R4),=C'(CONTINUED)'                                         
*                                                                               
         LA    R3,P+1                                                           
         AR    R3,R9                                                            
         LA    R4,PSECOND+1                                                     
         AR    R4,R9                                                            
*                                                                               
         MVC   SAVEPUB,PBUYKPUB                                                 
         CLI   QOPT6,C'Y'          SEE IF PRINTING PAYING ADDR                  
         BE    PRNTP1                                                           
         CLC   QSORT,=C'08'         OR REP SORT                                 
         BE    PRNTP1                                                           
         CLC   QSORT,=C'09'         OR PAY ADDR NAME                            
         BE    PRNTP1                                                           
         GOTO1 VPRINTIT                                                         
         B     PRNTPX                                                           
*                                                                               
PRNTP1   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNTP2                                                           
         IC    R0,SAVELINE                                                      
         CLC   QSORT,=C'08'        SEE IF SORTING BY REP                        
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDRS                         
         BNE   PRNTP1C                                                          
         AH    R0,=H'4'                                                         
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    *+8                                                              
         AH    R0,=H'1'                                                         
PRNTP1C  STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
PRNTP2   DS    0H                                                               
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    PRNTP2C             YES                                          
         MVC   0(20,R4),=C'** PAY PUB DIRECT **'                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7                                                           
         MVC   0(30,R3),PUBLINE1   P                                            
         MVC   0(30,R4),PUBLINE2   PSECOND                                      
         GOTO1 VPRINTIT                                                         
         LA    R6,PUBREC+33                                                     
PRNTP2A  CLI   0(R6),X'11'                                                      
         BE    PRNTP2B                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    PRNTP7              NO ATTN OR TELE                              
         B     PRNTP2A                                                          
*                                                                               
PRNTP2B  DS    0H                                                               
         USING PUBSADEL,R6                                                      
         LR    R4,R3                                                            
         CLI   PUBATTN,C' '                                                     
         BNH   PRNTP2B2                                                         
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(24,R4),PUBATTN                                                 
         LA    R4,31(R4)                                                        
*                                                                               
PRNTP2B2 CLI   PUBTEL,C' '                                                      
         BNH   PRNTP6              NO TELE                                      
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PUBTEL                                                  
         B     PRNTP6                                                           
*                                                                               
         DROP  R6                                                               
         GOTO1 VPRINTIT                                                         
         B     PRNTP7                                                           
*                                                                               
PRNTP2C  DS    0H                                                               
         CLC   QSORT,=C'08'        SEE IF DOING REP SORT                        
         BE    PRNTP2C5                                                         
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BE    PRNTP2C5                                                         
         B     PRNTP2F                                                          
*                                                                               
PRNTP2C5 CLI   PREPKEY+3,X'11'     SEE IF I FOUND A REP                         
         BNE   PRNTP2F             NO WAS PAY ADDR                              
         MVC   0(4,R4),=C'REP='                                                 
         MVC   5(4,R4),PREPKREP                                                 
         CLI   PREPKREP+4,0                                                     
         BE    PRNTP2D                                                          
         MVI   9(R4),C'.'                                                       
         MVC   10(1,R4),PREPKREP+4    SUFFIX                                    
PRNTP2D  GOTO1 VPRINTIT                                                         
         LR    R4,R3                                                            
*                                                                               
PRNTP2F  MVC   0(30,R4),PREPNAME                                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7              NO THEN DONE                                 
         CLI   PREPLIN1,C' '                                                    
         BNH   PRNTP3                                                           
         MVC   0(30,R3),PREPLIN1                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP3   CLI   PREPLIN2,C' '                                                    
         BNH   PRNTP4                                                           
         MVC   0(30,R3),PREPLIN2                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP4   LR    R4,R3                                                            
         CLI   PREPATTN,C' '                                                    
         BNH   PRNTP5                                                           
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(20,R4),PREPATTN                                                
         LA    R4,27(R4)                                                        
*                                                                               
PRNTP5   CLI   PREPTEL,C' '                                                     
         BNH   PRNTP6              NO TELEPHONE                                 
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PREPTEL                                                 
*                                                                               
PRNTP6   CLC   P,SPACES                                                         
         BE    PRNTP7                                                           
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP7   DS    0H                                                               
         GOTO1 VPRINTIT                                                         
PRNTPX   DS    0H                                                               
         MVI   PUBPSW,1            SET PUB PRINTED                              
         XIT1                                                                   
*                                                                               
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BLDMLST  CSECT                                                                  
         NMOD1 0,BLDMLST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     RF,=V(BUYOUTA)                                                   
         A     RF,RELO                                                          
         ST    RF,ABUYOUTA                                                      
         L     RF,=V(CLTF)                                                      
         A     RF,RELO                                                          
         ST    RF,VCLTF                                                         
         L     RF,=V(PRDTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,APRDTAB                                                       
         L     RF,=V(PUBEDIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPUBEDIT                                                      
         L     RF,=V(PUBFLOAT)                                                  
         A     RF,RELO                                                          
         ST    RF,VPUBFLT                                                       
         L     RF,=V(PUBEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPUBEND                                                       
         L     RF,=V(REPEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VREPEND                                                       
         L     RF,=V(PUBFRT)                                                    
         A     RF,RELO                                                          
         ST    RF,VPUBFRT                                                       
         L     RF,=V(CLTEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTEND                                                       
         L     RF,=V(RPTEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VRPTEND                                                       
         L     RF,=V(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=V(CLTTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTTAB                                                       
         L     RF,=V(PRNTPUB)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRNTPUB                                                      
         L     RF,=V(PRDF)                                                      
         A     RF,RELO                                                          
         ST    RF,VPRDF                                                         
         L     RF,=V(MTHEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VMTHEND                                                       
         L     RF,=V(PRDEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPRDEND                                                       
         L     RF,=V(CLTCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VCLTCSCT                                                      
         L     RF,=V(RPTCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VRPTCSCT                                                      
         L     RF,=V(REPCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VREPCSCT                                                      
         L     RF,=V(PPGETCG)                                                   
         A     RF,RELO                                                          
         ST    RF,VPPGETCG                                                      
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
*                                                                               
         L     RE,=V(PRNTOFC)                                                   
         ST    RE,VPRNTOFC         STORE PRNTOFC ADDRESS                        
*                                                                               
         XC    MTHTAB(250),MTHTAB                                               
         XC    MTHTAB+250(L'MTHTAB-250),MTHTAB+250                              
         XC    SAVECLT,SAVECLT                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         MVI   FCGTREP,C'Y'                                                     
         CLI   QOPT6,C'Y'           SEE IF SHOWING PAYING ADDR                  
         BE    BLDM3                                                            
         CLC   QSORT,=C'08'         REP SORT                                    
         BE    BLDM3                                                            
         CLC   QSORT,=C'09'         PAY ADDRESS SORT                            
         BE    BLDM3                                                            
         MVI   FCGTREP,C'N'                                                     
BLDM3    DS    0H                                                               
         MVC   RUNPROF,PROGPROF     MUST USE THIS PROFILE                       
*                                   FOR LINE/INCH TOTALLING OPT                 
         CLI   QOPT2,C' '                                                       
         BNE   *+8                                                              
         MVI   QOPT2,C'Y'       SET FOR DEFAULT OF ONE PUB/PAGE                 
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         OI    DMINBTS,X'08'        SET TO PASS DELETED RECS                    
         MVI   FCRDBUY,C'Y'                                                     
         CLC   QPRODUCT,=C'   '                                                 
         BNE   *+10                                                             
         MVC   QPRODUCT,=C'ALL'                                                 
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
         MVI   FCRDBUY,X'21'       USE 21 POINTERS                              
*                                                                               
         MVI   FCGTJOB,C'N'                                                     
         JIF   QOPT4,EQ,C' ',OR,QOPT4,EQ,C'A',BLDM5,JUMP=N                      
         MVI   FCGTJOB,C'Y'                                                     
*                                                                               
BLDM5    CLI   QREGION,C' '        SEE IF USING AS OF DATE                      
         BNE   BLDM10              YES - CAN'T USE PAID/UPPAID FILTERS          
         MVC   FCPDFILT,QOPT1      P=PAID,U=UNPAID                              
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
BLDM10   MVI   FCPDFILT,C'N'       RESET TO N                                   
         MVC   PAGE,=H'1'                                                       
         MVI   REQERR,0                                                         
SPRDOK   MVC   WORK(12),QSTART       SAVE STRT AND END                          
         CLI   QBPDATE,C'B'                                                     
         BE    BILLPAY                                                          
         CLI   QBPDATE,C'P'                                                     
         BE    BILLPAY                                                          
BLDLIST  LA    R6,ACCNUM                SET FOR BCT                             
         LA    R4,MTHTAB                                                        
         MVC   0(4,R4),QSTART                                                   
PUTMTH   PACK  DUB,2(2,R4)                                                      
         AP    DUB,=P'1'                                                        
         CP    DUB,=P'13'                                                       
         BL    SAMEYR                                                           
         SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         PACK  DUB,0(2,R4)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
*        AP    DUB,=P'1'           ADD 1 TO YR                                  
*        OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R4),DUB+6(2)                                                 
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE             EXIT                                         
         XC    8(8,R4),4(R4)       CLEAR LAST MTH                               
         B     MTHDONE             EXIT                                         
*                                                                               
SAMEYR   OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         MVC   8(2,R4),0(R4)                                                    
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE                                                          
         XC    8(8,R4),4(R4)                                                    
         B     MTHDONE             EXIT                                         
*                                                                               
NEXTMTH  LA    R4,8(R4)                                                         
         BCT   R6,PUTMTH           36 MTHS MAX                                  
         B     MTHDONE             EXIT                                         
*                                                                               
*                                                                               
*                                                                               
BILLPAY  PACK  DUB,QSTART+2(2)     SET START DATE BACK 3 MONTHS                 
         SP    DUB,=P'3'                                                        
         CP    DUB,=P'0'                                                        
         BNH   CHGSYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         B     CHGEND                                                           
*                                                                               
CHGSYR   AP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         PACK  DUB,QSTART(2)                                                    
         BAS   RE,BLDMDNYR         SUBTRACT 1 FROM YR                           
*        SP    DUB,=P'1'                                                        
*        OI    DUB+7,X'0F'                                                      
         UNPK  QSTART(2),DUB+6(2)                                               
*                                                                               
CHGEND   PACK  DUB,QEND+2(2)                                                    
         AP    DUB,=P'6'      ADVANCE END DATE 6 MONTHS                         
         CP    DUB,=P'12'                                                       
         BH    CHGEYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         B     BLDLIST                                                          
*                                                                               
CHGEYR   SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         PACK  DUB,QEND(2)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
*        AP    DUB,=P'1'                                                        
*        OI    DUB+7,X'0F'                                                      
         UNPK  QEND(2),DUB+6(2)                                                 
         B     BLDLIST                                                          
*                                                                               
MTHDONE  MVC   QSTART(12),WORK    RESTORE DATES                                 
*                                                                               
         MVC   WORK(6),QSTART                                                   
         MVC   WORK+4(1),=C'01'    DAY TO 01                                    
*                                                                               
* SUBTRACTING 75 DAYS WILL ALWAYS GET ME BACK 3 MONTHS                          
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-75'                                    
*                                                                               
         MVC   WORK+10(2),=C'01'    SET DAY TO THE 1ST                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,CDPDATE)                               
*                                                                               
*        CDPDATE SHOULD NOW BE SET TO THE FIRST DAY OF                          
*        OF THE OF THE 3RD MONTH PRIOR TO THE REQUEST                           
*        THIS DATE WILL BE USED TO DETERMINE THE CD STATUS OF THE PUB           
*        WHEN USING QOPT3  (CD OR NON CD PUBS ONLY)                             
*                                                                               
         XC    ASOFDTE,ASOFDTE                                                  
         CLI   QREGION,C' '        AS OF DATE IN QREGION YYMMDD                 
         BE    MTHD1                                                            
*        GOTO1 DTCNV,DMCB,(0,QREGION),(1,ASOFDTE)                               
         GOTO1 DATCON,DMCB,(0,QREGION),(3,ASOFDTE)                              
*        GOTO1 DTCNV,DMCB,(1,ASOFDTE),(3,ASDATE)                                
         GOTO1 DATCON,DMCB,(3,ASOFDTE),(5,ASDATE)                               
*          DATA SET PPREP2702  AT LEVEL 154 AS OF 09/08/89                      
MTHD1    DS    0H                  NOW CLEAR ALL ACCUMS                         
         LA    R6,6                                                             
         LA    R4,MTHTOTS                                                       
CLRMTH   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRMTH                                                        
         LA    R6,6                                                             
         LA    R4,PRDTOTS                                                       
CLRPRDA  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRDA                                                       
*                                                                               
         LA    R6,ACCNUM*6                                                      
         LA    R4,PUBTOTS                                                       
CLRPUB   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPUB                                                        
*                                                                               
         LA    R6,ACCNUM*6                                                      
         LA    R4,PROTOTS                                                       
CLRPRO   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRO                                                        
*                                                                               
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R6,ACCNUM*7                                                      
         LA    R4,REPTOTS                                                       
CLRREP   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRREP                                                        
         DROP  R9                                                               
*                                                                               
         L     R9,VRPTCSCT                                                      
         USING RPTDSCT,R9                                                       
         LA    R6,ACCNUM*7                                                      
         LA    R4,RPTTOTS                                                       
CLRRPT   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRRPT                                                        
         XC    RTOTPUBS,RTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         LA    R6,ACCNUM*6                                                      
         LA    R4,CLTTOTS                                                       
CLRCLT   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRCLT                                                        
         DROP  R9                                                               
         MVI   MTHACT,0            CLEAR ACTIVITY INDICATORS                    
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
         MVI   CLTACT,0                                                         
         MVI   RPTACT,0                                                         
         XC    CLTMTHS,CLTMTHS                                                  
         XC    PUBCLTS,PUBCLTS                                                  
         XC    RTOTPUBS,RTOTPUBS                                                
*        GOTO1 DTCNV,DMCB,(0,QSTART),(1,REQST)                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(3,REQST)                                 
*        GOTO1 DTCNV,DMCB,(0,QEND),(1,REQEND)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(3,REQEND)                                  
         L     R2,ABUYOUTA                                                      
         USING PPBYOUTD,R2                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R6,PBUYREC                                                       
         L     R7,DATCON                                                        
         LA    R8,GROSS                                                         
         STM   R6,R8,0(R2)                                                      
         MVI   PBYOCTL,X'28'                                                    
         DROP  R2                                                               
*                                                                               
CLCKT    DS    0H                  SEE IF I NEED TO BUILD TABLE OF              
*                                  CLIENTS AND NAMES                            
         L     R6,VCLTTAB                                                       
         MVC   STKEY,KEY                                                        
         CLC   0(3,R6),QAGENCY     WRONG AGENCY OR MEDIA - REBUILD              
         BNE   CKCLT1                                                           
         CLC   3(3,R6),=C'ALL'     SEE IF LAST WAS ALL CLTS                     
         BE    CKCLTX              NO NEED TO REBUILD                           
         CLI   3(R6),C'*'          WAS LAST AN OFFICE REQ                       
         BE    CKCLTX              YES - NO NEED TO REBUILD                     
         CLI   3(R6),C'&&'         WAS LAST AN GROUP REQ                        
         BE    CKCLTX              YES - NO NEED TO REBUILD                     
*                                  SINCE OFFICE REQS READ ALL CLIENTS           
         CLC   3(3,R6),QCLIENT                                                  
         BE    CKCLTX              SAME CLIENT                                  
         B     CKCLT1              NEED TO REBUILD                              
*                                                                               
CKCLT1   XC    KEY,KEY                                                          
         MVC   0(6,R6),QAGENCY        SET TABLE IDENTIFIER                      
*                                  QAGENCY,QMEDIA,QCLIENT                       
         LA    R6,6(R6)                                                         
         MVC   KEY(3),QAGENCY      QAGENCY AND QMEDIA                           
         MVI   KEY+3,X'02'                                                      
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CKCLT1A                                                          
         CLI   QCLIENT,C'*'        READ ALL CLIENTS FOR OFFICE REQS             
         BE    CKCLT1A                                                          
         CLI   QCLIENT,C'$'        READ ALL CLIENTS FOR OFFICE LIST             
         BE    CKCLT1A                                                          
         CLI   QCLIENT,C'&&'       READ ALL CLIENTS FOR GROUP REQS              
         BE    CKCLT1A                                                          
         MVC   KEY+4(3),QCLIENT                                                 
CKCLT1A  GOTO1 HIGH                                                             
         B     CKCLT2C                                                          
*                                                                               
CKCLT2   GOTO1 SEQ                                                              
CKCLT2C  CLC   KEY(4),KEYSAVE                                                   
         BNE   CKCLT6                                                           
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CKCLT4                                                           
         CLI   QCLIENT,C'*'        READ ALL CLTS FOR OFFICE REQ                 
         BE    CKCLT4                                                           
         CLI   QCLIENT,C'$'        READ ALL CLTS FOR OFFICE LIST                
         BE    CKCLT4                                                           
         CLI   QCLIENT,C'&&'       READ ALL CLTS FOR GROUP REQ                  
         BE    CKCLT4                                                           
         CLC   KEY(7),KEYSAVE                                                   
         BE    CKCLT4                                                           
         DC    H'0'                CLT NOT ON FILE                              
*                                                                               
CKCLT4   GOTO1 GETCLI                                                           
CKCLT5   MVC   0(3,R6),PCLTKCLT                                                 
         MVC   3(20,R6),PCLTNAME                                                
         MVC   23(2,R6),PCLTOFF                                                 
         LA    R6,25(R6)                                                        
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CKCLT2                                                           
         CLI   QCLIENT,C'*'                                                     
         BE    CKCLT2                                                           
         CLI   QCLIENT,C'$'                                                     
         BE    CKCLT2                                                           
         CLI   QCLIENT,C'&&'                                                    
         BE    CKCLT2                                                           
         B     CKCLT6                                                           
*                                                                               
CKCLT6   MVC   0(3,R6),=3X'FF'     SET END OF TABLE                             
*                                                                               
*              FOR ONE CLIENT REQ MUST READ CLIENT                              
*                                                                               
CKCLTX   DS    0H                                                               
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CKCLTXX                                                          
         CLI   QCLIENT,C'*'        OFFICE REQ                                   
         BE    CKCLTXX                                                          
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    CKCLTXX                                                          
         CLI   QCLIENT,C'&&'       GROUP REQ                                    
         BE    CKCLTXX                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY      QAGENCY AND QMEDIA                           
         MVC   KEY+4(3),QCLIENT                                                 
         MVI   KEY+3,X'02'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    CKCLTX4                                                          
         DC    H'0'                CLT NOT ON FILE                              
*                                                                               
CKCLTX4  GOTO1 GETCLI                                                           
CKCLTXX  MVC   KEY,STKEY           RESTORE KEY                                  
*                                                                               
         XC    SAVEPNAM(43),SAVEPNAM                                            
         XC    REQPUB,REQPUB                                                    
         CLC   QPUB(3),=C'ALL'                                                  
         BE    CKPRD                                                            
         CLC   QPUB,=6C' '                                                      
         BE    CKPRD               NO PUB FILTER                                
         PACK  REQPUB(6),QPUB(11)                                               
         MVC   REQPUB+5(1),QPUB+10                                              
         CLI   REQPUB+5,C' '                                                    
         BNE   *+8                                                              
         MVI   REQPUB+5,0                                                       
CKPRD    CLC   QPRODUCT,=C'ALL'                                                 
         BE    BLDMX               RETURN                                       
         CLC   QPRODUCT,=3C' '                                                  
         BNE   *+14                                                             
         MVC   QPRODUCT,=C'ALL'                                                 
         B     BLDMX                                                            
         CLC   QCLIENT,=C'ALL'    CAN'T READ PRD IF ALL CLIENTS                 
         BE    BLDMX                                                            
         CLI   QCLIENT,C'*'       CAN'T READ PRD IF OFFICE REQ                  
         BE    BLDMX                                                            
         CLI   QCLIENT,C'$'       CAN'T READ PRD IF OFFICE LIST                 
         BE    BLDMX                                                            
         CLI   QCLIENT,C'&&'      CAN'T READ PRD IF GROUP REQ                   
         BE    BLDMX                                                            
         MVC   STKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),QPRODUCT                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(10),KEY                                                  
         BE    HAVPRD                                                           
         MVC   P+10(25),=C'*** PRODUCT NOT FOUND ***'                           
         MVI   REQERR,2                                                         
         GOTO1 VPRINTIT                                                         
         B     BLDMX                                                            
*                                                                               
HAVPRD   GOTO1 GETPROD                                                          
         MVC   SAVEPNAM,PPRDNAME                                                
         MVC   KEY,STKEY                                                        
         CLC   QEST,=C'ALL'                                                     
         BE    BLDMX               RETURN                                       
         CLC   QEST,=3C' '                                                      
         BE    BLDMX                                                            
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,REQEST                                                        
         MVC   KEY,KEYSAVE         WILL HAVE CLT/PRD                            
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+10(2),REQEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    HAVEST                                                           
         MVC   P+10(26),=C'*** ESTIMATE NOT FOUND ***'                          
         MVI   REQERR,3                                                         
         GOTO1 VPRINTIT                                                         
         B     BLDMX                                                            
*                                                                               
HAVEST   GOTO1 GETEST                                                           
         MVC   SAVEENAM,PESTNAME                                                
         SR    R0,R0                                                            
         LH    R0,REQEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAVEEST(3),DUB+6(2)                                              
         MVC   KEY,STKEY                                                        
         B     BLDMX               RETURN                                       
*                                                                               
BLDMX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
****  INCREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." DATE FORMATS          
*                                                                               
BLDMUPYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'90'         YR ENDING IN 9 ??                            
         BO    BMCYUP              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         AP    DUB,=P'1'           INCREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMUPX             DONE WITH YR NOT ENDING IN 9                 
*                                                                               
BMCYUP   DS    0H                                                               
         MVI   DUB+7,X'0F'         LO-ORDER OF YR MUST BE 0 HERE                
         ZIC   R0,STRHIYR                                                       
         AH    R0,=H'1'            INCREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMUPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
****  DECREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." DATE FORMATS          
*                                                                               
BLDMDNYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'F0'         YR ENDING IN 0 ??                            
         BZ    BMCYDN              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         SP    DUB,=P'1'           DECREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMDNX             DONE WITH YR NOT ENDING IN 0                 
*                                                                               
BMCYDN   DS    0H                                                               
         MVI   DUB+7,X'9F'         LO-ORDER OF YR MUST BE 9 HERE                
         ZIC   R0,STRHIYR                                                       
         SH    R0,=H'1'            DECREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMDNX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CLTF     CSECT                                                                  
         NMOD1 0,CLTF                                                           
         L     RC,PPFILEC                                                       
         XC    SAVEYMD,SAVEYMD                                                  
         XC    SAVEPRD,SAVEPRD                                                  
*              FIRST BUILD LIST OF PRD AND NAMES                                
         MVC   PPGKEY,KEY                                                       
*                                                                               
CLIF1    DS    0H                                                               
         L     R6,APRDTAB                                                       
*                                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   CLIF6               YES - ONLY READ PRDS WHEN NEEDED             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     CLIF3                                                            
*                                                                               
CLIF2    LA    R4,DMRSEQ                                                        
CLIF3    BAS   RE,DIRRD                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CLIF6                                                            
*                                                                               
*                                                                               
CLIF4    LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,FILERD                                                        
CLIF5    MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         B     CLIF2                                                            
*                                                                               
CLIF6    MVC   0(3,R6),=X'FFFFFF'  SET END OF TABLE                             
*                                                                               
CLIF7    DS    0H                                                               
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CLIF7C                 SKIP NAME                                 
         CLI   QCLIENT,C'*'           OFFICE REQUEST                            
         BE    CLIF7C                 SKIP NAME                                 
         CLI   QCLIENT,C'$'           OFFICE LIST REQUEST                       
         BE    CLIF7C                 SKIP NAME                                 
         CLI   QCLIENT,C'&&'          GROUP REQUEST                             
         BE    CLIF7C                 SKIP NAME                                 
         B     CLIF8                                                            
*                                                                               
CLIF7C   L     R6,VCLTTAB                                                       
         LA    R6,6(R6)                                                         
CLIF7D   CLC   0(3,R6),PBUYKCLT                                                 
         BE    CLIF7X                                                           
         CLC   0(3,R6),=3X'FF'                                                  
         BNE   *+6                                                              
         DC    H'0'             CLIENT NOT IN TABLE                             
         LA    R6,25(R6)                                                        
         B     CLIF7D                                                           
**                                                                              
*                                                                               
CLIF7X   MVC   P+1(6),=C'CLIENT'                                                
         MVC   P+9(3),PBUYKCLT                                                  
         MVC   P+14(20),3(R6)                                                   
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
CLIF8    MVC   SAVECLT,PBUYKCLT                                                 
*                                                                               
         B     CPPGEXT                                                          
*                                                                               
DIRRD    NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    DIRX                                                             
         CLI   DMCB+8,X'02'         PASS DELETES                                
         BE    DIRX                                                             
         DC    H'0'                                                             
DIRX     XIT                                                                    
*                                                                               
FILERD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    FILX                                                             
         CLI   DMCB+8,X'02'            PASS DELETES                             
         BE    FILX                                                             
         DC    H'0'                                                             
FILX     XIT                                                                    
*                                                                               
CPPGEXT  MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
CLIEXT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         MVI   RCSUBPRG,0                                                       
         CLI   MODE,LBUYREQ                                                     
         BE    PRNT0                                                            
         MVI   RCSUBPRG,4                                                       
         CLI   RUNPROF,C'I'                                                     
         BNE   PRNT0                                                            
         MVI   RCSUBPRG,6                                                       
PRNT0    CLI   QCLIENT,C'*'                                                     
         BNE   PRNT0B                                                           
         MVC   HEAD5(6),=C'OFFICE'                                              
*****    MVC   HEAD5+7(2),QCLIENT+1                                             
*                                                                               
*        PRINT OFFICE CODE AND NAME                                             
*                                                                               
         GOTOR VPRNTOFC,DUB,QCLIENT+1,(C'L',HEAD5+7),VOFFICER,QAGENCY, X        
               VCOMFACS                                                         
*                                                                               
         B     PRNT1A                                                           
*                                                                               
PRNT0B   CLI   QCLIENT,C'$'                                                     
         BNE   PRNT1                                                            
         MVC   HEAD5(11),=C'OFFICE LIST'                                        
         MVC   HEAD5+12(1),QCLIENT+1                                            
*                                                                               
         B     PRNT1A                                                           
*                                                                               
PRNT1    CLI   QCLIENT,C'&&'            GROUP                                   
         BNE   PRNT1A                                                           
         MVC   HEAD5(5),=C'GROUP'                                               
         MVC   HEAD5+6(1),QCLIENT+1                                             
PRNT1A   CLC   QPRODUCT,=C'ALL'                                                 
         BE    CKMED                                                            
         CLC   QPRODUCT,=C'   '                                                 
         BE    CKMED                                                            
         MVC   HEAD6(7),=C'PRODUCT'                                             
         MVC   HEAD6+9(3),QPRODUCT                                              
         MVC   HEAD6+13(20),SAVEPNAM                                            
CKMED    CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
         CLC   QEST,=C'ALL'                                                     
         BE    CKESTX                                                           
         CLC   QEST,=C'   '                                                     
         BE    CKEST2                                                           
CKEST1   ZIC   R2,RCSUBPRG         LET PPG PRINT EST OR FILTERS                 
         LA    R2,1(R2)                                                         
         STC   R2,RCSUBPRG                                                      
         B     CKESTX                                                           
*                                                                               
CKEST2   CLC   QESTEND,=C'   '     FILTERS                                      
         BE    CKESTX              NO                                           
         B     CKEST1              FILTERS                                      
*                                                                               
CKESTX   DS    0H                                                               
         MVC   HEAD8(26),=C'** CASH DISC. PUBS ONLY **'                         
         CLI   QOPT3,C'C'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(30),=C'** NON CASH DISC. PUBS ONLY **'                     
         CLI   QOPT3,C'N'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(33),SPACES                                                 
CKPUP    MVC   HEAD8+75(26),=C'** UNCLEARED ITEMS ONLY **'                      
         CLI   QOPT1,C'U'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),=C' ** CLEARED ITEMS ONLY ** '                      
         CLI   QOPT1,C'P'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),SPACES                                              
CKBP     CLI   QBPDATE,C'B'                                                     
         BE    BILMSG                                                           
         CLI   MODE,LBUYREQ          SEE IF DOING REPORT TOTALS                 
         BNE   *+14                                                             
         MVC   HEAD10+4(9),=C'INSERTION'                                        
         MVI   RCSUBPRG,0                RESET TO 0 FOR LBUYREQ                 
         CLI   QBPDATE,C'P'                                                     
         BNE   CKASOF                                                           
         MVC   HEAD4+43(19),=C'** PAYING PERIOD **'                             
         B     CKASOF                                                           
*                                                                               
BILMSG   MVC   HEAD4+43(20),=C'** BILLING PERIOD **'                            
         CLI   MODE,LBUYREQ              SEE IF DOING REPORT TOTALS             
         BNE   CKASOF                                                           
         MVC   HEAD10+5(7),=C'BILLING'                                          
         MVI   RCSUBPRG,0                                                       
*                                                                               
CKASOF   DS    0H                                                               
         CLI   MODE,LBUYREQ             SEE IF DOING REPORT TOTALS              
         BE    CKASOF5                                                          
CKASOF5  DS    0H                                                               
         CLI   ASOFDTE,0                                                        
         BE    CKCLTS         AS OF DATE NOT USED                               
         MVC   HEAD5+45(5),=C'AS OF'                                            
         MVC   HEAD5+51(8),ASDATE                                               
CKCLTS   CLC   QCLIENT,=C'ALL'                                                  
         BE    PRINTX                                                           
         CLI   QCLIENT,C'*'        IF ALL CLTS OR OFFICE REQ                    
         BE    PRINTX                                                           
         CLI   QCLIENT,C'$'        IF ALL CLTS OR OFFICE LIST                   
         BE    PRINTX                                                           
         CLI   QCLIENT,C'&&'       OR GROUP                                     
         BE    PRINTX                                                           
         ZIC   R2,RCSUBPRG         BUMP RCSUBPRG BY 10                          
         AH    R2,=H'10'           FOR SINGLE CLT REQS                          
         STC   R2,RCSUBPRG                                                      
PRINTX   CLC   LINE,MAXLINES                                                    
         BL    PRINTX5                                                          
         CLI   PUBPSW,1            SEE IF PUB ALREADY PRINTED                   
         BNE   PRINTX5                                                          
         MVC   SAVEP,P                                                          
         MVC   SAVPSEC,PSECOND                                                  
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         GOTO1 REPORT                                                           
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVPSEC                                                  
PRINTX5  GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
SAVEP    DS    CL132                                                            
SAVPSEC  DS    CL132                                                            
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PUBEND   CSECT                                                                  
         NMOD1 0,PUBEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         CLI   PUBACT,C'Y'                                                      
         BNE   PUBENDX                  NO ACTIVITY                             
         GOTO1 VCLTEND                                                          
*                                                                               
*                                  BECAUSE CLT WILL HAVE BEEN PRINTED           
*                                                                               
*                                  SKIP PUB TOTALS                              
PUBE5    CLC   PUBCLTS,=F'1'                                                    
         BNH   PUBROLL                                                          
PUBE10   SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         MVC   P+2(16),=C'** PUB TOTALS **'                                     
         GOTO1 VPRINTIT                                                         
PUBEND0  XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBEND1  LA    R6,PUBTOTS                                                       
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
PUBEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,PRTMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBEND1                                                       
         B     PUBEND3                                                          
*                                                                               
PRTMTH   EQU   *                                                                
         L     R9,PUBMTHS                                                       
         A     R9,=F'1'                                                         
         ST    R9,PUBMTHS                                                       
         LA    R9,MTHTAB       R4 HAS MTH DISP                                  
         LA    R9,0(R4,R9)                                                      
         MVC   WORK(4),0(R9)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
*                                                                               
         LA    R2,PUBINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    NOPPINS             NO INSERTIONS                                
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'0'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPINS  LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    PRTMTH1             NO LINES                                     
         CLI   RUNPROF,C'I'                                                     
         BNE   PRTMTHA                                                          
         EDIT  (P8,(R2)),(9,P+41),2                                             
         MVI   P+50,C'I'                                                        
         B     PRTMTH1                                                          
PRTMTHA  EDIT  (P8,(R2)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PRTMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+94),2,COMMAS=YES,MINUS=YES                       
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
*                                                                               
PUBEND3  CLC   PUBMTHS,=F'1'                                                    
         BNH   PUBEND20                                                         
         MVC   P+5(5),=C'TOTAL'                                                 
PUBEND3B ZAP   WKDUB,PUBINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,PUBINS                                                        
PUBEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BZ    NOPPTINS            NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPTINS CLI   QMEDIA,C'N'                                                      
         BNE   PUBEND6                                                          
         ZAP   WKDUB,PUBLINES                                                   
         LA    R2,PUBLINES                                                      
         LA    R6,ACCNUM-1                                                      
PUBEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    PUBEND6             NO LINES                                     
         CLI   RUNPROF,C'I'                                                     
         BNE   PUBEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     PUBEND6                                                          
PUBEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
PUBEND6  ZAP   WKDUB,PUBGO                                                      
         LA    R2,PUBGO                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,PUBGLAC                                                    
         LA    R2,PUBGLAC                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,PUBCHCD                                                    
         LA    R2,PUBCHCD                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   WKDUB,PUBNP                                                      
         LA    R2,PUBNP                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND10                                                      
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
PUBEND20 SR    R3,R3               AND SKIP A LINE                              
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*                                  ROLL TO REPORT TOTALS                        
PUBROLL  DS    0H                                                               
         L     R9,VRPTCSCT                                                      
         USING RPTDSCT,R9                                                       
*                                                                               
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBRA    LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
PUBRB    CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,BUMPPUB                                                       
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBRB                                                         
         LA    R4,8(R4)                                                         
         BCT   R3,PUBRA                                                         
         B     PUBRC                                                            
*                                                                               
BUMPPUB  LA    R2,RPTPUBS          RPT/MTH PUB TOTALS                           
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         BR    R8                                                               
*                                                                               
PUBRC    LA    R2,ACCNUM*6                                                      
         LA    R3,RPTINS                                                        
         LA    R4,PUBINS                                                        
PUBRD    AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBRD                                                         
         L     R8,RTOTPUBS                                                      
         AH    R8,=H'1'                                                         
         ST    R8,RTOTPUBS                                                      
         DROP  R9                                                               
*                                                                               
PUBRD5   DS    0H                                                               
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R2,ACCNUM*6             ROLL TO REP ACCUMS                       
         LA    R3,REPINS                                                        
         LA    R4,PUBINS                                                        
PUBRD8   AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBRD8                                                        
         DROP  R9                                                               
*                                  CLEAR  PUB ACCUMS                            
PUBEND11 LA    R6,ACCNUM*6                                                      
         LA    R7,PUBTOTS                                                       
PUBEND12 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,PUBEND12                                                      
         XC    CLTMTHS,CLTMTHS                                                  
         XC    PUBCLTS,PUBCLTS                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         XC    SAVECLT,SAVECLT                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         MVI   MTHACT,0                                                         
         MVI   PRDACT,0                                                         
         MVI   CLTACT,0                                                         
         MVI   PUBACT,0                                                         
PUBENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REPEND   CSECT                                                                  
         NMOD1 0,REPEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         GOTO1 VMTHEND                                                          
         CLI   REPACT,C'Y'                                                      
         BNE   REPENDX                                                          
*                                                                               
REPE10   SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
*                                                                               
         MVI   PUBPSW,0         SO PUB CONTINUED MESSAGE WON'T                  
*                               PRINT IN REP TOTALS                             
*                                                                               
         CLI   PREPKEY+3,X'11'                                                  
         BNE   REPE15                                                           
         MVC   P+2(16),=C'** REP TOTALS **'                                     
         B     REPE20                                                           
REPE15   CLC   PREPNAME,SPACES                                                  
         BE    REPEND15                     GO TO CLEAR TOTALS                  
         MVC   P+2(16),=C'** ADR TOTALS **'                                     
REPE20   GOTO1 VPRINTIT                                                         
*                                                                               
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
REPEND1  LA    R6,REPINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
REPEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,REPMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,REPEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,REPEND1                                                       
         B     REPEND3                                                          
*                                                                               
REPMTH   EQU   *                                                                
         L     R2,PUBMTHS                                                       
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHS                                                       
         LA    R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,REPINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    RNOPINS              NO INSERTIONS                               
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPINS  LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   REPMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    REPMTH1             NO LINES                                     
         CLI   RUNPROF,C'I'                                                     
         BNE   REPMTHA                                                          
         EDIT (P8,(R2)),(9,P+41),2                                              
         MVI   P+50,C'I'                                                        
         B     REPMTH1                                                          
REPMTHA  EDIT (P8,(R2)),(6,P+44),0                                              
         MVI   P+50,C'L'                                                        
REPMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+94),2,COMMAS=YES,MINUS=YES                        
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
REPEND3  MVC   P+5(5),=C'TOTAL'                                                 
         ZAP   WKDUB,REPINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,REPINS                                                        
REPEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BE    RNOPTINS             NO INSERTIONS                               
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPTINS CLI   QMEDIA,C'N'                                                      
         BNE   REPEND6                                                          
         ZAP   WKDUB,REPLINES                                                   
         LA    R2,REPLINES                                                      
         LA    R6,ACCNUM-1                                                      
REPEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    REPEND6             NO LINES                                     
         CLI   RUNPROF,C'I'                                                     
         BNE   REPEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     REPEND6                                                          
REPEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
REPEND6  ZAP   WKDUB,REPGO                                                      
         LA    R2,REPGO                                                         
         LA    R6,ACCNUM-1                                                      
REPEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,REPGLAC                                                    
         LA    R2,REPGLAC                                                       
         LA    R6,ACCNUM-1                                                      
REPEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,REPCD                                                      
         LA    R2,REPCD                                                         
         LA    R6,ACCNUM-1                                                      
REPEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   WKDUB,REPNP                                                      
         LA    R2,REPNP                                                         
         LA    R6,ACCNUM-1                                                      
REPEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND10                                                      
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALWAYS PRINT TOTAL                              
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   REPROLL                                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
REPROLL  DS    0H                                                               
*                                  CLEAR  REP ACCUMS                            
REPEND15 LA    R6,ACCNUM*7                                                      
         LA    R7,REPINS                                                        
REPEND17 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,REPEND17                                                      
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   PUBACT,0                                                         
         MVI   CLTACT,0                                                         
         MVI   PRDACT,0                                                         
         MVI   MTHACT,0                                                         
         MVI   REPACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVECLT,SAVECLT                                                  
*                                                                               
REPENDX  XIT1                                                                   
         LTORG                                                                  
         DROP  R9                                                               
         EJECT                                                                  
CLTEND   CSECT                                                                  
         NMOD1 0,CLTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTENDX                                                          
*            SEE IF NEED TO PRINT PUB                                           
         CLI   P+1,C' '                                                         
         BE    CLTE                                                             
CLTA     SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'6'                                                         
         STC   R0,SAVELINE         NEED 4 LINES                                 
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT2,C'Y'          CHK ONE PUB PER PAGE                         
         BNE   CLTB                NO -NO NEW PAGE                              
         MVI   FORCEHED,C'Y'                                                    
CLTB     GOTO1 VPRINTIT                                                         
CLTE     DS    0H                                                               
*                                                                               
*                                                                               
CLTE2    SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE         NEED 4 LINES                                 
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
*                                  IT WILL NOW BE IN HEADLINES                  
CLTE2X   MVC   P+1(6),=C'CLIENT'                                                
         MVC   P+8(6),=C'TOTALS'                                                
         B     CLTE4X                                                           
**                                                                              
**       VCLTTAB TABLE LOGIC NO-OPED                                            
**                                                                              
         L     R6,VCLTTAB                                                       
         LA    R6,6(R6)            BUMP PAST IDENTIFIER                         
CLTE3    CLC   0(3,R6),SAVECLT                                                  
         BE    CLTE4                                                            
         CLC   0(3,R6),=3X'FF'                                                  
         BNE   *+6                                                              
         DC    H'0'                CLIENT NOT FOUND                             
         LA    R6,25(R6)                                                        
         B     CLTE3                                                            
*                                                                               
CLTE4    MVC   P+9(3),0(R6)                                                     
         MVC   P+13(20),3(R6)                                                   
*                                                                               
CLTE4X   GOTO1 VPRINTIT                                                         
CLTE5    DS    0H                                                               
         XC    CLTMTHS,CLTMTHS                                                  
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
CLTEND1  LA    R2,CLTINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,6                                                             
CLTEND2  CP    0(8,R2),=P'0'                                                    
         BNE   ACTIVITY                                                         
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,CLTEND2                                                       
CLTEND3  LA    R4,8(R4)                                                         
         BCT   R8,CLTEND1                                                       
         B     CLTEND5        GO TO TOTALS                                      
*                                                                               
ACTIVITY LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
*                                                                               
ACTV5    LA    R1,CLTINS                                                        
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    NOCINS              NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   ACTIV1                                                           
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    ACTIV1              NO LINES                                     
         CLI   RUNPROF,C'I'                                                     
         BNE   ACTIVA                                                           
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         B     ACTIV1                                                           
ACTIVA   EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
ACTIV1   LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         GOTO1 VPRINTIT                                                         
         L     R3,CLTMTHS         BUMP CLT MTHS COUNTER                         
         A     R3,=F'1'                                                         
         ST    R3,CLTMTHS                                                       
         B     CLTEND3                                                          
*                                                                               
*                                                                               
CLTEND5  CLC   CLTMTHS,=F'1'                                                    
         BNH   CLTEND20                                                         
         MVC   P+5(5),=C'TOTAL'                                                 
         MVI   SPACING,2                                                        
CLTEND5C ZAP   WKDUB,CLTINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTINS+8                                                      
CLTEND6  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND6                                                       
         CP    WKDUB,=P'0'                                                      
         BE    NOCTINS             NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCTINS  CLI   QMEDIA,C'N'                                                      
         BNE   CLTEND8                                                          
         ZAP   WKDUB,CLTLINES                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTLINES+8                                                    
CLTEND7  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND7                                                       
         CP    WKDUB,=P'0'                                                      
         BE    CLTEND8             NO LINES                                     
         CLI   RUNPROF,C'I'                                                     
         BNE   CLTEND7A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     CLTEND8                                                          
CLTEND7A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
CLTEND8  ZAP   WKDUB,CLTGO                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGO+8                                                       
CLTEND9  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND9                                                       
         EDIT  WKDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,CLTGLAC                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGLAC+8                                                     
CLTEND10 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND10                                                      
         EDIT  WKDUB,(15,P+67),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,CLTCD                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTCD+8                                                       
CLTEND11 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND11                                                      
         CP    WKDUB,=P'99999999'      IF OVER 99,999.99 NO COMMAS              
         BH    CLTE11B                                                          
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         B     CLTE11C                                                          
CLTE11B  EDIT  WKDUB,(12,P+82),2,MINUS=YES                                      
CLTE11C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   WKDUB,CLTNP                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTNP+8                                                       
CLTEND12 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND12                                                      
         CP    WKDUB,=P'9999999999'   IF OVER 99,999,999.99                     
         BH    CLTE12B                                                          
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         B     CLTE12C                                                          
CLTE12B  EDIT  WKDUB,(14,P+94),2,MINUS=YES                                      
CLTE12C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
CLTEND20 SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         MVI   SPACING,2           SKIP AFTER                                   
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*                  ROLL TO PUB ACCUMS                                           
CLTROLL  LA    R2,ACCNUM*6                                                      
         LA    R3,PUBINS                                                        
         LA    R4,CLTINS                                                        
CLTRL1   AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,CLTRL1                                                        
         XC    CLTMTHS,CLTMTHS                                                  
         XC    MTHACT(3),MTHACT                                                 
         LA    R3,CLTINS                                                        
         LA    R4,ACCNUM*6                                                      
CLTEND13 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,CLTEND13                                                      
         L     R3,PUBCLTS       BUMP PUB/CLTS COUNTER                           
         A     R3,=F'1'                                                         
         ST    R3,PUBCLTS                                                       
*                                                                               
CLTENDX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
CLTCSCT  CSECT                                                                  
         DS    2160C              45 X 8 X 6                                    
RPTCSCT  CSECT                                                                  
         DS    2520C              45 X 8 X 7                                    
REPCSCT  CSECT                                                                  
         DS    2520C              45 X 8 X 7                                    
*                                                                               
CLTTAB   CSECT                                                                  
         DS    CL6                 AGY/MED/CLT                                  
         DS    50000C              25 X 2000 CLIENTS                            
CLTTABX  EQU   *                                                                
PRDTAB   CSECT                                                                  
*NOP*    DS    11500C              23 X 500 PRODUCTS                            
*NOP*    DS    13800C              23 X 600 PRODUCTS (04/30/01)                 
*NOP*    DS    23000C             23 X 1000 PRODUCTS (01/15/03)                 
         DS    34500C             23 X 1500 PRODUCTS (05/04/06)                 
PRDTABX  EQU   *                                                                
*                                                                               
BUYOUTA  CSECT                                                                  
         DS    600C           OUTPUT AREA FOR PPBYOUT                           
         EJECT                                                                  
VENWORK  CSECT                                                                  
         DS    8100C                                                            
VENWORKX EQU   *                                                                
*                                                                               
CLTDSCT  DSECT                                                                  
CLTTOTS  DS    0D                                                               
CLTINS   DS    45PL8                                                            
CLTLINES DS    45PL8                                                            
CLTGO    DS    45PL8                                                            
CLTGLAC  DS    45PL8                                                            
CLTCD    DS    45PL8                                                            
CLTNP    DS    45PL8                                                            
*                                                                               
RPTDSCT  DSECT                                                                  
RPTTOTS  DS    0D                                                               
RPTINS   DS    45PL8                                                            
RPTLINES DS    45PL8                                                            
RPTGO    DS    45PL8                                                            
RPTGLAC  DS    45PL8                                                            
RPTCD    DS    45PL8                                                            
RPTNP    DS    45PL8                                                            
RPTPUBS  DS    45PL8                                                            
*                                                                               
REPDSCT  DSECT                                                                  
REPTOTS  DS    0D                                                               
REPINS   DS    45PL8                                                            
REPLINES DS    45PL8                                                            
REPGO    DS    45PL8                                                            
REPGLAC  DS    45PL8                                                            
REPCD    DS    45PL8                                                            
REPNP    DS    45PL8                                                            
REPPUBS  DS    45PL8                                                            
*                                                                               
VENWORKD DSECT                                                                  
STRHIYR  DS    XL1                                                              
MTHACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
PUBACT   DS    CL1                                                              
REPACT   DS    CL1                                                              
RPTACT   DS    CL1                                                              
PUBSW    DS    CL1              SET TO X'01' IF I SHOULD PROCESS PUB            
PUBPSW   DS    CL1                                                              
PRDSW    DS    CL1                                                              
LASTYM   DS    CL2                                                              
PAIDSW   DS    CL1                                                              
ELCODE   DS    CL1                                                              
SAVELINE DS    CL1                                                              
SAVEYMD  DS    CL3                                                              
SAVECLT  DS    CL3                                                              
SAVEPRD  DS    CL3                                                              
SAVEPUB  DS    CL6                                                              
RELO     DS    F                                                                
WKDUB    DS    PL8                                                              
RUNPROF  DS    CL16            PROGPROF SAVE AT FBUYREQ                         
PPGKEY   DS    CL32                                                             
STKEY    DS    CL32                                                             
REQERR   DS    CL1                                                              
REQST    DS    CL3                                                              
REQEND   DS    CL3                                                              
ASOFDTE  DS    CL3                    AS OF DATE YMD                            
ASDATE   DS    CL8           AS  OF DATE MMDD/YY                                
*                                                                               
CDPDATE  DS    XL3    FIRST DAY OF 3RD MONTH BEFORE QSTART                      
*                     CONTROLS THE IGNORING OF PUB CD EFFECTIVE DATE            
REQEST   DS    H                                                                
REQPUB   DS    CL6                                                              
SAVEPNAM DS    CL20                                                             
SAVEENAM DS    CL20                                                             
SAVEEST  DS    CL3                                                              
MTHTAB   DS    CL368                                                            
SAVPLIN1 DS    CL132                                                            
SAVPLIN2 DS    CL132                                                            
*                                                                               
ACCNUM   EQU   45            3 YRS + 3 MTHS BACK +6 MTHS FORWARD =45            
*                                                                               
VOFFICER DS    A                   A(OFFICER)                                   
VPRNTOFC DS    A                   A(PRNTOFC)                                   
*                                                                               
CLTMTHS  DS    F                                                                
PUBCLTS  DS    F                                                                
PUBMTHS  DS    F                                                                
PRDMTHS  DS    F                                                                
PUBPRDS  DS    F                                                                
RTOTPUBS DS    F                                                                
*                                                                               
AADCGWRK DS    A                                                                
*                                                                               
VMTHEND  DS    V                                                                
VPRDEND  DS    V                                                                
VPUBEND  DS    V                                                                
VCLTEND  DS    V                                                                
VRPTEND  DS    V                                                                
VPRINTIT DS    V                                                                
VCLTTAB  DS    V                                                                
VBLDMLST DS    V                                                                
VPUBFRT  DS    V                                                                
VPUBEDIT DS    V                                                                
VPUBFLT  DS    V                                                                
VPRNTPUB DS    V                                                                
VPRDF    DS    V                                                                
VCLTF    DS    V                                                                
VREPEND  DS    V                                                                
APRDTAB  DS    V                                                                
ABUYOUTA DS    V                                                                
VCLTCSCT DS    V                                                                
VRPTCSCT DS    V                                                                
VREPCSCT DS    V                                                                
VPPGETCG DS    V                   NEW 3/26/01                                  
*                                                                               
TOTALS   DS    5D                                                               
*                                                                               
BUYTOTS  DS    0D             BUY LINE TOTALS                                   
BUYGO    DS    F                                                                
BUYGLAC  DS    F                                                                
BUYCD    DS    F                                                                
BUYNP    DS    F                                                                
MTHTOTS  DS    0D             MONTH TOTALS                                      
MTHINS   DS    PL8                                                              
MTHLINES DS    PL8                                                              
MTHGO    DS    PL8                                                              
MTHGLAC  DS    PL8                                                              
MTHCD    DS    PL8                                                              
MTHNP    DS    PL8                                                              
*                                                                               
PRDTOTS  DS    0D                                                               
PRDINS   DS    PL8                                                              
PRDLINES DS    PL8                                                              
PRDGO    DS    PL8                                                              
PRDGLAC  DS    PL8                                                              
PRDCD    DS    PL8                                                              
PRDNP    DS    PL8                                                              
*                                                                               
PUBTOTS  DS    0D                                                               
PUBINS   DS    45PL8                                                            
PUBLINES DS    45PL8                                                            
PUBGO    DS    45PL8                                                            
PUBGLAC  DS    45PL8                                                            
PUBCHCD  DS    45PL8                                                            
PUBNP    DS    45PL8                                                            
*                                                                               
PROTOTS  DS    0D                                                               
PROINS   DS    45PL8                                                            
PROLINES DS    45PL8                                                            
PROGO    DS    45PL8                                                            
PROGLAC  DS    45PL8                                                            
PROCHCD  DS    45PL8                                                            
PRONP    DS    45PL8                                                            
*                                                                               
ADCGWRK  DS    CL(ADCDLNTH)   FOR ADDITIONAL CHARGES PRINT BLOCK                
*                             OUTPUT BY PPGETCG                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
       ++INCLUDE PPGETCGD     ADDCHGD DSECT FOR ADCGWRK                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPREP3702 07/09/14'                                      
         END                                                                    
