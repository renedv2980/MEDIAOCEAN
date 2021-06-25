*          DATA SET PPREP3602  AT LEVEL 019 AS OF 11/28/05                      
*PHASE PP3602A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE PRNTOFC                                                                
         TITLE 'PP3602      VENDOR SUMMARY'                                     
*        CHANGE LOG                                                             
*                                                                               
*  BOBY 11/05    2 CH MEDIA OFFICE CODES                                        
*                                                                               
*  SMYE 10/04/00 EXPANDED CLTTAB TO 87500C (25 X 3500 CLIENTS)                  
*                                                                               
* BPLA  6/99     ALTER CD/NON-CD PUBS ONLY FEATURE TO IGNORE                    
*                CD EFFECTIVE DATE IF IT 3 MONTHS OR MORE                       
*                BEFORE THE REQUEST START DATE                                  
*                                                                               
*  SMYE 09/30/97  MOVED OFFOUT TO BLDMSLT FOR "ONE-TIME-ONLY" CALL              
*                 PER REQUEST CARD FOR OFFICE REQUESTS - WAS DYING              
*                 IN PRREQREP ON ANY MORE THAN 1 OFFICE REQUEST                 
*                 AFTER COMPLETING 1ST OFFICE RUN WHEN OFFOUT WAS               
*                 CALLED FOR EACH NEW PAGE IN PRINTIT                           
*                                                                               
*  SMYE 11/18/96  USED OFFOUT FOR CLIENT OFFICE DISPLAY                         
*                                                                               
*  SMYE 01/12/96  MODIFIED MTHTAB HANDLING FOR YEARS AFTER 1999                 
*                                                                               
*  SMYE 12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                      
*                                                                               
*  BPLA 10/20/92  NEW PROFILE OPTION FOR "FREE" BUYS                            
*          PROGPROF+1  "Y" = SUPPRESS FREE BUYS ON UNPAID REQS                  
*          PROGPROF+2  "Y" = SUPPRESS FREE BUYS ON PAID REQS                    
*          PROGPROF+3  "Y" = SUPPRESS FREE BUYS ON ALL ITEM REQS                
*                                                                               
*                                                                               
*              OPT1 P=PAID ITEMS ONLY                                           
*                   U=UPAID ITEMS ONLY                                          
*                   BLANK=ALL ITEMS                                             
*              OPT3 C=CD PUBS ONLY                                              
*                   N=NON CD PUBS ONLY                                          
*                   BLANK=ALL PUBS                                              
*                                                                               
*              OPT4 P=SUPPRESS PUB DETAILS (BREAKOUT)                           
*                     SORT WILL BE SET TO NO IN REQ                             
*                     BUYS WILL NOT BE SORTED                                   
*                   C=SUPPRESS CLIENT BREAKOUT                                  
*              OPT5 Y=ONE PUB PER PAGE (DEFAULT)                                
*                   N=MULTIPLE PUBS PER PAGE                                    
*                                                                               
*              OPT6 M=SUPPRESS MONTH BREAKOUT                                   
*                                                                               
*                                                                               
PP3602   CSECT                                                                  
         NMOD1 0,PP3602,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R5,SPACEND                                                       
         USING VENWORKD,R5                                                      
         CLI   MODE,FBUYREQ                                                     
         BNE   PROCESS                                                          
         BAS   R9,BLDMLST          BUILD MONTH LIST (12 MONTHS MAX)             
         B     EXT                                                              
*                                                                               
PROCESS  CLI   MODE,PROCBUY                                                     
         BNE   PP26A                                                            
         CLI   QOPT4,C'P'          SEE IF SHOWING PUB DETAIL                    
         BNE   PROCB5                                                           
         OC    KEY+21(3),KEY+21    IGNORE PASSIVE POINTERS                      
         BNZ   EXT                                                              
PROCB5   CLI   PUBSW,1       CHECKS IF PROCESSING THIS PUB                      
         BNE   EXT        NO - EXIT                                             
CKPAID   GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         CLI   QBPDATE,C'B'                                                     
         BE    CKBDATE                                                          
         CLI   QBPDATE,C'P'                                                     
         BNE   CKQOPT1                                                          
         CLC   PBDPDATE,REQST                                                   
         BL    NEXTBUY                                                          
         CLC   PBDPDATE,REQEND                                                  
         BH    NEXTBUY                                                          
         B     CKQOPT1                                                          
*                                                                               
CKBDATE  CLC   BLBLDT,REQST                                                     
         BL    NEXTBUY                                                          
         CLC   BLBLDT,REQEND                                                    
         BH    NEXTBUY                                                          
*                                                                               
CKQOPT1  DS    0H                                                               
         CLI   QOPT1,C' '      SEE IF DOING PAID OR UNPAID REQ                  
         BNE   CKOPT1A                                                          
         CLI   PROGPROF+3,C'Y' SUPPRESSING FREE BUYS ON ALL ITEM REQ            
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
         CLI   PROGPROF+2,C'Y'  SUPPRESSING "FREE" BUYS ON PAID REQ             
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
         CLC   PBUYKDAT,=X'5C0901'   SEE IF BEFORE SEP1/92                      
         BL    NEXTBUY               THEN DON'T REPORT AS UNPAID                
         CLI   PROGPROF+1,C'Y'  SEE IF SKIPPING "FREE" ON UNPAID REQ            
         BE    NEXTBUY                                                          
*                                                                               
CKUNPD5  B     BUYOK                                                            
*                                                                               
*                                                                               
BUYOK    MVC   MTHACT(4),=4C'Y'                                                 
         MVI   RPTACT,C'Y'                                                      
         LM    R6,R9,GROSS                                                      
         STM   R6,R9,BUYGO                                                      
         SR    R6,R7                                                            
         ST    R6,BUYGLAC                                                       
         OC    PGROSS(12),PGROSS                                                
         BZ    RLMTH                    ADD TO MTH TOTALS                       
         CLI   QOPT1,C'P'                                                       
         BNE   PRTPPD                                                           
         CLC   GROSS(12),PGROSS                                                 
         BE    RLMTH               TOTALLY PAID SO ADD TO MTH ACCUMS            
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
PRTPPD   CLI   QOPT1,C'U'                                                       
         BE    *+12                                                             
         CLI   QOPT1,C'P'                                                       
         BNE   RLMTH        IF DOING ALL ITEMS - NO ADJUSTMENT                  
*                                                                               
         LM    R3,R4,PGROSS                                                     
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
*                                                                               
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
*                                                                               
RLMTH    CLI   PBDSPACE,C'*'      SEE IF REAL INSERTION                         
         BE    RLMTH1              NO - BYPASS INS TOTALS                       
         L     R3,MTHINS                                                        
         AH    R3,=H'1'                                                         
         ST    R3,MTHINS                                                        
RLMTH1   CLI   QMEDIA,C'N'         IF MAG                                       
         BNE   RLMTH2              BYPASS LINES                                 
         L     R3,MTHLINES                                                      
*                                                                               
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
*                                                                               
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
         A     R3,UNITS                                                         
         ST    R3,MTHLINES                                                      
RLMTH2   L     R3,MTHGO                                                         
         A     R3,BUYGO                                                         
         ST    R3,MTHGO                                                         
         L     R3,MTHGLAC                                                       
         A     R3,BUYGLAC                                                       
         ST    R3,MTHGLAC                                                       
         L     R3,MTHCD                                                         
         A     R3,BUYCD                                                         
         ST    R3,MTHCD                                                         
         L     R3,MTHNP                                                         
         A     R3,BUYNP                                                         
         ST    R3,MTHNP                                                         
         MVC   SAVECLT,PBUYKCLT                                                 
         MVC   SAVEYMD,BLBLDT                                                   
         CLI   QBPDATE,C'B'                                                     
         BE    ROLLBUY                                                          
         MVC   SAVEYMD,PBUYKDAT                                                 
         B     ROLLBUY                                                          
*                                                                               
*    ROLL TO CLT TOTALS                                                         
*                                                                               
ROLLBUY  L     R3,CLTMTHS                                                       
         A     R3,=F'1'                                                         
         ST    R3,CLTMTHS                                                       
*                                                                               
         MVI   PUBACT,C'Y'                                                      
         MVI   CLTACT,C'Y'                                                      
         MVI   RPTACT,C'Y'                                                      
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
MTHEND4  L     R9,VCRPCSCT                                                      
         USING CRPDSCT,R9                                                       
         LA    R3,6                                                             
         LA    R6,CLTINS                                                        
         LA    R7,MTHINS                                                        
         LA    R6,0(R4,R6)                                                      
MTHEND5  L     R8,0(R7)                                                         
         CVD   R8,DUB                                                           
         AP    0(8,R6),DUB                                                      
         LA    R6,ACCNUM*8(R6)                                                  
         LA    R7,4(R7)                                                         
         BCT   R3,MTHEND5                                                       
*                                                                               
         DROP  R9                                                               
*                                                                               
         XC    MTHTOTS(24),MTHTOTS      CLEAR  MTH ACCUMS                       
         XC    SAVEYMD,SAVEYMD                                                  
NEXTBUY  B     EXT                                                              
         EJECT                                                                  
PP26A    CLI   MODE,LBUYCLI                                                     
         BNE   PP26B                                                            
         GOTO1 VCLTEND                                                          
         B     EXT                                                              
*                                                                               
PP26B    CLI   MODE,LBUYPUB                                                     
         BNE   PP26C                                                            
         CLI   QOPT4,C'P'          SEE IF SUPPRESSING PUB DETAILS               
         BE    EXT                 YES  DO NOTHING AT PUBEND                    
         GOTO1 VPUBEND                                                          
         B     EXT                                                              
*                                                                               
PP26C    CLI   MODE,FBUYPUB                                                     
         BNE   PP26D                                                            
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
         BE    EXT       WILL BYPASS BUYS FOR THIS PUB SINCE PUBSW 0            
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
         B     EXT       WILL BYPASS BUYS FOR THIS PUB SINCE PUBSW              
*                                                                               
NCHDIS   CLI   QOPT3,C'C'                                                       
         BE    EXT       WILL BYPASS BUYS FOR THIS PUB SINCE PUBSW 0            
         B     SETFLT                                                           
*                                                                               
*                                                                               
*                                                                               
SETFLT   MVI   PUBSW,1                                                          
         CLI   QOPT4,C'P'          SEE IF SUPPRESSING PUB DETAIL                
         BE    EXT                 YES - SKIP PUB PRINTING                      
         CLI   QOPT5,C'Y'          CHK ONE PUB PER PAGE                         
         BNE   *+8                 NO                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,C' '             OLD PUB COULD STILL BE HERE                   
         MVC   P+1(70),P             IF NO ACTIVITY                             
         MVC   PSECOND+1(17),SPACES                                             
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(0,P+1),RR=RELO                     
SETR3    LA    R3,P+1                                                           
         LA    R4,PSECOND+1                                                     
SETR3A   CLI   0(R3),C' '                                                       
         BE    FLOAT                                                            
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     SETR3A                                                           
*                                                                               
FLOAT    LA    R3,1(R3)                                                         
         GOTO1 =V(PUBFLOAT),DMCB,PUBREC,(R3),RR=RELO                            
*                                                                               
         MVC   SAVEPUB,PBUYKPUB                                                 
         DROP  R7                                                               
         DROP  R3                                                               
*                                                                               
*           PUB NUMBER AND NAME ARE SAVED IN P AND PSECOND                      
*                                                                               
PP26D    CLI   MODE,LBUYREQ                                                     
         BNE   EXT                                                              
         GOTO1 VRPTEND                                                          
         B     EXT                                                              
         EJECT                                                                  
         DC    F'0'                                                             
BLDMLST  ST    R9,BLDMLST-4                                                     
         XC    MTHTAB(250),MTHTAB       LINKED VIA R9.                          
         XC    MTHTAB+250(L'MTHTAB-250),MTHTAB+250                              
         MVI   FORCEHED,C'Y'                                                    
         OI    DMINBTS,X'08'        SET TO PASS DELETED RECS                    
         L     RF,=V(PUBEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPUBEND                                                       
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
         L     RF,=V(CRPCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VCRPCSCT                                                      
*                                                                               
         CLI   QCLIENT,C'*'        OFFICE REQUEST ?                             
         BNE   BLDM2               NO                                           
*                                                                               
         L     RF,=V(PRNTOFC)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRNTOFC                                                      
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
*                SAVE QCLIENT+1 (OR 2-CHAR HEX TRANSLATION IN SVOFFHD)          
         GOTO1 VPRNTOFC,DMCB,QCLIENT+1,(C'L',SVOFFHD),VOFFICER,QAGENCY,X        
               VCOMFACS                                                         
*                                                                               
BLDM2    CLI   QOPT4,C'P'          SEE IF SUPPRESSING PUB                       
         BNE   BLDM5               NO                                           
         MVI   FCRDBUY,C'Y'                                                     
         CLC   QPRODUCT,=C'   '                                                 
         BNE   *+10                                                             
         MVC   QPRODUCT,=C'ALL'                                                 
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
         MVI   FCRDBUY,X'21'       USE 21 POINTERS                              
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
         XC    8(8,R4),8(R4)       CLEAR LAST MTH                               
         B     MTHDONE             EXIT                                         
*                                                                               
SAMEYR   OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         MVC   8(2,R4),0(R4)                                                    
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE                                                          
         XC    8(8,R4),8(R4)                                                    
         B     MTHDONE             EXIT                                         
*                                                                               
NEXTMTH  LA    R4,8(R4)                                                         
         BCT   R6,PUTMTH                                                        
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
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-75'                                    
*        SUBTRACTING 75 DAYS WILL ALWAYS GET ME BACK 3 MONTHS                   
         MVC   WORK+10(2),=C'01'    SET DAY TO THE 1ST                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,CDPDATE)                               
*                                                                               
*        CDPDATE SHOULD NOW BE SET TO THE FIRST DAY OF                          
*        OF THE OF THE 3RD MONTHT PRIOR TO THE REQUEST                          
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
MTHD1    XC    MTHTOTS(24),MTHTOTS NOW CLEAR ALL ACCUMS                         
         XC    PRDTOTS(24),PRDTOTS                                              
         LA    R6,ACCNUM*6                                                      
         LA    R4,PUBTOTS                                                       
CLRPUB   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPUB                                                        
*                                                                               
         L     R9,VCRPCSCT                                                      
         USING CRPDSCT,R9                                                       
         LA    R6,ACCNUM*6                                                      
         LA    R4,CLTTOTS                                                       
CLRCLT   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRCLT                                                        
*                                                                               
         LA    R6,ACCNUM*7                                                      
         LA    R4,RPTTOTS                                                       
CLRRPT   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRRPT                                                        
*                                                                               
         DROP  R9                                                               
*                                                                               
         MVI   MTHACT,0            CLEAR ACTIVITY INDICATORS                    
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   CLTACT,0                                                         
         MVI   RPTACT,0                                                         
         XC    CLTMTHS,CLTMTHS                                                  
         XC    PUBCLTS,PUBCLTS                                                  
         XC    RTOTPUBS,RTOTPUBS                                                
*        GOTO1 DTCNV,DMCB,(0,QSTART),(1,REQST)                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(3,REQST)                                 
*        GOTO1 DTCNV,DMCB,(0,QEND),(1,REQEND)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(3,REQEND)                                  
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
         CLI   3(R6),C'$'          WAS LAST AN OFFICE LIST REQUEST              
         BE    CKCLTX              YES - NO NEED TO REBUILD                     
         CLI   3(R6),C'&&'         WAS LAST AN OFFICE REQ                       
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
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
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
         BE    BLDMX               DONE                                         
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
         B     EXT                                                              
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
         B     EXT                                                              
*                                                                               
HAVEST   GOTO1 GETEST                                                           
         MVC   SAVEENAM,PESTNAME                                                
         SR    R0,R0                                                            
         LH    R0,REQEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAVEEST(3),DUB+6(2)                                              
         MVC   KEY,STKEY                                                        
BLDMX    L     R9,BLDMLST-4                                                     
         BR    R9                  RETURN                                       
*                                                                               
*                                                                               
EXT      XMOD1 1                                                                
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
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
*                                                                               
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R5,SPACEND                                                       
         USING VENWORKD,R5                                                      
         MVI   RCSUBPRG,0                                                       
         CLI   QCLIENT,C'*'                                                     
         BNE   PRNT0                                                            
         MVC   HEAD5(6),=C'OFFICE'                                              
         MVC   HEAD5+7(24),SVOFFHD  SVOFFHD FROM PRNTOFC IN BLDMLST             
*****    MVC   HEAD5+7(2),QCLIENT+1                                             
*****    GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,(C'L',HEAD5+7)                     
         B     PRNT1                                                            
*                                                                               
PRNT0    CLI   QCLIENT,C'&&'                                                    
         BNE   PRNT0C                                                           
         MVC   HEAD5(5),=C'GROUP'                                               
         MVC   HEAD5+6(1),QCLIENT+1                                             
         B     PRNT1                                                            
*                                                                               
PRNT0C   CLI   QCLIENT,C'$'              OFFICE LIST                            
         BNE   PRNT1                                                            
         MVC   HEAD4(11),=C'OFFICE LIST'                                        
         MVC   HEAD4+12(1),QCLIENT+1                                            
*****    GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,(C'L',HEAD4+12)                    
*                                                                               
PRNT1    CLC   QPRODUCT,=C'ALL'                                                 
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
         MVC   HEAD10+4(9),=C'INSERTION'                                        
         CLI   QBPDATE,C'P'                                                     
         BNE   CKASOF                                                           
         MVC   HEAD4+43(19),=C'** PAYING PERIOD **'                             
         B     CKASOF                                                           
*                                                                               
BILMSG   MVC   HEAD4+43(20),=C'** BILLING PERIOD **'                            
         MVC   HEAD10+5(7),=C'BILLING'                                          
CKASOF   MVC   HEAD11+6(5),=C'MONTH'                                            
         MVC   HEAD12+4(9),=C'---------'                                        
         CLC   P+2(9),=C'** REPORT'     SEE IF DOING REPORT TOTALS              
         BE    CKASOF5                                                          
         CLI   QOPT6,C'M'          SEE IF SUPPRESSING MTH                       
         BNE   CKASOF5             NO                                           
         MVC   HEAD10+4(9),SPACES  YES BLANK OUT MTH                            
         MVC   HEAD11+6(5),SPACES                                               
         MVC   HEAD12+4(9),SPACES                                               
CKASOF5  DS    0H                                                               
         CLI   ASOFDTE,0                                                        
         BE    CKCLTS         AS OF DATE NOT USED                               
         MVC   HEAD5+45(5),=C'AS OF'                                            
         MVC   HEAD5+51(8),ASDATE                                               
CKCLTS   CLC   QCLIENT,=C'ALL'                                                  
         BE    PRINTX                                                           
         CLI   QCLIENT,C'*'        IF ALL CLTS OR OFFICE REQ                    
         BE    PRINTX                                                           
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    PRINTX                                                           
         CLI   QCLIENT,C'&&'       ALSO CHECK GROUP REQ                         
         BE    PRINTX                                                           
         ZIC   R2,RCSUBPRG         BUMP RCSUBPRG BY 10                          
         AH    R2,=H'10'           FOR SINGLE CLT REQS                          
         STC   R2,RCSUBPRG                                                      
PRINTX   GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBEND   CSECT                                                                  
         NMOD1 0,PUBEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R5,SPACEND                                                       
         USING VENWORKD,R5                                                      
         CLI   PUBACT,C'Y'                                                      
         BNE   PUBENDX                  NO ACTIVITY                             
         GOTO1 VCLTEND                                                          
*                                                                               
         CLI   QOPT4,C'C'          SEE IF SUPPRESSING CLIENTS                   
         BNE   PUBE5               NO                                           
PUBE3    CLC   PUBCLTS,=F'0'       YES - CHK FOR NO CLTS                        
         BE    PUBENDX                                                          
         CLC   QCLIENT,=C'ALL'                                                  
         BE    PUBE10                                                           
         CLI   QCLIENT,C'*'                                                     
         BE    PUBE10           IF ALL CLT OR OFFICE REQ DO PUB TOTAL           
         CLI   QCLIENT,C'$'                                                     
         BE    PUBE10           IF ALL OFFICE LIST DO PUB TOTAL                 
         CLI   QCLIENT,C'&&'                                                    
         BE    PUBE10           IF ALL CLT OR OFFICE REQ DO PUB TOTAL           
         B     PUBROLL             ELSE SKIP TO ACCUMS                          
*                                  BECAUSE CLT WILL HAVE BEEN PRINTED           
*                                                                               
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
         CLI   QOPT6,C'M'          SEE IF SUPPRESSING MTH BREAKOUT              
         BE    PUBEND3B            YES SKIP TO TOTALS                           
         CLI   QOPT4,C'C'          SEE IF SUPPRESSING CLIENT BREAKOUT           
         BE    PUBEND0             SKIP THIS LINE                               
         MVC   P+2(16),=C'** PUB TOTALS **'                                     
         GOTO1 VPRINTIT                                                         
PUBEND0  XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBEND1  LA    R6,PUBINS                                                        
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
         LA    R2,PUBINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    NOPINS              NO INSERTIONS                                
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPINS   LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    PRTMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PRTMTHA                                                          
         EDIT  (P8,(R2)),(10,P+40),2                                            
         MVI   P+50,C'I'                                                        
         B     PRTMTH1                                                          
PRTMTHA  EDIT  (P8,(R2)),(8,P+42),0                                             
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
         BE    NOPTINS             NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPTINS  CLI   QMEDIA,C'N'                                                      
         BNE   PUBEND6                                                          
         ZAP   WKDUB,PUBLINES                                                   
         LA    R2,PUBLINES                                                      
         LA    R6,ACCNUM-1                                                      
PUBEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    PUBEND6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PUBEND5A                                                         
         EDIT  WKDUB,(10,P+40),2                                                
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     PUBEND6                                                          
PUBEND5A EDIT  WKDUB,(8,P+42),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
PUBEND6  ZAP   WKDUB,PUBGO                                                      
         LA    R2,PUBGO                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
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
         MVI   SPACING,2           ALWAYS PRINT TOTAL                           
PUBEND20 SR    R3,R3               AND SKIP A LINE                              
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*                                  ROLL TO REPORT TOTALS                        
PUBROLL  LA    R3,ACCNUM                                                        
         L     R9,VCRPCSCT                                                      
         LA    R8,4095(R9)                                                      
         LA    R8,1(R8)                                                         
         USING CRPDSCT,R9,R8                                                    
         LA    R4,0                                                             
PUBRA    LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
PUBRB    CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   RE,BUMPPUB                                                       
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
         BR    RE                                                               
*                                                                               
PUBRC    LA    R2,ACCNUM*6                                                      
         LA    R3,RPTINS                                                        
         LA    R4,PUBINS                                                        
PUBRD    AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBRD                                                         
         L     RE,RTOTPUBS                                                      
         AH    RE,=H'1'                                                         
         ST    RE,RTOTPUBS                                                      
*                                  CLEAR  PUB ACCUMS                            
PUBEND11 LA    R6,ACCNUM*6                                                      
         LA    R7,PUBINS                                                        
PUBEND12 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,PUBEND12                                                      
         XC    CLTMTHS,CLTMTHS                                                  
         XC    PUBCLTS,PUBCLTS                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   CLTACT,0                                                         
         MVI   PUBACT,0                                                         
PUBENDX  XIT1                                                                   
         DROP  R8                                                               
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
CLTEND   CSECT                                                                  
         NMOD1 0,CLTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R5,SPACEND                                                       
         USING VENWORKD,R5                                                      
         L     R9,VCRPCSCT                                                      
         USING CRPDSCT,R9                                                       
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
         CLI   QOPT5,C'Y'          CHK ONE PUB PER PAGE                         
         BNE   CLTB                NO -NO NEW PAGE                              
         MVI   FORCEHED,C'Y'                                                    
CLTB     GOTO1 VPRINTIT                                                         
CLTE     DS    0H                                                               
*                                                                               
         CLI   QOPT4,C'C'          SEE IF SUPPRESSING CLIENT BREAKOUT           
         BNE   CLTE2               NO - PROCESS                                 
CLTE1    CLC   QCLIENT,=C'ALL'     YES                                          
         BE    CLTROLL                                                          
         CLI   QCLIENT,C'*'        IF ALL CLTS OR OFFICE REQ                    
         BE    CLTROLL             SKIP TO ACCUMS                               
         CLI   QCLIENT,C'$'        IF OFICE LIST                                
         BE    CLTROLL             SKIP TO ACCUMS                               
         CLI   QCLIENT,C'&&'       IF ALL CLTS OR OFFICE OR GROUP               
         BE    CLTROLL             SKIP TO ACCUMS                               
*                                  ELSE PRINT CLIENT                            
*                                                                               
CLTE2    SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE         NEED 4 LINES                                 
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
CLTE2B   CLC   QCLIENT,=C'ALL'     YES                                          
         BE    CLTE2X                                                           
         CLI   QCLIENT,C'&&'       GROUP                                        
         BE    CLTE2X                                                           
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    CLTE2X                                                           
         CLI   QCLIENT,C'*'        IF ONE CLT                                   
         BNE   CLTE5               SKIP PRINTING OF CLT NAME                    
*                                  IT WILL NOW BE IN HEADLINES                  
CLTE2X   MVC   P+1(6),=C'CLIENT'                                                
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
         GOTO1 VPRINTIT                                                         
CLTE5    CLI   QOPT6,C'M'          SEE IF SUPPRESSING MTH BREAKOUT              
         BE    CLTEND5B            SKIP TO TOTALS                               
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
         LA    R1,CLTINS                                                        
         LA    R1,0(R4,R1)                                                      
         CP    0(8,R1),=P'0'                                                    
         BE    NOCINS              NO INSERTIONS                                
         EDIT  (P8,(R1)),(5,P+23),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   ACTIV1                                                           
         CP    0(8,R1),=P'0'                                                    
         BE    ACTIV1              NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   ACTIVA                                                           
         EDIT  (P8,(R1)),(10,P+40),2                                            
         MVI   P+50,C'I'                                                        
         B     ACTIV1                                                           
ACTIVA   EDIT  (P8,(R1)),(8,P+42),0                                             
         MVI   P+50,C'L'                                                        
ACTIV1   LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+94),2,COMMAS=YES,MINUS=YES                       
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
CLTEND5B ZAP   WKDUB,CLTINS                                                     
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
         L     R2,CLTLINES                                                      
         ZAP   WKDUB,CLTLINES                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTLINES+8                                                    
CLTEND7  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND7                                                       
         CP    WKDUB,=P'0'                                                      
         BE    CLTEND8             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   CLTEND7A                                                         
         EDIT  WKDUB,(10,P+40),2                                                
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     CLTEND8                                                          
CLTEND7A EDIT  WKDUB,(8,P+42),0                                                 
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
         CP    WKDUB,=P'99999999'        SEE IF CD OVER 999,999.99              
*                                        THEN NO COMMAS                         
         BH    CLTE11B                                                          
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         B     CLTE11C                                                          
CLTE11B  EDIT  WKDUB,(12,P+82),2,MINUS=YES                                      
CLTE11C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         L     R2,CLTNP                                                         
         ZAP   WKDUB,CLTNP                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTNP+8                                                       
CLTEND12 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND12                                                      
         CP    WKDUB,=P'9999999999'        SEE IF OVER 99,999,999.99            
         BH    CLTE12B                     THEN NO COMMAS                       
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         B     CLTE12C                                                          
CLTE12B  EDIT  WKDUB,(14,P+94),2,MINUS=YES                                      
CLTE12C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         MVI   SPACING,2                                                        
CLTEND20 SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*                  ROLL TO PUB ACCUMS OR RPT TOTALS IF QOPT4=P                  
CLTROLL  LA    R2,ACCNUM*6                                                      
         LA    R3,PUBINS                                                        
         CLI   QOPT4,C'P'          SEE IF SUPPRESSING PUB DETAILS               
         BNE   *+8                                                              
         LA    R3,RPTINS           YES ROLL TO REPORT ACCUMS                    
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
*                                                                               
         DROP  R9                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
RPTEND   CSECT                                                                  
         NMOD1 0,RPTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R5,SPACEND                                                       
         USING VENWORKD,R5                                                      
         L     R9,VCRPCSCT                                                      
         USING CRPDSCT,R9                                                       
         DS    0H                  PRINT REPORT TOTALS                          
         CLI   RPTACT,C'Y'                                                      
         BNE   RPTEXT                                                           
         CLI   QOPT4,C'P'          SEE IF SUPPRESSING PUB DETAILS               
         BE    RPTE                YES - CHK CLIENT                             
         CLI   QPUB,C' '                                                        
         BNH   RPTE0               ONE PUB SO NO REPORT TOTALS                  
         CLC   QPUB+8(3),=C'ZZZ'   UNLESS ALL ZONES/EDTS                        
         BE    RPTE0                                                            
         B     RPTEXT                                                           
*                                                                               
RPTE     CLC   QCLIENT,=C'ALL'                                                  
         BE    RPTE0                                                            
         CLI   QCLIENT,C'*'       OFFICE REQ                                    
         BE    RPTE0                                                            
         CLI   QCLIENT,C'$'       OFFICE LIST                                   
         BE    RPTE0                                                            
         CLI   QCLIENT,C'&&'      GROUP REQ                                     
         BE    RPTE0                                                            
         B     RPTEXT                                                           
*                                                                               
RPTE0    MVI   FORCEHED,C'Y'                                                    
         MVI   P,C' '             OLD PUB COULD STILL BE HERE                   
         MVC   P+1(70),P             IF NO ACTIVITY                             
         MVC   PSECOND+1(17),SPACES                                             
         MVC   P+1(20),=C' ** REPORT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
RPTE1    LA    R2,RPTINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,7                                                             
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
         CLI   QOPT4,C'P'          SEE IF SUPPRESSING PUB DETAILS               
         BE    RACTV5              YES - CAN'T COUNT PUBS                       
*                                                                               
         LA    R7,4095(R9)                                                      
         LA    R7,1(R7)                                                         
         USING CRPDSCT+4096,R7                                                  
         LA    R1,RPTPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         EDIT  (P8,(R1)),(4,P+12),0                                             
         MVC   P+17(4),=C'PUBS'                                                 
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
*                                                                               
         DROP  R7                                                               
*                                                                               
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
         CLI   PROGPROF,C'I'                                                    
         BNE   RACTIVA                                                          
         EDIT  (P8,(R1)),(10,P+40),2                                            
         MVI   P+50,C'I'                                                        
         B     RACTIV1                                                          
RACTIVA  EDIT  (P8,(R1)),(8,P+42),0                                             
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
         CLI   QOPT4,C'P'          SEE IF SUPPRESSING PUB DETAILS               
         BE    RPTE5B              YES - CAN'T COUNT PUBS                       
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
         EDIT  WKDUB,(5,P+23),0                                                 
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
         CLI   PROGPROF,C'I'                                                    
         BNE   RPTE7A                                                           
         EDIT  WKDUB,(10,P+40),2                                                
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     RPTE8                                                            
RPTE7A   EDIT  WKDUB,(8,P+42),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
RPTE8    ZAP   WKDUB,RPTGO                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTGO+8                                                       
RPTE8A   AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,RPTE8A                                                        
         EDIT  WKDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,RPTGLAC                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTGLAC+8                                                     
         AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,*-10                                                          
         EDIT  WKDUB,(15,P+67),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,RPTCD                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTCD+8                                                       
         AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,*-10                                                          
         CP    WKDUB,=P'99999999'   IF OVER 999,999.99                          
         BNH   RPTE8C                NO COMMAS                                  
         EDIT  WKDUB,(12,P+82),2,MINUS=YES                                      
         B     RPTE8D                                                           
RPTE8C   EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
RPTE8D   CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   WKDUB,RPTNP                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,RPTNP+8                                                       
         AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,*-10                                                          
         CP    WKDUB,=P'9999999999' IF OVER 99,999,999.99                       
         BNH   RPTE8G                NO COMMAS                                  
         EDIT  WKDUB,(14,P+94),2,MINUS=YES                                      
         B     RPTE8H                                                           
RPTE8G   EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
RPTE8H   CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         GOTO1 VPRINTIT                                                         
         B     RPTEXT                                                           
RPTEXT   XIT1                                                                   
         LTORG                                                                  
*                                                                               
CRPCSCT  CSECT                                                                  
         DS    4992C               45 X 8 X 13 = 4992                           
CRPCSCTX EQU   *                                                                
*                                                                               
*                                                                               
CLTTAB   CSECT                                                                  
         DS    CL6                 AGY/MED/CLT                                  
*****    DS    75000C              25 X 3000 CLIENTS                            
         DS    87500C              25 X 3500 CLIENTS (10/04/00)                 
CLTTABX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
VENWORKD DSECT                                                                  
SVOFFHD  DS    CL24    OFFOUT STORES QCLIENT+1 HERE FOR QCLIENT=*               
*                                    OFFICE NAME INCLUDED                       
STRHIYR  DS    XL1                                                              
MTHACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
PUBACT   DS    CL1                                                              
RPTACT   DS    CL1                                                              
PUBSW    DS    CL1              SET TO X'01' IF I SHOULD PROCESS PUB            
PAIDSW   DS    CL1                                                              
ELCODE   DS    CL1                                                              
SAVELINE DS    CL1                                                              
SAVEYMD  DS    CL3                                                              
SAVECLT  DS    CL3                                                              
SAVEPUB  DS    CL6                                                              
PPGKEY   DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
SAVEPKEY DS    CL32                                                             
STKEY    DS    CL32                                                             
REQERR   DS    CL1                                                              
REQST    DS    CL3                                                              
REQEND   DS    CL3                                                              
CDPDATE  DS    CL3           PUB CD EFF. DATE CHECK DATE                        
ASOFDTE  DS    CL3                    AS OF DATE YMD                            
ASDATE   DS    CL8           AS  OF DATE MMDD/YY                                
WKDUB    DS    PL8                                                              
REQEST   DS    H                                                                
REQPUB   DS    CL6                                                              
SAVEPNAM DS    CL20                                                             
SAVEENAM DS    CL20                                                             
SAVEEST  DS    CL3                                                              
MTHTAB   DS    CL368                                                            
VOFFICER DS    A                   A(OFFICER)                                   
VPRNTOFC DS    A                   A(PRNTOFC)                                   
*                                                                               
ACCNUM   EQU   45            3 YRS + 3 MTHS BACK +6 MTHS FORWARD =45            
*                                                                               
CLTMTHS  DS    F                                                                
PUBCLTS  DS    F                                                                
PUBMTHS  DS    F                                                                
*                                                                               
VMTHEND  DS    V                                                                
VPRDEND  DS    V                                                                
VPUBEND  DS    V                                                                
VCLTEND  DS    V                                                                
VPRINTIT DS    V                                                                
VPAYWORK DS    V                                                                
VCLTTAB  DS    V                                                                
VRPTEND  DS    V                   SPARE                                        
VCRPCSCT DS    V                                                                
VOFFOUT  DS    V                   SPARE                                        
*          DATA SET PPUNBUYD   AT LEVEL 004 AS OF 11/18/76                      
         SPACE                                                                  
* DSECT FOR PPUNBUY FIELD EXPANSIONS                                            
         SPACE                                                                  
PPUNBPRT DS    H                   NUMBER OF PRINT LINES REQUIRED               
PPUNBDT  DS    CL8       MN        INSERTION DATE                               
PPUNBLNS DS    CL5        N        LINEAGE                                      
PPUNBRT  DS    CL8        N        LINE RATE                                    
PPUNBPR  DS    CL10       N        PREMIUM                                      
PPUNBGR  DS    CL12      MN        GROSS                                        
         DS    CL5                 SPARE                                        
PPUNBZZZ DS    CL50      MN        POOL ALLOCATIONS                             
PPUNBCOM DS    CL47                COMMENT                                      
PPUNBBFD DS    CL27                BFD COMMENT                                  
         DS    CL26                                                             
*                                                                               
* TOTAL LEN= 200 BYTES                                                          
         SPACE                                                                  
*                                                                               
*                                                                               
BUYTOTS  DS    0D             BUY LINE TOTALS                                   
BUYGO    DS    F                                                                
BUYGLAC  DS    F                                                                
BUYCD    DS    F                                                                
BUYNP    DS    F                                                                
MTHTOTS  DS    0D             MONTH TOTALS                                      
MTHINS   DS    F                                                                
MTHLINES DS    F                                                                
MTHGO    DS    F                                                                
MTHGLAC  DS    F                                                                
MTHCD    DS    F                                                                
MTHNP    DS    F                                                                
*                                                                               
PRDTOTS  DS    0D                                                               
PRDINS   DS    F                                                                
PRDLINES DS    F                                                                
PRDGO    DS    F                                                                
PRDGLAC  DS    F                                                                
PRDCD    DS    F                                                                
PRDNP    DS    F                                                                
PUBTOTS  DS    0D                                                               
PUBINS   DS    45PL8                                                            
PUBLINES DS    45PL8                                                            
PUBGO    DS    45PL8                                                            
PUBGLAC  DS    45PL8                                                            
PUBCHCD  DS    45PL8                                                            
PUBNP    DS    45PL8                                                            
*                                                                               
RTOTPUBS DS    F                                                                
**                                                                              
CRPDSCT  DSECT                     TO COVER CRPCSCT                             
CLTTOTS  DS    0D                                                               
CLTINS   DS    45PL8                                                            
CLTLINES DS    45PL8                                                            
CLTGO    DS    45PL8                                                            
CLTGLAC  DS    45PL8                                                            
CLTCD    DS    45PL8                                                            
CLTNP    DS    45PL8                                                            
*                                  REPORT TOTALS                                
RPTTOTS  DS    0D                                                               
RPTINS   DS    45PL8                                                            
RPTLINES DS    45PL8                                                            
RPTGO    DS    45PL8                                                            
RPTGLAC  DS    45PL8                                                            
RPTCD    DS    45PL8                                                            
RPTNP    DS    45PL8                                                            
RPTPUBS  DS    45PL8                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPREP3602 11/28/05'                                      
         END                                                                    
