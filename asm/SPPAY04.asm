*          DATA SET SPPAY04    AT LEVEL 058 AS OF 10/17/19                      
*PHASE T21304C                                                                  
         TITLE 'T21304 - SPOTPAK PAY - BUYREC PROCESSING'                       
***********************************************************************         
*                                                                     *         
*                  M O D I F I C A T I O N S   L O G                  *         
*                                                                     *         
*-DATE----LVL-BY-----CHANGE-------------------------------------------*         
*                                                                     *         
* 10AUG98 08  NRK -- BUCKET PW OVERRIDE AMOUNTS.                      *         
* 22JUL98         -- HISTORY LOST.                                    *         
***********************************************************************         
T21304   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21304,RR=R8                                                   
         ST    R8,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
*                                                                               
         L     R3,VTWA                                                          
         USING T213FFD,R3                                                       
*                                                                               
         XC    SVCTADTA(SVCTADTX-SVCTADTA),SVCTADTA                             
         MVI   GSTCODE,0           CLEAR INITIAL GST CODE                       
         MVI   GOTSRC,C'N'         RESET SRC COMMENT FLAG                       
         XC    UNCLRSEQ,UNCLRSEQ   CLEAR UNPAY CLRSTAT SEQNUMS                  
*                                                                               
         OC    SVPWPCT,SVPWPCT     PW CLIENT                                    
         JZ    PAY1A               NO - SO CONTINUE                             
         CLI   SVEOWSD,0           TEST OWPW CLEARANCE                          
         BE    *+8                 NO                                           
         BRAS  RE,PWBKBLD          INIT AND BUILD PW OVERRIDE BUCKETS           
*                                                                               
PAY1A    LA    R2,PAYPRH                                                        
         MVI   ERRCD,NOBUYREC                                                   
         MVC   KEY,SVKEY           SAVE THE BUY KEY                             
*                                                                               
         CLI   SVPPROF+10,C'C'     TEST CCUSA                                   
         BE    PAY1F                                                            
*                                                                               
         MVI   RDUPDATE,C'Y'       SET READ FOR UPDATE                          
         GOTO1 HIGH                                                             
*                                                                               
         TM    QSTA,X'F0'          IF THIS IS CABLE                             
         BNO   PAY1E                                                            
*                                                                               
PAY1B    CLC   KEY(6),KEYSAVE      TEST A-M/CLT/PRD/MKT                         
         BNE   PAYERR                                                           
*                                                                               
         GOTO1 ,DMCB,(X'80',KEY+4),WORK,WORK+4                                  
         BRAS  RE,GOMSUNPK                                                      
         CLC   QSTA(4),WORK+4      COMPARE STATIONS                             
         BNE   PAYERR                                                           
         CLI   QNET,C'A'           IF NO NETWORK - CONTINUE                     
         BL    PAY1C                                                            
         CLC   QNET,WORK+9         ELSE COMPARE NETWORKS                        
         BNE   PAY1D                                                            
*                                                                               
PAY1C    CLI   SVKEY+9,0           TEST PAYING ALL ESTS                         
         BE    PAY2                                                             
         CLC   KEY+9(1),SVKEY+9    NO - SAME ESTIMATE                           
         BE    PAY2                                                             
*                                                                               
PAY1D    MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     PAY1B                                                            
*                                                                               
PAY1E    CLC   KEY(9),KEYSAVE      TEST A-M/CLT/PRD/MKT/STA                     
         BNE   PAYERR                                                           
*                                                                               
         CLI   SVKEY+9,0           TEST PAYING ALL ESTS                         
         BE    PAY2                YES                                          
         CLC   KEY+9(1),SVKEY+9    NO - SAME EST                                
         BNE   PAYERR                                                           
         B     PAY2                                                             
*                                                                               
PAY1F    MVC   KEY+9(1),SVXFREST   SET LOW EST                                  
         MVI   RDUPDATE,C'Y'       SET READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      TEST A-M/CLT/PRD/MKT/STA                     
         BNE   PAYERR                                                           
         CLC   KEY+9(1),SVXFRESX   TEST EST IN RANGE                            
         BH    PAYERR              NO                                           
         B     PAY2                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
RELO     DS    A                                                                
*                                                                               
PAYERR   GOTO1 ERROR                                                            
         SPACE 2                                                                
PAY2     DS    0H                                                               
         MVI   PASS,1                                                           
         XC    TOTG(52),TOTG       TOTG/TOTN/TOTGST/TOTPST                      
         XC    TOTSPOTS,TOTSPOTS                                                
         XC    RATETEST,RATETEST                                                
*                                                                               
PAY4     XC    BUYTOTG(52),BUYTOTG                                              
         MVI   BUYSPOTS,0          SET FLAG FOR UNPAID SPOT IN PER              
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
         L     R2,AREC1                                                         
         ST    R2,AREC                                                          
         USING BUYRECD,R2                                                       
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08'                                              
         TM    BUYRCNTL,X'80'      TEST REC DELETED                             
         BO    PAY54               YES - SKIP                                   
*                                                                               
         CLC   BUYKEY+4(2),KEY+4   TEST SPILL POINTER (DIFF MKTS)               
         BNE   PAY54                                                            
*                                                                               
         CLC   BDREP,SVSPREP       TEST SAME SPECIAL REP (OR NONE)              
         BNE   PAY54                                                            
*                                                                               
         TM    SVAFLAG1,X'20'      TEST CTA ACTIVE                              
         BZ    PAY8                NO                                           
         CLI   SVTRADE,C'Y'        TEST PAYING TRADE                            
         BNE   PAY6                NO                                           
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BZ    PAY54               NO - SKIP                                    
         B     PAY8                                                             
*                                                                               
PAY6     TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BO    PAY54               YES - SKIP                                   
*                                                                               
PAY8     MVI   ERRCD,BADNETDT                                                   
         TM    BDSTAT,X'01'        TEST NETPAK                                  
         BO    PAYERR                                                           
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
         CLI   SVPPROF+10,C'C'     TEST CCUSA                                   
         BE    PAY8CC                                                           
         OC    SVID,SVID           TEST PAYING BY ID                            
         BZ    PAY8X               NO                                           
         BRAS  RE,NEXTEL           GET ID ELEMENT                               
         BE    PAY8A               GOT IT                                       
         CLI   SVPPROFA+7,C'Y'     NO ID ELEMENT - TEST ID REQD                 
         BNE   PAY54               IF ID NOT REQD THEN ERROR                    
         CLC   =C'NONE ',SVID      'NONE' PAYS BUYS WITHOUT ID'S                
         BE    PAY8CC                                                           
PAY8A    OC    3(12,R6),SPACES     INSURE SPACES IN ELEMENT                     
         CLC   3(12,R6),SVID                                                    
         BE    PAY8X                                                            
         CLI   SVCXTRA+2,C'A'                                                   
         BL    PAY54                                                            
         CLI   SVCXTRA+2,C'K'                                                   
         BH    PAY54                                                            
* IF ID=MKTGRP THEN COMPARE 5 CHARACTERS ONLY                                   
         CLC   3(5,R6),SVID                                                     
         BE    PAY8X                                                            
         B     PAY54                                                            
*                                                                               
PAY8CC   CLI   SVXFRSW,C'N'        TEST TRANSFER ACTIVE                         
         BE    PAY8X               NO                                           
         BRAS   RE,NEXTEL          SEARCH FOR ID ELEM                           
         BNE   PAY8X               IF NO ID IN BUY, PROCESS IT                  
         CLC   XFRACN-PAYXFRD+SVXFRDTA(5),3(R6)  MATCH ACN                      
         BE    PAY8X                             IF YES PROCESS,                
         B     PAY54                             ELSE IGNORE BUY                
         EJECT                                                                  
PAY8X    MVI   OWRDAYS,0           RESET OUT-OF-WEEK ROT FLAG                   
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   PAY30                                                            
         CLI   SVPPROF+3,C'N'      IF MATCHING NOT REQUIRED                     
         BE    PAY9                ==>  NEVER DEFER SPOTS                       
         SR    R0,R0                                                            
         IC    R0,BDSEDAY                                                       
         SRDL  R0,4                                                             
         SRL   R1,28                                                            
         CR    R0,R1               TEST OUT-OF-WEEK ROT                         
         BNH   PAY9                NO                                           
         LA    R1,7(R1)            CALC DAYS TO END OF ROT                      
         SR    R1,R0                                                            
         STC   R1,OWRDAYS          SET OUT-OF-WEEK ROT FLAG                     
*                                                                               
PAY9     MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0D'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY10    EQU   *                                                                
*                                                                               
         MVI   PRUNPDFL,0          CLEAR THE 'UNPAID PRIOR MONTHS' FLAG         
         BRAS  RE,NEXTEL                                                        
         BNE   PAY50                                                            
*                                                                               
         OC    BDMGDATE,BDMGDATE   TEST BUY IS A MAKEGOOD                       
         BZ    PAY12                                                            
         CLI   SVCXTRA+7,C'Y'      TEST OPTION TO PAY MISSED DATE               
         BNE   PAY12                                                            
         CLC   BDMGDATE(2),SVMGDATE  COMPARE TO FEATURE START                   
         BL    PAY12                 IGNORE IF PRIOR                            
         MVC   RUNDATE(2),BDMGDATE   ELSE SET TO MISSED DATE                    
         B     PAY16                                                            
*                                                                               
PAY12    MVC   RUNDATE(2),2(R6)    MOVE ELEMENT DATE                            
         CLI   OWRDAYS,0           TEST OUT-OF-WEEK ROT                         
         BE    PAY16               NO                                           
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   PAY16               YES - NO DEFERRAL                            
         LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'         TEST SPOT MATCHED                            
         BNE   PAY14               NO                                           
* SPOT IS MATCHED - PAY IF AFFID DATE IN THIS MONTH *                           
         MVC   RUNDATE,2(R7)       SAVE AFFID DATE                              
         B     PAY16                                                            
         EJECT                                                                  
* SPOT IS NOT MATCHED - ADVANCE TO LAST DAY OF ROT TO TEST IN MONTH *           
*                                                                               
PAY14    CLC   RUNDATE,SVENDP      TEST SPOT AFTER MONTH                        
         BH    PAY10               YES - IGNORE                                 
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK                                      
         LLC   R0,OWRDAYS                                                       
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,RUNDATE)                                  
*                                                                               
PAY16    CLC   RUNDATE,SVSTARTP    TEST BEFORE START                            
         JNL   PAY17               NO - SO CONTINUE                             
*                                                                               
         OC    SVPWPCT,SVPWPCT     PW CLIENT?                                   
         JZ    PAY10               NO - SO GET NEXT ELEMENT                     
         CLI   SVEOWSD,0           ELSE - OOWR ESTIMATE?                        
         JE    PAY10               NO - SO GET NEXT ELEMENT                     
*                                                                               
         MVI   PRUNPDFL,1          SET 'PRIOR MONTHS UNPAID' FLAG               
*                                                                               
PAY17    EQU   *                                                                
*                                                                               
         CLC   RUNDATE,SVENDP      OR AFTER END                                 
         BH    PAY10                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   PAY10                                                            
         CLI   QPRD+3,X'FF'        TEST PAYING POL                              
         BE    PAY20               YES                                          
*MATCH ALLOCATION                                                               
         CLC   QPRD+3(1),10(R6)    MATCH PRD 1                                  
         BNE   PAY10                                                            
         CLI   1(R6),14            TEST P/B SPOT                                
         BH    PAY18               YES                                          
* NOT A P/B SPOT                                                                
         OC    QPRD2,QPRD2         TEST PAYING P/B SPOTS                        
         BNZ   PAY10               YES - SKIP                                   
         B     PAY20               ELSE PROCESS                                 
*                                                                               
* SPOT IS P/B                                                                   
*                                                                               
PAY18    OC    QPRD2,QPRD2         TEST PAYING P/B SPOTS                        
         BZ    PAY10               NO - SKIP                                    
         CLC   =C'ALL',QPRD2                                                    
         BE    PAY20                                                            
         CLC   QPRD2+3(1),14(R6)   MATCH PRD 2                                  
         BNE   PAY10                                                            
         B     PAY20                                                            
         EJECT                                                                  
PAY20    CLI   SVPPROF+3,C'X'      TEST IGNORE UNMATCHED SPOTS                  
         BNE   PAY21                                                            
         TM    SVTSTOPT,X'80'      TEST NOAFD OPTION ON                         
         BO    PAY21                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED SPOT                  
         BNZ   PAY21               PAY, SINCE IT CAN NEVER BE MATCHED           
         LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'         TEST AFFID FOLLOWS                           
         BNE   PAY10               NO - IGNORE                                  
*                                                                               
PAY21    OI    PAYELLO,X'80'       SET FLAG FOR SPOT IN PERIOD                  
*                                                                               
         BRAS  RE,TESTPD           TEST PAID                                    
         BNZ   PAY10                                                            
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+8                                                              
         BRAS  RE,POSTSEQ                                                       
*                                                                               
         MVC   12(1,R6),STATSEQ    SET SEQNUM (=0 IF UNPAY)                     
         CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION                            
         BE    *+10                                                             
         MVC   4(2,R6),TODAYP      SET PAY DATE IN ELEM                         
*                                                                               
         CLC   =C'NOCHECK',PAYER                                                
         BNE   *+10                                                             
         MVC   4(2,R6),SVPAYDT                                                  
*                                                                               
         OI    PASS,X'80'          SET RECORD UPDATED FLAG                      
         OI    PAYELLO,X'40'       SET FLAG FOR UNPAID SPOT                     
*                                                                               
         BRAS  RE,TSTRATE                                                       
         CLI   SVPPROF+3,C'Y'      TEST PAY ONLY IF MATCHED                     
         BNE   PAY24                                                            
         TM    SVTSTOPT,X'80'      TEST NOAFD OPTION ON                         
         BO    PAY24                                                            
*                                                                               
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   PAY24                                                            
*                                                                               
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BZ    PAY22A                                                           
*                                                                               
         TM    BDCIND2,BDC2NEG     NEGATIVE                                     
         BO    PAY24                                                            
         B     PAY22B                                                           
*                                                                               
PAY22A   TM    BDCIND,X'01'        TEST NEGATIVE RATE                           
         BO    PAY24               NEVER MATCH THESE                            
PAY22B   LLC   R7,1(R6)                                                         
         AR    R7,R6                                                            
         CLI   0(R7),X'10'                                                      
         BE    PAY24                                                            
* LOOK FOR MATCH=NO COMMENT                                                     
         ST    R6,DUB              SAVE ELEMENT ADDRESS                         
         MVC   DUB+4(2),ELCDLO     SAVE ELEMENT ARGS                            
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENT ELEM CODE                        
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY23A   BRAS  RE,NEXTEL                                                        
         BNE   PAY23C                                                           
         CLC   =C'MATCH=NO',3(R6)                                               
         BNE   PAY23A                                                           
         CLI   SVPPROFB+6,C'Z'     TEST CLEAR IF $0 AND UNMATCHED               
         BNE   PAY23B                                                           
         OC    BDCOST,BDCOST       TEST $0 BUY                                  
         BE    PAY23D              YES - DON'T SET UNMATCHED FLAG               
         B     PAY23C                                                           
*                                                                               
PAY23B   CLI   SVPPROFB+6,C'N'     TEST MUST MATCH EVEN IF MATCH=NO             
         BNE   PAY23D              NO - SO DON'T SET UNMATCHED FLAG             
*                                                                               
PAY23C   OI    PAYELLO,X'01'       SET FLAG FOR UNMATCHED SPOTS                 
*                                                                               
PAY23D   L     R6,DUB              RESTORE ELEMENT ADDRESS                      
         MVC   ELCDLO(2),DUB+4     RESTORE ELEMENT ARGS                         
*                                                                               
PAY24    DS    0H                                                               
         XC    XCHDATA,XCHDATA                                                  
         SR    R0,R0               PRESET FOR NO XCHAREA                        
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BZ    *+8                                                              
         LA    R0,C'Z'             SET TO INDICATE XCHAREA REQ                  
*                                                                               
         CLI   PRUNPDFL,0          WAS THERE A PRIOR UNPAID MONTH?              
         BE    PAY26               NO - SO CONTINUE                             
*                                                                               
         MVC   NERRCD,=Y(PRUNPAID) ELSE - PRIOR MONTHS UNPAID ERROR             
         MVI   ERRCD,NEWERRS       SET THE ERROR CODE                           
         GOTO1 ERROR               AND EXIT                                     
*                                                                               
PAY26    EQU   *                                                                
         CLI   SVUNPAY,C'Y'                                                     
         BE    PAY26X                                                           
         CLI   BUYSPOTS,0          DO THIS ONCE FOR EACH BUYLINE                
         BNZ   PAY26X                                                           
         MVI   BUYSPOTS,1          SET FLAG FOR UNPAID SPOT IN PERIOD           
         CLI   SVGSTVAL,0          TEST ANY GST OVERRIDE                        
         BNE   *+14                YES - UPDATE                                 
         OC    SVPSTEL,SVPSTEL     TEST ANY PST OVERRIDES                       
         BZ    PAY26X                                                           
*                                                                               
         ICM   R0,3,4(R6)          SAVE PAY DATE                                
         XC    4(2,R6),4(R6)       CLEAR FOR PSTFIX ROUTINE                     
         GOTO1 PSTFIX              SUBR IN 00 IS CALLED BLDPST                  
         STCM  R0,3,4(R6)          AND THEN RESTORE                             
*                                                                               
* ALL FILTERING IS DONE, PROCESS THE DATA NOW                                   
*                                                                               
PAY26X   XC    XCHDATA,XCHDATA                                                  
         SR    R0,R0               PRESET FOR NO XCHAREA                        
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BZ    *+8                                                              
         LA    R0,C'Z'             SET TO INDICATE XCHAREA REQ                  
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),AREC,((R0),(R6)),            X        
               (C'C',XCHDATA)                                                   
*                                                                               
         TM    PASS,1              ACCUMULATE ON PASS 1 ONLY                    
         BZ    PAY27                                                            
         L     R0,TOTSPOTS                                                      
         A     R0,SPOTS                                                         
         ST    R0,TOTSPOTS                                                      
*                                                                               
         OC    SVPWPCT,SVPWPCT     PW CLIENT                                    
         JZ    PAY27               NO - SO CONTINUE                             
         CLI   SVEOWSD,0           OOWR ESTIMATE?                               
         JE    PAY27               NO - SO CONTINUE                             
*                                                                               
         GOTO1 =A(PWOVRIDE),DMCB,GROSS,OWPWGRS-OWPWD,RR=RELO GROSS $            
         GOTO1 (RF),(R1),NET,OWPWNET-OWPWD              NET $                   
*                                                                               
PAY27    L     R0,GROSS                                                         
         A     R0,BUYTOTG                                                       
         ST    R0,BUYTOTG                                                       
*                                                                               
         L     R0,NET                                                           
         A     R0,BUYTOTN                                                       
         ST    R0,BUYTOTN                                                       
*                                                                               
         TM    PASS,1                                                           
         BZ    PAY28                                                            
         OC    BDNTAX,BDNTAX       TEST ANY TAX                                 
         BZ    PAY28                                                            
         OC    SVPWPCT,SVPWPCT     TEST PW CLIENT                               
         BZ    PAY28                                                            
         CLI   SVEOWSD,0           TEST OOWR                                    
         BE    PAY28                                                            
* NEED TO CALCULATE TAX                                                         
         MVC   FULL,NET            SAVE NET INCLUDING TAX                       
         SR    R0,R0                                                            
         ICM   R0,3,BDNTAX                                                      
         XC    BDNTAX,BDNTAX                                                    
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),AREC,(R6)                             
         STCM  R0,3,BDNTAX                                                      
         L     R0,FULL             FULL HAS NET INCL TAX                        
         S     R0,NET              CURRENT NET IS W/O TAX                       
         ST    R0,FULL             STORE TAX IN FULL FOR PWOVRIDE               
         A     R0,TOTTAX                                                        
         ST    R0,TOTTAX                                                        
*                                                                               
         GOTO1 =A(PWOVRIDE),DMCB,FULL,OWPWTAX-OWPWD,RR=RELO TAX $               
*                                                                               
PAY28    TM    BDCIND2,X'20'       TEST CANADIAN                                
         BZ    PAY29                                                            
         L     R0,XGSTAMT                                                       
         A     R0,BUYTGST                                                       
         ST    R0,BUYTGST                                                       
         CLI   GSTCODE,0                                                        
         BNE   *+10                                                             
         MVC   GSTCODE,XGSTCODE    SAVE GST CODE                                
         CLC   GSTCODE,XGSTCODE    TEST SAME GST CODE                           
         BNE   GSTERR                                                           
*                                                                               
         MVI   BYTE,1                                                           
         BRAS  RE,ACCPST           ACCUMULATE PST                               
*                                                                               
PAY29    B     PAY10                                                            
*                                                                               
GSTERR   MVC   NERRCD,=Y(DIFFGST)                                               
         MVI   ERRCD,NEWERRS                                                    
         SR    R0,R0                                                            
         IC    R0,BUYREC+9         ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRTEXT(3),DUB                                                   
         MVI   ERRTEXT+3,C'-'                                                   
         SR    R0,R0                                                            
         IC    R0,BUYREC+10        GET LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRTEXT+4(3),DUB                                                 
         GOTO1 ERROR                                                            
         EJECT                                                                  
*=========================================================*                     
* NON-POL BUY                                             *                     
*=========================================================*                     
         SPACE 1                                                                
PAY30    DS    0H                                                               
         CLC   BUYKEY+3(1),KEY+3   TEST OUR BRAND ACTIVE                        
         BNE   PAY50               NO -SKIP                                     
         OC    QPRD2,QPRD2         TEST PAYING P/B SPOTS                        
         BNZ   PAY31               YES                                          
         CLI   BDTIME,0            TEST P/B BUY                                 
         BNZ   PAY50               YES - SKIP                                   
         B     PAY32                                                            
*                                                                               
PAY31    CLI   BDTIME,0            TEST P/B BUY                                 
         BE    PAY50               NO - SKIP                                    
         CLC   =C'ALL',QPRD2                                                    
         BE    PAY32                                                            
         MVI   ELCDLO,4                                                         
         MVI   ELCDHI,4                                                         
         LA    R6,BDELEM                                                        
         BRAS   RE,NEXTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   2(1,R6),QPRD2+3                                                  
         BNE   PAY50                                                            
*                                                                               
PAY32    DS    0H                                                               
         GOTO1 GETRATE,DMCB,SPOTS,BUYREC,0   GET BUY DESC RATES                 
*                                                                               
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
         LA    R6,BDELEM                                                        
         XC    HALF,HALF           CLEAR SAVE DATE                              
*                                                                               
PAY34    BRAS   RE,NEXTEL                                                       
         BNE   PAY48                                                            
*                                                                               
PAY35    MVC   RUNDATE(2),2(R6)    MOVE ELEMENT DATE                            
*                                                                               
PAY35X   CLC   RUNDATE(2),SVSTARTP                                              
         BL    PAY34                                                            
         CLC   RUNDATE(2),SVENDP                                                
         BH    PAY34                                                            
         OI    PAYELLO,X'80'       SET FLAG FOR SPOTS IN PERIOD                 
*                                                                               
         CLC   HALF,2(R6)          TEST SAME DATE                               
         BE    *+6                                                              
         SR    R9,R9               CLEAR SPOT COUNTER                           
         MVC   HALF,2(R6)          AND SAVE DATE                                
*                                                                               
         LLC   R0,7(R6)                                                         
         TM    6(R6),X'80'                                                      
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    R9,R0               ADD TO ALL SPOT COUNTER                      
*                                                                               
         BRAS  RE,TESTPD           TEST PAID                                    
         BNZ   PAY34                                                            
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+8                                                              
         BRAS  RE,POSTSEQ          ADD CLRSTAT SEQNUM TO LIST                   
*                                                                               
         MVC   8(1,R6),STATSEQ     SET SEQNUM (=0 IF UNPAY)                     
         CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION                            
         BE    *+10                                                             
         MVC   4(2,R6),TODAYP      SET PAY DATE IN ELEM                         
*                                                                               
         OI    PASS,X'80'          SET RECORD UPDATED                           
         OI    PAYELLO,X'40'       SET FLAG FOR UNPAID SPOTS                    
*                                                                               
         BRAS  RE,TSTRATE                                                       
         SPACE 1                                                                
* SUM SPOTS THIS DATE *                                                         
         SPACE 1                                                                
         SR    R8,R8               RESET UNPAID SPOT COUNTER                    
         LR    R7,R6               SAVE ELEM ADDR                               
         B     PAY37                                                            
*                                                                               
PAY36    BRAS   RE,NEXTEL                                                       
         BNE   PAY38                                                            
         CLC   HALF,2(R6)                                                       
         BNE   PAY38                                                            
         LLC   R0,7(R6)                                                         
         TM    6(R6),X'80'                                                      
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    R9,R0               ADD TO ALL SPOT COUNTER                      
*                                                                               
         LR    R7,R6               SAVE ELEM ADDR                               
         BRAS  RE,TESTPD           TEST PAID                                    
         BNZ   PAY36                                                            
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+8                                                              
         BRAS  RE,POSTSEQ          ADD CLRSTAT SEQNUM TO LIST                   
*                                                                               
         MVC   8(1,R6),STATSEQ     SET SEQNUM (=0 IF UNPAY)                     
         CLI   SVUNPAY,C'Y'                                                     
         BE    *+10                                                             
         MVC   4(2,R6),TODAYP                                                   
*                                                                               
PAY37    LLC   R0,7(R6)                                                         
         TM    6(R6),X'80'                                                      
         BZ    *+6                                                              
         LCR   R0,R0                                                            
         AR    R8,R0                                                            
         B     PAY36                                                            
         EJECT                                                                  
PAY38    L     R0,SPOTS                                                         
         AR    R0,R8                                                            
         ST    R0,SPOTS                                                         
*                                                                               
         CLI   SVPPROF+3,C'Y'      TEST PAY ONLY IF MATCHED                     
         BNE   PAY44                                                            
         TM    SVTSTOPT,X'80'      TEST NOAFD OPTION ON                         
         BO    PAY44                                                            
* SHOULD HAVE AFFID FOR EVERY SPOT                                              
         LTR   R9,R9                                                            
         BNP   PAY44                                                            
         LR    R6,R7               RESTORE ADDR OF LAST ELEM ON DATE            
*                                                                               
PAY40    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'                                                      
         BNE   PAY42                                                            
         BCT   R9,PAY40                                                         
         B     PAY44                                                            
*                                                                               
* AFFID MISSING BUT NEED TO LOOK FOR MATCH=NO COMMENT                           
*                                                                               
PAY42    ST    R6,DUB              SAVE ELEMENT ADDRESS                         
         MVC   DUB+4(2),ELCDLO     SAVE ELEMENT CODES                           
*                                                                               
         MVI   ELCDLO,X'66'        SET COMMENT ELEM CODE                        
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
PAY42A   BRAS   RE,NEXTEL                                                       
         BNE   PAY42B                                                           
         CLC   =C'MATCH=NO',3(R6)                                               
         BNE   PAY42A                                                           
         B     PAY42X                                                           
*                                                                               
PAY42B   OI    PAYELLO,X'01'       SET FLAG FOR MISSING AFFIDS                  
*                                                                               
PAY42X   L     R6,DUB              RESTORE ELEMENT POINTER                      
         MVC   ELCDLO(2),DUB+4     RESTORE ELEMENT CODES                        
*                                                                               
PAY44    DS    0H                                                               
         SR    R9,R9               RESET ALL SPOT COUNTER                       
         BRAS   RE,NEXTEL2                                                      
         BE    PAY35                                                            
*                                                                               
PAY48    XC    XCHDATA,XCHDATA                                                  
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BO    PAY49                                                            
         L     R1,GROSS                                                         
         M     R0,SPOTS                                                         
         A     R1,BUYTOTG                                                       
         ST    R1,BUYTOTG                                                       
         L     R1,NET                                                           
         M     R0,SPOTS                                                         
         A     R1,BUYTOTN                                                       
         ST    R1,BUYTOTN                                                       
         L     R0,TOTSPOTS                                                      
         A     R0,SPOTS                                                         
         ST    R0,TOTSPOTS                                                      
         B     PAY50                                                            
         EJECT                                                                  
* CANADIAN - BUILD A DUMMY REGELEM *                                            
         SPACE 1                                                                
PAY49    XC    ELEM,ELEM                                                        
         MVI   ELEM,6                                                           
         MVI   ELEM+1,10                                                        
         MVC   ELEM+2(2),HALF      GET GST RATE FOR THIS DATE                   
         ICM   R0,15,SPOTS                                                      
         BZ    PAY50                                                            
         LPR   RE,R0               MUST PASS POSITIVE SPOTS                     
         STC   RE,ELEM+7                                                        
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),BUYREC,(C'Z',ELEM),          X        
               (C'C',XCHDATA)                                                   
*                                                                               
         L     R1,XGSTAMT                                                       
         LTR   R0,R0               TEST SPOTS NEGATIVE                          
         BP    *+6                                                              
         LCR   R1,R1                                                            
         A     R1,BUYTGST          ADD TO GST TOTAL                             
         ST    R1,BUYTGST                                                       
         MVC   GSTCODE,XGSTCODE    SAVE GST CODE                                
*                                                                               
         MVI   BYTE,2                                                           
         BRAS  RE,ACCPST           ACCUMULATE PST                               
*                                                                               
         L     R1,GROSS                                                         
         LTR   R0,R0               TEST SPOTS NEGATIVE                          
         BP    *+6                                                              
         LCR   R1,R1                                                            
         A     R1,BUYTOTG                                                       
         ST    R1,BUYTOTG                                                       
*                                                                               
         L     R1,NET                                                           
         LTR   R0,R0               TEST SPOTS NEGATIVE                          
         BP    *+6                                                              
         LCR   R1,R1                                                            
         A     R1,BUYTOTN                                                       
         ST    R1,BUYTOTN                                                       
         EJECT                                                                  
* END OF BUYREC                                                                 
*                                                                               
PAY50    TM    PASS,2              TEST PASS 2                                  
         BZ    PAY52                                                            
         TM    PASS,X'80'          TEST RECORD UPDATED                          
         BZ    PAY54                                                            
         CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   PAY50X                                                           
PAY50X   DS    0H                                                               
         GOTO1 PUTREC                                                           
         B     PAY54                                                            
*                                                                               
PAY52    TM    PASS,X'80'          TEST RECORD UPDATED                          
         BZ    PAY52A                                                           
         BRAS  RE,SCOM                                                          
*                                                                               
PAY52A   L     R0,BUYTOTG                                                       
         A     R0,TOTG                                                          
         ST    R0,TOTG                                                          
*                                                                               
         L     R0,BUYTOTN                                                       
         A     R0,TOTN                                                          
         ST    R0,TOTN                                                          
*                                                                               
         L     R0,BUYTGST                                                       
         A     R0,TOTGST                                                        
         ST    R0,TOTGST                                                        
*                                                                               
         C     R0,=F'10000000'     TEST GST > 99999.99                          
         BL    PAY52B                                                           
         B     PAY52               NOT ANYMORE                                  
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(GST2MUCH)                                            
         B     PAYERR                                                           
*                                                                               
PAY52B   LA    R1,BUYTPST          ACCUMULATE PST                               
         LA    RE,TOTPST                                                        
         LA    R4,10                                                            
*                                                                               
PAY52C   L     R0,0(R1)                                                         
         A     R0,0(RE)                                                         
         ST    R0,0(RE)                                                         
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         BCT   R4,PAY52C                                                        
*                                                                               
PAY53    CLI   SVTRADE,C'Y'        TEST TRADE CLEARANCE                         
         BNE   PAY53X                                                           
         TM    PASS,1                                                           
         BZ    PAY53X                                                           
         TM    PASS,X'80'          TEST RECORD UPDATED                          
         BZ    PAY53X                                                           
         BRAS  RE,GETCTA           ACCUMULATE DOLLARS BY CONTRACT               
*                                                                               
PAY53X   OC    SVTST(3),SVTST      IS TEST OPTION ACTIVE                        
         BZ    PAY54                                                            
         TM    PASS,X'80'          TEST ACTIVITY                                
         BZ    PAY54                                                            
*                                                                               
         CLC   BUYREC+9(3),SVTSTEST  REC TO START EST(1)/LIN(2)                 
         BL    PAY54                                                            
*                                                                               
         LLC   R0,BUYREC+9         EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         MVI   WORK+3,C'-'                                                      
         SR    R0,R0                                                            
         ICM   R0,3,BUYREC+10      LINE                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+4(3),DUB                                                    
*                                                                               
         GOTO1 PAYTEST             DISPLAY TEST DATA                            
*                                                                               
PAY54    NI    PASS,X'7F'          RESET FLAG                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(10),KEYSAVE     A-M/CLT/PRD/MKT/STA/EST                      
         BE    PAY4                                                             
*                                                                               
         CLI   SVPPROF+10,C'C'     TEST CCUSA                                   
         BE    PAY54CC                                                          
*                                                                               
         TM    QSTA,X'F0'          IF THIS IS NOT CABLE                         
         BO    PAY54A                                                           
         CLI   SVKEY+9,0           TEST PAYING ONE EST                          
         BNE   PAY56               YES - DONE                                   
         CLC   KEY(9),KEYSAVE      A-M/CLT/PRD/MKT/STA                          
         BE    PAY4                                                             
         B     PAY56               YES - DONE                                   
*                                                                               
PAY54A   CLC   KEY(6),KEYSAVE      CABLE - TEST A-M/CLT/PRD/MKT                 
         BNE   PAY56                                                            
*                                                                               
         GOTO1 ,DMCB,(X'80',KEY+4),WORK,WORK+4                                  
         BRAS  RE,GOMSUNPK                                                      
         CLC   QSTA(4),WORK+4      COMPARE STATIONS                             
         BNE   PAY56                                                            
         CLI   QNET,C'A'           IF NO NETWORK - CONTINUE                     
         BL    PAY54B                                                           
         CLC   QNET,WORK+9         ELSE COMPARE NETWORKS                        
         BNE   PAY54C                                                           
*                                                                               
PAY54B   CLI   SVKEY+9,0           TEST PAYING ONE EST                          
         BE    PAY4                                                             
         CLC   KEY+9(1),SVKEY+9    IS THIS IT                                   
         BE    PAY4                                                             
*                                                                               
PAY54C   MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     PAY54A                                                           
*                                                                               
PAY54CC  CLC   KEY(9),KEYSAVE      A-M/CLT/PRD/MKT/STA                          
         BNE   PAY56                                                            
         CLC   KEY+9(1),SVXFRESX   TEST PAST HIGH EST                           
         BNH   PAY4                                                             
*                                                                               
PAY56    DS    0H                                                               
         TM    PASS,2                                                           
         BO    PAY70                                                            
         EJECT                                                                  
*==========================================================*                    
* ALL RECORDS PROCESSED                                    *                    
*==========================================================*                    
                                                                                
         CLI   GOTSRC,C'Y'         IF ANY SRC DATA, NEED TO FIX                 
         BNE   *+8                                                              
         BRAS  RE,FIXSCRN                                                       
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   PAY57                                                            
         BRAS  RE,CHKCLS           SEE IF ANY CHECKS WRITTEN                    
         BE    PAY57               NO                                           
         LA    R2,PAYERH                                                        
         B     PAYERR                                                           
                                                                                
PAY57    LA    R2,PAYMDH           PUT CURSOR TO MEDIA IF WON'T PAY             
         OC    SVID,SVID                                                        
         BZ    *+8                                                              
         LA    R2,PAYOPH           OR TO 'OPT' IF PAY BY ID                     
*                                                                               
         MVI   ERRCD,NOSPOTS                                                    
         CLI   SVPPROF+3,C'X'      TEST OPTION TO PAY MATCHED ONLY              
         BNE   *+8                                                              
         MVI   ERRCD,NOUNPAFD                                                   
         TM    PAYELLO,X'80'                                                    
         BZ    PAYERR                                                           
         MVI   ERRCD,ALLPAID                                                    
         TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         BZ    *+8                                                              
         MVI   ERRCD,ALLUNPD                                                    
         TM    PAYELLO,X'40'                                                    
         BZ    PAYERR                                                           
         MVI   ERRCD,MSSNGAFD                                                   
         TM    SVTSTOPT,X'80'      TEST OVERRIDE TO IGNORE AFFIDS               
         BO    *+12                                                             
         TM    PAYELLO,X'01'                                                    
         BO    PAY68                                                            
         MVI   ERRCD,MXDRATES                                                   
         TM    PAYELLO,X'02'                                                    
         BO    PAY68                                                            
***                                                                             
*                   ** IF CANADIAN **                                           
*                   **     AND     **                                           
* IF PAYING GROSS & GROSS = 0 & THERE IS A TAX VALUE THEN ERROR                 
* IF PAYING NET & NET = 0 & THERE IS A TAX VALUE THEN ERROR                     
***                                                                             
         CLI   SVAPROF+7,C'C'      CANADIAN?                                    
         BNE   PAY57E              NO, SKIP TEST                                
*                                                                               
         CLI   SVPPROF+0,C'G'      GROSS?                                       
         BNE   PAY57A              NO, TEST NET = 0                             
         OC    TOTG,TOTG           GROSS = 0?                                   
         BZ    PAY57B              YES, IF ANY TAX, ERROR                       
         B     PAY57E                                                           
PAY57A   OC    TOTN,TOTN           NET = 0?                                     
         BNZ   PAY57E              NO                                           
*                                                                               
PAY57B   LA    R1,TOTGST           GST/PST TAX                                  
         LA    R4,11                                                            
*                                                                               
PAY57C   OC    0(4,R1),0(R1)       ANY TAX?                                     
         BNZ   PAY57D              YES, ERROR                                   
         LA    R1,4(R1)            BUMP                                         
         BCT   R4,PAY57C           CHECK NEXT TAX VALUE                         
         B     PAY57E                                                           
*                                                                               
PAY57D   LA    R2,PAYERH           NO, TAX ERROR DO NOT PAY THIS!!              
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(1156)                                                  
         J     PAYERR                                                           
* COMPARE TOTALS                                                                
PAY57E   L     R0,TOTG                                                          
         CLI   SVPPROF+0,C'G'                                                   
         BE    *+8                                                              
         L     R0,TOTN                                                          
         A     R0,TOTGST                                                        
         LA    R1,TOTPST                                                        
         LA    R4,10                                                            
*                                                                               
PAY58    A     R0,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R4,PAY58                                                         
*                                                                               
         CLI   SVPPROFB+5,1        TEST PAY INVOICE AMOUNT                      
         BNH   PAY58A              NO                                           
                                                                                
*==============================================================                 
* NEW FEATURE TO PAY INVOICE AMOUNT WITHIN PERCENTAGE TOLERANCE                 
*==============================================================                 
                                                                                
         L     RF,TOTAMT           TOTAL OF INVOICES                            
         AR    RF,RF               X 2                                          
         LLC   RE,SVPPROFB+5       PCT DIFF TO 2 DEC (1%=100)                   
         MR    RE,RE                                                            
         D     RE,=F'10000'        SCALE DIFFERENCE TO PENNIES                  
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AHI   RF,1                                                             
         SRA   RF,1                GIVES MAX DIFFERENCE                         
         LPR   RF,RF               TAKE ABSVAL                                  
*                                                                               
         L     RE,TOTAMT           TOTAL OF INVOICES                            
         SR    RE,R0               LESS FILE TOTAL                              
         LPR   RE,RE               GIVES ABSVAL                                 
         CR    RE,RF               COMPARE TO ABSVAL OF DIFF                    
         BH    PAY66                                                            
         B     PAY58B                                                           
*                                                                               
PAY58A   C     R0,TOTAMTLO                                                      
         BL    PAY66                                                            
         C     R0,TOTAMTHI                                                      
         BH    PAY66                                                            
         CLI   SVPPROFB+5,1        TEST PAY INV AMT BUT CHECK $                 
         BL    PAY58C              NO                                           
*                                                                               
PAY58B   L     RE,TOTAMT           WITHIN PERCENTAGE TOLERANCE                  
         SR    RE,R0                                                            
         ST    RE,FILEDIFF         SAVE DIFF BETWEEN FILE/INVOICES              
*                                                                               
PAY58C   MVI   STATSEQ,0                                                        
         CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION ACTIVE                     
         BE    PAY59               YES - CONTINUE                               
         TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         BO    PAY66               YES - EXIT                                   
*                                                                               
PAY59    L     R8,ASVAMTS          CHECK FOR INVOICE 1 AMOUNT 1                 
         USING AMTD,R8                                                          
         LA    R1,AMTLEN(R8)       IS THERE MORE THAN 1 INVOICE                 
         CLI   AMTFLAGS-AMTD(R1),0                                              
         BNE   PAY60               YES THEN CONTINUE AS USUAL                   
         CLC   AMTINV(4),=C'1   '  INVOICE 1?                                   
         BNE   PAY60                                                            
         CLC   AMT,=F'100'         AMOUNT 1?                                    
         BE    PAY66               ERROR GIVEN FROM PAYICL                      
         DROP  R8                                                               
*                                                                               
PAY60    DS    0H                                                               
         BRAS  RE,TSTLOCK          TEST DATA LOCKED                             
         BE    PAY60A                                                           
         MVC   NERRCD,=AL2(DATALOCK)                                            
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
PAY60A   BRAS  RE,UPDINV           MARK INVOICES PAID                           
*                                                                               
         GOTO1 BLDSTAT             UPDATE STATUS RECORD/GET PAY SEQNUM          
*                                                                               
PAY61    OC    SVPWPCT,SVPWPCT     TEST PW CLIENT                               
         BZ    PAY61A                                                           
         CLI   SVEOWSD,0           TEST OOWR ESTIMATE                           
         BE    PAY61A                                                           
         BRAS  RE,PAYPW                                                         
*                                                                               
PAY61A   MVI   PASS,2                                                           
         MVC   KEY,SVKEY                                                        
*                                                                               
         CLI   SVPPROF+10,C'C'     TEST CCUSA                                   
         BNE   *+10                                                             
         MVC   KEY+9(1),SVXFREST   SET LOW EST                                  
*                                                                               
         MVI   RDUPDATE,C'Y'       SET READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         TM    QSTA,X'F0'          IF THIS IS CABLE                             
         BNO   PAY64                                                            
         CLC   KEY(6),KEYSAVE      TEST A-M/CLT/PRD/MKT                         
         BE    *+6                 YES                                          
         DC    H'0'                HOW DID THIS HAPPEN ???                      
*                                                                               
         GOTO1 ,DMCB,(X'80',KEY+4),WORK,WORK+4                                  
         BRAS  RE,GOMSUNPK                                                      
         CLC   QSTA(4),WORK+4      COMPARE STATIONS                             
         BNE   PAYERR                                                           
         CLI   QNET,C'A'           IF NO NETWORK - CONTINUE                     
         BL    PAY62                                                            
         CLC   QNET,WORK+9         ELSE COMPARE NETWORKS                        
         BNE   PAY54C              NOT EQUAL - READ SEQ                         
*                                                                               
PAY62    CLI   SVKEY+9,0           TEST PAYING ALL ESTS                         
         BE    PAY4                                                             
         CLC   KEY+9(1),SVKEY+9    NO - SAME ESTIMATE ?                         
         BE    PAY4                                                             
         B     PAY54C              WRONG EST - READ SEQ                         
*                                                                               
PAY64    CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PAY4                                                             
*                                                                               
PAY66    DS    0H                                                               
         GOTO1 PAYICL                                                           
         B     EXIT                                                             
*                                                                               
PAY68    DS    0H                                                               
         GOTO1 PAYICL                                                           
         B     PAYERR                                                           
*                                                                               
*                                                                               
PAY70    CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION ACTIVE                     
         BE    PAY80               YES                                          
         CLI   T213FFD+1,C'*'      TEST DDS TERMINAL                            
         BNE   PAY72                                                            
         CLC   =C'NOCHECK',PAYER                                                
         BNE   PAY72                                                            
         XC    PAYMSG,PAYMSG                                                    
         MVC   PAYMSG(26),=C'** BUYS PAID - NO CHECK **'                        
         B     EXIT                                                             
*                                                                               
* NORMAL PAYMENT                                                                
PAY72    CLI   SVPPROF+10,C'C'     TEST CCUSA INTERFACE                         
         BE    PAY75                                                            
         GOTO1 PAYCHK                                                           
         B     EXIT                                                             
*                                                                               
PAY75    DS    0H                                                               
         BRAS  RE,PAYCC                                                         
         GOTO1 PAYCHK                                                           
         B     EXIT                                                             
         SPACE 1                                                                
*==============================================================*                
* UNPAY OPTION IS ACTIVE - TEST WHETHER TO REVERSE CHECK       *                
*==============================================================*                
         SPACE 1                                                                
PAY80    MVC   PAYMSG(11),=C'BUYS UNPAID'                                       
         CLC   =C'UNCLEAR',PAYER                                                
         BE    PAY81                                                            
         CLC   =C'REVCHECK',PAYER  TEST ISSUE REVERSE CHECK                     
         BNE   EXIT                                                             
*                                                                               
PAY81    CLI   SVPPROF+10,C'C'     TEST CCUSA INTERFACE                         
         BE    PAY82                                                            
         GOTO1 PAYCHK                                                           
         B     PAY84                                                            
*                                                                               
PAY82    DS    0H                                                               
         BRAS  RE,PAYCC                                                         
*                                                                               
PAY84    XC    PAYMSG,PAYMSG       OVERWRITE ANY OTHER MESSAGE                  
         MVC   PAYMSG(11),=C'BUYS UNPAID'                                       
         MVC   PAYMSG+12(18),=C'AND CHECK REVERSED'                             
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* NOTE THIS CODE BEYOND BASE REGISTER ADDRESSING                                
*==================================================================             
                                                                                
TSTRATE  DS    0H                  MIXED RATES CHECK                            
         CLI   PAYMD,C'X'                                                       
         JNE   TSTMXD1             A0B PROFILE - FOR MEDIA X                    
         CLI   SVPPROFB,C'N'       NO MIXED RATES (MEDIA X)                     
         JE    TSTMXD2                                                          
*                                                                               
TSTMXD1  CLI   SVPPROF+1,C'Y'      ALLOW PAY MIXED RATES                        
         BER   RE                                                               
*                                                                               
         BRAS  RF,*+8              NEED TO AVOID LITERALS                       
         DC    CL4'TAX '                                                        
         CLC   BDPROGRM,0(RF)                                                   
         BER   RE                                                               
*                                                                               
         CLI   PAYMD,C'N'                                                       
         BER   RE                                                               
         CLI   PAYMD,C'X'          A0 PROFILE SKIPPED FOR X                     
         BER   RE                                                               
* TEST FOR MIXED RATE TYPES                                                     
TSTMXD2  CLI   RATETEST,0          TEST FIRST BUY                               
         JNE   TSTMXD5                                                          
         MVI   RATETEST,C'X'                                                    
         MVC   RATETEST+1(1),BDCIND                                             
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         JNZ   TSTMXD5                                                          
         NI    RATETEST+1,X'FE'    IGNORE X'01'                                 
*                                                                               
TSTMXD5  MVC   RATETEST+2(1),BDCIND                                             
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         JNZ   TSTMXD10                                                         
         NI    RATETEST+2,X'FE'                                                 
*                                                                               
TSTMXD10 CLC   RATETEST+1(1),RATETEST+2                                         
         JE    *+8                                                              
         OI    PAYELLO,X'02'       SET FLAG                                     
         BR    RE                                                               
         EJECT                                                                  
TESTPD   DS    0H                                                               
         TM    SVTSTOPT,X'10'      TEST PAID OPTION ACTIVE                      
         JO    TESTPD2             YES                                          
         OC    4(2,R6),4(R6)       NO - SET CC                                  
         BR    RE                   AND EXIT                                    
*                                                                               
TESTPD2  OC    4(2,R6),4(R6)       TEST PAID                                    
         JZ    TESTPDNE            NO - SET TO IGNORE UNPAID                    
         OC    SVPAYDT,SVPAYDT     TEST DATE SPECIFIED                          
         JZ    TESTPDEQ            NO                                           
         CLC   SVPAYDT,4(R6)       TEST RIGHT DATE                              
         JNE   TESTPDNE            NO                                           
         CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION ACTIVE                     
         JNE   TESTPDEQ            NO - EXIT                                    
         XC    4(2,R6),4(R6)       CLEAR PAYMENT DATE                           
         J     TESTPDEQ            AND EXIT                                     
*                                                                               
TESTPDNE LTR   RE,RE                                                            
         BR    RE                                                               
TESTPDEQ CR    RE,RE                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************                                               
* ACCUMULATE PST TOTALS         *                                               
* BYTE = 1 POL BUY              *                                               
* BYTE = 2 NON - POL BUY        *                                               
*   R0 = N'SPOTS                *                                               
*********************************                                               
         SPACE                                                                  
ACCPST   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,XPSTPROV                                                      
         LA    R2,PSTCODE                                                       
         XC    BYTE2,BYTE2         COUNTER                                      
         LA    R5,BUYTPST                                                       
*                                                                               
ACCPST10 CLI   2(R1),C' '          IF NO PST CODE - SKIP                        
         BNH   ACCPST30                                                         
         L     R6,0(R5)                                                         
         L     R4,8(R1)            PST AMOUNT                                   
         CLI   BYTE,2              TEST IF FROM POL OR NOT                      
         BNE   ACCPST20                                                         
         LTR   R0,R0               TEST SPOTS NEGATIVE                          
         BP    *+6                                                              
         LCR   R4,R4                                                            
*                                                                               
ACCPST20 AR    R6,R4               ADD IN AMOUNT FOR PROVINCE                   
         MVC   0(1,R2),2(R1)       SET CODE FOR PROVINCE                        
         ST    R6,0(R5)                                                         
*                                                                               
ACCPST30 LA    R1,XPSTLEN(R1)                                                   
         LA    R2,1(R2)                                                         
         LA    R5,4(R5)            BUMP                                         
         LLC   R4,BYTE2                                                         
         LA    R4,1(R4)                                                         
         STC   R4,BYTE2                                                         
         CLI   BYTE2,10                                                         
         BL    ACCPST10                                                         
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
*===============================================================                
* IF AGENCY UNPAY, ADD CLRSTAT SEQUENCE NUMBER TO LIST                          
* FOR ELEMENT AT 0(R6)                                                          
*===============================================================                
                                                                                
POSTSEQ  NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'UNCLEAR',PAYER                                                
         BNE   POSTSEQX                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,12(R6)           GET POL ELEM SEQNUM                          
         CLI   0(R6),X'0B'         TEST POL ELEM                                
         BNL   *+8                                                              
         IC    RF,8(R6)            NON-POL ELEM SEQNUM                          
*                                                                               
         LA    R1,UNCLRSEQ                                                      
         LA    R0,L'UNCLRSEQ-1                                                  
*                                                                               
POSTSEQ2 CLI   0(R1),0                                                          
         BE    POSTSEQ4                                                         
         CLM   RF,1,0(R1)          MATCH SEQNUM                                 
         BE    POSTSEQX                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,POSTSEQ2                                                      
*                                                                               
POSTSEQ4 STC  RF,0(R1)                                                          
*                                                                               
POSTSEQX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* IF AGENCY UNPAY, MAKE SURE NO CHECKS ISSUED FOR UNCLRSEQ                      
*   SEQUENCE NUMBERS. IF NONE, EXIT WITH CC EQ                                  
*   AND MAKE SURE THAT CURRENT PAYEE MATCHES ORIGINAL PAYEE                     
*===============================================================                
                                                                                
CHKCLS   NTR1  BASE=*,LABEL=*                                                   
         CLC   =C'UNCLEAR',PAYER                                                
         JNE   EQXIT                                                            
*                                                                               
         LA    R7,UNCLRSEQ         POINT TO LIST OF SEQNUMS                     
*                                                                               
CHKCLS2  BAS   RE,GETCLS                                                        
         JNE   NEQXIT                                                           
         LA    R7,1(R7)                                                         
         CLI   0(R7),0                                                          
         BNE   CHKCLS2                                                          
         J     EQXIT                                                            
                                                                                
*===============================================================                
* BUILD KEY OF FIRST STATUS RECORD                                              
*===============================================================                
                                                                                
GETCLS   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D76'                                                  
         MVC   KEY+2(3),SVKEY      A-M/CLT                                      
         MVC   KEY+5(5),SVKEY+4    MKT/STA                                      
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BE    GETCLS2                                                          
         CLI   KEY+7,X'E8'         IF THIS IS CABLE                             
         BL    *+8                                                              
         NI    KEY+9,X'80'         DROP NETWORK BITS                            
*                                                                               
GETCLS2  MVI   RDUPDATE,C'Y'       SET READ FOR UPDATE                          
         GOTO1 HIGH                                                             
*                                                                               
GETCLS8  CLC   KEY(10),KEYSAVE     TEST SAME MKT/STA                            
         JNE   EQXIT               NO - BIG PROBLEM HERE                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
GETCLS10 DS    0H                                                               
         L     R1,AREC                                                          
         LA    R1,24(R1)                                                        
*                                                                               
GETCLS12 CLI   0(R1),0                                                          
         BNE   GETCLS14                                                         
* NOT IN THIS RECORD - TRY THE NEXT                                             
         L     R1,AREC                                                          
         MVC   KEY(13),0(R1)       MOVE KEY FROM RECORD                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     GETCLS8                                                          
*                                                                               
GETCLS14 CLI   0(R1),1                                                          
         BNE   GETCLS16                                                         
*                                                                               
         USING CLSTEL01,R1                                                      
         CLC   CLSTCLRD,SVPAYDT    MATCH CLEARANCE DATE                         
         BNE   GETCLS16                                                         
         CLC   CLSTCLSQ,0(R7)      MATCH SEQNUM                                 
         BE    GETCLS20                                                         
*                                                                               
GETCLS16 SR    R0,R0                                                            
         ICM   R0,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R0                                                            
         B     GETCLS12                                                         
*                                                                               
GETCLS20 CLC   CLSTCHK,=C'VOID  '                                               
         JE    EQXIT                                                            
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(WROTECHK)                                              
         CLC   PAYER(10),=C'UNCLEARCHK'                                         
         BE    GETCLS22                                                         
         CLC   PAYER(10),=C'UNCLEAR!!!'                                         
         BE    GETCLS22                                                         
         OC    CLSTCHK,CLSTCHK                                                  
         JNZ   NEQXIT                                                           
*                                                                               
GETCLS22 MVC   NERRCD,=Y(NEWPAYEE)                                              
         CLC   CLSTPYEE,QREP       MATCH REP OR 000 FOR DIRECT                  
         JE    EQXIT                                                            
         CLC   PAYER(10),=C'UNCLEAR!!!'                                         
         BE    GETCLS24                                                         
         CLC   PAYER(10),=C'UNCLEAR:-)'                                         
         JNE   NEQXIT                                                           
*                                                                               
GETCLS24 J     EQXIT                                                            
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
GOMSUNPK NTR1 BASE=*,LABEL=*                                                    
         LM    R5,R7,0(R1)         GET MSUNPK PARAMS                            
*                                                                               
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGYALPHA                                                 
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,PAYMD                                                    
         MVC   STAPACOM,VCOMFACS                                                
         MVC   STAPMKST,0(R5)      MKTSTA                                       
*                                                                               
         GOTO1 STAPACK,(R1)                                                     
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R6),STAPQMKT    RETURN RESULT                                
         MVC   0(5,R7),STAPQSTA                                                 
         LTR   R5,R5               R5 IS NEG FOR 8 BYTE OUTPUT                  
         BNM   *+10                                                             
         MVC   0(8,R7),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
* BUILD CCUSA POSTING INTERFACE                                    *            
*==================================================================*            
         SPACE 1                                                                
PAYCC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SVXFRSW,C'N'        TEST SUPPRESS XFR                            
         BE    PAYCCX                                                           
         OC    TOTG,TOTG           TEST GROSS = 0                               
         BNZ   PAYCC1                                                           
         OC    TOTN,TOTN           TEST NET = 0 TOO                             
         BZ    PAYCCX                                                           
*                                                                               
PAYCC1   LA    R2,PAYINV1H                                                      
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA  POINT TO INTERFACE AREA             
         USING XFRINVD,R4                                                       
         LA    R0,5                                                             
*                                                                               
PAYCC2   MVC   XFRINV,8(R2)        USE FULL INVOICE NUMBER                      
         OC    XFRINV,SPACES                                                    
         ZAP   XFRGROSS,=P'0'                                                   
         ZAP   XFRNET,=P'0'                                                     
         ZAP   XFRCD,=P'0'                                                      
         MVC   XFRNARR,SPACES                                                   
         LLC   RF,0(R2)                                                         
         AR    R2,RF               POINT TO AMOUNT                              
         IC    RF,0(R2)                                                         
         AR    R2,RF               POINT TO COMMENT                             
         LLC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   XFRNARR(0),8(R2)    MOVE NARRATIVE                               
*                                                                               
         IC    RF,0(R2)                                                         
         AR    R2,RF               POINT TO INVOICE                             
         LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC2                                                        
*                                                                               
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         USING XFRINVD,R4                                                       
*                                                                               
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
         LHI   R0,MAXAMTS                                                       
*                                                                               
PAYCC4   L     RF,0(R8)                                                         
         CLI   AMTTYPE,C'2'        TEST CR                                      
         BE    PAYCC6                                                           
         CLI   AMTTYPE,C'3'        OR CK                                        
         BE    PAYCC6                                                           
         B     *+6                                                              
PAYCC6   LNR   RF,RF               ALL CR'S AND CK'S ARE -                      
         CVD   RF,DUB                                                           
*                                                                               
         CLI   SVPPROF+0,C'G'      TEST INVOICES GROSS/NET                      
         BNE   PAYCC8                                                           
* GROSS                                                                         
         ZAP   XFRGROSS,DUB                                                     
         M     RE,TOTN                                                          
         OC    TOTG,TOTG           TEST GROSS=0                                 
         BZ    PAYCC10                                                          
         D     RE,TOTG                                                          
         CVD   RF,DUB                                                           
         ZAP   XFRNET,DUB                                                       
         B     PAYCC10                                                          
* NET                                                                           
PAYCC8   ZAP   XFRNET,DUB                                                       
         OC    TOTN,TOTN           TEST NET=0                                   
         BZ    PAYCC10                                                          
         M     RE,TOTG                                                          
         D     RE,TOTN                                                          
         CVD   RF,DUB                                                           
         ZAP   XFRGROSS,DUB                                                     
*                                                                               
PAYCC10  LA    R4,XFRINLEN(R4)                                                  
         LA    R8,AMTLEN(R8)       NEXT AMOUNT                                  
         BCT   R0,PAYCC4                                                        
         SPACE 1                                                                
* NOW HAVE TO GET THESE GROSS/NET AMOUNTS TO EQUAL FILE AMOUNTS *               
         SPACE 1                                                                
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         LA    R0,5                                                             
         ZAP   PGR,=P'0'                                                        
         ZAP   PNET,=P'0'                                                       
*                                                                               
PAYCC12  AP    PGR,XFRGROSS                                                     
         AP    PNET,XFRNET                                                      
         LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC12                                                       
*                                                                               
         L     R0,TOTG                                                          
         CVD   R0,DUB                                                           
         SP    PGR,DUB             SAVE DIFFERENCE GROSS                        
         L     R0,TOTN                                                          
         CVD   R0,DUB                                                           
         SP    PNET,DUB            SAVE DIFFERENCE NET                          
* FIND LARGEST AMOUNT AND ADJUST                                                
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         LA    R0,5                                                             
         LR    R1,R4                                                            
PAYCC14  CP    XFRGROSS-XFRINVD(6,R4),=P'0'  TEST CURRENT = 0                   
         BE    PAYCC14A                      YES - IGNORE                       
         CP    XFRGROSS-XFRINVD(6,R1),XFRGROSS-XFRINVD(6,R4)                    
         BH    *+6                                                              
         LR    R1,R4                                                            
PAYCC14A LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC14                                                       
         EJECT                                                                  
         SP    XFRGROSS-XFRINVD(6,R1),PGR                                       
         SP    XFRNET-XFRINVD(6,R1),PNET                                        
*                                                                               
         LLC   R0,SVPPROF+12       GET COMM ADJ PCT                             
         CVD   R0,DUB                                                           
         ZAP   HALF,DUB                                                         
         SPACE 1                                                                
* NOW CHANGE NET AMOUNTS TO REFLECT IPG COMMISSION *                            
         SPACE 1                                                                
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         LA    R0,5                                                             
*                                                                               
PAYCC16  ZAP   DUB,XFRGROSS                                                     
         SP    DUB,XFRNET          GET GROSS-NET                                
         MP    DUB,HALF                                                         
         DP    DUB,=P'100'                                                      
         AP    XFRNET,DUB(6)       ADD DIFFERENCE TO NET                        
         LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC16                                                       
*                                                                               
         CLI   SVUNPAY,C'Y'                                                     
         BNE   PAYCC20                                                          
* FOR UNPAY, NEED TO CHANGE SIGNS ON AMOUNT FIELDS                              
         LA    R4,XFRDATA-PAYXFRD+SVXFRDTA                                      
         LA    R0,5                                                             
PAYCC18  CP    XFRGROSS,=P'0'                                                   
         BE    *+10                                                             
         MP    XFRGROSS,=P'-1'                                                  
         CP    XFRNET,=P'0'                                                     
         BE    *+10                                                             
         MP    XFRNET,=P'-1'                                                    
         LA    R4,XFRINLEN(R4)                                                  
         BCT   R0,PAYCC18                                                       
* SWITCH TO ACC *                                                               
PAYCC20  L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVXFRACC                                                 
******************************************* FOR TESTING                         
         CLC   T213FFD+10(2),=X'0011' TEST ID = SJR                             
         BNE   *+8                                                              
         MVI   DMCB,6                                                           
******************************************* FOR TESTING                         
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 PAYXFR,DMCB,=C'POST',SVXFRDTA,PAYMSGH,VCOMFACS                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
* SWITCH BACK TO SPOT                                                           
         L     RF,VCOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PAYCCX   DS    0H                                                               
         XIT1                                                                   
         DROP  R4,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* UPDATE INVOICE RECORDS                                         *              
*================================================================*              
         SPACE 1                                                                
UPDINV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASVAMTS          POINT TO FIRST INVOICE NUMBER                
         USING AMTD,R8                                                          
         LA    R0,MAXAMTS                                                       
*                                                                               
UPDINV2  DS    0H                                                               
         TM    AMTFLAGS,X'80'      TEST INVOICE PRESENT                         
         BZ    UPDINV10                                                         
*                                                                               
         XC    BIGKEY,BIGKEY       NOTE USES ** ELEM **                         
         LA    R5,BIGKEY                                                        
         USING SNVKEYD,R5                                                       
         MVI   SNVKTYPE,X'0E'                                                   
         MVI   SNVKSUB,X'03'                                                    
         MVC   SNVKAM,SVKEY        A/M                                          
         MVC   SNVKCLT,SVKEY+1     CLT                                          
         MVC   SNVKSTA,SVKEY+6     STATION                                      
* NOW GET END YMD                                                               
         GOTO1 VDATCON,DMCB,(2,SVENDP),(3,WORK)                                 
         MVI   WORK+2,1            SET TO 1ST OF MONTH                          
         GOTO1 (RF),(R1),(3,WORK),(2,SNVKMOS)                                   
         XC    SNVKMOS,=X'FFFF'                                                 
         MVC   SNVKINV,AMTINV      NOTE USE 10 CHAR INVOICE NUMBER              
         OC    SNVKINV,SPACES                                                   
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 VDATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'XSPDIR',BIGKEYSV,   X        
               BIGKEY                                                           
*                                                                               
         CLC   BIGKEY(22),BIGKEYSV  COMPARE THROUGH INVOICE NUMBER              
         BE    UPDINV4                                                          
         TM    SVTSTOPT,X'08'      TEST NOINV OPTION ON                         
         BO    UPDINV10            THEN CONTINUE PAYMENT WHEN NO INV            
         CLI   SVPPROFA+15,C'D'    DISALLOW PAYMENT IF NO INVOICE               
         BNE   UPDINV10            NO TRY NEXT INVOICE                          
         MVC   PAYMSG(15),=C'MISSING INVOICE'                                   
         MVC   PAYMSG+16(10),AMTINV                                             
         LA    R2,PAYMDH                                                        
         OI    6(R2),X'40'         CURSOR                                       
         DC    H'0',C'$ABEND'      NEED TO UNWIND OTHER INV PAID                
***      MVI   ERRAREA,X'FF'                                                    
***      GOTO1 ERROR                                                            
*                                                                               
UPDINV4  DS    0H                                                               
         GOTO1 VDATAMGR,DMCB,(X'80',=C'GETREC'),=C'XSPFIL',SNVDDA,     X        
               AREC2,DMWORK                                                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AREC2                                                         
         LA    R5,SNVELS                                                        
         CLI   0(R5),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SNVHDELD,R5                                                      
         OI    SNVHDCTL,SNVHDPDQ   SET INVOICE PAID FLAG                        
         CLI   SVUNPAY,C'Y'        TEST UNPAY OPTION ACTIVE                     
         BNE   *+8                                                              
         NI    SNVHDCTL,X'FF'-SNVHDPDQ                                          
         GOTO1 (RF),(R1),=C'PUTREC'                                             
         DROP  R5                                                               
*                                                                               
UPDINV10 LA    R8,AMTLEN(R8)                                                    
         BCT   R0,UPDINV2                                                       
*                                                                               
UPDINVX  XIT1                                                                   
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================*             
* TEST DATA LOCKED BY OFFLINE APPLICATION                         *             
* THIS CODE SHOULD BE CHANGED TO CALL LOCKUP WHEN ALL CONVENTIONS *             
* ARE AGREED. LOCKUP/LOCKET DSECTS ARE IDENTICAL                  *             
*=================================================================*             
         SPACE 1                                                                
         DS    0D                                                               
TSTLOCK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BU'    LOCK BUYS                                    
         L     RE,AREC                                                          
         MVC   L.LOCKMED,PAYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         MVC   L.LOCKSTA,QSTA                                                   
         CLI   L.LOCKSTA,C'0'                                                   
         BL    *+8                                                              
         MVI   L.LOCKSTA+4,C'/'                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
* TEST ALLOCATION KEYS                                                          
         XC    LKUPKEY,LKUPKEY                                                  
L        USING LKKEYD,LKUPKEY                                                   
*                                                                               
         MVC   L.LOCKAGY,AGYALPHA                                               
         MVC   L.LOCKRTY,=C'BA'    LOCK BUYS                                    
         MVC   L.LOCKMED,PAYMD                                                  
         MVC   L.LOCKCLT,QCLT                                                   
         CLI   SVKEY+9,0           NO EST = LEAVE AS NULLS                      
         BE    TSTLOCK1            LOCKED IF ANY EST IS LOCKED                  
         SR    R0,R0                                                            
         IC    R0,SVKEY+9                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  L.LOCKEST,DUB                                                    
         MVC   DUB(3),L.LOCKEST                                                 
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
TSTLOCK1 BAS   RE,TSTIT                                                         
*                                                                               
         CLI   SVAPROF+7,C'C'      FOR CAN, MAKE SURE MED C NOT LOCKED          
         BNE   TSTLKEQ                                                          
         CLI   PAYMD,C'T'                                                       
         BE    *+12                                                             
         CLI   PAYMD,C'N'                                                       
         BNE   TSTLKEQ                                                          
         MVI   L.LOCKMED,C'C'                                                   
         CLI   SVKEY+9,0           NO EST = LEAVE AS NULLS                      
         BE    TSTLOCK3            LOCKED IF ANY EST IS LOCKED                  
         MVC   L.LOCKEST,DUB                                                    
         BAS   RE,TSTIT                                                         
*                                                                               
         MVC   L.LOCKEST,SPACES                                                 
TSTLOCK3 BAS   RE,TSTIT                                                         
         B     TSTLKEQ                                                          
         DROP  L                                                                
*                                                                               
TSTIT    LR    R0,RE                                                            
*                                                                               
TSTIT2   L     RF,VCOMFACS                                                      
         L     RF,(CLOCKET-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB,('LKTESTQ',LKUPKEY),VCOMFACS                           
         CLI   4(R1),1             TEST LOCKED                                  
         BE    TSTLKNEQ                                                         
         CLI   4(R1),2             TEST TABLE BUSY                              
         BE    TSTIT2                                                           
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
TSTLKEQ  CR    RB,RB                                                            
         B     *+6                                                              
TSTLKNEQ LTR   RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==========================================================*                    
* R6 POINTS TO THE COMMENT ELEMENT                         *                    
*==========================================================*                    
SCOM     NTR1  BASE=*,LABEL=*                                                   
         CLI   SVSRCCOM,C'N'       TEST SUPPRESS SRC COMMENTS                   
         BE    SCOMEX                                                           
*                                                                               
         LA    R6,BDELEM                                                        
*                                                                               
SCOM5    SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    SCOMEX                                                           
         CLI   0(R6),X'66'                                                      
         BNE   SCOM5                                                            
         CLC   =C'SRC=',3(R6)                                                   
         BNE   SCOM5                                                            
*                                                                               
         LHI   R0,MAXAMTS          MAXIMUM NUMBER OF COMMENT LINES              
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
                                                                                
* FIND BLANK COMMENT LINE                                                       
* IF NONE FOUND, GIVE ERROR MESSAGE                                             
                                                                                
SCOM10   TM    AMTFLAGS,X'20'      TEST COMMENT THIS LINE                       
         BZ    SCOM20                                                           
         AHI   R8,AMTLEN                                                        
         BCT   R0,SCOM10                                                        
*                                                                               
         BRAS  RE,FIXSCRN          BUILD SCREEN TO SHOW THE PROBLEM             
         LA    R2,PAYMDH                                                        
         MVC   NERRCD,=AL2(SRC2MANY)                                            
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
*                                                                               
SCOM20   DS    0H                                                               
         LLC   RE,1(R6)            ELEMENT LEN (3 + SRC= + L'COMMENT)           
         AHI   RE,-8               7 CHARS NOT MOVED -1 FOR EX                  
         BNP   SCOMEX                                                           
*                                                                               
         MVC   AMTCOM(2),=C'//'    MOVE // SO CAN RECOGNIZE COMMENT             
         CHI   RE,37               38 CHARS REMAINING                           
         BNH   *+8                                                              
         LHI   RE,37                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   AMTCOM+2(0),7(R6)                                                
*                                                                               
         OI    AMTFLAGS,X'20'      SET COMMENT PRESENT                          
         MVI   GOTSRC,C'Y'         AND SET 'GOT SRC' FLAG                       
*                                                                               
SCOMEX   XIT1                                                                   
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================*               
* ACCUMULATE DOLLARS BY CONTRACT FOR CONTRACT TRADE ANALYSIS    *               
*===============================================================*               
         SPACE 1                                                                
GETCTA   NTR1  BASE=*,LABEL=*                                                   
         TM    BDCIND2,X'02'       TEST TRADE BUY                               
         BO    GETCTA2                                                          
         TM    BDSTAT2,X'20'       TEST NON-CTA TRADE BUY                       
         BZ    GETCTAX                                                          
*                                                                               
GETCTA2  MVI   ELCDLO,X'70'        FIND ID ELEMENT                              
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BE    GETCTA10                                                         
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(NOCTACON) SET MISSING CONTRACT ERR                   
         GOTO1 ERROR                                                            
*                                                                               
GETCTA10 PACK  DUB(4),3(5,R6)      PACK CONTRACT NUMBER                         
*                                                                               
* SEARCH BUFFER FOR CONTRACT                                                    
*                                                                               
         LA    R0,NSVCTA           NUMBER OF SLOTS                              
         LA    R1,SVCTADTA         POINT TO FIRST                               
*                                                                               
GETCTA12 OC    0(4,R1),0(R1)       TEST ENTRY                                   
         BZ    GETCTA20                                                         
         CP    0(4,R1),DUB(4)                                                   
         BE    GETCTA22                                                         
         LA    R1,L'SVCTADTA(R1)                                                
         BCT   R0,GETCTA12                                                      
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(MANYCONS)                                            
         GOTO1 ERROR                                                            
*                                                                               
GETCTA20 ZAP   0(4,R1),DUB(4)      SET CONTRACT NUMBER                          
*                                                                               
GETCTA22 L     R0,4(R1)            GET CURRENT TOTAL                            
         A     R0,BUYTOTG                                                       
         ST    R0,4(R1)            ADD TO IT                                    
*                                                                               
GETCTAX  XIT1                                                                   
         EJECT                                                                  
*==================================================================*            
* FOR PW OOWR ESTIMATES, PUT PAID SPOTS ELEMENT IN PW RECORDS      *            
*==================================================================*            
         SPACE 1                                                                
PAYPW    NTR1  BASE=*,LABEL=*                                                   
         OC    VPWCALC,VPWCALC                                                  
         BNZ   PAYPW2                                                           
         GOTO1 VCALLOV,DMCB,0,X'D9000A79'                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPWCALC,0(R1)                                                    
*                                                                               
PAYPW2   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'                                                  
         MVC   KEY+2(4),SVKEY      A-M/CLT/PRD                                  
         MVC   KEY+6(1),SVKEY+9    EST                                          
         MVC   KEY+7(2),SVKEY+4    MKT                                          
         XC    KEY+9(4),KEY+9                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   PAYPWER1                                                         
*                                                                               
         L     R2,AREC1                                                         
         ST    R2,AREC                                                          
         USING PWRECD,R2                                                        
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
* USE ANY PW ELEMENT FOR THIS MONTH TO GET PW PERCENT                           
*                                                                               
         XC    PAYPWPCT,PAYPWPCT                                                
         LA    R6,24(R2)                                                        
         MVI   ELCDLO,X'05'                                                     
         MVI   ELCDHI,X'05'                                                     
*                                                                               
         SR    R7,R7                                                            
PAYPW10  BRAS  RE,NEXTEL                                                        
         BNE   PAYPW12                                                          
* TEST IF WEEK END DATE IS IN THIS BROADCAST MONTH                              
         GOTO1 VDATCON,DMCB,(2,2(R6)),WORK                                      
         LA    R0,6                                                             
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         MVC   WORK(6),WORK+6                                                   
*                                                                               
         GOTO1 VGETBRD,DMCB,(1,WORK),WORK+6,VGETDAY,VADDAY                      
         GOTO1 VDATCON,DMCB,WORK+12,(3,WORK)                                    
*                                                                               
         CLC   SVBRDYM,WORK        SAME YEAR/MONTH                              
         BNE   PAYPW10                                                          
         LR    R7,R6               SAVE LAST 05 EL THIS MONTH                   
         TM    4(R6),X'80'         TEST OVERRIDE THIS WEEK                      
         BO    PAYPW10             WEEK OVERRIDDEN - GET NEXT WEEK              
         MVC   PAYPWPCT,4(R6)      SAVE PW PERCENT                              
         B     PAYPW14                                                          
*                                                                               
PAYPW12  LTR   R7,R7               TEST FOUND ANY PW EL THIS MONTH              
         BZ    PAYPWER2                                                         
*                                                                               
PAYPW14  BAS   RE,DOPW                                                          
         XC    OWPWBLK,OWPWBLK     CLEAR THE PW OVERRIDE BLOCK                  
         XC    OVRDTGRS,OVRDTGRS   CLEAR THE OVERRIDE GROSS                     
*                                                                               
* NOW DO STATION LEVEL RECORD                                                   
         MVC   KEY+7(5),SVKEY+4    MKT/STA                                      
         CLI   KEY+9,X'E8'         TEST CABLE                                   
         BL    *+8                                                              
         NI    KEY+11,X'80'        DROP NETWORK                                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   PAYPWER1                                                         
*                                                                               
         L     R2,AREC1                                                         
         ST    R2,AREC                                                          
         USING PWRECD,R2                                                        
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,DOPW                                                          
         XIT1                                                                   
         EJECT                                                                  
* MAINTAIN ELEMENTS IN STATION AND MARKET LEVEL RECORDS                         
*                                                                               
DOPW     NTR1                                                                   
*                                                                               
         XC    ELEM,ELEM           BUILD SKELETON ELEMENT                       
         MVI   ELEM,PWOOWCDQ                                                    
         MVI   ELEM+1,PWOOWLNQ                                                  
         MVC   ELEM+2(2),SVBRDYM                                                
*                                                                               
         LA    R6,24(R2)                                                        
         USING PWOOWEL,R6                                                       
         MVI   ELCDLO,PWOOWCDQ                                                  
         MVI   ELCDHI,PWOOWCDQ                                                  
*                                                                               
DOPW0100 BRAS  RE,NEXTEL                                                        
         BNE   DOPW0200                                                         
         CLC   SVBRDYM,PWOOWYM                                                  
         BNE   DOPW0100                                                         
         MVC   ELEM(PWOOWLNQ),0(R6)                                             
         GOTO1 VRECUP,DMCB,(R2),(R6)                                            
*                                                                               
DOPW0200 LA    R6,ELEM                                                          
         OC    PWOOWSPT,PWOOWSPT   1ST TIME FOR THIS MONTH?                     
         JZ    DOPW0300            YES - SO CONTINUE                            
*                                                                               
         L     R0,PWOOWCG          ELSE - GET OLD CLT GROSS                     
         S     R0,OVRDTGRS         MINUS OUT THE OVERRIDE GROSS                 
         ST    R0,PWOOWCG          AND STORE IT BACK                            
DOPW0300 EQU   *                                                                
         L     R0,PWOOWSPT                                                      
         L     RE,TOTSPOTS                                                      
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+6                                                              
         LCR   RE,RE                                                            
         AR    R0,RE                                                            
         ST    R0,PWOOWSPT                                                      
*                                                                               
         L     R0,PWOOWWG          WIMGROSS                                     
         L     RE,TOTG                                                          
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+6                                                              
         LCR   RE,RE                                                            
         AR    R0,RE                                                            
         ST    R0,PWOOWWG                                                       
*                                                                               
         L     R0,PWOOWWN          WIMNET                                       
         L     RE,TOTN                                                          
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+6                                                              
         LCR   RE,RE                                                            
         AR    R0,RE                                                            
         ST    R0,PWOOWWN                                                       
*                                                                               
         L     R0,PWOOWTAX         WIMTAX                                       
         L     RE,TOTTAX                                                        
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+6                                                              
         LCR   RE,RE                                                            
         AR    R0,RE                                                            
         ST    R0,PWOOWTAX                                                      
*                                                                               
* GET THE TOTAL OVERRIDDEN AMOUNTS                                              
*                                                                               
         LA    R5,OWPWBLK          A(PW WEEKLY BUCKET BLOCK)                    
         USING OWPWD,R5                                                         
         LA    R0,OWPWBLK+(5*LOWPWD) A(END OF BLOCK)                            
         XC    WORK,WORK           INIT TEMP GRS/NET/TAX TOTALS STORAGE         
*                                                                               
DOPW0400 EQU   *                                                                
*                                                                               
         OC    OWPWSDT,OWPWSDT     ANY MORE DATA IN BUCKETS?                    
         JZ    DOPW0500            NO - SO ALL DONE                             
*                                                                               
* GROSS DOLLARS                                                                 
*                                                                               
         L     R1,WORK             GET TOTAL GROSS OVERRIDE                     
         A     R1,OWPWGRS          + AMOUNT OVERRIDDEN FOR WEEK                 
         ST    R1,WORK             AND STORE IT FOR LATER                       
*                                                                               
* NET DOLLARS                                                                   
*                                                                               
         L     R1,WORK+4           GET TOTAL NET OVERRIDE                       
         A     R1,OWPWNET          + AMOUNT OVERRIDDEN FOR WEEK                 
         ST    R1,WORK+4           AND STORE IT FOR LATER                       
*                                                                               
* TAX DOLLARS                                                                   
*                                                                               
         L     R1,WORK+8           GET TOTAL TAX OVERRIDE                       
         A     R1,OWPWTAX          + AMOUNT OVERRIDDEN FOR WEEK                 
         ST    R1,WORK+8           AND STORE IT FOR LATER                       
*                                                                               
         LA    R5,LOWPWD(R5)       INC TO NEXT BUCKET                           
         CR    R5,R0               PAST E.O.B.?                                 
         JL    DOPW0400            NO - SO PROCESS NEXT BUCKET                  
         DROP  R5                                                               
*                                                                               
DOPW0500 EQU   *                                                                
*                                                                               
* NOW SET UP CALL TO PW CALC FOR CLIENT AMOUNTS                                 
*                                                                               
         LA    R4,ELEM+128         POINT TO PW BLOCK                            
         USING PWBLKD,R4                                                        
         MVI   PWACT,PWGETBUY                                                   
****     MVC   PWACTBUY,PWOOWWG    WIMGRS INCL TAX                              
         MVC   PWACTBUY,TOTG       THIS RUN'S GROSS (INCL TAX)                  
         MVC   PWPCT,PAYPWPCT      SAVED PW RATE                                
****     MVC   PWTAX,PWOOWTAX                                                   
         MVC   PWTAX,TOTTAX        THIS RUN'S TAX                               
*                                                                               
* SUBTRACT OUT THE OVERRIDDEN WEEKS $                                           
*                                                                               
         L     R1,PWACTBUY         GET THIS RUN'S GROSS                         
         S     R1,WORK             - TOTAL AMOUNT OVERRIDDEN                    
         ST    R1,PWACTBUY         AND STORE IT                                 
*                                                                               
         L     R1,PWTAX            GET THIS RUN'S TAX                           
         S     R1,WORK+8           - TOTAL AMOUNT OVERRIDDEN                    
         ST    R1,PWTAX            AND STORE IT                                 
*                                                                               
         OC    PWPCT,PWPCT         ANY PW PCT?                                  
         JZ    DOPW0600            NO - EVERYTHING OVERRIDDEN                   
*                                                                               
         GOTO1 VPWCALC,DMCB,(R4)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,PWOOWCG          PREVIOUS CLT GROSS                           
         L     RE,PWVAL            THIS RUN'S CLT GROSS                         
         CLI   SVUNPAY,C'Y'        UNPAYING                                     
         BNE   *+6                 NO - SO CONTINUE                             
         LCR   RE,RE               ELSE COMPLIMENT THE AMOUNT                   
         AR    R0,RE               ADD TO PREVIOUS CLT GROSS                    
         ST    R0,PWOOWCG          AND STORE IT                                 
****     MVC   PWOOWCG,PWVAL       CLTGRS W/ TAX (MINUS OVERRIDE $)             
*                                                                               
DOPW0600 EQU   *                                                                
*                                                                               
         CLI   SVUNPAY,C'Y'        UNPAYING?                                    
         BNE   DOPW0700            NO - SO CONTINUE                             
*                                                                               
         OC    PWOOWSPT,PWOOWSPT   ELSE - ANY SPOTS LEFT?                       
         BZ    DOPW0750            NO - SO CONTINUE                             
*                                                                               
DOPW0700 EQU   *                                                                
*                                                                               
         L     R0,PWOOWCG          GET CLIENT GROSS                             
         A     R0,OVRDTGRS         ADD IN THE OVERRIDE AMOUNT                   
         ST    R0,PWOOWCG          AND STORE NEW CLIENT GROSS                   
*                                                                               
DOPW0750 EQU   *                                                                
*                                                                               
* COMPUTE CLIENT NET                                                            
         L     R0,PWOOWCG                                                       
         S     R0,PWOOWTAX                                                      
         LA    R1,170              85*2                                         
         MR    R0,R0                                                            
         D     R0,=F'100'                                                       
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,PWOOWCN                                                       
*                                                                               
         L     R0,PWOOWCTX                                                      
         L     RE,PWCLTTAX                                                      
         CLI   SVUNPAY,C'Y'                                                     
         BNE   *+6                                                              
         LCR   RE,RE                                                            
         AR    R0,RE                                                            
         ST    R0,PWOOWCTX                                                      
****     MVC   PWOOWCTX,PWCLTTAX                                                
         DROP  R4                                                               
* FIND INSERTION POINT IN RECORD                                                
         LA    R6,24(R2)                                                        
DOPW0800 BRAS  RE,NEXTEL                                                        
         BNE   DOPW0900                                                         
         CLC   ELEM+2(2),PWOOWYM   COMPARE Y/M                                  
         BH    DOPW0800                                                         
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0               INSERT AFTER CURRENT ELEMENT                 
         B     DOPW1000                                                         
* IF NO X'26' ELEMENT PRESENT, SEARCH AGAIN FOR X'F1' ACTIVITY ELEM             
DOPW0900 LA    R6,24(R2)                                                        
         MVI   ELCDLO,X'F1'                                                     
         MVI   ELCDHI,X'F1'                                                     
         BRAS  RE,NEXTEL                                                        
*                                                                               
DOPW1000 GOTO1 VRECUP,DMCB,(R2),ELEM,(R6)                                       
         GOTO1 PUTREC                                                           
         XIT1                                                                   
*                                                                               
PAYPWER1 MVC   NERRCD,=Y(NOPWREC)                                               
         B     PAYPWERR                                                         
PAYPWER2 MVC   NERRCD,=Y(NOPWPCT)                                               
*                                                                               
PAYPWERR MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* THIS ROUTINE BUILDS THE BUCKETS FOR THE OVERRIDDEN WEEKS.                     
*                                                                               
         DS    0D                                                               
PWBKBLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,PAYMDH           PUT CURSOR TO MEDIA IF WON'T PAY             
         OC    SVID,SVID                                                        
         BZ    *+8                                                              
         LA    R2,PAYOPH           OR TO 'OPT' IF PAY BY ID                     
*                                                                               
         XC    OWPWBLK,OWPWBLK     CLEAR THE BLOCK OF BUCKETS                   
         LA    R5,OWPWBLK          A(BLOCK)                                     
         USING OWPWD,R5                                                         
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(2),=X'0D7A'     PW REC KEYID                                 
         MVC   KEY+2(4),SVKEY      A-M/CLT/PRD                                  
         MVC   KEY+6(1),SVKEY+9    EST                                          
         MVC   KEY+7(2),SVKEY+4    MKT                                          
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME KEY?                                    
         JNE   PWBKERR             NO - SO ERROR                                
*                                                                               
         L     R1,AREC1                                                         
         ST    R1,AREC                                                          
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
* GET THE WEEKLY COST OVERRIDE ELEMENT TO SET UP BUCKETS FOR OVER-              
* RIDDEN WEEKS.                                                                 
*                                                                               
         LA    R6,24(R1)           POINT TO BEGINING OF DATA                    
         MVI   ELCDLO,X'15'        OVERRIDE ELEMENT CODE                        
         MVI   ELCDHI,X'15'                                                     
*                                                                               
PWBK0100 EQU   *                                                                
*                                                                               
         BRAS  RE,NEXTEL           GET THE ELEMENT                              
         JNZ   PWBK0200            NONE LEFT - ALL DONE                         
*                                                                               
         USING PWCLCEL,R6                                                       
         GOTO1 VDATCON,DMCB,(2,PWCLCWK),(0,WORK) CNVT TO YYMMDD                 
         LA    R0,6                # DAYS TO END OF WEEK                        
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0) GET THE DATE                        
         GOTO1 VDATCON,DMCB,(0,WORK+6),(2,WORK) CNVT TO 2-BYTE                  
*                                                                               
         CLC   WORK(2),SVSTARTP    ELEMENT BEFORE BC MONTH START?               
         JL    PWBK0100            YES - SO GET NEXT ELEMENT                    
*                                                                               
         CLC   WORK(2),SVENDP      ELEMENT AFTER BC MONTH END?                  
         JH    PWBK0200            YES - SO ALL DONE                            
*                                                                               
         MVC   OWPWSDT,PWCLCWK     ELSE - SET BUCKET START DATE                 
         MVC   OWPWEDT,WORK        AND BUCKET END DATE                          
*                                                                               
         L     R0,OVRDTGRS         GET TOTAL OVERRIDDEN GROSS                   
         A     R0,PWCLCAMT         ADD CURRENT ELEMENT AMOUNT                   
         ST    R0,OVRDTGRS         STORE IT BACK IN TOTAL                       
*                                                                               
         LA    R5,LOWPWD(R5)       INC TO NEXT BUCKET                           
         J     PWBK0100            AND GET NEXT OVERRIDE ELEMENT                
*                                                                               
PWBK0200 EQU   *                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
PWBKERR  EQU   *                                                                
*                                                                               
         MVC   NERRCD,=Y(NOPWREC)                                               
         MVI   ERRCD,NEWERRS                                                    
         GOTO1 ERROR                                                            
         DROP  R5,R6                                                            
         EJECT                                                                  
*                                                                               
* THIS ROUTINE BUCKETS BUY $ IF THE WEEK HAS BEEN OVERRIDDEN.                   
*                                                                               
* DMCB:   A(CURRENT BUY $)                                                      
* DMCB+4: DISPLACEMENT OF $ CELL IN BUCKET                                      
*                                                                               
PWOVRIDE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,OWPWBLK          A(PW BUCKETS BLOCK)                          
         USING OWPWD,R5                                                         
         LA    R7,OWPWBLK+(5*LOWPWD) A(END OF BLOCK)                            
*                                                                               
PWOV0100 EQU   *                                                                
*                                                                               
         CLC   RUNDATE,OWPWSDT     BUY BEFORE BUCKET START?                     
         JL    PWOVEXIT            YES - SO ALL DONE                            
*                                                                               
         CLC   RUNDATE,OWPWEDT     ELSE - BUY AFTER BUCKET END?                 
         JNH   PWOV0200            NO - SO BUCKET THE $                         
*                                                                               
         LA    R5,LOWPWD(R5)       ELSE - INC TO NEXT BUCKET                    
         CR    R5,R7               PAST E.O.B.?                                 
         JL    PWOV0100            NO - SO CHECK NEXT BUCKET                    
*                                                                               
         J     PWOVEXIT            ELSE - ALL DONE                              
*                                                                               
PWOV0200 EQU   *                                                                
*                                                                               
         L     R1,DMCB             A(CURRENT $ AMOUNT)                          
         L     R2,DMCB+4           DISPLACEMENT OF $ CELL IN BKT                
         AR    R2,R5               A($ CELL)                                    
         L     R0,0(R2)            CELL $                                       
         A     R0,0(R1)            ADD CURRENT $ TO CELL $                      
         ST    R0,0(R2)            STORE BACK IN THE CELL                       
*                                                                               
PWOVEXIT EQU   *                                                                
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*====================================================================           
* IF ANY SRC COMMENTS HAVE BEEN ADDED TO THE AMT BUFFER, NEED TO                
* MOVE THEM TO THE FF BUY SCREEN, WHICH MAY BE SAVED IN TWA1 IF TEST            
* OPTION IS ACTIVE.                                                             
*====================================================================           
                                                                                
FIXSCRN  NTR1  BASE=*,LABEL=*                                                   
         CLI   SVSCRN,X'FE'        TEST SCREEN LOADED?                          
         BNE   FIXSCR2             NO                                           
                                                                                
* SAVE FE SCREEN IN TWA2 AND READ FF SCREEN FROM TWA1                           
                                                                                
         XC    DMCB(24),DMCB                                                    
         L     RF,VTWA             SAVE CURRENT SCREEN IN TWA1                  
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         MVI   DMCB+8,2            SAVE FE SCREEN ON PAGE 2                     
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,VTWA                        
*                                                                               
         MVI   DMCB+8,1                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,VTWA                       
*                                                                               
FIXSCR2  LHI   R0,MAXAMTS                                                       
         L     R8,ASVAMTS                                                       
         USING AMTD,R8                                                          
         LA    R2,PAYCMT1H                                                      
*                                                                               
FIXSCR4  TM    AMTFLAGS,X'20'      TEST COMMENT IN BUFFER                       
         BZ    FIXSCR6                                                          
         CLI   5(R2),0             TEST COMMENT ON SCREEN                       
         BNE   FIXSCR6             YES                                          
* MOVE COMMENT TO SCREEN                                                        
         LHI   RE,L'PAYCMT1-1      SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),AMTCOM                                                   
         LA    RE,1(RE)                                                         
         STC   RE,5(R2)            SET OUTPUT LENGTH                            
         OI    6(R2),X'81'         SET XMT/MODIFIED                             
*                                                                               
FIXSCR6  AHI   R8,AMTLEN                                                        
         AHI   R2,PAYCMT2H-PAYCMT1H NEXT COMMENT FIELD                          
         BCT   R0,FIXSCR4                                                       
                                                                                
*================================================================               
* NEED TO MAKE SURE NO MORE THAN 5 COMMENTS FOR EACH INVOICE                    
*================================================================               
                                                                                
         LA    R2,PAYINV2H                                                      
         LHI   R0,MAXAMTS-1                                                     
*                                                                               
FIXSCR10 LHI   R4,5                SET MAX 4 MORE COMMENTS                      
*                                                                               
FIXSCR12 AHI   R8,AMTLEN                                                        
         AHI   R2,PAYINV2H-PAYINV1H  NEXT INVOICE FIELD                         
         BCT   R0,*+8                                                           
         B     FIXSCR20              REACHED EOS                                
*                                                                               
         CLI   AMTFLAGS,0            TEST NO MORE INPUT                         
         BE    FIXSCR20                                                         
         TM    AMTFLAGS,X'80'        TEST INVOICE THIS LINE                     
         BO    FIXSCR10                                                         
         BCT   R4,FIXSCR12                                                      
         GOTO1 ERROR                                                            
*                                                                               
FIXSCR20 CLI   SVSCRN,X'FE'        TEST SCREEN LOADED?                          
         BNE   FIXSCRX             NO                                           
                                                                                
*===================================================================            
* EXIT IF NOT IN TEST MODE                                                      
* ELSE SAVE SCREEN WITH COMMENTS IN PAGE 1 AND RESTORE TEST SCREEN              
*===================================================================            
                                                                                
         XC    DMCB(24),DMCB                                                    
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    2 BYTE TERM NO.                              
         MVI   DMCB+8,1            SAVE IN PAGE 1                               
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,VTWA                        
*                                                                               
         MVI   DMCB+8,2            RESTORE FE SCREEN IN TWA                     
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,VTWA                       
*                                                                               
FIXSCRX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0             TEST E-O-R                                   
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLC   0(1,R6),ELCDLO                                                   
         JL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         JH    NEXTEL                                                           
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
NEXTELX  LTR   RE,RE               EXIT WITH CC NEQ                             
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPPAYWORK                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FALOCKUPD                                                      
LKKEYD   DSECT                                                                  
         ORG   LOCKKEY                                                          
LOCKMED  DS    CL1                                                              
LOCKCLT  DS    CL3                                                              
LOCKSTA  DS    CL5                                                              
         ORG   LOCKSTA                                                          
LOCKEST  DS    CL3                                                              
         ORG                                                                    
*                                                                               
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLRST                                                     
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPPWBLOCK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058SPPAY04   10/17/19'                                      
         END                                                                    
