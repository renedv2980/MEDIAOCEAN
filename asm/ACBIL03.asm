*          DATA SET ACBIL03    AT LEVEL 009 AS OF 01/16/13                      
*PHASE T60E03A                                                                  
*INCLUDE GETNARR                                                                
*INCLUDE PQPROF                                                                 
         TITLE 'ACBIL03 - CREATIVE BILLING - PRINT'                             
ACBIL03  CSECT                                                                  
         PRINT NOGEN               PHASE SENDS DRAFT OR LIVE BILLS TO           
         NMOD1 LWSX-LWSD,**BIL3**,RA,RR=R3,CLEAR=YES    PRINT QUEUE             
         USING GWS,R9                                                           
         USING TWAD,R8                                                          
         USING LWSD,RC                                                          
*                                  INITIALIZE LOCAL W/S                         
         ST    RD,AWORK1                                                        
         ST    R3,LRELO            SAVE  RELOCATION AMOUNT                      
         LA    R1,CHOP                                                          
         ST    R1,ACHOP            GET   ADDRESS OF CHOP AREA                   
         L     R2,=AL4(L'CHOP)                                                  
         AR    R1,R2                                                            
         ST    R1,APWRK            PRINT WORK AREA                              
         ST    R1,ANXTWK                                                        
         L     R2,=AL4(L'PWRK)                                                  
         AR    R1,R2                                                            
         ST    R1,APBUF            AND   PRINT BUFFER                           
         XC    P,P                                                              
         MVC   MIDL1,SPACES                                                     
         MVC   MIDL2,SPACES                                                     
         MVI   PREVBILL,C'N'                                                    
         ZAP   TOTALN,=P'0'                                                     
         ZAP   TOTALC,=P'0'                                                     
         ZAP   TOTALD,=P'0'                                                     
         ZAP   TOTALG,=P'0'                                                     
         ZAP   TOTALP,=P'0'                                                     
         LA    R1,MAXPAR                                                        
         STH   R1,MAXPARH                                                       
         LA    R1,PWIDTH                                                        
         STH   R1,PWIDTHH                                                       
         MVI   LASTPC,TRMINATE                                                  
*                                                                               
         GOTO1 VGETFACT,DMCB,0,0                                                
         L     RF,0(,R1)                                                        
*                                                                               
         USING FACTSD,RF                                                        
         AP    FATIME,=P'80000'    USE   NEW YORK TIME IN US                    
         UNPK  DUB,FATIME                                                       
         MVC   TIMENOW,DUB+2                                                    
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* INITIALIZE PRINT QUEUE CALL                                         *         
***********************************************************************         
*                                                                               
         USING PQPLD,R7                                                         
         LA    R7,PC                                                            
         XC    PROFWRK,PROFWRK                                                  
         XC    PROFKEY,PROFKEY                                                  
         MVI   PROFKEY,C'A'                                                     
         MVC   PROFKEY+1(2),=C'BI'                                              
         MVC   PROFKEY+3(2),TWAUSRID                                            
         GOTO1 =V(PQPROF),DMCB,(X'80',PROFKEY),(1,(R7)),ACOMFACS,RR=LREC        
               LO                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'48'      SET  RETENTION TO 48 HRS                     
         MVC   QLRETND,=H'48'                                                   
         MVC   QLDESC,SPACES                                                    
         MVC   QLDESC(2),=C'BI'                                                 
         MVC   QLDESC+2(6),BILNUM                                               
         MVI   QLDESC+10,1         ONE  COPY                                    
         MVC   QLSUBID,=C'ABI'                                                  
         CLI   ACTION,DRA                                                       
         BNE   INIT2                                                            
         MVI   QLSUBID+2,C'D'      ABD FOR DRAFT                                
         MVI   QLTYP1,0            CLEAR REPORT FLAG                            
*                                                                               
INIT2    MVI   QLLPP,68                                                         
         MVC   QLSRCID,TWAUSRID                                                 
         MVI   QLCLASS,C'A'                                                     
         CLI   BILNUM,C'L'         LINEUP                                       
         BNE   INIT4                                                            
         MVC   QLSUBID,=C'LU1'                                                  
         MVC   QLDESC(11),=C'*LINEUP#01*'                                       
         MVI   QLCLASS,0                                                        
         MVI   QLSTAT,KEEP                                                      
         MVI   QLTYP1,0            CLEAR REPORT FLAG                            
*                                                                               
INIT4    MVC   QLSYS(L'QLSUBID),QLSUBID                                         
         BAS   RE,PRIN                                                          
         CLI   BILNUM,C'L'                                                      
         BE    LINEUP                                                           
         B     NARR                                                             
         DROP  R7                                                               
         EJECT ,                                                                
***********************************************************************         
* PRINT LINE-UP PATTERN                                               *         
***********************************************************************         
         SPACE 1                                                                
LINEUP   MVI   PC,PR1SP1                                                        
         XC    P,P                                                              
         BAS   RE,PRIN                                                          
         MVI   BYTE,MID1LNE                                                     
         BAS   RE,SKIPLNE          SKIP TO FIRST MIDLINE                        
         MVI   PC,PR1SP1                                                        
         LA    R0,3                PUT  SOME X'S IN THE MIDLINES                
*                                                                               
LINEUP2  MVI   P,C'X'                                                           
         MVC   P+1(84),P                                                        
         BAS   RE,PRIN                                                          
         BCT   R0,LINEUP2                                                       
         MVI   PC,TRMINATE                                                      
         BAS   RE,PRIN                                                          
         B     OKEND                                                            
         EJECT ,                                                                
***********************************************************************         
* PRINT NARRATIVE AND AMOUNT PER PARAGRAPH                            *         
***********************************************************************         
         SPACE 1                                                                
NARR     L     R6,AIOAREA1         FIRST SAVE BILLED STATUS FROM HDR            
         MVI   WRKPARA,0                                                        
         GOTO1 AWRKLGET,AIOAREA1                                                
         BNE   EXIT                                                             
*                                                                               
         ZAP   PNET,=P'0'                                                       
         ZAP   PCOM,=P'0'                                                       
         ZAP   PNON,=P'0'                                                       
         ZAP   PCSD,=P'0'                                                       
         ZAP   PGST,=P'0'                                                       
         ZAP   PPST,=P'0'                                                       
*                                                                               
         LA    R7,1                PREPARE PARA READING LOOP                    
         STC   R7,WRKPARA          SET  FOR FIRST PARAGRAPH                     
*                                                                               
         USING PARAD,R6                                                         
NARR04   GOTO1 AWRKLGET,AIOAREA1   READ INTO IOAREA1                            
         BNE   EXIT                                                             
*                                                                               
         MVI   CONTIN,C'Y'         ASSUME CONTINUATION                          
         LA    R1,PARAWRK+(L'PARAWRK-1)                                         
         LA    R0,L'PARAWRK                                                     
*                                                                               
NARR08   CLI   0(R1),X'41'         UNLESS IT ENDS WITH A ','                    
         BH    NARR09                                                           
         BCTR  R1,0                                                             
         BCT   R0,NARR08                                                        
*                                                                               
NARR09   CLI   0(R1),C','                                                       
         BE    NARR10              LAST IS A COMMA     - IT'S CONTINUED         
         CLI   0(R1),C';'                                                       
         BE    NARR10              LAST IS A SEMICOLON - IT'S CONTINUED         
         MVI   CONTIN,C'N'                                                      
*                                                                               
NARR10   AP    PNET,PARANET        PARAGRAPH TOTALS                             
         AP    TOTALN,PARANET      AND  BILL TOTALS                             
         AP    PCOM,PARACOM                                                     
         AP    TOTALC,PARACOM                                                   
         AP    PNON,PARANON                                                     
         AP    TOTALN,PARANON                                                   
         AP    PCSD,PARACSD                                                     
         AP    TOTALD,PARACSD                                                   
         AP    PGST,PARAGST                                                     
         AP    TOTALG,PARAGST                                                   
         AP    PPST,PARAPST                                                     
         AP    TOTALP,PARAPST                                                   
         TM    SAVHSTAT,MANBILL    IS   THIS A MANUAL BILL ?                    
         BO    NARR11              YES, KEEP PARAGRAPH TOTALS THEN              
*                                                                               
         USING CHRD,R5                                                          
         LA    R5,CHRTOT           NO,  USE  BILLED TOTALS                      
         ZAP   TOTALC,CHRBCOM                                                   
         DROP  R5                                                               
*                                                                               
NARR11   ZIC   R0,PARAHLNS         NUMBER OF HEADLINES                          
         LTR   R0,R0               IN   PARAGRAPH                               
         BZ    NARR18                                                           
         MVC   MIDL1,SPACES                                                     
         MVC   MIDL2,SPACES                                                     
         LA    R3,PARAEL                                                        
         LA    RE,MIDL1                                                         
*                                                                               
NARR14   ZIC   R1,1(,R3)                                                        
         AR    R3,R1               ->   COMMENT ELEMENT                         
         IC    R1,1(,R3)           LENGTH OF ELEMENT                            
         BCTR  R1,0                                                             
         EXMVC R1,0(RE),SCMNARR-SCMELD(R3)    SAVE HEADLINE                     
         MVI   0(R3),X'FF'         NOP  THE ELEMENT                             
         LA    RE,PWIDTH(,RE)                                                   
         BCT   R0,NARR14                                                        
*                                                                               
NARR18   TM    PARASTAT,FOOTLINE                                                
         BO    NARR30              LEAVE FOOTLINE IN IOAREA1                    
         CLC   PARADSC,SPACES                                                   
         BE    NARR22              NO   DESCRIPTION                             
         CLI   REMLNE,4            IF   I DON'T HAVE 4 LINES                    
         BH    *+8                                                              
         MVI   REMLNE,0            FORCE NEW PAGE                               
         MVC   P+1(36),PARADSC     PRINT DESCRIPTION                            
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
         GOTO1 VUNDERLN,DMCB,(36,PARADSC),P+1                                   
         MVI   PC,PR1SP1           AND  UNDERLINE IT                            
         BAS   RE,PRIN                                                          
*                                                                               
NARR22   GOTO1 PRNARR,AIOAREA1     SET  UP NARRATIVE                            
         CLC   WRKPARA,WRKLPARA                                                 
         BE    NARR25              JUST FORMATTED LAST                          
         LA    R7,1(,R7)                                                        
         STC   R7,WRKPARA          INCREMENT PARAGRAPH NUMBER                   
         CLI   CONTIN,C'Y'         IF   LAST WAS CONTINUED                      
         BE    NARR04              HOLD UP PRINT FOR AWHILE                     
         BAS   RE,ANYPRN           PRINT PREVIOUS                               
         B     NARR04              PROCESS NEXT                                 
*                                                                               
NARR25   BAS   RE,ANYPRN           NOW  PRINT THE LAST PARAGRAPH                
         MVI   PC,SP1                                                           
         BAS   RE,PRIN                                                          
*                                                                               
NARR30   ZIC   R1,CURLNE           WORKOUT LINE NUMBER FOR TOTAL                
         LA    R1,9(,R1)                                                        
         CP    TOTALC,=P'0'                                                     
         BE    NARR34                                                           
         CLI   PFCOMM,C'W'                                                      
         BE    NARR34              COMMISSION IS AT WORKCODE LEVEL              
         LA    R1,2(,R1)           FOR  COMMISSION NEED 2 EXTRA LINES           
*                                                                               
NARR34   CP    TOTALD,=P'0'                                                     
         BE    *+8                                                              
         LA    R1,2(,R1)           FOR  CASH DISCOUNT - ADD 2                   
         STC   R1,GETLNE                                                        
         ZIC   R1,CURLNE                                                        
         LA    R1,2(,R1)                                                        
         CLI   GETLNE,MAXLNE                                                    
         BNH   NARR38              NOT  PASSED TOTAL LINE                       
         MVI   REMLNE,0            PASSED TOTLNE / FORCE NEW PAGE               
         MVI   PC,SP1                                                           
         BAS   RE,PRIN                                                          
         LA    R1,DTLLNE                                                        
         LA    R1,2(,R1)                                                        
*                                                                               
NARR38   STC   R1,GETLNE                                                        
         MVC   BYTE,GETLNE                                                      
         BAS   RE,SKIPLNE          SKIP TO TOTAL LINE                           
*                                                                               
         CLI   PFCOMM,C'W'                                                      
         BE    NARR42              ALREADY ADDED COMMISSION                     
*                                                                               
         CP    TOTALC,=P'0'        ANY  COMMISSION ?                            
         BE    NARR42              NO,  NO NEED TO PRINT NET THEN               
*                                                                               
         ZAP   MYAMOUNT,TOTALN                                                  
         MVCDD P+54(15),AC#NETTO   NET  TOTAL                                   
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
         BAS   RE,EDAMTPND                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         ZAP   MYAMOUNT,TOTALC                                                  
         CP    TOTALC,=P'0'                                                     
         BE    NARR42                                                           
         MVCDD P+54(15),AC#CMN     COMMISSION                                   
*                                                                               
         CLI   PFCOMM,C'F'                                                      
         BNE   *+10                                                             
         MVCDD P+54(15),AC#FEE     FEE                                          
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
         BAS   RE,EDAMTPND                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
NARR42   ZAP   MYAMOUNT,TOTALN                                                  
         AP    MYAMOUNT,TOTALC                                                  
         MVCDD P+54(15),AC#TOTAL   TOTAL                                        
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
         BAS   RE,EDAMTPND                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         ZAP   MYAMOUNT,TOTALG     SAVE GST AMOUNT HERE                         
         ZAP   DUB,TOTALP          SAVE PST IN THERE                            
         ZAP   TOTALG,=P'0'        CLEAR IN CASE NOT NEEDED                     
         ZAP   TOTALP,=P'0'        DITTO                                        
         TM    RUNOPT,NEEDGST      ARE  WE GOING GST LOGIC ?                    
         BZ    NARR50              NO                                           
*                                                                               
         ZAP   TOTALG,MYAMOUNT     PUT  GST BACK NOW                            
         TM    RUNOPT,NEEDPST      NEED PST                                     
         BZ    *+10                NO                                           
         ZAP   TOTALP,DUB                                                       
*                                                                               
         TM    SAVHSTAT,MANBILL    IS   THIS A MANUAL BILL ?                    
         BO    NARR46              YES, KEEP PARAGRAPH TOTALS THEN              
*                                                                               
         USING CHRD,R5                                                          
         LA    R5,CHRTOT           NO,  USE BILLED TOTALS                       
         ZAP   TOTALG,CHRBGST                                                   
         ZAP   TOTALP,CHRBPST                                                   
         DROP  R5                                                               
*                                                                               
NARR46   ZAP   MYAMOUNT,TOTALG                                                  
         CP    TOTALG,=P'0'                                                     
         BE    NARR47                                                           
*                                                                               
         MVCDD P+54(4),AC#VAT      GST AMOUNT                                   
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
*        MVCDD P+61(11),AC#AMT2                                                 
*        LA    R1,P+61                                                          
*        GOTO1 AGETPFLD                                                         
         BAS   RE,EDAMTPND                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
NARR47   CP    TOTALP,=P'0'        ANY  PST HERE ?                              
         BE    NARR48              NO                                           
*                                                                               
         USING TWA1D,RF                                                         
         LA    R5,VATNUM           LOOP THRU SVTAB, PRINT PROVINCES             
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         DROP  RF                                                               
*                                                                               
         USING SVTABD,R4                                                        
         BAS   RE,SORTSV           SORT SVTAB BY PROVINCE DESCRIPTION           
         XC    SVPRD,SVPRD         PREV PROVINVE DESC                           
         ZAP   MYAMOUNT,=P'0'                                                   
*                                                                               
NARR47B  CLI   SVTYPE,SVT_PST      IS   THIS A PST ENTRY ?                      
         BNE   NARR47D             NO,  GET NEXT                                
*                                                                               
         OC    SVPRD,SVPRD         IS   THERE A PREV ?                          
         BZ    NARR47C             NO                                           
*                                                                               
         CLC   SVPRDESC,SVPRD      SAME PROVINCE DESCRIPTION ?                  
         BE    NARR47C             YES                                          
*                                                                               
         BAS   RE,PRTDESC          PRINT CURRENT TOTAL                          
         ZAP   MYAMOUNT,SVPST                                                   
         MVC   SVPRD,SVPRDESC                                                   
         B     NARR47D                                                          
*                                                                               
NARR47C  AP    MYAMOUNT,SVPST                                                   
         MVC   SVPRD,SVPRDESC                                                   
*                                                                               
NARR47D  LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R5,NARR47B                                                       
         CP    MYAMOUNT,=P'0'      ANYTHING LEFT ?                              
         BE    NARR48                                                           
         BAS   RE,PRTDESC                                                       
         DROP  R4                                                               
*                                                                               
         EJECT ,                                                                
         SPACE 1                                                                
NARR48   ZAP   MYAMOUNT,TOTALN     PRODUCE GRAND TOTAL (WITH TAX)               
         AP    MYAMOUNT,TOTALC                                                  
         AP    MYAMOUNT,TOTALG                                                  
         AP    MYAMOUNT,TOTALP                                                  
*                                                                               
         MVCDD P+54(15),AC#GRTOT   GRAND TOTAL                                  
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
         BAS   RE,EDAMTPND                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
NARR50   CP    TOTALD,=P'0'        CASH DISCOUNT                                
         BE    NARR54                                                           
         MVI   PC,SP1                                                           
         BAS   RE,PRIN                                                          
         ZAP   MYAMOUNT,TOTALD                                                  
         MVCDD P+54(15),AC#CSHDS   CASH DISCOUNT                                
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
         BAS   RE,EDAMTPND                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
NARR54   MVI   PC,SP1                                                           
         BAS   RE,PRIN                                                          
         MVI   P+54,C'-'                                                        
         MVC   P+55(29),P+54                                                    
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
         ZAP   MYAMOUNT,TOTALN                                                  
         AP    MYAMOUNT,TOTALC                                                  
         SP    MYAMOUNT,TOTALD                                                  
         AP    MYAMOUNT,TOTALG                                                  
         AP    MYAMOUNT,TOTALP                                                  
*                                                                               
         MVCDD P+54(15),AC#INVTO   INVOICE TOTAL                                
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
         BAS   RE,EDAMTPND                                                      
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         MVCDD P+54(15),AC#CRAM    CREDIT AMOUNT                                
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
         CP    MYAMOUNT,=P'0'                                                   
         BL    NARR62                                                           
         BE    NARR64                                                           
*                                                                               
         MVCDD P+54(30),AC#PPTA2   PLEASE PAY THIS AMOUNT                       
         LA    R1,P+54                                                          
         GOTO1 AGETPFLD                                                         
*                                                                               
NARR62   MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
         MVI   P+54,C'-'                                                        
         MVC   P+55(29),P+54                                                    
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
NARR64   TM    RUNOPT,NEEDGST      ARE  WE USING GST ?                          
         BZ    NARR66              NO                                           
         TM    SAVHSTAT,MANBILL    YES, IS THIS A MANUAL BILL ?                 
         BO    NARR66              YES, SKIP RECAP                              
         BAS   RE,DORECAP          NO,  PRINT RECAP                             
*                                                                               
         USING PARAD,R6                                                         
NARR66   L     R6,AIOAREA1         PRINT FOOTLINE IF ANY                        
         TM    PARASTAT,FOOTLINE                                                
         BNO   NARR68                                                           
         MVI   PC,SP1                                                           
         BAS   RE,PRIN                                                          
         GOTO1 PRNARR,AIOAREA1                                                  
         BAS   RE,ANYPRN                                                        
*                                                                               
NARR68   CLI   PFSTND,C'Y'                                                      
         BE    *+12                PRINT STANDARD COMMENTS                      
         CLI   PFSTND,C'M'                                                      
         BNE   NARR69              STANDARD COMMENTS  - MATCH MEDIA             
*                                                                               
NARR68A  MVI   COMSW,C'Y'                                                       
         L     R5,ADJOB                                                         
         BAS   RE,COMPRT           JOB     COMMENTS                             
         L     R5,ADPRD                                                         
         BAS   RE,COMPRT           PRODUCT COMMENTS                             
         L     R5,ADCLI                                                         
         BAS   RE,COMPRT           CLIENT  COMMENTS                             
         MVI   COMSW,0                                                          
         BAS   RE,ANYPRN                                                        
*                                                                               
NARR69   MVC   WORK,SPACES                                                      
         MVC   WORK(4),=C'ABIL'                                                 
         MVC   WORK+4(2),TWAUSRID  ORIGIN                                       
         MVC   WORK+6(3),JOBKEY+3  CLIENT CODE                                  
         GOTO1 =V(GETNARR),DMCB,WORK,(R6),VDATAMGR,RR=RB                        
         LA    R5,5*132(,R6)                                                    
         LA    R0,6                                                             
*                                                                               
NARR72   CLC   SPACES,0(R5)                                                     
         BNE   NARR76                                                           
         SH    R5,=H'132'                                                       
         BCT   R0,NARR72                                                        
         B     NARR92                                                           
*                                                                               
*                                  SUBSTITUTE  VARIABLE SYMBOLS                 
NARR76   LA    R3,6*132(,R6)       END  OF NARRATIVE CHUNK                      
         LA    R2,1                BUMP BY ONE BYTE ONLY                        
         LR    R4,R6               AND  START AT FIRST LINE                     
*                                                                               
NARR80   CLC   0(2,R4),=C'&&&&'    VARIABLE?                                    
         BNE   NARR84                                                           
         GOTO1 VDATCON,DMCB,(1,BILDATE),(0,P)                                   
         PACK  DUB,4(2,R4)         NUM  OF DAYS FORWARD                         
         CVB   R1,DUB                                                           
         ST    R1,DMCB+8                                                        
         GOTO1 VADDAY,DMCB,P,P+8                                                
         MVC   0(7,R4),SPACES      CLEAR VARIABLE                               
         GOTO1 VDATCON,DMCB,(0,P+8),(8,(R4))                                    
*                                                                               
NARR84   BXLE  R4,R2,NARR80                                                     
*                                                                               
NARR88   MVC   P,0(R6)                                                          
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
         LA    R6,132(R6)                                                       
         BCT   R0,NARR88                                                        
*                                                                               
NARR92   MVI   PC,TRMINATE                                                      
         BAS   RE,PRIN                                                          
         B     OKEND                                                            
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
* PRINT COMMENTS FROM CLIENT, PRODUCT OR JOB RECORDS                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SCMELD,R5           MAP  STANDARD COMMENT ELEMENT                
         SPACE 1                                                                
COMPRT   NTR1                                                                   
         LR    RF,R5                                                            
         MVI   PLCNT,0                                                          
         L     R3,ACHOP                                                         
         AH    R5,DATADISP         ->   1ST  ELEMENT IN RECORD                  
*                                                                               
COMPRT1  CLI   0(R5),SCMELQ        X'3E' - STANDARD COMMENTS ELEMENT            
         BNE   COMPRT9                                                          
         TM    SCMTYPE,SCMTPRBI    PRINT ON BILLS ?                             
         BZ    COMPRT8             NO,  NOP THE ELEMENT                         
         CLI   PFSTND,C'M'         MATCH MEDIA ?                                
         BNE   COMPRT9             NO,  DO NOT MATCH                            
         LA    R1,SCMNARR                                                       
*                                                                               
COMPRT3  CLI   0(R1),C' '                                                       
         BNE   COMPRT4                                                          
         LA    R1,1(,R1)                                                        
         B     COMPRT3                                                          
*                                                                               
COMPRT4  CLC   LJOB(1),0(R1)                                                    
         BE    COMPRT9                                                          
*                                                                               
COMPRT8  MVI   0(R5),X'FF'         IGNORE THIS COMMENT                          
*                                                                               
COMPRT9  ZIC   R0,1(,R5)                                                        
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BNE   COMPRT1                                                          
         LR    R5,RF                                                            
         AH    R5,DATADISP         R5 TO FIRST ELEMENT IN RECORD                
         B     PRNARR1             FINISHED ADJUSTING - GO PRINT                
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* RECURSIVE ROUTINE TO SET UP AND PRINT NARRATIVE LINES FROM A RECORD *         
* (ASSUMES ONLY ONE LEVEL OF NESTING IN USE OF IOAREAS)               *         
*                                                                     *         
* RECORD (PARAGRAPH OR COMMENT) IS ADDRESSED BY WORD AT R1            *         
***********************************************************************         
         SPACE 1                                                                
PRNARR   NTR1                                                                   
         MVI   PLCNT,0                                                          
         L     R5,0(,R1)                                                        
         CLI   0(R5),X'0C'                                                      
         BNE   PRNARR0                                                          
         AH    R5,DATADISP         COMMENT REC                                  
         B     PRNARR0A                                                         
*                                                                               
PRNARR0  LA    R5,2(,R5)           PARAGRAPH REC                                
*                                                                               
PRNARR0A L     R3,ACHOP            FIRST BUILD IN CHOP AREA                     
*                                                                               
PRNARR1  CLI   0(R5),0             ANY  MORE ELEMENTS ?                         
         BNE   PRNARR2                                                          
         BAS   RE,ANYCHOP          FORMAT THE LAST BIT OF DATA                  
         B     EXIT                                                             
*                                                                               
PRNARR2  CLI   0(R5),SCMELQ        X'3E' - STANDARD COMMENT ELEMENT ?           
         BE    PRNARR4                                                          
*                                                                               
PRNARR3  ZIC   R0,1(,R5)                                                        
         AR    R5,R0                                                            
         B     PRNARR1                                                          
*                                                                               
         USING SCMELD,R5           MAP  STANDARD COMMENT ELEMENT                
PRNARR4  CLI   SCMTYPE,0           NESTED - READ COMMENT RECORD                 
         BE    PRNARR6             AND  MAKE RECURSIVE CALL                     
         CLI   COMSW,C'Y'          PRINTING CLI/PRD/COMMENTS ?                  
         BNE   PRNARR5             BRANCH IF NOT                                
         XC    0(PWIDTH,R3),0(R3)  LEAVE A BLANK LINE BETWEEN                   
         MVI   0(R3),1             STANDARD COMMENTS                            
         LA    R3,PWIDTH(,R3)                                                   
*                                                                               
PRNARR5  BAS   RE,ANYCHOP                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(6),SCMNARR                                                 
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   PRNARR6             ANY  PROBLEM- TREAT AS STANDARD NARR         
         GOTO1 PRNARR                                                           
         B     PRNARR3                                                          
*                                                                               
PRNARR6  DS    0H                  STANDARD NARRATIVE                           
         CLC   SCMNARR(2),=C'S='   CHECK FOR SPACE LINE                         
         BNE   PRNARR7                                                          
         ZIC   R1,SCMLN                                                         
         SH    R1,=H'7'                                                         
         BM    PRNARR8             MISSING OR NON-NUMERIC TREATED AS            
         MVC   WORK(3),=3C'0'      STANDARD                                     
         EX    R1,PRNARRM                                                       
         CLC   WORK(3),=3C'0'                                                   
         BNE   PRNARR8                                                          
         EX    R1,PRNARRP                                                       
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    PRNARR6A                                                         
         CH    R1,=H'9'                                                         
         BNH   PRNARR6B                                                         
*                                                                               
PRNARR6A LA    R1,1                0   OR   >    9, TREATED AS 1                
*                                                                               
PRNARR6B XC    0(PWIDTH,R3),0(R3)                                               
         MVI   0(R3),1             SPACING INDICATOR                            
         LA    R3,PWIDTH(,R3)                                                   
         BCT   R1,PRNARR6B                                                      
         B     PRNARR3                                                          
*                                                                               
PRNARRM  MVZ   WORK(0),SCMNARR+2                                                
PRNARRP  PACK  DUB,SCMNARR+2(0)                                                 
*                                                                               
PRNARR7  CLC   SCMNARR(4),=C'<PA>' CHECK FOR NEW PAGE                           
         BNE   PRNARR8                                                          
         XC    0(PWIDTH,R3),0(R3)                                               
         MVI   0(R3),2             NEW  PAGE INDICATOR                          
         B     PRNARR3                                                          
*                                                                               
PRNARR8  CLI   SCMNARR,C' '        SPACE AT   START OF LINE MEANS FORCE         
         BNE   PRNARR9                   TO   NEW LINE                          
         BAS   RE,ANYCHOP                                                       
         MVI   SCMNARR,0           STOP CHOPPER DROPPING LEADING SPACES         
*                                                                               
PRNARR9  ZIC   R1,SCMLN                                                         
         SH    R1,=H'5'                                                         
         BZ    PRNARR9A            ONE INPUT CHARACTER MEANS BLANK LINE         
         EX    R1,PRNOC                                                         
         BZ    PRNARR3             DO   NOT  ADD ALL BINARY ZERO                
*                                                                               
PRNARR9A EX    R1,PRNMVC                                                        
         AR    R3,R1               BUMP POINTER                                 
         MVI   1(R3),C' '                                                       
         LA    R3,2(,R3)                                                        
         B     PRNARR3                                                          
*                                                                               
PRNOC    OC    SCMNARR(0),SCMNARR                                               
PRNMVC   MVC   0(0,R3),SCMNARR     MOVE COMMENT TO PRINT BUFFER AND             
         DROP  R5                                                               
         EJECT ,                                                                
         SPACE 1                                                                
ANYCHOP  ST    RE,SAVRE                                                         
         C     R3,ACHOP            CHECK FOR ANYTHING IN MULTI-LINE             
         BER   RE                  PRINT BUFFER - IF SO CHOP AND PRINT          
         BCTR  R3,0                                                             
         LR    R0,R3                                                            
         L     R3,ACHOP                                                         
         SR    R0,R3                                                            
         L     R2,ANXTWK                                                        
         LA    RF,PWIDTH                                                        
         GOTO1 VCHOPPER,DMCB,(R3),((RF),(R2)),20,C'LEN=',(R0)                   
         L     R7,8(,R1)                                                        
         MH    R7,PWIDTHH                                                       
         A     R7,ANXTWK                                                        
         ST    R7,ANXTWK           ADDRESS OF NEXT PRINT LINE                   
         L     R3,ACHOP            RE-INIT CHOP AREA                            
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* PRINT THE FORMATTED LINES                                           *         
***********************************************************************         
         SPACE 1                                                                
ANYPRN   NTR1                                                                   
         ZAP   MYAMOUNT,PNET       NET                                          
         AP    MYAMOUNT,PNON       NON-COMMISSIONABLE                           
         CLI   PFCOMM,C'W'                                                      
         BNE   *+10                ADD  COMMISSION BY WORKCODE                  
         AP    MYAMOUNT,PCOM       PARAGRAPH TOTAL                              
*                                                                               
         ZAP   PNET,=P'0'                                                       
         ZAP   PCOM,=P'0'                                                       
         ZAP   PNON,=P'0'                                                       
         ZAP   PCSD,=P'0'                                                       
         ZAP   PGST,=P'0'                                                       
         ZAP   PPST,=P'0'                                                       
*                                                                               
         L     R7,APWRK            START OF PRINT WORK                          
         L     R5,ANXTWK           NEXT AVAILABLE LINE                          
         CR    R7,R5                                                            
         BNE   ANYPRN2                                                          
         CP    MYAMOUNT,=P'0'                                                   
         BE    EXIT                NO   LINES AND NO TOTAL                      
*                                                                               
ANYPRN2  SH    R5,PWIDTHH          BACK UP TO FIND LAST LINE                    
         CR    R7,R5               TOP  OF PARAGRAPH/PRINT LINES                
         BH    ANYPRN4                                                          
         CLI   0(R5),1             EXCEPT SPACING OR NEW PAGE                   
         BE    ANYPRN2                                                          
         CLI   0(R5),2                                                          
         BE    ANYPRN2                                                          
         ST    R5,ATOTLN           SAVE ADDRESS OF TOTAL LINE                   
         B     ANYPRN6                                                          
*                                                                               
ANYPRN4  L     R5,ANXTWK           NEED TO PRINT ONE LINE                       
         MVC   0(PWIDTH,R5),SPACES                                              
         ST    R5,ATOTLN           SAVE ADDRESS OF TOTAL LINE                   
         LA    R5,PWIDTH(,R5)      ->   LINE AFTER LAST                         
         ST    R5,ANXTWK                                                        
*                                                                               
ANYPRN6  L     R7,APWRK            START OF PRINT WORK                          
         L     R5,ANXTWK                                                        
         BCTR  R5,0                                                             
         LA    R4,PWIDTH           SET  INCREMENT FOR LOOP                      
*                                                                               
ANYPRN8  MVC   P,SPACES                                                         
         CLI   0(R7),2                                                          
         BE    ANYPRN16            NEW  PAGE                                    
         CLI   0(R7),1             SPECIAL SPACE LINE                           
         BE    *+10                                                             
         MVC   P+1(PWIDTH),0(R7)                                                
         C     R7,ATOTLN           IS   THIS TOTAL LINE ?                       
         BNE   ANYPRN9                                                          
         CP    MYAMOUNT,=P'0'      DO   NOT  PRINT ZERO TOTAL                   
         BE    ANYPRN9                                                          
         BAS   RE,EDAMTPND         EDIT THE PARAGRAPH TOTAL                     
*                                                                               
ANYPRN9  MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
         B     ANYPRN20                                                         
*                                                                               
ANYPRN16 MVI   REMLNE,0            FORCE NEW PAGE                               
*                                                                               
ANYPRN20 BXLE  R7,R4,ANYPRN8                                                    
         MVI   PC,PR1SP1           SPACE AFTER PARAGRAPH                        
         BAS   RE,PRIN                                                          
         MVC   ANXTWK,APWRK        RESET START ADDRESS                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* PRINT GST RECAP BASED ON FIGURES CALCULATED IN BLDCHR               *         
***********************************************************************         
         SPACE 1                                                                
DORECAP  NTR1                                                                   
         MVCDD P+41(17),AC#TAXAN   TAX  ANALYSIS                                
         LA    R1,P+41                                                          
         GOTO1 AGETPFLD                                                         
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
*                                  TAX  ANALYSIS UNDERLINED                     
         MVCDD P+41(17),AC#TAXAN,LU                                             
         LA    R1,P+41                                                          
         GOTO1 AGETPFLD                                                         
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         MVCDD P+20(18),AC#TAXAC   TAX  ACCOUNT #                               
         LA    R1,P+20                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+44(5),AC#BASIS,R  BASIS                                        
         LA    R1,P+44                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+55(4),AC#RATE,R   RATE                                         
         LA    R1,P+55                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+67(7),AC#AMT,R    AMOUNT                                       
         LA    R1,P+67                                                          
         GOTO1 AGETPFLD                                                         
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
*                                  TAX  ACCOUNT # UNDERLINED                    
         MVCDD P+20(18),AC#TAXAC,LU                                             
         LA    R1,P+20                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+44(5),AC#BASIS,RU BASIS          UNDERLINED                    
         LA    R1,P+44                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+55(4),AC#RATE,RU  RATE           UNDERLINED                    
         LA    R1,P+55                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+67(7),AC#AMT,RU   AMOUNT         UNDERLINED                    
         LA    R1,P+67                                                          
         GOTO1 AGETPFLD                                                         
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         BAS   RE,SORTSV           SORT SVTABLE FOR PRINT                       
*                                                                               
         MVI   BYTE,SVT_GST        PRINT GST                                    
         MVCDD P+17(4),AC#VAT      GST                                          
         LA    R1,P+17                                                          
         GOTO1 AGETPFLD                                                         
         BAS   RE,PRTTYPE                                                       
*                                                                               
         MVI   BYTE,SVT_PST        PRINT PST                                    
         XC    SVPRD,SVPRD         FOR  PROVINCE DESCRIPTION                    
         BAS   RE,PRTTYPE                                                       
         MVC   P+20(4),SPACES      IN   CASE NOTHING PRINTED                    
*                                                                               
DORECX   B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
         USING TWA1D,RF                                                         
         SPACE 1                                                                
PRTTYPE  NTR1                                                                   
         LA    R5,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         DROP  RF                                                               
*                                                                               
         USING SVTABD,R4                                                        
PT10     CLI   SVTYPE,X'00'        IS   TYPE DEFINED ?                          
         BE    PT50                NO,  GET NEXT RECORD                         
*                                                                               
         CLC   SVTYPE,BYTE         PRINTING THIS TYPE ?                         
         BNE   PT50                NO,  GET NEXT RECORD                         
*                                                                               
         CLI   SVTYPE,SVT_PST      IS   THIS PST ?                              
         BNE   PT20                NO,  CONTINUE                                
         CLC   SVPRDESC,SVPRD      IS   THIS A NEW PROVINCE DESC ?              
         BE    PT35                NO,  ONLY PRINT REG # ON NEW PROV            
         B     PT30                YES, ATTEMPT TO PRINT THE DESC               
*                                                                               
PT20     MVCDD WORK(4),AC#VAT      GST                                          
         LA    R1,WORK                                                          
         GOTO1 AGETPFLD                                                         
         CLC   P+17(4),WORK        IS   THIS THE FIRST LINE PRINTED ?           
         BNE   PT35                NO,  ONLY PRINT THE GST REG 1X               
*                                                                               
PT30     CLI   PFREG,C'N'                                                       
         BE    *+10                                                             
         MVC   P+25(12),SVREG                                                   
*                                                                               
PT35     CLI   BYTE,SVT_PST        IS   THIS PST ?                              
         BNE   PT40                NO                                           
         MVI   P+40,C'/'           PRINT PROVINCE                               
         MVC   P+41(2),SVPRVNCE                                                 
         CLC   SVPRDESC,SVPRD      IS   THIS A NEW PROVINCE DESC ?              
         BE    PT40                NO                                           
         MVC   P+17(6),SVPRDESC    YES, PRINT DESCRIPTION                       
         MVC   SVPRD,SVPRDESC      SAVE CURRENT PROVINCE DESCRIPTION            
*                                                                               
PT40     EXTED SVBASE,(11,P+38),2,MINUS=YES,LANG=PLANG                          
*        CLC   SVPSTPRV,=C'PQ'                                                  
*        BNE   PT45                                                             
         EXTED SVRATE,(8,P+52),3,MINUS=YES,TRAIL=C'%',LANG=PLANG                
         B     PT48                                                             
*                                                                               
PT45     EXTED SVRATE,(7,P+53),2,MINUS=YES,TRAIL=C'%',LANG=PLANG                
*                                                                               
PT48     EXTED SVGST,(11,P+63),2,MINUS=YES,LANG=PLANG                           
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
PT50     LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R5,PT10                                                          
*                                                                               
PTX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         USING TWA1D,RF                                                         
         SPACE 1                                                                
SORTSV   NTR1                                                                   
         LA    R2,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         LA    R3,SVKEYLN                                                       
         LA    R5,SVTABLNQ                                                      
         GOTO1 VXSORT,DMCB,(0,(R4)),(R2),(R5),(R3),0                            
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* NOTE: AMOUNT IS PASSED IN MYAMOUNT                                  *         
***********************************************************************         
         SPACE 1                                                                
PRTDESC  NTR1                                                                   
         CLI   PLANG,LANGFRE       FRENCH ?                                     
         BE    PRTDES10            YES, SKIP                                    
         MVC   P+54(6),SVPRD       QST, ONT, PST, OR   HST                      
*        MVCDD P+61(11),AC#AMT2    AMOUNT                                       
*        LA    R1,P+61                                                          
         B     PRTDES20            CONTINUE                                     
*                                                                               
PRTDES10 DS    0H                                                               
*        MVCDD P+54(10),AC#AMT2    MONTANT   DE                                 
*        LA    R1,P+54                                                          
         MVC   P+54(6),SVPRD       TVQ, ONT, OR   HST                           
*                                                                               
*RTDES20 DS    0H                                                               
*        GOTO1 AGETPFLD            GET  PRINT     FIELD                         
PRTDES20 BAS   RE,EDAMTPND         EDIT AMOUNT    VALUE                         
         MVI   PC,PR1SP1           PRINT     1    SPACE     1                   
         BAS   RE,PRIN             PRINT     THE  LINE                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO PRINT TO PRINT QUEUE                                     *         
*                                                                     *         
* HANDLES LINE AND PAGE CONTROL AND CALLS HEADS FOR HEADINGS          *         
* ON ENTRY PC CONTAINS PRINT COMMAND CODE AND P CONTAINS PRINT LINE   *         
* ON EXIT P IS SPACE-FILLED                                           *         
* ON ERROR CONTROL IS RETURNED DIRECT TO ROOT WITH PRINT SEQUENCE     *         
* TERMINATED                                                          *         
***********************************************************************         
         SPACE 1                                                                
PRIN     NTR1                                                                   
         CLI   PC,INITIAL          INITIAL                                      
         BNE   PRIN2                                                            
         ZAP   PAGE,=P'0'                                                       
         MVI   REMLNE,0            FORCE FIRST HEADLINE                         
         MVI   CURLNE,MAXLNE       CURRENT LINE                                 
         B     PRIN5                                                            
*                                                                               
PRIN2    TM    PC,X'F0'            PRINT OR SPACE ONE                           
         BNZ   PRIN5                                                            
         CLI   REMLNE,0                                                         
         BH    PRIN4                                                            
         AP    PAGE,=P'1'          FORCE NEW PAGE & PRINT HEADS                 
         MVI   REMLNE,MAXLNE                                                    
         MVI   CURLNE,1            CURRENT LINE                                 
         MVC   SPC(133),PC                                                      
         BAS   RE,HEADS                                                         
         MVC   PC(133),SPC                                                      
*                                                                               
PRIN4    ZIC   R0,REMLNE                                                        
         SH    R0,=H'1'                                                         
         STC   R0,REMLNE                                                        
         ZIC   R0,CURLNE                                                        
         AH    R0,=H'1'                                                         
         STC   R0,CURLNE                                                        
*                                                                               
PRIN5    L     R2,APBUF                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PC,(R2)                   
         CLI   PC,INITIAL                                                       
         BNE   PRIN7                                                            
*                                                                               
         USING PQPLD,RF                                                         
PRIN6    LA    RF,PC               SAVE REPORT ID AFTER INITIAL CALL            
         MVC   SUBID,QLSUBID                                                    
         MVC   REPNO,QLREPRNO                                                   
         DROP  RF                                                               
*                                                                               
PRIN7    MVC   P,SPACES                                                         
         TM    DMCB+8,X'FE'                                                     
         BNZ   PRIN8                                                            
         MVC   LASTPC,PC                                                        
         B     EXIT                                                             
*                                                                               
PRIN8    CLI   LASTPC,TRMINATE     IF   ERROR ENSURE PRINT SEQUENCE             
         BE    PRIN9                    TERMINATED                              
         MVI   LASTPC,TRMINATE                                                  
         MVI   PC,TRMINATE                                                      
         MVC   WORK(1),DMCB+8                                                   
         BAS   RE,PRIN                                                          
         MVC   DMCB+8(1),WORK                                                   
*                                                                               
PRIN9    L     RD,AWORK1           AND  EXIT TO ROOT                            
         LM    RE,RC,12(RD)                                                     
         MVI   FERN,IOERROR                                                     
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO PRINT HEADINGS                                           *         
***********************************************************************         
         SPACE 1                                                                
HEADS    NTR1                                                                   
         MVI   PC,HEADOF                                                        
         XC    P,P                                                              
         BAS   RE,PRIN                                                          
         MVI   BYTE,RUNLNE         GET  TO RUN ON LINE                          
         BAS   RE,SKIPLNE                                                       
*                                                                               
         CLI   PFRUNDTE,C'Y'       SUPPRESS RUN DATE AND TIME?                  
         BE    HEADS06             YES                                          
*                                                                               
         MVCDD P+1(6),AC#RUNON     RUN  ON                                      
         LA    R1,P+1                                                           
         GOTO1 AGETPFLD                                                         
         GOTO1 VDATCON,DMCB,(1,TODAYP),(8,P+8)                                  
         LA    R2,P+15                                                          
*                                                                               
HEADS02  CLI    0(R2),C' '                                                      
         BE    HEADS04                                                          
         LA    R2,1(,R2)                                                        
         B     HEADS02                                                          
*                                                                               
HEADS04  MVCDD 1(2,R2),AC#AT        AT                                          
         LA    R1,1(,R2)                                                        
         GOTO1 AGETPFLD                                                         
         MVC   4(2,R2),TIMENOW                                                  
         MVI   6(R2),C'.'                                                       
         MVC   7(2,R2),TIMENOW+2                                                
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
HEADS06  OC    PFNAME,PFNAME       PRINT AGENCY NAME AND/OR ADDRESS ?           
         BZ    HEADS10             NO                                           
         CLI   PFNAME,C'N'                                                      
         BE    HEADS10                                                          
         XC    P,P                                                              
         LA    R4,P+27                                                          
         MVI   PC,PR1SP1                                                        
         MVC   0(L'USERNAME,R4),USERNAME                                        
         GOTO1 VCENTER,DMCB,(R4),L'USERNAME                                     
         CLI   PFNAME,C'Y'         PRINT NAME ONLY ?                            
         BE    HEADS08             YES                                          
         CLI   PFNAME,C'B'         NO,  PRINT BOTH ?                            
         BNE   *+8                 NO                                           
         BAS   RE,PRIN                                                          
         LA    R4,P+27                                                          
         MVC   0(L'USERADDR,R4),USERADDR                                        
         GOTO1 VCENTER,DMCB,(R4),L'USERADDR                                     
*                                                                               
HEADS08  MVI   PC,PR1SP1           PRINT IT AND SKIP A LINE                     
         BAS   RE,PRIN                                                          
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
HEADS10  CLI   BILNUM,C'L'                                                      
         BE    EXIT                                                             
         MVI   BYTE,INVLNE                                                      
         BAS   RE,SKIPLNE          GET  TO INVOICE LINE                         
         MVCDD P+40(10),AC#INV     INVOICE                                      
         LA    R1,P+40                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+63(14),AC#INVC    INVOICE NUMBER                               
         LA    R1,P+63                                                          
         GOTO1 AGETPFLD                                                         
         MVC   P+78(6),INVOICE                                                  
         CLI   ACTION,DRA                                                       
         BNE   HEADS12                                                          
         MVCDD P+53(9),AC#DRAFT,R  DRAFT                                        
         LA    R1,P+53                                                          
         GOTO1 AGETPFLD                                                         
         EDIT  BILNUMP,(6,P+78)    USE  REFERENCE NUMBER IF DRAFT               
*                                                                               
HEADS12  MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         MVCDD P+40(10),AC#INV,LU  UNDERLINE INVOICE                            
         LA    R1,P+40                                                          
         GOTO1 AGETPFLD                                                         
         MVCDD P+63(12),AC#INVDT   INVOICE DATE                                 
         LA    R1,P+63                                                          
         GOTO1 AGETPFLD                                                         
         GOTO1 VDATCON,DMCB,(1,BILDATE),(8,P+76)                                
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         CLI   PFDUED,C'Y'         SUPPRESS DUE DATE                            
         BE    HEADS14                                                          
*                                                                               
         LA    R2,JOBINFO                                                       
         ZIC   R0,DUEDAYS          DUE  DATE (US ONLY)                          
         GOTO1 VDATCON,DMCB,(1,BILDATE),(0,WORK)                                
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(8,P+76)                                     
         MVCDD P+67(8),AC#DUEDT    DUE  DATE                                    
         LA    R1,P+67                                                          
         GOTO1 AGETPFLD                                                         
*                                                                               
HEADS14  MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         MVCDD P+76(5),AC#PAGE     PAGE                                         
         LA    R1,P+76                                                          
         GOTO1 AGETPFLD                                                         
         EDIT  PAGE,(2,P+82),ALIGN=LEFT                                         
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         MVI   BYTE,CLILNE                                                      
         BAS   RE,SKIPLNE          GET  TO   CLIENT LINE                        
*                                                                               
         USING ADRELD,R5                                                        
         LA    R5,ADDR                                                          
         MVCDD P+1(7),AC#CLINT     CLIENT                                       
         LA    R1,P+1                                                           
         GOTO1 AGETPFLD                                                         
         MVC   P+9(6),BILCLI                                                    
         MVC   P+16(36),BILCLIN    CLIENT NAME                                  
         CLI   ADRLN,ADRLN1Q                                                    
         BL    *+10                                                             
         MVC   P+57(L'ADRADD1),ADRADD1   AND ADDRESS 1                          
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         MVCDD P+1(7),AC#PRO       PRODUCT                                      
         LA    R1,P+1                                                           
         GOTO1 AGETPFLD                                                         
         MVC   P+9(6),BILPRO                                                    
         MVC   P+16(36),BILPRON    PRODUCT NAME                                 
         CLI   ADRLN,ADRLN2Q                                                    
         BL    *+10                                                             
         MVC   P+57(L'ADRADD1),ADRADD2                                          
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN             AND  ADDRESS 2                               
*                                                                               
         MVCDD P+1(7),AC#MED       MEDIA                                        
         LA    R1,P+1                                                           
         GOTO1 AGETPFLD                                                         
         MVC   P+9(1),MEDIA                                                     
         MVC   P+16(12),MEDNAME    MEDIA NAME                                   
         CLI   ADRLN,ADRLN3Q                                                    
         BL    *+10                                                             
         MVC   P+57(L'ADRADD1),ADRADD3                                          
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN             AND  ADDRESS 3                               
*                                                                               
         MVCDD P+1(7),AC#JOB       JOB                                          
         LA    R1,P+1                                                           
         GOTO1 AGETPFLD                                                         
         MVC   P+9(6),BILJOB                                                    
         MVC   P+16(36),BILJOBN    JOB  NAME                                    
         CLI   ADRLN,ADRLN4Q                                                    
         BL    *+10                                                             
         MVC   P+57(L'ADRADD1),ADRADD4                                          
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN             AND  ADDRESS 4                               
*                                                                               
         USING JOBD,R2                                                          
         LA    R2,JOBINFO                                                       
         MVC   P+16(50),JINFO                                                   
         CLI   ADRLN,ADRLN5Q                                                    
         BL    *+10                                                             
         MVC   P+57(L'ADRADD1),ADRADD5                                          
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN             PRINT ON BILLS                               
         DROP  R2,R5                                                            
*                                                                               
         MVI   BYTE,MID1LNE        FIRST MIDLINE                                
         BAS   RE,SKIPLNE                                                       
         MVC   P+1(PWIDTH),MIDL1                                                
         MVCDD P+78(15),AC#TOTAL   TOTAL                                        
         LA    R1,P+78                                                          
         GOTO1 AGETPFLD                                                         
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
         MVC   P+1(PWIDTH),MIDL2                                                
*                                  TOTAL UNDERLINED                             
         MVCDD P+78(15),AC#TOTAL,LU                                             
         LA    R1,P+78                                                          
         GOTO1 AGETPFLD                                                         
         MVI   PC,PR1SP1                                                        
         BAS   RE,PRIN                                                          
*                                                                               
HEADX    MVI   BYTE,DTLLNE                                                      
         BAS   RE,SKIPLNE          SKIP TO FIRST DETAIL LINE                    
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO SKIP TO DESIRED LINE                                     *         
***********************************************************************         
         SPACE 1                                                                
SKIPLNE  NTR1                                                                   
         ZIC   R0,BYTE             DESIRED LINE                                 
         ZIC   R1,CURLNE           CURRENT                                      
         SR    R0,R1                                                            
         BNP   EXIT                                                             
*                                                                               
SKIPLNE2 MVI   PC,SP1                                                           
         BAS   RE,PRIN                                                          
         BCT   R0,SKIPLNE2                                                      
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* EDIT ROUTINES                                                       *         
***********************************************************************         
         SPACE 1                                                                
EDAMTPND ST    RE,SAVRE            SAVE REGISTER                                
         CLI   PFDOL,C'Y'          SUPPRESS THE DOLAR SIGN ?                    
         BE    EDAMOUNT            YES                                          
         EXTED MYAMOUNT,(13,P+71),2,MINUS=YES,CURSYMB=S,LANG=PLANG              
         B     EDAMTEX                                                          
*                                                                               
EDAMOUNT EXTED MYAMOUNT,(13,P+71),2,MINUS=YES,LANG=PLANG                        
*                                                                               
EDAMTEX  L     RE,SAVRE            RESTORE  REGISTER                            
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* EXITS TO ROOT AND EXIT FROM A ROUTINE                               *         
***********************************************************************         
         SPACE 1                                                                
OKEND    MVI   FERN,OK                                                          
         EDIT  REPNO,(5,TEMP),ALIGN=LEFT                                        
         LR    R3,R0               SAVE   LENGTH                                
*                                  CREATE SUBSTITUTION TABLE IN WORK            
         XC    WORK,WORK           CLEAR  FIELD                                 
         MVI   WORK,10             LENGTH OF DRAFT + 1                          
         MVC   WORK+1(9),SPACES    CLEAR  FIELD                                 
         CLI   ACTION,DRA          DRAFT ?                                      
         BNE   OKEND10             NO,    SKIP                                  
         MVCDD WORK+1(9),AC#DRAFT  DRAFT                                        
         LA    R2,WORK+1                                                        
         BAS   RE,CALLDICT                                                      
*                                                                               
OKEND10  MVC   WORK+11(3),SUBID    GET    ID                                    
         CLI   ACTION,DRA          DRAFT ?                                      
         BE    OKEND20             YES,   SKIP                                  
         MVI   WORK+13,C'I'        SAY    LIVE                                  
*                                                                               
OKEND20  MVI   WORK+14,C','        COMMA                                        
         BCTR  R3,0                MINUS  ONE FOR EXECUTE                       
         EXMVC R3,WORK+15,TEMP     GET    THE NUMBER                            
*                                  GET    LENGTH OF SUBID (3)                   
*                                         + 1 (FOR COMMA)                       
         LA    R3,6(,R3)                  + LENGTH OF TEMP + 1                  
         STC   R3,WORK+10          SAVE   LENGTH                                
*                                                                               
*                                  (DRAFT) BILL SENT TO PQ (ID=XXD,N) -         
*                                         ENTER NEXT ACTION                     
         GOTO1 ATXTGET,DMCB,('FVMINFO',2113),(L'MSG,MSG),0,(0,WORK)             
*                                                                               
         GOTO1 VSQUASHR,DMCB,MSG,L'MSG                                          
         B     EXIT                                                             
*                                                                               
OKXIT    SR    RB,RB               CC =   EQU                                   
*                                                                               
ERRXIT   LTR   RB,RB               CC =   NEQ                                   
*                                                                               
EXIT     XIT1  ,                   RETURN TO CALLER                             
         EJECT ,                                                                
***********************************************************************         
* CALL DICTATE                                                        *         
*                                                                     *         
* INPUT:                                                              *         
*   R2=  ADDRESS OF FIELD TO BE TRANSLATED                            *         
*        NOTE: THE FIELD MUST HAVE BEEN PREVIOUSLY INITIALIZED WITH   *         
*              AN MVCDD INSTRUCTION.                                  *         
***********************************************************************         
         SPACE 1                                                                
CALLDICT DS    0H                                                               
         ST    RE,SAVRE            SAVE     RE                                  
         GOTO1 VDICTAT,DMCB,C'SL  ',(R2),0                                      
         L     RE,SAVRE            RESTORE  RE                                  
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
ACHOP    DS    A                   A(CHOP)                                      
APWRK    DS    A                   A(PWRK)                                      
APBUF    DS    A                   A(PBUF)                                      
ANXTWK   DS    A                   A(NEXT PWIDTH PRINT LINE)                    
ATOTLN   DS    A                   A(TOTAL LINE)                                
AWORK1   DS    A                   A(RD)                                        
*                                                                               
LRELO    DS    F                   RELOCATION AMOUNT                            
SAVRE    DS    F                   SAVE RE                                      
*                                                                               
PWIDTHH  DS    H                                                                
MAXPARH  DS    H                                                                
*                                                                               
PREVBILL DS    CL1                 PREVIOUS BILLS BEING PRINTED (Y/N)           
PC       DS    CL1                 PRINT COMMAND CODE                           
P        DS    CL132               PRINT LINE                                   
SPC      DS    CL133               SAVED PC + P                                 
CONTIN   DS    CL1                                                              
LASTPC   DS    CL1                 LAST   SUCCESSFUL PRINT COMMAND CODE         
SUBID    DS    CL3                 3-CHAR REPORT ID                             
REPNO    DS    CL2                 REPORT NUMBER                                
REMLNE   DS    CL1                 REMAINING LINES                              
CURLNE   DS    CL1                 CURRENT   LINE                               
GETLNE   DS    CL1                                                              
PAGE     DS    PL2                 PAGE  NUMBER                                 
MYAMOUNT DS    PL8                 VALUE FOR EDAMOUNT                           
TOTALN   DS    PL(PLAMTLNQ)        TOTAL NET                                    
TOTALC   DS    PL(PLAMTLNQ)        TOTAL COMM                                   
TOTALD   DS    PL(PLAMTLNQ)        CASH  DISCOUNT                               
TOTALG   DS    PL(PLAMTLNQ)        GST   AMOUNT                                 
TOTALP   DS    PL(PLAMTLNQ)        PST   AMOUNT                                 
TIMENOW  DS    CL4                 TIME  AS EBCDIC HHMM                         
COMSW    DS    CL1                 Y WHEN PRINTING CLI/PRD/JOB COMMENTS         
*                                                                               
PROFKEY  DS    CL16                                                             
PROFWRK  DS    CL80                                                             
*                                                                               
PLCNT    DS    CL1                 NUMBER OF 70 BYTE LINES IN APBUFF            
MIDL1    DS    CL(PWIDTH)          PARAGRAPH HEADLINES                          
MIDL2    DS    CL(PWIDTH)                                                       
CHOP     DS    CL2100              CHOP WORK AREA                               
PWRK     DS    CL(MAXPAR*PWIDTH)   PRINT WORKAREA                               
*                                                                               
PBUF     DS    CL14336             PRINT BUFFER                                 
LWSX     DS    0C                                                               
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
*                                  PRINT   COMAND CODES                         
*                                                                               
INITIAL  EQU   X'00'                                                            
HEADOF   EQU   X'89'                                                            
PR1SP1   EQU   X'09'                                                            
SP1      EQU   X'0B'                                                            
TRMINATE EQU   X'FF'                                                            
*                                                                               
KEEP     EQU   X'08'               PQ      STATUS                               
*                                                                               
*                                  EQUATES FOR    LINE NUMBERS                  
*                                                                               
RUNLNE   EQU   2                   RUN            LINE                          
INVLNE   EQU   5                   INVOICE NUMBER LINE                          
CLILNE   EQU   9                   CLIENT  NAME   LINE                          
MID1LNE  EQU   15                  MIDLINE 1                                    
MID2LNE  EQU   16                          2                                    
MID3LNE  EQU   17                          3                                    
DTLLNE   EQU   19                  1ST     DETAIL LINE                          
TOTLNE   EQU   57                  NET     TOTAL  LINE                          
MAXLNE   EQU   62                  LAST    USABLE LINE                          
*                                                                               
*                                  MAXIMUMS                                     
*                                                                               
PWIDTH   EQU   70                                                               
MAXPAR   EQU   150                                                              
         EJECT ,                                                                
         SPACE 1                                                                
* ACBILDSECT                                                                    
       ++INCLUDE ACBILDSECT                                                     
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* ACBILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACBIL03   01/16/13'                                      
         END                                                                    
