*          DATA SET ACREPI502  AT LEVEL 054 AS OF 03/23/15                      
*PHASE ACI502A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE CONVMOS                                                                
*                                                                               
*        NOTE: REQUEST TAPE DEFAULT IS NO TAPE (QOPT1)                          
*                                                                               
*        ABBREVIATIONS   B(R)K   BREAK                                          
*                        BKS     BUCKETS                                        
*                        CK      CHECK                                          
*                        CLI     CLIENT                                         
*                        CMP     COMPANY                                        
*                        DTL     DETAIL                                         
*                        GN      GENERAL                                        
*                        M       MEDIA                                          
*                        OFF     OFFICE                                         
*                        Q       QUERY OR REQUEST                               
*                        RCP     RECAP                                          
*                        RQ      QUERY OR REQUEST                               
         TITLE 'ACI502 - BILLING INTERFACE'                                     
ACI502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACI5**,RA       BASE REGISTERS---RB,RA---11,10               
         USING ACWORKD,RC                                                       
         L     RC,0(R1)            POINTS TO MONACC INFO, ETC                   
         CLI   MODE,PROCTRNS       Q, A TRANSACTION                             
         BE    TRNRT                Y, PROCESS TRANSACTION/ELEMENTS             
         CLI   MODE,PROCACC        Q, AN ACCOUNT RECORD                         
         BE    ACCRT                Y, PROCESS ACCOUNT LEVEL RECORD             
         CLI   MODE,LEVAFRST       Q, LEVEL-A RECORD                            
         BE    ALEVEL               Y, PROCESS LEVEL-A RECORD                   
         CLI   MODE,LEVBFRST       Q, LEVEL-B RECORD                            
         BE    BLEVEL               Y, PROCESS LEVEL-B RECORD                   
         CLI   MODE,REQFRST        Q, FIRST TIME FOR A REQUEST                  
         BE    REQINIT              Y, DO REQUEST INITIALIZATION                
         CLI   MODE,REQLAST        Q, END OF REQUEST                            
         BE    SRTRD                Y, SORT, PRINT REPORTS & WRITE TAPE         
         CLI   MODE,RUNFRST        Q, VERY FIRST TIME AROUND                    
         BE    RUNINIT              Y, DO RUN INITIALIZATION                    
EXIT     XIT1                                                                   
         EJECT                                                                  
SRTRD    TM    IOSW,SRTOPEN        Q, ANY INUT RECORDS                          
         BZ    EXIT                 N, EXIT                                     
         MVC   OMCMPNAM,QCOMPANY                                                
         CLI   QOPT1,C'Y'          Q, TAPE OUTPUT OPTION                        
         BNE   SRTRD050             N, DON'T CLOSE TAPE OUTPUT                  
         NI    IOSW,X'FF'-TPOPEN    Y, CLOSE TAPE OUTPUT FILE                   
         CLOSE (OUTP)                                                           
SRTRD050 GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R5,15,DMCB+4        LAST RECORD FROM SORT                        
         BZ    SRTRD300                                                         
         MVC   SRTWK(SRTLN),0(R5)  MOVE SORTED REC                              
         TM    BRKSW,FSTQDTL       FIRST DETAIL FROM SORT                       
         BO    *+8                                                              
         BAS   RE,QDTL1                                                         
         BAS   RE,CKBRKRT                                                       
SRTRD100 TM    BRKSW,NWCLI                                                      
         BZ    *+8                                                              
         BAS   RE,NWCLIRT          CLIENT BREAK                                 
         TM    BRKSW,NWOFF                                                      
         BZ    *+8                                                              
         BAS   RE,NWOFFRT          OFFICE BREAK                                 
         TM    BRKSW,RQEND         Q, END OF REQUEST                            
         BO    SRTRD400             Y, GO DO REQUEST TOTALS                     
         CLC   SVINVNO,SRTINVNO                                                 
         MVC   SVINVNO,SRTINVNO                                                 
         BNE   SRTRD120                                                         
         AP    TCOUNT,=P'1'                                                     
         B     SRTRD160                                                         
         SPACE 1                                                                
SRTRD120 CP    TCOUNT,=P'0'                                                     
         BE    SRTRD150                                                         
         ZAP   TCOUNT,=P'0'                                                     
         CLC   TRCV(40),PZEROS                                                  
         BE    SRTRD160                                                         
         MVC   P+32(5),=C'TOTAL'                                                
         LA    R3,TRCV                                                          
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
         AP    DTLCT,=P'1'                                                      
SRTRD150 MVC   TRCV(40),PZEROS                                                  
SRTRD160 MVC   P+1(3),SRTCLI        N, MOVE DETAIL TO PRINT                     
         MVC   P+5(3),SRTPRD                                                    
         MVC   P+9(6),SRTJOB                                                    
         GOTO1 SQUASHER,DMCB,P+1,(0,15)                                         
         MVC   P+32(6),SRTINVNO                                                 
         MVC   P+31(1),SRTMCODE                                                 
         CLI   SRTINVDT,C' '       Q, NO INVOICE DATE                           
         BE    SRTRD200             Y, SKIP---DO NOT USE                        
         GOTO1 DATCON,DMCB,(1,SRTINVDT),(8,P+17)                                
SRTRD200 MVC   BUFMEDIA,SRTJOB     SET BUFFALO RECORD FOR PUT                   
         MVC   BUFCLI,SRTCLI                                                    
         MVC   BUFMDESC(BUFLN2),SRTMDESC                                        
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         LA    R3,SRTRCV           STARTING AMOUNT FIELD                        
         LA    R4,P+38             STARTING AMOUNT PRINT POSITION               
         BAS   RE,GNEDT            EDITS AMOUNT TO PRINT                        
         LA    R3,SRTRCV           ADD TO BUCKET FOR INVOICE TOTAL              
         LA    R5,TRCV                                                          
         BAS   RE,GNADDA                                                        
         LA    R3,SRTRCV           STARTING AMOUNT FIELD                        
         LA    R5,CLIRCV           SUM DTLS FOR CLIENT TOTS                     
         BAS   RE,GNADD                                                         
         BAS   RE,PRINTIT                                                       
         AP    DTLCT,=P'1'                                                      
         B     SRTRD050                                                         
         SPACE 1                                                                
SRTRD300 GOTO1 SORTER,DMCB,=C'END' END THE SORT                                 
         NI    IOSW,X'FF'-SRTOPEN                                               
         OI    BRKSW,RQEND+NWOFF+NWCLI   EQUIVALENT TO END OF REQUEST           
         B     SRTRD100                                                         
         SPACE 1                                                                
SRTRD400 BAS   RE,COMPTOT          PRINT COMPANY GROUP TOTALS                   
         NI    BRKSW,X'FF'-(RQEND+FSTQDTL)                                      
         OI    RCPSW,QRCP                                                       
         LA    R2,2                REQUEST - ROW 2                              
         BAS   RE,GNRCP            REQUEST MEDIA RECAP                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+24(14),=C'REQUEST TOTALS'                                      
         LA    R3,QRCV             REQUEST TOTALS BUCKETS                       
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
         ZAP   OFFCT,=P'0'                                                      
         ZAP   QRCV,=P'0'                                                       
         ZAP   QRCV+(1*L'QRCV),=P'0'                                            
         ZAP   QRCV+(2*L'QRCV),=P'0'                                            
         ZAP   QRCV+(3*L'QRCV),=P'0'                                            
         ZAP   QRCV+(4*L'QRCV),=P'0'                                            
         GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFC                                    
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(22),=C'TAPE RECORDS WRITTEN: '                               
         EDIT  (P4,TPCT),(9,P+23),COMMAS=YES,ALIGN=LEFT                         
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
CKBRKRT  TM    BRKSW,RQEND         END OF REQUEST                               
         BO    CKBRK100                                                         
         CLC   SRTOFF,SVOFF        NEW OFFICE                                   
         BNE   CKBRK100                                                         
         CLC   SRTCLI,SVCLI        NEW CLIENT                                   
         BNE   CKBRK200                                                         
         BR    RE                                                               
         SPACE 1                                                                
CKBRK100 OI    BRKSW,NWOFF                                                      
CKBRK200 OI    BRKSW,NWCLI                                                      
         BR    RE                                                               
         EJECT                                                                  
NWCLIRT  NTR1                                                                   
         NI    BRKSW,X'FF'-(NWCLI)                                              
         CP    DTLCT,=P'1'                                                      
         BNH   NWCLI100                                                         
         MVC   P+25(13),=C'CLIENT TOTALS'                                       
         LA    R3,CLIRCV           CLIENT TOTALS                                
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
NWCLI100 BAS   RE,PRINTIT                                                       
         LA    R3,CLIRCV           CLIENT TOTALS                                
         LA    R5,OFFRCV           OFFICE TOTALS                                
         BAS   RE,GNADD                                                         
         ZAP   DTLCT,=P'0'                                                      
         AP    CLICT,=P'1'                                                      
         MVC   SVCLI,SRTCLI                                                     
         B     EXIT                                                             
         EJECT                                                                  
NWOFFRT  NTR1                                                                   
         NI    BRKSW,X'FF'-(NWOFF)                                              
         CP    CLICT,=P'1'                                                      
         BNH   NWOFF100                                                         
         BAS   RE,PRINTIT          BLANK LINE                                   
         MVC   P+25(13),=C'OFFICE TOTALS'                                       
         LA    R3,OFFRCV           OFFICE TOTALS                                
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
NWOFF100 LA    R3,OFFRCV           ADD OFFICE TOTAL TO GROUP COUNTER            
         LA    R5,GRPRCV                                                        
         BAS   RE,GNADDA           ADD BUT DON'T ZERO OFFICE TOTAL              
         LA    R3,OFFRCV           OFFICE TOTALS                                
         LA    R5,QRCV             REQUEST TOTALS                               
         BAS   RE,GNADD                                                         
         ZAP   CLICT,=P'0'                                                      
         AP    OFFCT,=P'1'                                                      
         MVC   GCHECK1,SVOFF                                                    
         MVC   GCHECK2,SRTOFF      SAVE OFFICES FOR GROUP                       
         BAS   RE,NWGRPRT          PRINT GROUP TOTALS                           
         BAS   RE,OFFNM                                                         
         LA    R2,1                OFFICE LEVEL - ROW 1                         
         BAS   RE,GNRCP            OFFICE MEDIA RECAP                           
         GOTO1 BUFFALO,DMCB,=C'ADD',ADBUFC,1,(X'80',2)                          
         GOTO1 (RF),(R1),=C'CLEAR',ADBUFC,(X'80',1)                             
         MVC   SVOFF,SRTOFF                                                     
         BAS   RE,OFFNM                                                         
         B     EXIT                                                             
         EJECT                                                                  
NWGRPRT  NTR1                      PRINT GROUP TOTALS                           
         MVC   GROUP(1),GCHECK1    GET OFFICE GROUPING                          
         BAS   RE,SETGRP           GET THE GROUP                                
         MVC   GCHECK1(1),GROUP    SAVE GROUP                                   
         MVC   GROUP(1),GCHECK2    GET SECOND OFFICE GROUP                      
         BAS   RE,SETGRP           CALL THE ROUTINE FOR GROUPING                
         TM    BRKSW,RQEND         TEST FOR END OF REQUEST                      
         BNO   *+8                 NORMAL CHECK IF NOT EO REQUEST               
         B     NWGRPRT1            FORCE GROUP PRINTOUT                         
         CLC   GROUP(1),GCHECK1    IN SAME GROUP?                               
         BE    NWGRPRT9            NO                                           
*                                                                               
NWGRPRT1 CLI   GCHECK1,C'D'        A-F GROUP?                                   
         BNE   NWGRPRT2                                                         
         LA    R5,DTOTAL           FINAL TOTAL FOR DIRECT                       
         MVC   P+25(6),=C'DIRECT'  DIRECT TOTALS                                
         B     NWGRPRT4                                                         
*                                                                               
NWGRPRT2 CLI   GCHECK1,C'P'        L-O GROUP?                                   
         BNE   NWGRPRT3                                                         
         LA    R5,PTOTAL           FINAL TOTAL FOR PROMOTIONS                   
         MVC   P+22(9),=C'PROMOTION'                                            
         B     NWGRPRT4                                                         
*                                                                               
NWGRPRT3 CLI   GCHECK1,C'U'        1-7 GROUP                                    
         BNE   NWGRP3A             OTHERS                                       
         LA    R5,UTOTAL           FINAL TOTAL FOR MAIN AGENCY                  
         MVC   P+20(11),=C'MAIN AGENCY'                                         
         B     NWGRPRT4                                                         
*                                                                               
NWGRP3A  LA    R5,OTOTAL           OTHER GROUP TOTALS                           
         MVC   P+26(5),=C'OTHER'                                                
*                                                                               
NWGRPRT4 LA    R3,GRPRCV           POINT TO GROUP TOTAL                         
         BAS   RE,GNADDA           STORE THE TOTAL FOR PRINTOUT                 
*                                                                               
NWGRPRT5 MVC   P+32(6),=C'TOTALS'                                               
         LA    R3,GRPRCV           POINT TO WHERE TOTALS ARE                    
         LA    R4,P+38             WHERE TO PUT TOTALS ON LINE                  
         BAS   RE,GNEDT            EDIT THE OUTPUT                              
         BAS   RE,PRINTIT          PRINT THE LINE                               
*                                                                               
         LA    R3,GRPRCV                                                        
         LA    R6,5                NUMBER OF TIMES TO LOOP                      
NWGRPRT6 ZAP   0(8,R3),=P'0'       CLEAR GROUP TOTAL COUNTER                    
         LA    R3,8(R3)            BUMP TO NEXT BYTE                            
         BCT   R6,NWGRPRT6         LOOP BACK UP                                 
*                                                                               
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
NWGRPRT9 B     EXIT                RETURN TO CALLER                             
         EJECT                                                                  
COMPTOT  NTR1                      PRINT THE COMPANY TOTALS                     
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         MVC   HEAD3+1(35),SPACES  CLEAR SPACE WHERE OFFICE PRINTS              
         MVI   RCSUBPRG,2          SET FLAG FOR HEADER                          
         MVC   P+2(18),=C'     DIRECT TOTALS'                                   
         LA    R3,DTOTAL           POINT TO DIRECT TOTALS                       
         LA    R4,P+38             PRINT IN COLUMN 38                           
         BAS   RE,GNEDT            FORMAT OUTPUT                                
         BAS   RE,PRINTIT          PRINT THE LINE                               
*                                                                               
         MVC   P+2(18),=C'  PROMOTION TOTALS'                                   
         LA    R3,PTOTAL                                                        
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   P+2(18),=C'MAIN AGENCY TOTALS'                                   
         LA    R3,UTOTAL                                                        
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         LA    R3,OTOTAL           CHECK IF OTHER TOTALS =0                     
         LA    R6,5                NUMBER OF TIMES TO LOOP                      
COMPTOT1 CP    0(8,R3),=P'0'       ANY TOTALS NOT EQUAL TO 0?                   
         BNE   COMPTOT2            YES                                          
         LA    R3,8(R3)            BUMP TO NEXT TOTAL                           
         BCT   R6,COMPTOT1         LOOP BACK UP                                 
         B     COMPTOT3            DONT PRINT OTHER GROUP TOTALS                
*                                                                               
COMPTOT2 MVC   P+2(18),=C'OTHER GROUP TOTALS'                                   
         LA    R3,OTOTAL                                                        
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
*                                                                               
COMPTOT3 LA    R5,CTOTAL           CALCULATE COMPANY TOTAL                      
         LA    R3,DTOTAL           ADD IN DIRECT TOTAL                          
         BAS   RE,GNADDA                                                        
         LA    R5,CTOTAL                                                        
         LA    R3,PTOTAL           ADD IN PROMOTION TOTALS                      
         BAS   RE,GNADDA                                                        
         LA    R5,CTOTAL                                                        
         LA    R3,UTOTAL           ADD IN MAIN AGENCY TOTALS                    
         BAS   RE,GNADDA                                                        
         LA    R5,CTOTAL                                                        
         LA    R3,OTOTAL           ADD IN OTHER TOTALS                          
         BAS   RE,GNADDA                                                        
*                                                                               
         BAS   RE,PRINTIT          BLANK LINE                                   
         MVC   P+2(18),=C'    COMPANY TOTALS'                                   
         LA    R3,CTOTAL                                                        
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         EJECT                                                                  
         USING ACKEYD,R6                                                        
TRNRT    L     R6,ADTRANS          POINTS TO ADDRESS TRANSACTION KEY            
         SH    R6,DATADISP                                                      
         CLI   ACKEYACC+9,C'X'     Q, IS MEDIA CODE X                           
         BE    EXIT                 Y, EXCLUDE                                  
         USING TRANSD,R7                                                        
         L     R7,ADTRANS          GETS ADDRESS OF TRAN ELEMENT                 
         CLI   TRNSEL,X'44'        Q, TRAN ELEMENT-44, ALWAYS FIRST             
         BNE   EXIT                 N, EXIT---BAD TRAN                          
         CLC   TRNSANAL,=C'99'     BUILD TABLE IF WORK CODE NOT '99'            
         BE    TRN020                                                           
         CLI   TRNSTYPE,X'2'       EXCLUDE TYPE 2 ITEMS                         
         BE    EXIT                                                             
         CLC   ACKEYCON+1(2),=C'SK'                                             
         BNE   EXIT                                                             
         USING SKD,R5                                                           
         LA    R5,SKREC                                                         
         MVC   SKSOURCE,ACKEYCON+3 MOVE SOURCE CODE TO TABLE                    
*                                                                               
         USING TRANSD,R7                                                        
         L     R7,ADTRANS                                                       
         ZAP   DUB,TRNSAMNT        HOLD TRANS AMOUNT FOR 77 ELEMENTS            
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         SH    R7,DATADISP                                                      
         GOTO1 ACMAPRAT,DMCB,(X'C0',(R7)),0,ADCOMFAC,0,                X        
               ACMAPROB,ACMAPRO2                                                
*                                                                               
         MVI   ELCODE,PTAELQ       LOOK FOR 77 ELEMENTS                         
         L     R7,ACMAPRO2                                                      
         CLI   0(R7),PTAELQ                                                     
         B     *+8                                                              
         DROP  R4                                                               
*                                                                               
         USING PTAELD,R7                                                        
TRN010   BAS   RE,NEXTEL                                                        
         BNE   EXIT                NO (MORE) 77'S                               
         CLI   PTATYPE,PTATRAL     IS THIS A BILLING ELEMENTS?                  
         BNE   TRN010                                                           
         TM    PTASTAT1,PTASPEND   IS IT PENDING?                               
         BO    TRN010                                                           
         CLI   QOPT7,C'X'                                                       
         BNE   TRN010A                                                          
         GOTO1 PRNTBL,DMCB,=C'ELEM',(R7),C'DUMP',PTARLN1Q,=C'1D'                
TRN010A  TM    PTASTAT1,PTASREVU   REVERSAL                                     
         BO    TRN013                                                           
         TM    PTASTAT1,PTASREVS                                                
         BO    TRN014                                                           
         B     TRN015                                                           
*                                                                               
TRN013   MVC   SKDATE,PTARBLDT     SAVE DATE ORIGINALLY BILLED                  
         MVC   SKINVC,=6X'FF'      FF TO INVOICE                                
         ZAP   SKAMT,DUB             TRNSAMNT                                   
         BAS   RE,ADDIT              ADD TO BINTABLE                            
         B     TRN010                                                           
*                                                                               
TRN014   MVC   SKDATE,PTARDATE     SAVE DATE REVERSED                           
         MVC   SKINVC,=6X'FF'      FF TO INVOICE                                
         ZAP   SKAMT,DUB             TRNSAMNT,                                  
         MP    SKAMT,=P'-1'          'REVERSED'                                 
         BAS   RE,ADDIT              ADD TO BINTABLE                            
         B     TRN010                                                           
*                                                                               
TRN015   MVC   SKINVC,PTARBLNO     MOVE INVOICE AND AMOUNT TO TABLE             
         MVC   SKDATE,=X'0000'                                                  
         OC    ACDTUSED,ACDTUSED                                                
         BNZ   *+14                                                             
         ZAP   SKAMT,PTANET                                                     
         B     *+10                                                             
         ZAP   SKAMT,DUB                                                        
         BAS   RE,ADDIT                                                         
         B     TRN010                                                           
*                                                                               
         USING TRANSD,R7                                                        
TRN020   MVC   SRTINVDT,TRNSDATE   SAVE TRANS DATE                              
         MVC   SVDT3,SRTINVDT      SAVE TRANSACTION DATE FOR COMPARISON         
         MVI   ELCODE,X'60'        LOOK FOR STATUS ELEMENT-60                   
         BAS   RE,NEXTEL           GETS STATUS ELEMENT---MAYBE                  
         BNE   TRN040              BRANCH IF NO STATUS ELEMENT FOUND            
         USING TRSTATD,R7          STATUS DSECT - ELEMENT-60                    
         TM    TRSTATUS,X'40'      Q, OLD JOINT VENTURE                         
         BO    EXIT                 Y, DO NOT USE                               
         GOTO1 DATCON,DMCB,(2,TRSTDATE),(1,SVDT3) SAVE STATUS DATE              
TRN040   CLC   SVDT3,QSTR3         Q, TEST AGAINST BEGIN DATE                   
         BL    EXIT                 LOW SVDT3, BYPASS TRAN                      
         CLC   SVDT3,QEND3         Q, TEST AGAINST END DATE                     
         BH    EXIT                 HIGH SVDT3, BYPASS TRAN                     
         MVC   SRTOFF,SVOFF         OFFICE CODE TO SORT OFF                     
         MVC   GROUP,SVOFF         NEXT ROUTINE EXPECTS 'GROUP'=OFFICE          
         BAS   RE,SETGRP           DETERMINE WHAT GROUP TO SORT BY              
         MVC   SRTGRP,GROUP        RETURNS PROPER GROUP CLASSIFICATION          
         USING TRANSD,R7                                                        
         L     R7,ADTRANS          GETS ADDRESS OF TRAN ELEMENT                 
         MVC   SVTRNSDT,TRNSDATE                                                
         MVC   SVMOS,TRNSBTCH                                                   
         MVC   SRTINVNO,TRNSREF    REFERENCE NO. TO SORT REC                    
         ZAP   DIS99,TRNSNARR+21(6) MOVES/SAVES DISCOUNT AMOUNT                 
         ZAP   COM99,TRNSNARR+15(6) MOVES/SAVES COMMISSION AMT                  
         ZAP   INVTY99,TRNSAMNT     MOVES/SAVES INVENTORY AMOUNT                
         ZAP   RCV99,TRNSNARR+27(6) MOVES/SAVES RECEIVABLE AMOUNT               
         MVC   SVBILLDT(2),TRNSNARR+33  SAME FOR BILL DATE                      
         MVC   SVUNBLDT(2),TRNSNARR+35  SAME FOR REVERSAL DATE                  
         MVC   SVDT3,TRNSDATE      TRANSACTION DATE FOR COMPARISON              
         MVC   SRTCLINM,SVCLINM    CLIENT NAME TO SORT REC                      
         USING ACMEDIAD,R7                                                      
         L     R7,ADCOMP                                                        
         MVI   ELCODE,X'11'        MEDIA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
TRN060   BNE   TRN080                                                           
         CLC   ACMDCODE,SRTMCODE                                                
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     TRN060                                                           
         MVC   SRTMDESC,ACMDDESC                                                
TRN080   CP    INVTY99,RCV99       Q, SAME NET AND RECEIVABLE AMTS              
         BNE   *+10                 N, SKIP NEXT INSTR                          
         ZAP   COM99,=P'0'          Y, ZERO OUT COMM---NO COMM                  
         MVI   SKSTAT,C'Y'           LOOK FOR DATES OR INVOICE NUMBERS          
         USING BIND,R5                                                          
         L     R5,SKTAB                                                         
         UNPK  TPFEE,=P'0'         CLEAR FIELD IN CASE NOTHING FOUND            
         OC    BININ,BININ                                                      
         BNZ   TRN120              GET TABLE ENTRIES                            
TRN100   MVC   TPSOURCE,SPACES                                                  
         ZAP   SRTFEE,=P'0'                                                     
         ZAP   SRTRCV,RCV99                                                     
         ZAP   SRTCOM,COM99                                                     
         ZAP   SRTINVTY,INVTY99                                                 
         ZAP   SRTDIS,DIS99                                                     
         BAS   RE,SENDIT                                                        
         B     EXIT                                                             
         SPACE 1                                                                
TRN120   L     R0,BININ                                                         
         LA    R5,BINTABLE                                                      
         USING SKD,R5                                                           
TRN140   CLC   SKINVC,SRTINVNO                                                  
         BE    TRN180                                                           
         CLC   SKDATE,=X'0000'        IS THERE A DATE ON THIS SK RECORD         
         BE    TRN160                 NO, DONT TRY TO MATCH                     
         CLC   SKDATE,SVBILLDT        IS THERE A BILL FOR THIS                  
         BE    TRN180                 JOB/DATE                                  
         CLC   SKDATE,SVUNBLDT        IS THERE A REVERSAL FOR THIS              
         BE    TRN180A                JOB/DATE                                  
TRN160   LA    R5,SKLEN(R5)                                                     
         BCT   R0,TRN140                                                        
         B     TRN100                                                           
         SPACE 1                                                                
TRN180   EQU   *                     GOT AN INVOICE NUMBER                      
         MVI   SKSTAT,C'N'           DONT USE DATE-MATCHED SKAMT'S              
         B     TRN180B                                                          
TRN180A  CLI   SKSTAT,C'Y'           STILL LOOKING?                             
         BNE   TRN160                NO, LOOP OUT                               
TRN180B  MVC   TPSOURCE,SPACES                                                  
         MVC   SKDATE,=X'0000'       DONT USE THIS ITEM ANYMORE                 
         CP    SKAMT,=P'0'                                                      
         BE    TRN200                                                           
         SP    RCV99,SKAMT                                                      
         SP    INVTY99,SKAMT                                                    
         MVC   TPSOURCE,SKSOURCE                                                
TRN200   ZAP   SRTRCV,SKAMT                                                     
         ZAP   SRTINVTY,=P'0'                                                   
         ZAP   SRTDIS,=P'0'                                                     
         ZAP   SRTCOM,=P'0'                                                     
         ZAP   SRTFEE,SKAMT                                                     
         BAS   RE,SENDIT                                                        
         B     TRN160                                                           
         DROP  R6,R7                                                            
         EJECT                                                                  
         GETEL R7,DATADISP,ELCODE  GET ELEMENT                                  
         SPACE 2                                                                
SETGRP   NTR1                      SAVE REGISTERS                               
         LA    R4,GRPTBLE          POINT TO START OF TABLE                      
         LA    R1,GRPTBLNQ         NUMBER OF TIMES TO LOOP                      
SETGRP1  CLC   0(1,R4),GROUP       COMPARE FOR A MATCH IN OFFICE                
         BE    SETGRP2             Y, USE TABLE ELEMENT FOR GRP                 
         LA    R4,2(R4)            BUMP TO NEXT ELEMENT IN TABLE                
         BCT   R1,SETGRP1          LOOP BACK UP                                 
         MVC   GROUP(1),=C'Z'      GROUP CODE FOR 'OTHERS'                      
         B     EXIT                EXIT BACK TO CALLER                          
SETGRP2  MVC   GROUP(1),1(R4)      STORE IN GROUP CODE                          
         B     EXIT                EXIT BACK TO CALLER                          
         EJECT                                                                  
ADDIT    EQU   *                     ADD AN ENTRY TO SKTAB                      
         ST    RE,SVRE                                                          
         GOTO1 BINADD,DMCB,(R5),SKTAB                                           
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT                                                                  
         USING  BIND,R4                                                         
         USING ACKEYD,R7                                                        
ACCRT    L     R4,SKTAB            CLEAR TABLE AT START OF EACH ACCOUNT         
         XC    BININ,BININ                                                      
         LA    R4,BINTABLE                                                      
         XR    R5,R5                                                            
         XR    R6,R6                                                            
         LA    R6,2500                                                          
         STH   R6,HALF                                                          
         LA    R5,SKLEN                                                         
         MH    R5,HALF                                                          
         LA    R6,*                                                             
         XR    R7,R7                                                            
         MVCL  R4,R6                                                            
         L     R7,ADACC            POINTS TO ACCOUNT RECORD                     
         MVC   SRTCLI(L'SRTCLI+L'SRTPRD+L'SRTJOB),ACKEYACC+3                    
         USING ACPROFD,R7                                                       
         L     R7,ADPROFIL                                                      
         MVC   SVOFF,ACPRUNIT      SAVE OFFICE CODE                             
         MVC   TPJOB,SRTJOB                                                     
         MVC   TPDDSJOB,SPACES                                                  
         MVC   TPESTDSC,SPACES                                                  
         USING ACNAMED,R7                                                       
         L     R7,ADACCNAM                                                      
         ZIC   RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8              SAVE JOB NAME                                
         B     *+10                                                             
         MVC   TPESTDSC(0),ACNMNAME                                             
ACC200   ZAP   TPLBLCT,=P'0'                                                    
         MVC   TPRLSDT(TPLN1),SPACES                                            
         MVC   TPSOURCE,SPACES                                                  
         L     R7,ADACC                                                         
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
ACC250   BNE   EXIT                                                             
         USING ACUFD,R7                                                         
         LA    R6,L'TPPRDEST-1                                                  
         LA    R8,TPPRDEST                                                      
         CLC   ACUFCODE,=C'EN'     ESTIMATE NO                                  
         BE    ACC300                                                           
         LA    R6,L'TPRLSDT-1                                                   
         LA    R8,TPRLSDT                                                       
         CLC   ACUFCODE,=C'RD'     RELEASE DATE                                 
         BE    ACC300                                                           
         LA    R6,L'TPCHGPER-1                                                  
         LA    R8,TPCHGPER                                                      
         CLC   ACUFCODE,=C'CP'     CHRGE PERIOD                                 
         BE    ACC300                                                           
         LA    R6,L'TPPONO-1                                                    
         LA    R8,TPPONO                                                        
         CLC   ACUFCODE,=C'PO'     PURCHASE ORD                                 
         BE    ACC300                                                           
         LA    R6,L'TPACCNO-1                                                   
         LA    R8,TPACCNO                                                       
         CLC   ACUFCODE,=C'AN'     ACCOUNT NO                                   
         BE    ACC300                                                           
         LA    R6,L'TPREFNO-1                                                   
         LA    R8,TPREFNO                                                       
         CLC   ACUFCODE,=C'RN'     REFERENCE NO                                 
         BE    ACC300                                                           
         LA    R6,L'TPJOB-1                                                     
         LA    R8,TPJOB                                                         
         CLC   ACUFCODE,=C'RJ'     REFERENCE JOB                                
         BNE   ACC400                                                           
         MVC   TPDDSJOB,TPJOB      SAVE DDS JOB                                 
ACC300   ZIC   R2,ACUFLEN                                                       
         SH    R2,=H'33'                                                        
         BM    ACC400                                                           
         CR    R2,R6               Q, ACTUAL LENGTH GREATER THAN MAX.           
         BNH   *+6                                                              
         LR    R2,R6                Y, ALLOW ONLY THE MAX. LENGTH               
         EX    R2,ACC500                                                        
         AP    TPLBLCT,=P'1'                                                    
         CP    TPLBLCT,=P'6'                                                    
         BE    EXIT                                                             
ACC400   BAS   RE,NEXTEL                                                        
         B     ACC250                                                           
         SPACE 1                                                                
ACC500   MVC   0(0,R8),ACUFDATA                                                 
         DROP  R4,R7                                                            
         EJECT                                                                  
ALEVEL   MVC   SVCLINM,SPACES                                                   
         USING ACNAMED,R7          CLIENT NAME DSECT                            
         L     R7,ADLVANAM                                                      
         LA    R8,SVCLINM                                                       
         B     ABLEVNM                                                          
         SPACE 3                                                                
BLEVEL   MVC   TPPRDNM,SPACES                                                   
         USING ACNAMED,R7          PRODUCT NAME DSECT                           
         L     R7,ADLVBNAM                                                      
         LA    R8,TPPRDNM                                                       
ABLEVNM  ZIC   RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8              SAVE NAME                                    
         B     EXIT                                                             
         USING ACNAMED,R7          NAME DSECT                                   
         MVC   0(0,R8),ACNMNAME                                                 
         EJECT                                                                  
WRTP     NTR1                                                                   
         MVC   TPOFF,SRTOFF                                                     
         MVC   TPMCODE,SRTMCODE                                                 
         MVC   TPCLI,SRTCLI                                                     
         MVC   TPPRD,SRTPRD                                                     
         MVC   TPCLPRCD,SPACES                                                  
         USING ACNOD,R7                                                         
         L     R7,ADHEIRB                                                       
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   WRTP050                                                          
         ZIC   R5,ACNOLEN                                                       
         SH    R5,=H'3'                                                         
         CLI   ACNOLEN,X'06'       SEE IF L'ACNO GREATER THAN 4                 
         BNH   *+8                                                              
         LA    R5,3                IF YES, JUST MOVE 4                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   TPCLPRCD(0),ACNO                                                 
WRTP050  MVC   TPINVNO,SRTINVNO                                                 
         UNPK  TPRCV,SRTRCV                                                     
         UNPK  TPNONCOM,=P'0'                                                   
         UNPK  TPDIS,SRTDIS                                                     
         UNPK  TPCOM,SRTCOM                                                     
         UNPK  TPFEE,SRTFEE                                                     
         UNPK  TPINVTY,SRTINVTY                                                 
         GOTO1 DATCON,DMCB,(1,SRTINVDT),(X'20',SVYYMMDD)                        
         MVC   TPINVDT(4),SVMMDD                                                
         MVC   TPINVDT+4(2),SVYY                                                
         XC    WORK,WORK                                                        
         USING TRANSD,R8                                                        
         LA    R8,WORK             SEND CONVMOS DUMMY 44 ELEMENT                
         MVI   0(R8),X'44'                                                      
         MVC   TRNSDATE-TRANSD(L'SVTRNSDT,R8),SVTRNSDT                          
         MVC   TRNSBTCH-TRANSD(L'SVMOS,R8),SVMOS                                
         GOTO1 CONVMOS,DMCB,(R8),SVYYMM CONVERT MOS TO YYMM                     
         MVC   TPADMMYY(2),SVYYMM+2                                             
         MVC   TPADMMYY+2(2),SVYYMM                                             
         MVC   TPADMMYY,SPACES     SPACES OVERRIDE PREVIOUS CODE!!              
         CLI   QOPT7,C'Y'          Q, DUMP TAPE RECORD                          
         BNE   *+8                  N, JUST WRITE TAPE OUTPUT RECORD            
         BAS   RE,TPDUMP            Y, DUMP TAPE RECORD ALSO                    
         CLI   QOPT1,C'Y'          Q, WRITE TAPE RECORD                         
         BNE   EXIT                 N, EXIT                                     
         TM    IOSW,TPOPEN         Q, TAPE OPENED YET                           
         BO    WRTP100              Y, DON'T OPEN AGAIN                         
         AP    TPVOLS,=P'1'                                                     
         CVB   R4,TPVOLS                                                        
         MVC   OMDSN+13(L'ALPHAID),ALPHAID DYNAMIC ALLOCATION                   
         GOTO1 DYNALLOC,DMCB,(0,=CL8'OMTP'),((R4),OMDSN)                        
         OI    IOSW,TPOPEN                                                      
         OPEN  (OUTP,(OUTPUT))                                                  
         ZAP   TPCT,=P'0'                                                       
WRTP100  PUT   OUTP,TPREC                                                       
         AP    TPCT,=P'1'                                                       
         B     EXIT                                                             
         EJECT                                                                  
SENDIT   NTR1                                                                   
         TM    IOSW,SRTOPEN                                                     
         BO    SEND020                                                          
         OI    IOSW,SRTOPEN                                                     
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
SEND020  GOTO1 SORTER,DMCB,=C'PUT',SRTWK                                        
         BAS   RE,WRTP             GO TO WRITE TAPE RECORD ROUTINE              
         B     EXIT                                                             
         EJECT                                                                  
GNRCP    NTR1                      GENERAL RECAP ROUTINE                        
         ZAP   MEDIACT,=P'0'                                                    
         MVI   RCSUBPRG,1                                                       
         MVC   SVMEDIA,SPACES                                                   
         BAS   RE,OFFNM                                                         
         TM    RCPSW,QRCP                                                       
         BNO   *+10                                                             
         MVC   SVHEAD3,=CL36'SUM OF ALL OFFICES IN THIS REQUEST'                
         XC    BUFKEY,BUFKEY                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFREC,(R2)                         
         TM    DMCB+8,X'80'                                                     
         BO    EXIT                                                             
         B     GNRCP250                                                         
GNRCP100 GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFREC,(R2)                          
         TM    DMCB+8,X'80'        Q, END OF RECORDS                            
         BO    GNRCP400             Y, PRINT RECAP TOTALS LINE                  
         CLC   BUFMEDIA,SVMEDIA    Q, BREAK IN MEDIA                            
         BNE   GNRCP300             Y, PRINT MEDIA TOTALS                       
GNRCP150 LA    R4,P                                                             
GNRCP200 MVC   1(3,R4),BUFCLI       N, SET CLIENT LINE AND PRINT                
         MVC   5(36,R4),BUFCLINM                                                
         LA    R3,BUFBKTS          MEDIA BUCKETS                                
         LA    R4,38(R4)           R4 POINTS TO P OR PTHIRD                     
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
         LA    R3,BUFBKTS          MEDIA BUCKETS                                
         LA    R5,MEDIARCV         SUM UP MEDIA TOTAL                           
         BAS   RE,GNADD                                                         
         MVC   SVMEDIA,BUFMEDIA                                                 
         B     GNRCP100                                                         
         SPACE 1                                                                
GNRCP250 MVC   P+1(1),BUFMEDIA                                                  
         MVC   P+3(15),BUFMDESC                                                 
         GOTO1 SQUASHER,DMCB,P+1,17                                             
         MVC   WORK40(17),P+1                                                   
         GOTO1 UNDERLIN,DMCB,(17,P+1),PSECOND+1                                 
         LA    R4,PTHIRD                                                        
         B     GNRCP200                                                         
         SPACE 1                                                                
GNRCP300 AP    MEDIACT,=P'1'                                                    
         MVC   P+1(17),WORK40      MEDIA CODE AND MEDIA NAME                    
         MVC   P+26(12),=C'MEDIA TOTALS'                                        
         LA    R3,MEDIARCV         MEDIA TOTAL BUCKETS                          
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT          BLANK LINE                                   
         LA    R3,MEDIARCV         MEDIA TOTAL BUCKETS                          
         LA    R5,QMRCV            MEDIA RECAP FOR GRAND TOTALS                 
         BAS   RE,GNADD                                                         
         TM    RCPSW,ENDRCP                                                     
         BNO   GNRCP250                                                         
         CP    MEDIACT,=P'1'                                                    
         BNH   GNRCP350                                                         
         MVC   P+20(18),=C'MEDIA RECAP TOTALS'                                  
         LA    R3,QMRCV            MEDIA RECAP TOTALS                           
         LA    R4,P+38                                                          
         BAS   RE,GNEDT                                                         
GNRCP350 ZAP   QMRCV,=P'0'                                                      
         ZAP   QMRCV+(1*L'QMRCV),=P'0'                                          
         ZAP   QMRCV+(2*L'QMRCV),=P'0'                                          
         ZAP   QMRCV+(3*L'QMRCV),=P'0'                                          
         ZAP   QMRCV+(4*L'QMRCV),=P'0'                                          
         BAS   RE,PRINTIT                                                       
         NI    RCPSW,X'FF'-(ENDRCP+QRCP)                                        
         MVI   RCSUBPRG,0                                                       
         B     EXIT                                                             
         SPACE 1                                                                
GNRCP400 OI    RCPSW,ENDRCP                                                     
         B     GNRCP300                                                         
         EJECT                                                                  
OFFNM    ST    RE,SVRE                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   HEAD3+1(8),=C'OFFICE: '                                          
         MVC   HEAD3+9(1),SVOFF                                                 
         MVC   HEAD3+11(26),SPACES                                              
         MVC   OMOFF,SVOFF                                                      
         L     R7,ADOMIOA                                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'ACCOUNTS',OMOFFKY,(R7)              
         CLI   DMCB+8,0            Q, OFFICE RECORD FOUND                       
         BNE   OFFNM100             N, PUT IN OFFICE NO.                        
         L     R7,ADOMIOA           Y, GET NAME ELEMENT                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            Q, NAME ELEMENT FOUND                        
         BNE   OFFNM100             N, PUT IN OFFICE NO.                        
         USING ACNAMED,R7          OFFICE NAME DSECT                            
         L     R7,ADLVANAM                                                      
         ZIC   RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,OFFNM200                                                      
OFFNM100 MVC   SVHEAD3,HEAD3+1                                                  
         L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 1                                                                
OFFNM200 MVC   HEAD3+11(0),ACNMNAME OFFICE NAME INTO HEAD3+9                    
         SPACE 3                                                                
GNEDT    ST    RE,SVRE             GENERAL EDIT ROUTINE                         
         LA    R6,5                                                             
GNEDT100 EDIT  (P8,(R3)),(15,(R4)),2,COMMAS=YES,MINUS=YES                       
         LA    R3,8(R3)            NEXT INPUT CTR                               
         LA    R4,16(R4)           NEXT PRINT FIELD                             
         BCT   R6,GNEDT100                                                      
         L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 1                                                                
GNADD    ST    RE,SVRE             GENERAL ADD ROUTINE                          
         LA    R6,5                                                             
GNADD100 AP    0(8,R5),0(8,R3)     ADD TO HIGHER CTR                            
         ZAP   0(8,R3),=P'0'       ZERO OUT INPUT CTR                           
         LA    R3,8(R3)            NEXT INPUT CTR                               
         LA    R5,8(R5)            NEXT OUTPUT CTR                              
         BCT   R6,GNADD100                                                      
         L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 3                                                                
         SPACE 1                                                                
GNADDA   ST    RE,SVRE             GENERAL ADD ROUTINE                          
         LA    R6,5                                                             
GNADDA10 AP    0(8,R5),0(8,R3)     ADD TO HIGHER CTR                            
         LA    R3,8(R3)            NEXT INPUT CTR                               
         LA    R5,8(R5)            NEXT OUTPUT CTR                              
         BCT   R6,GNADDA10                                                      
         L     RE,SVRE                                                          
         BR    RE                                                               
         SPACE 3                                                                
PRINTIT  ST    RE,SVRE                                                          
         MVC   HEAD3+1(36),SVHEAD3                                              
         GOTO1 ACREPORT            PRINT SUBROUTINE                             
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         SPACE 3                                                                
TPDUMP   ST    RE,SVRE                                                          
         LA    R2,256                                                           
         GOTO1 PRNTBL,DMCB,(5,=C'TPREC'),TPREC,C'DUMP',(R2),=C'1D'              
         L     RE,SVRE                                                          
         BR    RE                  RETURN TO USER                               
         SPACE 3                                                                
RUNINIT  GOTO1 DATCON,DMCB,(5,0),(X'20',TODAY)                                  
         MVC   TPTODAY+4(2),TODAY                                               
         MVC   TPTODAY(4),TODAY+2                                               
         L     RF,=A(ADRC)         SAVE RC FOR BINADD                           
         ST    RC,0(RF)                                                         
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'    NOOP GETOPT SINCE I DON'T NEED IT            
         B     EXIT                                                             
         SPACE 3                                                                
REQINIT  CLC   QSTART(L'QSTART+L'QEND),SPACES                                   
         BE    REQINITA                                                         
         CLC   QSTART,SPACES                                                    
         BNE   *+10                                                             
         MVC   QSTART,QEND                                                      
         CLC   QEND,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEND,QSTART                                                      
         GOTO1 DATCON,DMCB,(0,QEND),(1,QEND3)                                   
         GOTO1 DATCON,DMCB,(0,QSTART),(1,QSTR3)                                 
         B     EXIT                                                             
         SPACE 1                                                                
REQINITA GOTO1 DATCON,DMCB,(0,TODAY),(1,QSTR3)                                  
         MVC   QEND3,QSTR3                                                      
         B     EXIT                                                             
         SPACE 3                                                                
QDTL1    NTR1                                                                   
         OI    BRKSW,FSTQDTL                                                    
         MVI   RCSUBPRG,0                                                       
         MVC   SVOFF,SRTOFF                                                     
         MVC   SVCLI,SRTCLI                                                     
         BAS   RE,OFFNM                                                         
         B     EXIT                                                             
         EJECT                                                                  
OMDSN    DC    CL20'ACCTAPE.AC0I5XX1'                                           
OUTP     DCB   DDNAME=OMTP,DSORG=PS,LRECL=TPLN,MACRF=PM,BLKSIZE=TPLN            
         SPACE 1                                                                
         DS    0D                  DOUBLEWORD ALIGNMENT FOR TPVOLS              
TPVOLS   DC    PL8'1'              GENERATE TAPE(+1) FOR FIRST TAPE             
SQUASHER DC    V(SQUASHER)                                                      
CONVMOS  DC    V(CONVMOS)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
UNDERLIN DC    V(UNDERLIN)                                                      
SORTER   DC    V(SORTER)                                                        
ADBUFC   DC    A(BUFFALOC)                                                      
ADOMIOA  DC    A(OMOFFIOA)                                                      
SKTAB    DC    A(SKTABC)                                                        
BINADD   DC    A(BINADDC)                                                       
SVRE     DC    F'0'                                                             
         SPACE 1                                                                
BUFREC   DS    0D                                                               
BUFKEY   DS    0CL4                                                             
BUFMEDIA DS    C                                                                
BUFCLI   DS    CL3                                                              
BUFMDESC DS    CL15                                                             
BUFCLINM DS    CL36                                                             
BUFBKTS  DS    5PL8                                                             
BUFLN2   EQU   *-BUFMDESC                                                       
         SPACE 1                                                                
SRTWK    DS    0C                                                               
SRTGRP   DS    CL1                                                              
SRTOFF   DS    CL1                                                              
SRTCLI   DS    CL3                                                              
SRTPRD   DS    CL3                                                              
SRTMCODE DS    0C                                                               
SRTJOB   DS    CL6                                                              
SRTINVDT DS    CL(L'SVDT3)                                                      
SRTINVNO DS    CL(L'TRNSREF)                                                    
SRTMDESC DS    CL(L'ACMDDESC)                                                   
SRTCLINM DS    CL(L'ACNMNAME)                                                   
SRTRCV   DS    PL8                                                              
SRTINVTY DS    PL8                                                              
SRTCOM   DS    PL8                                                              
SRTDIS   DS    PL8                                                              
SRTFEE   DS    PL8                                                              
SRTLN    EQU   *-SRTWK                                                          
         SPACE 1                                                                
CLIRCV   DC    5PL8'0'                                                          
OFFRCV   DC    5PL8'0'                                                          
GRPRCV   DC    5PL8'0'                                                          
QRCV     DC    5PL8'0'                                                          
MEDIARCV DC    5PL8'0'                                                          
QMRCV    DC    5PL8'0'                                                          
DTOTAL   DC    5PL8'0'             DIRECT TOTALS                                
PTOTAL   DC    5PL8'0'             PROMOTION TOTALS                             
UTOTAL   DC    5PL8'0'             MAIN AGENCY TOTALS                           
OTOTAL   DC    5PL8'0'             OTHERS TOTAL                                 
CTOTAL   DC    5PL8'0'             COMPANY TOTAL                                
         SPACE 1                                                                
SVINVNO  DS    CL(L'TRNSREF)                                                    
TRCV     DC    5PL8'0'                                                          
PZEROS   DC    5PL8'0'                                                          
TCOUNT   DC    PL4'0'                                                           
         SPACE 1                                                                
GRPTBLE  DC    C'A',C'D'           TABLE OF OFFICE GROUPINGS                    
         DC    C'B',C'D'           OFFICE A-F  ==>  GROUP D(DIRECT)             
         DC    C'C',C'D'                  L-0  ==>  GROUP P(PROMOTION)          
         DC    C'D',C'D'                  1-7  ==>  GROUP U(MAIN AGNCY)         
         DC    C'E',C'D'                                                        
         DC    C'F',C'D'                                                        
         DC    C'L',C'P'                                                        
         DC    C'M',C'P'                                                        
         DC    C'N',C'P'                                                        
         DC    C'O',C'P'                                                        
         DC    C'1',C'U'                                                        
         DC    C'2',C'U'                                                        
         DC    C'3',C'U'                                                        
         DC    C'4',C'U'                                                        
         DC    C'5',C'U'                                                        
         DC    C'6',C'U'                                                        
         DC    C'7',C'U'                                                        
GRPTBLNQ EQU   (*-GRPTBLE)/2       NUMBER OF TIMES TO LOOP                      
         SPACE 1                                                                
OLDOFF   DS    CL1                                                              
NEWOFF   DS    CL1                                                              
TPREC    DS    0C                                                               
         DC    C'X'                                                             
TPOFF    DS    CL1                                                              
TPMCODE  DS    CL1                                                              
TPCLI    DS    CL3                                                              
TPPRD    DS    CL3                                                              
TPCLPRCD DS    CL4                                                              
TPPRDNM  DS    CL20                                                             
TPINVNO  DS    CL6                                                              
         DC    CL7' '                                                           
TPESTDSC DS    CL35                                                             
         DC    CL5' '                                                           
TPADMMYY DS    CL4                                                              
TPRCV    DS    CL12                                                             
TPNONCOM DS    CL12                                                             
TPDIS    DS    CL12                                                             
TPCOM    DS    CL12                                                             
TPFEE    DS    CL12                                                             
TPINVTY  DS    CL12                                                             
TPINVDT  DS    CL6                                                              
TPJOB    DS    CL6                                                              
TPRLSDT  DS    CL6                                                              
TPCHGPER DS    CL3                                                              
TPPONO   DS    CL10                                                             
TPACCNO  DS    CL20                                                             
TPREFNO  DS    CL15                                                             
         DS    CL6                                                              
TPPRDEST DS    CL6                                                              
TPLN1    EQU   *-TPRLSDT                                                        
TPTODAY  DS    CL6                                                              
TPDDSJOB DS    CL6                                                              
TPSOURCE DS    CL2                                                              
         DC    CL02' '                                                          
TPLN     EQU   *-TPREC                                                          
         SPACE 1                                                                
OMOFFKY  DS    0CL4                                                             
OMCMPNAM DS    C                                                                
         DC    C'2D'                                                            
OMOFF    DS    C                                                                
         SPACE 1                                                                
SVOFF    DC    CL1' '                                                           
SVCLI    DC    CL3' '                                                           
SVCLINM  DC    CL36' '                                                          
SVHEAD3  DC    CL36' '                                                          
SVDT3    DC    XL3'0'                                                           
SVMEDIA  DC    X'00'                                                            
SVYYMMDD DS    0C                                                               
SVYY     DS    CL2                                                              
SVMMDD   DS    CL4                                                              
SVYYMM   DS    CL2                                                              
SVTRNSDT DS    CL3                 DATE FOR TRNSBTCH CONVMOS                    
SVMOS    DS    CL2                 MOS  FOR TRNSBTCH                            
RCV99    DS    PL8                                                              
DIS99    DS    PL8                                                              
COM99    DS    PL8                                                              
INVTY99  DS    PL8                                                              
QSTR3    DC    XL3'0'                                                           
QEND3    DC    XL3'0'                                                           
IOSW     DC    X'00'                                                            
SRTOPEN  EQU   B'10000000'                                                      
TPOPEN   EQU   B'01000000'                                                      
RCPSW    DC    X'00'                                                            
ENDRCP   EQU   B'10000000'                                                      
QRCP     EQU   B'00000010'                                                      
BRKSW    DC    X'00'                                                            
RQEND    EQU   B'01000000'                                                      
NWOFF    EQU   B'00100000'                                                      
NWCLI    EQU   B'00010000'                                                      
FSTQDTL  EQU   B'00001000'                                                      
SORTCARD DC    CL80'SORT FIELDS=(1,14,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=114'                                   
TODAY    DC    CL6' '                                                           
DTLCT    DC    PL4'0'                                                           
CLICT    DC    PL4'0'                                                           
OFFCT    DC    PL4'0'                                                           
MEDIACT  DC    PL4'0'                                                           
TPCT     DC    PL4'0'                                                           
TPLBLCT  DC    PL4'0'                                                           
ELCODE   DC    XL1'0'                                                           
SVBILLDT DS    CL2                                                              
SVUNBLDT DS    CL2                                                              
SKSTAT   DS    CL1                   SEARCH FOR MORE SK RECS OR DONT            
WORK40   DC    CL40' '                                                          
SKREC    DS    (SKLEN)C                                                         
*                                                                               
GCHECK1  DS    CL1                 TEMP STORAGE FOR GROUP TOTALS                
GCHECK2  DS    CL1                 "       "     "    "     "                   
GROUP    DS    CL1                 HOLDS CHARACTER CODE FOR GROUP               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
SKTABC   DS    0D                                                               
         DC    F'0'                NUMBER OF TABLE ENTRIES                      
         DC    AL4(SKLEN)          RECORD LENGTH                                
         DC    AL4(SKKEYLN)        DISP OF KEY/ KEY LENGTH                      
         DC    F'2500'             MAXIMUM NUMBER OF ENTRIES                    
         DC    AL1(1)              NUMBER OF BUCKETS                            
         DC    AL1(SKBK-SKD)       DISPLACEMENT TO BUCKETS                      
         DC    AL1(0)              SPARE                                        
         DS    (2500*SKLEN)C       TABLE                                        
         EJECT                                                                  
OMOFFIOA DS    CL1000                                                           
         BUFF  LINES=200,ROWS=2,COLUMNS=5,COMMENT=51,FLAVOR=PACKED,    X        
               KEYLIST=(4,A)                                                    
         EJECT                                                                  
*              ROUTINE TO ADD TO A BINSRCH TABLE                                
*              PARAM1              A(RECORD TO BE ADDED)                        
*              PARAM2              A(BINSRCH PARAMS)                            
         USING BIND,R5                                                          
BINADDC  DS    0D                                                               
         NMOD1 0,*BINADD*                                                       
         L     RC,ADRC             RESTORE REGISTER 12                          
         L     R3,0(R1)            A(RECORD)                                    
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,(R3)),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1                                                           
         BE    BINXIT              NOT FOUND - ADDED                            
         L     R4,DMCB             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R0,BINNUMB          NUMBER OF BUCKETS                            
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,*-14                                                          
BINXIT   XIT1                                                                   
         SPACE 3                                                                
ADRC     DS    F                   SAVE ADDRESS OF REGISTER 12                  
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
SKD      DSECT                                                                  
SKINVC   DS    CL6                 INVOICE NUMBER                               
SKSOURCE DS    CL2                 SOURCE CODE (SK ACCOUNT)                     
SKDATE   DS    CL2                 DATE BILL REVERSED (IF APPLICABLE)           
SKKEYLN  EQU   *-SKD                                                            
SKBK     EQU   *                                                                
SKAMT    DS    CL8                 ACCUMULATOR                                  
SKLEN    EQU   *-SKD                                                            
         EJECT                                                                  
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER OF ENTRIES IN TABLE                   
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT OF KEY                          
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER OF ENTRIES                    
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISPLACEMENT OF 1ST BUCKET                   
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
* ACGENMODES ACGENBOTH ACGENFILE ACREPWORKD ARE INCLUDED                        
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054ACREPI502 03/23/15'                                      
         END                                                                    
