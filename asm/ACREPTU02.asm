*          DATA SET ACREPTU02  AT LEVEL 006 AS OF 12/11/09                      
*PHASE ACTU02A,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'TIME UPDATE PROGRAM'                                            
ACTU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTU**,R9,R8    BASE REGISTERS 11, 9 AND 8                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACTUD,RC            RC=A(SAVE W/S)                               
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
*                                                                               
         CLI   MODE,PROCSPEC       PROCESS SPECS                                
         BE    SPEC                                                             
         CLI   MODE,PROCRCVR       RECOVERY                                     
         BE    PRCV                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PRAC                                                             
         CLI   MODE,RUNLAST        RUN LAST = MERGE NEW RECORDS                 
         BE    RUNL                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS SPEC                                                        *         
***********************************************************************         
         SPACE 1                                                                
SPEC     L     RF,=A(BXHOOK)                                                    
         ST    RF,HEADHOOK                                                      
         L     RF,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(RF)                                                         
*                                                                               
         L     RF,AMONACC                                                       
         USING ACMD,RF                                                          
         L     R4,ACMVHELO                                                      
         ST    R4,AHELLO                                                        
*                                                                               
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'                                                 
*                                                                               
         MVC   ACURRMOA,AMOATAB                                                 
         L     R1,AMOATAB                                                       
         MVI   0(R1),X'FF'         EOT                                          
*                                                                               
         MVI   FCRDRCVR,C'Y'       DEFAULT IS RECOVERY                          
         MVI   FCGETOPT,C'N'       DON'T USE GETOPT                             
         OI    PRCSW,PRCRCV        PROCESS RECOVERY                             
         OI    PRNSW,PRNPER+PRNCLI+PRNCHA                                       
         XC    MOASTR,MOASTR                                                    
         MVC   MOAEND,=X'FFFF'                                                  
         LA    R3,RCVRECRD-RCVRECD                                              
         OI    PRCSW,PRCSPCL                                                    
         L     R6,APRSNTAB                                                      
         MVI   0(R6),X'FF'         MARK EOT FOR PERSON TABLE                    
*                                                                               
*        MVC   MCFFPARM(6),=C'UPDATE'                                           
*                                                                               
         CLI   MCFFPARM,C' '                                                    
         BNH   SPEC7                                                            
*                                                                               
         LA    R3,4                        GET DISPLACEMENT TO KEY              
         OI    PRCSW,PRCACC                ACCOUNT FILE + RECOVERY              
         MVI   FCRDACC,C'Y'                                                     
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRDREQS,C'Y'                                                    
         CLC   MCFFPARM(5),=C'UPDATE'      PARM=UPDATE(UPDATE FILE)             
         BNE   SPEC5                                                            
         NI    PRCSW,ALL-PRCSPCL                                                
         OI    PRCSW,PRCUPDT                                                    
         B     SPEC7                                                            
*                                                                               
SPEC5    CLC   MCFFPARM(4),=C'TAPE'        PARM=TAPE(OUTPUT TAPE)               
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PRCSW,PRCTAP                                                     
         MVI   RCWRITE,C'N'                                                     
         OPEN  (OUTFIL,(OUTPUT))                                                
*                                                                               
SPEC7    CVD   R3,DUB              SET DISPLACEMENT TO KEY                      
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         BAS   RE,WKND             BUILD LIST OF WEEK ENDING DATES              
         B     XIT                                                              
         DROP  R4,RF                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS RECOVERY RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
PRCV     L     R5,ADTRANS                                                       
         USING RCVRECD,R5          R5=A(RECOVERY HEADER)                        
         CLI   RCVSEQNO,X'FF'      IGNORE IF DELETED RECOVERY RECORD            
         BE    XIT                                                              
         CLI   RCVFILTY,RCVFAMST   TEST ACCMST RECORD                           
         BNE   XIT                                                              
         LA    R2,RCVRECRD                                                      
         USING TRNRECD,R2                                                       
         SR    RF,RF               SET EOR                                      
         ICM   RF,3,TRNRLEN                                                     
         AR    RF,R2                                                            
         MVI   0(RF),0                                                          
         BAS   RE,FLTK             TEST TIME TRANSACTION                        
         BNE   XIT                                                              
         TM    PRCSW,PRCSPCL                                                    
         BZ    PRCV3                                                            
         BAS   RE,ADDPER           ADD PERSON TO TABLE                          
         B     PRCV5                                                            
*                                                                               
PRCV3    GOTO1 ADSORTER,DMCB,=C'PUT',RCVLEN                                     
PRCV5    OI    PRCSW,PRCACT        SET ACTIVITY SWITCH                          
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD PERSON TO PERSON TABLE                                          *         
***********************************************************************         
         USING TRNRECD,R2                                                       
ADDPER   NTR1                                                                   
         L     R6,APRSNTAB                                                      
*                                                                               
         CLI   QOPT7,C'G'                                                       
         BNE   ADDPER5                                                          
         GOTO1 PRNTBL,DMCB,=C'DUMP',(R2),C'DUMP',2000,=C'1D'                    
*                                                                               
ADDPER5  CLI   0(R6),X'FF'         EOT, ADD PERSON TO END OF TABLE              
         BE    ADDPER10                                                         
         CLC   TRNKCULA,0(R6)      MATCHED, LEAVE                               
         BE    XIT                                                              
         LA    R6,L'TRNKCULA(R6)   BUMP TO NEXT PERSON                          
         B     ADDPER5                                                          
*                                                                               
ADDPER10 MVC   0(L'TRNKCULA,R6),TRNKCULA                                        
         LA    R6,L'TRNKCULA(R6)                                                
         MVI   0(R6),X'FF'                                                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVC   QUNIT(2),=C'1R'                                                  
         LA    RF,QACCOUNT+(L'QACCOUNT-1)                                       
         LA    R1,14                                                            
REQF1    CLI   0(RF),C' '          GET LENGTH OF REQUESTED ACCOUNT              
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,REQF1                                                         
         STC   R1,LNQAC                                                         
*                                                                               
         L     RF,AMONACC          SAVE START / END MOA                         
         USING ACMD,RF                                                          
         MVC   MOASTR,ACMMSTR                                                   
         MVC   MOAEND,ACMMEND                                                   
         OC    MOASTR,MOASTR                                                    
         BNZ   *+10                                                             
         MVC   MOASTR,STRDTE       DEFAULT START                                
*                                                                               
         OI    FLAG,REVTODAY       SET REVERSE TODAYS ACTIVITY                  
         TM    PRCSW,PRCBOTH       TEST RECORDS FROM RECOVERY                   
         BNO   *+8                                                              
         BAS   RE,FRCV             PROCESS RECOVERY RECORDS                     
*                                                                               
         NI    PRCSW,ALL-PRCRCV    TURN OFF RECOVERY                            
         NI    FLAG,ALL-REVTODAY                                                
         L     R2,ADCOMP                                                        
         MVC   COMPANY,0(R2)                                                    
         BAS   RE,COMP             SET COMPANY OPTIONS                          
         MVI   PRNSW,0             SET PRINT CONTROL                            
         CLI   QOPT1,C'B'                                                       
         BNE   *+8                                                              
         OI    PRNSW,PRNPER+PRNCLI PRINT PERIOD AND CLIENT                      
         CLI   QOPT1,C'P'                                                       
         BNE   *+8                                                              
         OI    PRNSW,PRNPER        PERIOD ONLY                                  
         CLI   QOPT1,C'C'                                                       
         BNE   *+8                                                              
         OI    PRNSW,PRNCLI        CLIENT ONLY                                  
*                                                                               
         CLI   QOPT2,C'C'                                                       
         BNE   *+8                                                              
         OI    PRNSW,PRNCHA        CHANGES ONLY                                 
*                                                                               
         CLI   QOPT3,C'R'          REVERSE TODAYS UPDATES                       
         BNE   *+8                                                              
         OI    PRCSW,PRCREV                                                     
*                                                                               
         CLI   QOPT4,C'B'                                                       
         BNE   *+8                                                              
         OI    PRNSW,PRNDMP+PRNDMC DUMP PERIOD AND CLIENT                       
         CLI   QOPT4,C'P'                                                       
         BNE   *+8                                                              
         OI    PRNSW,PRNDMP        PERIOD ONLY                                  
         CLI   QOPT4,C'C'                                                       
         BNE   *+8                                                              
         OI    PRNSW,PRNDMC        CLIENT ONLY                                  
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAYC)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         GOTO1 ADDAY,DMCB,(C'Y',TODAY),PRYRS,F'-3'                              
         GOTO1 DATCON,DMCB,(0,PRYRS),(1,PRYRSP)                                 
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* PROCACC                                                             *         
***********************************************************************         
         SPACE 1                                                                
PRAC     DS    0H                                                               
         TM    PRCSW,PRCSPCL       SPECIAL PROCESSING?                          
         BZ    PRAC2                                                            
*                                                                               
PRAC0    NTR1                                                                   
         B     *+8                 R2 POINTS TO RCV'S ACC                       
PRAC2    L     R2,ADACC                                                         
         MVC   PERACC,0(R2)        SAVE ACCOUNT CODE                            
         CLI   QOPT7,C'X'                                                       
         BNE   PRAC2A                                                           
         MVC   XP(8),=CL8'PERSON: '                                             
         MVC   XP+10(L'PERACC-1),PERACC+1                                       
         GOTO1 ACREPORT                                                         
*                                                                               
PRAC2A   LA    R2,DKEY                                                          
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,PERACC     SET PERSON CODE IN KEY                       
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                NO ACCOUNT RECORD                            
*                                                                               
PRAC3    LA    R2,DIR                                                           
         BAS   RE,DMSEQ                                                         
         CLC   TRNKCULA,PERACC     SAME ACCOUNT                                 
         BNE   PRAC5                                                            
         BAS   RE,FLTK                                                          
         BNE   PRAC3               NOT A TIME RECORD                            
         LA    RF,PTRN             PROCESS TRANSACTION                          
         TM    TYPSW,TYPTRN                                                     
         BO    *+8                                                              
         LA    RF,PTIM             PROCESS TIME RECORD                          
         BASR  RE,RF                                                            
         B     PRAC3                                                            
*                                                                               
PRAC5    BAS   RE,BLDT             BUILD NEW TOTALS RECORDS                     
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* RUN LAST - UPDATE TOTAL RECORDS                                     *         
***********************************************************************         
         SPACE 1                                                                
RUNL     TM    PRCSW,PRCACT        TEST ACTIVITY                                
         BNO   RUNL9                                                            
         TM    PRCSW,PRCRCV        TEST RECOVERY ONLY                           
         BNO   *+8                                                              
         BAS   RE,FRCV             GET RECORDS FROM RECOVERY                    
         MVI   COMPANY,0                                                        
*                                                                               
         USING TTHRECD,R3                                                       
RUNL3    BAS   RE,GETNXT           GET THE NEXT SORT RECORD                     
         BNE   RUNL7               EOF                                          
         L     R3,AIO2                                                          
         CLC   COMPANY,TTHKCPY     TEST SAME COMPANY                            
         BE    *+14                                                             
         MVC   COMPANY,TTHKCPY                                                  
         BAS   RE,COMP             GET COMPANY NAME/OPTIONS                     
         TM    PRCSW,PRCSPCL                                                    
         BO    RUNL5                                                            
         BAS   RE,UPDT             UPDATE EXISTING RECORD                       
*                                                                               
RUNL5    TM    PRCSW,PRCTAP        OUTPUT TO TAPE                               
         BNO   RUNL3                                                            
         SR    R1,R1                                                            
         L     R3,AIO2                                                          
         ICM   R1,3,TTHRLEN                                                     
         AHI   R1,4                FIX LENGTH FOR TAPE                          
         SHI   R3,4                                                             
         STCM  R1,3,0(R3)                                                       
         L     R0,AIO2                                                          
         SHI   R0,4                                                             
         PUT   OUTFIL,(R0)                                                      
         B     RUNL3                                                            
*                                                                               
RUNL7    TM    PRCSW,PRCTAP                                                     
         BNO   RUNL9                                                            
         CLOSE OUTFIL                                                           
*                                                                               
RUNL9    GOTO1 ADSORTER,DMCB,=C'END'                                            
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,CNTS             PRINT RECORD COUNTS                          
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         LA    R3,PLACC                                                         
*                                                                               
RUNL11   EDIT  (P5,0(R2)),(6,0(R3))                                             
         MVC   PLNME(25),5(R2)                                                  
         BAS   RE,ACRPT                                                         
         LA    R2,L'CNTS(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   RUNL11                                                           
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* GET THE NEXT RECORD FROM SORT                                       *         
***********************************************************************         
         SPACE 1                                                                
GETNXT   NTR1  ,                                                                
         CLI   COMPANY,0           FIRST TIME                                   
         BE    GETNXT7                                                          
*                                                                               
         L     RE,AIO4                                                          
         CLI   0(RE),0             TEST EOF                                     
         BE    GETNXTXN                                                         
*                                                                               
GETNXT5  L     R0,AIO2             MOVE IO4 TO IO2                              
         LA    R1,RLNMX                                                         
         L     RE,AIO4                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
GETNXT7  L     R0,AIO4             CLEAR IO4                                    
         LA    R1,RLNMX                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   RE,15,4(R1)                                                      
         BZ    GETNXTX                                                          
         L     R0,AIO4             SAVE SORT RECORD IN IO4                      
         SHI   R0,4                                                             
         SR    R1,R1                                                            
         ICM   R1,3,0(RE)                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         CLI   COMPANY,0           TEST FIRST TIME                              
         BNE   GETNXT9                                                          
         MVI   COMPANY,X'FF'                                                    
         B     GETNXT5             GET ANOTHER                                  
*                                                                               
GETNXT9  L     R3,AIO4                                                          
         L     R2,AIO2                                                          
         CLC   0(L'ACCKEY,R3),0(R2)   SAME KEY                                  
         BNE   GETNXTXY                                                         
         BAS   RE,MRGE             MERGE                                        
         B     GETNXT7             AND GET ANOTHER                              
*                                                                               
GETNXTX  CLI   COMPANY,0                                                        
         BNE   GETNXTXY                                                         
GETNXTXN LTR   RB,RB                                                            
         B     *+6                                                              
GETNXTXY CR    RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD TOTAL RECORDS FROM RECOVERY                                   *         
***********************************************************************         
         SPACE 1                                                                
FRCV     NTR1  ,                                                                
         TM    PRCSW,PRCSPCL                                                    
         BZ    *+12                                                             
         BAS   RE,SPCLRCV          USE PERSON TABLE FOR RECOVERY                
         B     XIT                                                              
*                                                                               
         MVI   COMPANY,0                                                        
         OPEN  (TMWRK,(OUTPUT))                                                 
*                                                                               
FRCV3    GOTO1 ADSORTER,DMCB,=C'GET' GET RCV RECORDS BY ACCOUNT                 
         ICM   R5,15,4(R1)                                                      
         BZ    FRCV5                                                            
         CLI   QCOMPANY,X'FF'      ALL COMPANIES                                
         BE    FRCV4                                                            
         CLI   QCOMPANY,X'41'                                                   
         BL    FRCV4               RECOVERY ONLY                                
         USING RCVRECD,R5                                                       
         LA    R2,RCVRECRD                                                      
         USING TRNRECD,R2                                                       
         CLC   TRNKCPY,RCCOMPFL    TEST COMPANY                                 
         BNE   FRCV3                                                            
         SR    R1,R1               ACCOUNT FILTER                               
         ICM   R1,1,LNQAC                                                       
         BZ    FRCV4                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRNKUNT(0),QUNIT                                                 
         BNE   FRCV3                                                            
*                                                                               
FRCV4    PUT   TMWRK,(R5)          WRITE THEM TO WORK                           
         B     FRCV3                                                            
*                                                                               
FRCV5    CLOSE TMWRK                                                            
         LA    R1,4                SET NEW SORT PARAMETERS                      
         CVD   R1,DUB              SORT BY TIME RECORD KEY                      
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+13(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         OPEN  (TMWRK,(INPUT))                                                  
         XC    PERACC,PERACC                                                    
*                                                                               
FRCV7    L     R5,AIO1                                                          
         SHI   R5,4                                                             
         GET   TMWRK,(R5)                                                       
         USING RCVRECD,R5                                                       
         LA    R2,RCVRECRD                                                      
         USING TRNRECD,R2                                                       
         CLC   COMPANY,TRNKCPY     TEST SAME COMPANY                            
         BE    *+14                                                             
         MVC   COMPANY,TRNKCPY                                                  
         BAS   RE,COMP             GET COMPANY OPTIONS                          
*                                                                               
         OC    PERACC,PERACC       TEST FIRST TIME                              
         BZ    FRCV9                                                            
         CLC   PERACC,0(R2)        TEST CHANGE OF ACCOUNT                       
         BNE   FRCV11                                                           
*                                                                               
FRCV9    MVC   PERACC,0(R2)        SAVE PERSON ACCOUNT                          
         TM    PRCSW,PRCSPCL                                                    
         BO    FRCV9C                                                           
         BAS   RE,FLTK             TEST TIME TRANSACTION                        
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
FRCV9C   CLI   RCVRECTY,RCVRCPYQ   COPY                                         
         BNE   *+8                                                              
         OI    TYPSW,TYPCPY                                                     
         CLI   RCVRECTY,RCVRCHAQ   CHANGE                                       
         BNE   *+8                                                              
         OI    TYPSW,TYPCHA                                                     
         CLI   RCVRECTY,RCVRADDQ   ADD                                          
         BNE   *+8                                                              
         OI    TYPSW,TYPADD                                                     
*                                                                               
         TM    PRCSW,PRCSPCL                                                    
         BZ    FRCV10                                                           
         OI    PRCSW,PRCACC                                                     
         NI    PRCSW,ALL-PRCRCV                                                 
         BAS   RE,PRAC                                                          
         BAS   RE,UPDT                                                          
         L     RE,ATSTAB                                                        
         L     RF,=A(TABLEN)                                                    
         XCEF                                                                   
         B     FRCV7                                                            
*                                                                               
FRCV10   LA    RF,PTRN             PROCESS TRANSACTION                          
         TM    TYPSW,TYPTRN                                                     
         BO    *+8                                                              
         LA    RF,PTIM             PROCESS TIME RECORD                          
         BASR  RE,RF                                                            
         B     FRCV7                                                            
*                                                                               
FRCV11   BAS   RE,BLDT             BUILD NEW TIME RECORD                        
         B     FRCV9                                                            
*                                                                               
FRCVX    CLOSE TMWRK                                                            
         BAS   RE,BLDT             PROCESS LAST                                 
         B     XIT                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* SPECIAL RECOVERY, DO UPDATE ON PERSON TABLE                         *         
***********************************************************************         
SPCLRCV  NTR1                                                                   
         L     R2,APRSNTAB                                                      
SPCLRCV2 CLI   0(R2),X'FF'         EOT?                                         
         BE    XIT                                                              
         OI    PRCSW,PRCACC                                                     
         NI    PRCSW,ALL-PRCRCV                                                 
         CLC   COMPANY,0(R2)                                                    
         BE    SPCLRCV5                                                         
         MVC   COMPANY,0(R2)                                                    
         BAS   RE,COMP                                                          
SPCLRCV5 LA    R3,DKEY             DON'T BOTHER W/ DELETED PEOPLE               
         USING TRNRECD,R3                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,0(R2)                                                   
         BAS   RE,DMRD                                                          
         LA    R3,DIR                                                           
         TM    TRNKSTA,TRNSDELT    DELETED?                                     
         BO    SPCLRCV7            YES, NEXT ONE                                
*                                                                               
         BAS   RE,PRAC                                                          
SPCLRCV7 LA    R2,L'TRNKCULA(R2)                                                
         B     SPCLRCV2                                                         
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER TRANSACTIONS AND TIME RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
FLTK     LA    RF,TRNKSTA                                                       
         TM    PRCSW,PRCRCV        TEST RECOVERY                                
         BNO   *+8                                                              
         LA    RF,TRNRSTA                                                       
         MVI   TYPSW,0                                                          
         CLC   TRNKUNT(2),PERSUL   PERSON LEDGER ONLY                           
         BNE   FLTKNO                                                           
         CLC   TRNKREF,SPACES      TEST TRANSACTION OR TIME RECORD              
         BL    FLTKNO                                                           
         CLC   TRNKREF,=C'*TIME*'  TEST TIME RECORD                             
         BNE   FLTK3                                                            
         USING TIMRECD,R2                                                       
         CLI   TIMKRI1-TIMKSTA(RF),TIMKRI1Q                                     
         BNE   FLTK3                                                            
         CLI   TIMKRI2-TIMKSTA(RF),TIMKRI2Q                                     
         BNE   FLTK3                                                            
         CLI   TIMKRI3-TIMKSTA(RF),TIMKRI3Q                                     
         BNE   FLTK3                                                            
         CLI   TIMKRI4-TIMKSTA(RF),TIMKRI4Q                                     
         BNE   FLTK3                                                            
         B     FLTKTIM                                                          
*                                                                               
         USING TRNRECD,R2                                                       
FLTK3    TM    TRNKSTAT-TRNKSTA(RF),TRNSDRFT  SKIP DRAFT TRANSACTIONS           
         BO    FLTKNO                                                           
         CLI   TRNKSTYP-TRNKSTA(RF),27        TYPE 27, 34, 41 AND 49            
         BE    FLTKTRN                                                          
         CLI   TRNKSTYP-TRNKSTA(RF),34                                          
         BE    FLTKTRN                                                          
         CLI   TRNKSTYP-TRNKSTA(RF),41                                          
         BE    FLTKTRN                                                          
         CLI   TRNKSTYP-TRNKSTA(RF),49                                          
         BE    FLTKTRN                                                          
*                                                                               
FLTKNO   LTR   RB,RB               NOT A TIME TRANSACTION                       
         BR    RE                                                               
FLTKTRN  OI    TYPSW,TYPTRN        OLD TIME TRANSACTION                         
         B     *+8                                                              
FLTKTIM  OI    TYPSW,TYPTIM        NEW TIME RECORD                              
         CR    RB,RB                                                            
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET HOURS FROM TRANSACTION RECORDS                                 *          
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
PTRN     NTR1  ,                                                                
         TM    PRCSW,PRCRCV        TEST RECOVERY FILE                           
         BO    PTRN3                                                            
         L     R2,AIO1                                                          
         BAS   RE,DMGETR           GET TRANSACTION RECORD                       
*                                                                               
         USING TRNELD,R4                                                        
PTRN3    LA    R4,TRNRFST                                                       
         CLI   0(R4),TRNELQ        TRANSACTION ELEMENT                          
         BNE   XIT                                                              
         SR    R0,R0                                                            
         SR    R3,R3                                                            
         SR    R5,R5                                                            
         LR    RF,R4                                                            
*                                                                               
PTRN7    IC    R0,1(RF)            GET HOURS/STATUS ELEMENTS                    
         AR    RF,R0                                                            
         CLI   0(RF),0             TEST EOR                                     
         BE    PTRN9                                                            
         CLI   0(RF),TRSELQ        STATUS ELEMENT                               
         BNE   *+6                                                              
         LR    R3,RF                                                            
         CLI   0(RF),SCIELQ        SUBSIDIARY CASH ELEMENT                      
         BNE   PTRN7                                                            
         CLI   SCITYPE-SCIELD(RF),SCITHOUR    TEST HOURS ELEMENT                
         BNE   PTRN7                                                            
         LR    R5,RF                                                            
         B     PTRN7                                                            
*                                                                               
PTRN9    LA    R6,TS               PERIOD TOTAL HOURS                           
         USING TSD,R6                                                           
         LTR   R3,R3               TEST STATUS ELEMENT                          
         BZ    XIT                                                              
         LTR   R5,R5               TEST HOURS ELEMENT                           
         BZ    XIT                                                              
         USING TRSELD,R3                                                        
         USING SCIELD,R5                                                        
         CLC   TRSPMOS,MOASTR      TEST BEFORE START                            
         BL    XIT                                                              
         CLC   TRSPMOS,MOAEND      OR AFTER END                                 
         BH    XIT                                                              
         XC    TS,TS                                                            
         NI    TSTYPE,X'FF'-TSADJSTD                                            
         TM    TRSSTAT2,TRSSTADJ   TEST ADJUSTED HOURS                          
         BZ    *+8                                                              
         OI    TSTYPE,TSADJSTD     MARK AS SUCH                                 
         MVC   TSPEDT,TRNDATE      PERIOD END                                   
         MVC   TSMOA,TRSPMOS       MOA                                          
         ZAP   TSTHRS,SCIAMNT                                                   
         ZAP   TSCLTH,=P'0'                                                     
         TM    TYPSW,TYPCPY        TEST COPY                                    
         BNO   *+10                                                             
         MP    TSTHRS,=P'-1'       COPIES ARE SUBTRACTED                        
*                                                                               
PTRN11   CLC   TRNKULC(2),COSTUL   MUST BE CLIENT TIME                          
         BNE   *+10                                                             
         ZAP   TSCLTH,TSTHRS       MONTH                                        
         BAS   RE,GETWK            GET WEEK ENDING DATE                         
         BAS   RE,ADDT                                                          
         B     XIT                                                              
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT                                                                  
***********************************************************************         
* GET HOURS FROM TIME RECORDS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMRECD,R2                                                       
PTIM     NTR1  ,                                                                
         TM    PRCSW,PRCRCV        TEST RECOVERY FILE                           
         BO    PTIM3                                                            
         L     R2,AIO1                                                          
         BAS   RE,DMGETR           GET TRANSACTION RECORD                       
*                                                                               
         USING TIMELD,R4                                                        
PTIM3    SR    RF,RF                                                            
         ICM   RF,3,TIMRLEN                                                     
         AR    RF,R2                                                            
         MVI   0(RF),0                                                          
         LA    R4,TIMRFST                                                       
         SR    R0,R0                                                            
*                                                                               
PTIM5    CLI   0(R4),TIMELQ        TIME DETAIL ELEMENT                          
         BNE   PTIM9               SKIP IT                                      
         CLI   TIMETYP,TIMEINP     INPUT DETAILS                                
         BNE   PTIM9                                                            
         LA    R6,TS               PERIOD TOTAL HOURS                           
         USING TSD,R6                                                           
         CLC   TIMMOA,MOASTR       TEST BEFORE START                            
         BL    PTIM9                                                            
         CLC   TIMMOA,MOAEND       OR AFTER END                                 
         BH    PTIM9                                                            
         SR    RF,RF                                                            
         ICM   RF,3,TIMRLEN                                                     
         NI    TSTYPE,X'FF'-TSADJSTD                                            
         TM    TIMIND,TIMIADJ      ADJUSTED HOURS?                              
         BZ    *+8                                                              
         OI    TSTYPE,TSADJSTD     MARK AS SUCH                                 
         MVC   TSPEDT,TIMKPEDT     PERIOD END DATE                              
         MVC   TSMOA,TIMMOA        MOA                                          
         ZAP   TSTHRS,TIMHRS                                                    
         ZAP   TSCLTH,=P'0'                                                     
         TM    TYPSW,TYPCPY        TEST COPY                                    
         BNO   *+10                                                             
         MP    TSTHRS,=P'-1'       COPIES ARE SUBTRACTED                        
*                                                                               
PTIM7    CLC   TIMKULC(2),COSTUL                                                
         BNE   *+10                                                             
         ZAP   TSCLTH,TIMHRS                                                    
         BAS   RE,GETWK            GET WEEK ENDING DATE                         
         BAS   RE,ADDT                                                          
*                                                                               
PTIM9    IC    R0,1(R4)            GET NEXT TIME ELEMENT                        
         AR    R4,R0                                                            
         CLI   0(R4),0             TEST EOR                                     
         BNE   PTIM5                                                            
         DROP  R2,R4,R6                                                         
         B     XIT                 GET ANOTHER RECORD                           
         EJECT                                                                  
***********************************************************************         
* GET WEEK ENDING DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TSD,R6                                                           
GETWK    NTR1  ,                                                                
         MVC   OFC,SPACES                                                       
         SR    R1,R1                                                            
         IC    R1,LVLS1R           LENGTH OF OFFICE                             
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   OFC(0),ACTKACT-ACTRECD(R2) GET OFFICE CODE                       
         L     R5,ACLNTAB          R5=CALENDAR TABLE                            
         USING CLND,R5                                                          
         LA    R0,CLNMX            MAX NUMBER CALENDARS                         
*                                                                               
GETWK0   OC    CLNNUM,CLNNUM       TEST ENTRIES                                 
         BZ    GETWK3                                                           
         CLC   OFC,CLNOFC          MATCH OFFICE                                 
         BE    GETWK5                                                           
         CLI   CLNOFC,C' '         CORPORATE CALENDAR IS LAST                   
         BE    GETWK5                                                           
GETWK3   A     R5,=A(CLNLNQ)                                                    
         BCT   R0,GETWK0                                                        
         B     GETWK11                                                          
*                                                                               
GETWK5   ICM   R0,3,CLNNUM         R0=NUMBER OF ENTRIES                         
         LA    R4,CLNWK            FIRST LOOK TO CALENDAR                       
GETWK7   CLC   TSPEDT,0(R4)                                                     
         BL    GETWK9                                                           
         CLC   TSPEDT,3(R4)                                                     
         BH    GETWK9                                                           
         MVC   TSPEDT,3(R4)        SET TO WEEK END DATE                         
         B     GETWK20                                                          
GETWK9   LA    R4,L'CLNWK(R4)                                                   
         BCT   R0,GETWK7                                                        
*                                                                               
         CLI   CLNOFC,C' '         CAN'T FIND ANY                               
         BE    GETWK11             USE SATURDAY METHOD                          
         L     R5,ACLNTAB          POINT TO CORPORATE CALENDAR                  
         A     R5,=AL4((CLNMX-1)*CLNLNQ)                                        
         B     GETWK5                                                           
*                                                                               
GETWK11  LA    R5,STRDTE           USE SATURDAY END AS DEFAULT                  
         LA    R0,NWKNDQ                                                        
GETWK13  CLC   TSPEDT,0(R5)                                                     
         BH    GETWK15                                                          
         MVC   TSPEDT,0(R5)                                                     
         B     GETWK20                                                          
GETWK15  LA    R5,L'STRDTE(R5)                                                  
         BCT   R0,GETWK13                                                       
*                                                                               
GETWK20  BAS   RE,FINDMOA          FIND MOA FOR PERIOD DATE AND OFFICE          
*                                                                               
GETWKX   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* FIND MOA FOR THE PERIOD DATE                                        *         
***********************************************************************         
         USING TIMRECD,R2                                                       
         USING CALMD,R3                                                         
FINDMOA  NTR1                                                                   
         TM    CMPSW,CMPCAL        DOES COMPANY HAVE CALENDARS?                 
         BZ    FMOA100             ALTERNATE METHOD                             
         SR    R1,R1                                                            
         XC    ACURRMOA,ACURRMOA                                                
         L     R3,AMOATAB                                                       
FMOA10   CLI   0(R3),X'FF'         EOT                                          
         BE    FMOA40                                                           
         CLC   TSPEDT,CALMPSTR     SEE IF PERIOD IN RANGE                       
         BL    FMOA20                                                           
         CLC   TSPEDT,CALMPEND                                                  
         BH    FMOA20                                                           
         OC    ACURRMOA,ACURRMOA   DID WE FIND CORPORATE ONE?                   
         BZ    FMOA30                                                           
         IC    R1,LVLS1R                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TIMKACT(0),CALMPOFF    YES, OFFICES HAVE TO MATCH                
         BE    FMOA40                                                           
FMOA20   LA    R3,CALMLNQ(R3)      NEXT MOA ENTRY                               
         B     FMOA10                                                           
*                                                                               
FMOA30   CLC   CALMPOFF,SPACES     FOUND CORPORATE ONE?                         
         BNE   *+8                                                              
         ST    R3,ACURRMOA                                                      
         B     FMOA20                                                           
*                                                                               
FMOA40   CLI   0(R3),X'FF'         EOT?                                         
         BNE   FMOA50                                                           
         OC    ACURRMOA,ACURRMOA                                                
         BZ    FMOA100             USE ALTERNATE METHOD                         
         L     R3,ACURRMOA                                                      
*                                                                               
FMOA50   ST    R3,ACURRMOA                                                      
         MVC   TSNMOA,CALMNMOA                                                  
         MVC   TSSMOA,CALMSMOA                                                  
         B     XIT                                                              
*                                                                               
FMOA100  DS    0H                                                               
         CLI   CMPFINYR,1                                                       
         BH    FMOA150                                                          
         MVC   TSNMOA,TSPEDT       TAKE YEAR FROM PERIOD DATE                   
         MVI   TSNMOA+1,X'12'      JAN/??-DEC/??                                
         MVC   TSSMOA,TSPEDT                                                    
         MVI   TSSMOA+1,1                                                       
         B     XIT                                                              
*                                                                               
FMOA150  DS    0H                                                               
         MVC   TSSMOA,TSPEDT                                                    
         MVC   TSSMOA+1(1),CMPFINYR                                             
         MVC   TSNMOA,TSPEDT                                                    
         SR    RE,RE                                                            
         IC    RE,CMPFINYR                                                      
         BCTR  RE,0                                                             
         STC   RE,TSNMOA+1(1)                                                   
         MVC   WORK(3),TSPEDT                                                   
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+4)                                  
         CLC   TSPEDT+1(1),TSNMOA+1                                             
         BH    FMOA170                                                          
         GOTO1 ADDAY,DMCB,(C'Y',WORK+4),WORK+4,F'-1'                            
         GOTO1 DATCON,DMCB,(0,WORK+4),(1,WORK)                                  
         MVC   TSSMOA(1),WORK                                                   
         B     XIT                                                              
*                                                                               
FMOA170  GOTO1 ADDAY,DMCB,(C'Y',WORK+4),WORK+4,F'1'                             
         GOTO1 DATCON,DMCB,(0,WORK+4),(1,WORK)                                  
         MVC   TSNMOA(1),WORK                                                   
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TSD,R6                                                           
NEW0     USING TSD,R3                                                           
*                                                                               
ADDT     NTR1  ,                                                                
         ZAP   DUB,TSTHRS          SET TOTAL HOURS                              
*                                                                               
         TM    FLAG,REVTODAY       TEST REVERSE TODAYS ACTIVITY                 
         BNO   ADDT2                                                            
         TM    PRCSW,PRCREV        TEST OPTION TO EXCLUDE TODAY                 
         BNO   XIT                 IGNORE RECOVERY ITEM                         
         MP    DUB,=P'-1'          REVERSE RECOVERY ITEM                        
*                                                                               
ADDT2    LA    R6,TS               NEW ENTRY                                    
         ZAP   TSTHRS,DUB                                                       
*                                                                               
         L     R3,ATSTAB                                                        
         ICM   R0,15,NUMTAB        NUMBER IN TABLE                              
         BZ    ADDT9                                                            
*                                                                               
ADDT3    CLC   0(TSKLNQ,R6),0(R3)  MATCH ITEM TO TABLE                          
         BE    ADDT5                                                            
         LA    R3,TSLNQ(R3)                                                     
         BCT   R0,ADDT3                                                         
         B     ADDT9                                                            
*                                                                               
ADDT5    AP    NEW0.TSTHRS,TSTHRS  ADD TOTAL HOURS                              
         AP    NEW0.TSCLTH,TSCLTH  CLIENT HOURS                                 
         B     XIT                                                              
*                                                                               
ADDT9    MVC   0(TSLNQ,R3),0(R6)   SAVE NEW ITEM                                
         L     R0,NUMTAB                                                        
         AH    R0,=H'1'                                                         
         ST    R0,NUMTAB                                                        
         CH    R0,=Y(TSNMX)                                                     
         BNH   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         B     XIT                                                              
         DROP  NEW0                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD NEW TIME RECORDS                                              *         
***********************************************************************         
         USING TSD,R6                                                           
BLDT     NTR1  ,                                                                
         ICM   R2,15,NUMTAB        SORT BY DATE                                 
         BZ    BLDT0               NO RECORDS, DON'T SORT                       
         LA    R3,TSLNQ                                                         
         LA    RF,TSKLNQ                                                        
         L     R6,ATSTAB                                                        
         GOTO1 XSORT,DMCB,(0,(R6)),(R2),(R3),(RF),0                             
*                                                                               
BLDT0    TM    PRCSW,PRCUPDT       ARE WE UPDATING?                             
*        BZ    *+8                 SKIP PURGING HOURS                           
*        BAS   RE,PRGEHRS          PURGE ALL HOURS FROM RECORDS                 
         LTR   R2,R2                                                            
         BZ    XIT                                                              
*                                                                               
BLDT3    L     R0,AIO2                                                          
         LA    R1,RLNMX                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         L     R3,AIO2             OUTPUT IO                                    
         USING TTHRECD,R3                                                       
         MVI   TTHKTYP,TTHKTYPQ    RECORD TYPE                                  
         MVI   TTHKSUB,TTHKSUBQ                                                 
         MVC   TTHKCULA,PERACC     ACCOUNT CODE                                 
         MVI   TTHKTIME,TTHKTTOT                                                
         MVC   TTHKEMOA,TSNMOA                                                  
         MVC   TTHKSMOA,TSSMOA                                                  
         MVC   TTHRLEN,=Y(TTHRFST-TTHRECD)                                      
*                                                                               
BLDT5    CLC   TTHKEMOA,TSNMOA     ON THE SAME CALENDAR YEAR?                   
         BNE   BLDT10              NO, UPDATE THIS RECORD                       
         CLC   TTHKSMOA,TSSMOA                                                  
         BNE   BLDT10                                                           
*                                                                               
BLDT5J   LA    R4,ELEMENT                                                       
         USING PTHELD,R4                                                        
         MVI   PTHEL,PTHELQ                                                     
         TM    TSTYPE,TSADJSTD                                                  
         BZ    *+8                                                              
         MVI   PTHEL,PTHELQA                                                    
         MVI   PTHLN,PTHLNQ                                                     
         MVC   PTHEDT,TSPEDT                                                    
         MVC   PTHMOA,TSMOA                                                     
         ZAP   PTHCHRS,TSCLTH                                                   
         ZAP   PTHTHRS,TSTHRS                                                   
         LA    R5,PTHMOA                                                        
*                                                                               
BLDT7    LA    R6,TSLNQ(R6)        BUMP TO NEXT ELEMENT                         
         CLC   PTHEDT,TSPEDT       CHECK IF PERIOD THE SAME                     
         BNE   BLDT9                                                            
         CLI   PTHEL,PTHELQ        REGULAR X'8D' ELEMENT                        
         BNE   BLDT7C              NO, HAS TO BE X'8E' ADJUSTED                 
         TM    TSTYPE,TSADJSTD     NEW ONE IS ADJUSTED?                         
         BO    BLDT9               YES, SEND THIS ELEMENT OUT                   
         B     BLDT8                                                            
*                                                                               
BLDT7C   TM    TSTYPE,TSADJSTD     ADJUSTED ELEMENT, IS NEW ONE ADJSTD?         
         BZ    BLDT9               NO, SEND THIS ELEMENT OUT                    
*                                                                               
BLDT8    BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    BLDT10                                                           
         USING PTHMOA,R5                                                        
         LA    R5,L'PTHSLNQ(R5)                                                 
         MVC   PTHMOA,TSMOA        MORE SUBELEMENT FOR MOAS                     
         ZAP   PTHCHRS,TSCLTH                                                   
         ZAP   PTHTHRS,TSTHRS                                                   
         SR    RF,RF               BUMP UP LENGTH OF ELEMENT                    
         IC    RF,PTHLN                                                         
         LA    RF,L'PTHSLNQ(RF)                                                 
         STC   RF,PTHLN                                                         
         B     BLDT7                                                            
*                                                                               
BLDT9    L     RF,AIO2                                                          
         LA    R4,ELEMENT                                                       
         BAS   RE,ADDL             ADD THE NEW ELEMENT                          
         XC    ELEMENT,ELEMENT                                                  
         LTR   R2,R2                                                            
         BZ    BLDT10                                                           
         BCT   R2,BLDT5                                                         
*                                                                               
BLDT10   OC    ELEMENT,ELEMENT     TEST ANYTHING LEFT                           
         BZ    BLDT11                                                           
         L     RF,AIO2                                                          
         LA    R4,ELEMENT                                                       
         BAS   RE,ADDL             ADD THE NEW ELEMENT                          
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
BLDT11   CLI   TTHRFST,0           TEST ANYTHING ADDED TO RECORD                
         BE    BLDT13Z                                                          
*                                                                               
         L     RF,AIO2                                                          
         SHI   RF,4                                                             
         XC    0(4,RF),0(RF)                                                    
         SR    R1,R1                                                            
         ICM   R1,3,TTHRLEN        RECORD LENGTH                                
         AHI   R1,4                                                             
         STCM  R1,3,0(RF)                                                       
         TM    PRCSW,PRCSPCL                                                    
         BZ    BLDT13                                                           
         BAS   RE,UPDT                                                          
         B     BLDT13X                                                          
*                                                                               
BLDT13   GOTO1 ADSORTER,DMCB,=C'PUT',(RF)                                       
BLDT13X  OI    PRCSW,PRCACT        SET ACTIVITY SWITCH                          
BLDT13Z  LTR   R2,R2                                                            
         BNZ   BLDT3                                                            
*                                                                               
BLDT15   XC    NUMTAB,NUMTAB       CLEAR TABLE                                  
         L     R0,ATSTAB                                                        
         L     R1,=F'9500'                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* UPDATE TIME RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TTHRECD,R3                                                       
UPDT     NTR1  ,                                                                
         L     R3,AIO2             R3=NEW RECORD (AIO2)                         
         BAS   RE,GETPER           GET 1R RECORD                                
         MVI   RECSW,0                                                          
         MVI   ACTION,ADD                                                       
         MVC   DKEY,TTHKEY                                                      
         BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'TTHKEY),DIR                                               
         BE    UPDT3                                                            
         LR    R2,R3                                                            
         BAS   RE,DMADDR           ADD NEW RECORD                               
         AP    CNTADD,=P'1'                                                     
         MVC   APIO,AIO2                                                        
         BAS   RE,PRNT                                                          
         BAS   RE,DMPNEW                                                        
         B     XIT                                                              
*                                                                               
UPDT3    MVI   ACTION,CHA                                                       
         LA    R3,DIR                                                           
         TM    TTHKSTAT,TTHSDELT   TEST DELETED                                 
         BO    XIT                 DON'T UPDATE DELETED RECORDS                 
*                                                                               
UPDT5    L     R0,AIO1             CLEAR IO1                                    
         LA    R1,RLNMX                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO1             R2=OLD RECORD (AIO1)                         
         BAS   RE,DMGETR           GET OLD RECORD                               
****************                                                                
         TM    PRCSW,PRCRCV        TEST RECOVERY FILE                           
         BZ    UPDT7                                                            
         MVC   APIO,AIO2           PRINT RECOVERY CHANGES                       
         BAS   RE,PRNT                                                          
         L     R3,AIO1                                                          
         BAS   RE,MRGE             UPDATE FILE WITH RECOVERY                    
         B     UPDT9                                                            
*                                                                               
UPDT7    DS    0H                                                               
         BAS   RE,MODI             CREATE RECORD OF CHANGES IN IO5              
         MVC   APIO,AIO2           PRINT NEW RECORD                             
         TM    PRNSW,PRNCHA        TEST PRINT CHANGES ONLY                      
         BNO   *+10                                                             
         MVC   APIO,AIO5           PRINT CHANGES                                
         BAS   RE,PRNT                                                          
*                                                                               
         L     R3,AIO5                                                          
         CLI   TTHRFST,0            TEST ANY MODIFICATIONS LEFT                 
         BNE   UPDT9                                                            
         AP    CNTOK,=P'1'         OK RECORDS                                   
         B     XIT                                                              
*                                                                               
UPDT9    L     R2,AIO2                                                          
         BAS   RE,DMPUTR           PUT CHANGED RECORD                           
         AP    CNTCHA,=P'1'                                                     
         BAS   RE,DMPOLD           DUMP OLD, CHANGED AND NEW RECORDS            
         BAS   RE,DMPMOD                                                        
         BAS   RE,DMPNEW                                                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* MERGE TIME CHANGES WITH EXISTING RECORD                             *         
*  R3 = A(INPUT IO)                                                   *         
*  IO2 IS THE OUTPUT RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TTHRECD,R3                                                       
*                                                                               
MRGE     NTR1  ,                                                                
         LA    R2,TTHRFST                                                       
         SR    R0,R0                                                            
*                                                                               
MRGE3    CLI   0(R2),0             EOR?                                         
         BE    XIT                                                              
         CLI   0(R2),PTHELQ        PERIOD ELEMENT, REGULAR?                     
         BE    *+12                                                             
         CLI   0(R2),PTHELQA       ADJUSTED?                                    
         BNE   MRGE9                                                            
         LR    R4,R2                                                            
         USING PTHELD,R4                                                        
NEW1     USING PTHELD,R5                                                        
*                                  ELEMENT SEARCH  8D / 8E                      
         GOTO1 AHELLO,LPARM,(C'G',ACCMST),(PTHEL,AIO2),(3,2(R4)),0              
         CLI   LP4,0               TEST ELEMENT FOUND                           
         BNE   MRGE7               ADD TO NEW RECORD                            
         L     R5,LP4              MERGE ELEMENT DATA                           
         BAS   RE,APPENDEL                                                      
         B     MRGE9                                                            
*                                                                               
MRGE7    L     RF,AIO2             ADD ELEMENT TO RECORD                        
         BAS   RE,ADDL                                                          
*                                                                               
MRGE9    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     MRGE3                                                            
         EJECT                                                                  
**********************************************************************          
* APPEND NEW MOA TO ELEMENT IF NECESSARY                             *          
**********************************************************************          
APPENDEL NTR1                                                                   
         LR    R1,R5               SAVE FOR LATER                               
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(PTHBLNQ),NEW1.PTHEL                                      
         ZIC   R0,NEW1.PTHLN                                                    
         SH    R0,=Y(PTHBLNQ)                                                   
         ZIC   RE,PTHLN                                                         
         SH    RE,=Y(PTHBLNQ)                                                   
         LA    RF,ELEMENT                                                       
         LA    RF,PTHBLNQ(RF)                                                   
*                                                                               
APEL     USING PTHMOA,RF                                                        
APPEL03  CLC   NEW1.PTHMOA,PTHMOA                                               
         BNE   APPEL05                                                          
         MVC   0(L'PTHSLNQ,RF),NEW1.PTHMOA                                      
         AP    APEL.PTHTHRS,PTHTHRS                                             
         AP    APEL.PTHCHRS,PTHCHRS                                             
         LA    R5,L'PTHSLNQ(R5)                                                 
         SH    R0,=Y(L'PTHSLNQ)                                                 
         B     APPEL10                                                          
*                                                                               
APPEL05  BH    APPEL07                                                          
         MVC   0(L'PTHSLNQ,RF),NEW1.PTHMOA                                      
         LA    RF,L'PTHSLNQ(RF)                                                 
         LA    R5,L'PTHSLNQ(R5)                                                 
         SH    R0,=Y(L'PTHSLNQ)                                                 
         BZ    APPEL60                                                          
         B     APPEL03                                                          
*                                                                               
APPEL07  MVC   0(L'PTHSLNQ,RF),PTHMOA        APPEND TO ELEMENT                  
*                                                                               
APPEL10  LA    RF,L'PTHSLNQ(RF)                                                 
         LA    R4,L'PTHSLNQ(R4)                                                 
         SH    RE,=Y(L'PTHSLNQ)                                                 
         BZ    APPEL50                                                          
         LTR   R0,R0                                                            
         BZ    APPEL60                                                          
         B     APPEL03                                                          
*                                                                               
APPEL50  LTR   R0,R0                                                            
         BZ    APPEL70                                                          
         MVC   0(L'PTHSLNQ,RF),NEW1.PTHMOA                                      
         LA    R5,L'PTHSLNQ(R5)                                                 
         LA    RF,L'PTHSLNQ(RF)                                                 
         SH    R0,=Y(L'PTHSLNQ)                                                 
         BZ    APPEL70                                                          
         B     APPEL50                                                          
*                                                                               
APPEL60  LTR   RE,RE                                                            
         BZ    APPEL70                                                          
         MVC   0(L'PTHSLNQ,RF),PTHMOA                                           
         LA    R4,L'PTHSLNQ(R4)                                                 
         LA    RF,L'PTHSLNQ(RF)                                                 
         SH    RE,=Y(L'PTHSLNQ)                                                 
         BZ    APPEL70                                                          
         B     APPEL60                                                          
*                                                                               
APPEL70  MVI   0(RF),0                                                          
         LA    RE,ELEMENT                                                       
         SR    RF,RE               CALCULATE LENGTH OF ELEMENT                  
         STC   RF,ELEMENT+1                                                     
         LR    R5,R1                                                            
         CLC   NEW1.PTHLN,ELEMENT+1                                             
         BNE   APPEL80                                                          
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   NEW1.PTHEL(0),ELEMENT                                            
         B     APPEL90                                                          
*                                                                               
APPEL80  MVI   NEW1.PTHEL,X'FF'                                                 
         GOTO1 AHELLO,LPARM,(C'D',ACCMST),(X'FF',AIO2),0,0                      
         CLI   LP4,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO2                                                          
         LA    R4,ELEMENT                                                       
         BAS   RE,ADDL                                                          
*                                                                               
APPEL90  B     XIT                                                              
         DROP  NEW1                                                             
         DROP  APEL                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CREATE A RECORD OF MODIFICATIONS                                    *         
*  IO1  HAS OLD RECORD                                                *         
*  IO2  HAS NEW RECORD (REQUESTED MONTHS ONLY)                        *         
*  IO5  WILL CONTAIN IO2 LESS IO1                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TTHRECD,R2                                                       
NEWRC    USING TTHRECD,R4                                                       
MODI     NTR1  ,                                                                
         L     R0,AIO5             COPY IO2'S RECORD HEADER TO AIO5             
         LA    R1,RLNMX                                                         
         L     RE,AIO2                                                          
         LH    RF,=Y(TTHRFST-TTHRECD)                                           
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AIO5                                                          
         LH    R1,=Y(TTHRFST-TTHRECD)   MOVE IN NEW LENGTH                      
         STH   R1,TTHRLEN                                                       
*        MVI   TTHRFST,0                MARK EOR                                
*                                                                               
         L     R2,AIO1                                                          
         LA    R3,TTHRFST                                                       
         L     R4,AIO2                                                          
         LA    R5,NEWRC.TTHRFST                                                 
*        CLC   TTHKACT,=C'71120A528126'                                         
*        BNE   *+6                                                              
*        DC    H'00'                                                            
*                                                                               
         USING PTHELD,R3                                                        
NEWEL    USING PTHELD,R5                                                        
MODI10   XC    ELEMENT,ELEMENT                                                  
         CLC   NEWEL.PTHEL,PTHEL     ARE ELEMENTS THE SAME?                     
         BL    MODI20                                                           
         BH    MODI25                                                           
         CLC   NEWEL.PTHEDT,PTHEDT   MATCHING ELEMENT/PERIOD?                   
         BL    MODI20                NEW ONE                                    
         BH    MODI25                OLD ONE                                    
         SR    R1,R1                                                            
         IC    R1,PTHLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NEWEL.PTHEL(0),PTHEL          SEE IF WHOLE ELEMENT SAME          
         BE    MODI80                        MATCH, BUMP BOTH UP                
*                                                                               
         BAS   RE,MODI200          DIFFERENCE & ADD TO IO5                      
         B     MODI80                                                           
*                                                                               
MODI20   LR    R6,R5               COPY NEW RECORD'S ELEMENT                    
         SR    R7,R7               DON'T NEGATE HOURS                           
         BAS   RE,ADDL2IO5                                                      
         SR    R1,R1                                                            
         IC    R1,NEWEL.PTHLN                                                   
         AR    R5,R1                                                            
         CLI   NEWEL.PTHEL,0       EOR ON NEW RECORD?                           
         BNE   MODI10              NO, LOOP BACK                                
         LR    R6,R3               YES, ADD REMAINING OLD ONES TO IO5           
         LA    R7,1                                                             
         B     MODI100                                                          
*                                                                               
MODI25   LR    R6,R3               COPY OLD RECORD'S ELEMENT                    
         LA    R7,1                NEGATE HOURS                                 
         BAS   RE,ADDL2IO5                                                      
         SR    R1,R1                                                            
         IC    R1,PTHLN                                                         
         AR    R3,R1                                                            
         CLI   PTHEL,0             EOR ON OLD RECORD?                           
         BNE   MODI10              NO,LOOP BACK                                 
         LR    R6,R5               YES, ADD REMAINING NEW ONES TO IO5           
         SR    R7,R7                                                            
         B     MODI100                                                          
*                                                                               
MODI80   SR    R1,R1               BUMPS TO NEXT ELEMENT IN OLD REC             
         IC    R1,PTHLN                                                         
         AR    R3,R1                                                            
         IC    R1,NEWEL.PTHLN      BUMPS TO NEXT ELEMENT IN NEW REC             
         AR    R5,R1                                                            
         CLI   PTHEL,0             ANY MORE IN OLD REC ?                        
         BNE   MODI85              YES                                          
         LR    R6,R5                                                            
         SR    R7,R7                                                            
         B     MODI100                                                          
*                                                                               
MODI85   CLI   NEWEL.PTHEL,0                                                    
         BNE   MODI10                                                           
         LR    R6,R3                                                            
         LA    R7,1                                                             
         B     MODI100                                                          
*                                                                               
MODI100  DS    0H                  COPIES REST OF ELEMENTS TO IO5               
         CLI   0(R6),0             EOR?                                         
         BE    XIT                                                              
         BAS   RE,ADDL2IO5         ADD ELEMENT TO IO5                           
         SR    R1,R1               BUMP TO NEXT ELEMENT                         
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     MODI100                                                          
         DROP  R2,NEWRC                                                         
         EJECT                                                                  
*-------------------------------------------------------------                  
* FIND DIFFERENCE OF 2 ELEMENTS                                                 
*        R3 --> OLD ELEMENT                                                     
*        R5 --> NEW ELEMENT                                                     
*        ELEMENT WILL HAVE THE DIFFERENCE                                       
*-------------------------------------------------------------                  
REPEL    USING PTHELD,R6                                                        
SUBEL    USING PTHELD,RF                                                        
MODI200  NTR1                                                                   
         LA    RF,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVC   SUBEL.PTHEL(PTHBLNQ),PTHEL   COPY ELEMENT HEADER                 
         MVI   SUBEL.PTHLN,PTHBLNQ                                              
         SR    R2,R2                                                            
         IC    R2,PTHLN                                                         
         SH    R2,=Y(PTHBLNQ)           LEN(ELEMENT) - LEN(HEADER)              
         SR    R4,R4                                                            
         IC    R4,NEWEL.PTHLN                                                   
         SH    R4,=Y(PTHBLNQ)           LEN(ELEMENT) - LEN(HEADER)              
*                                                                               
MODI210  CLC   NEWEL.PTHMOA,PTHMOA      MATCHING MOA'S?                         
         BL    MODI220                                                          
         BH    MODI230                                                          
         CP    NEWEL.PTHCHRS,PTHCHRS    COMPARE HOURS                           
         BNE   MODI280                                                          
         CP    NEWEL.PTHTHRS,PTHTHRS                                            
         BNE   MODI280                                                          
MODI215  LA    R5,L'PTHSLNQ(R5)                                                 
         SH    R4,=Y(L'PTHSLNQ)                                                 
         LA    R3,L'PTHSLNQ(R3)                                                 
         SH    R2,=Y(L'PTHSLNQ)                                                 
         LTR   R4,R4                                                            
         BZ    MODI225                                                          
         LTR   R2,R2                                                            
         BZ    MODI235                                                          
         B     MODI210                                                          
*                                                                               
MODI220  LR    R6,R5                                                            
         SR    R7,R7                                                            
         BAS   RE,MODI270                                                       
         LA    R5,L'PTHSLNQ(R5)                                                 
         SH    R4,=Y(L'PTHSLNQ)                                                 
         BNZ   MODI210                                                          
MODI225  LR    R6,R3               COPY REST OF OLD ELEMENT                     
         LR    R0,R2                                                            
         LA    R7,1                                                             
         B     MODI290                                                          
*                                                                               
MODI230  LR    R6,R3                                                            
         LA    R7,1                                                             
         BAS   RE,MODI270                                                       
         LA    R3,L'PTHSLNQ(R3)                                                 
         SH    R2,=Y(L'PTHSLNQ)                                                 
         BNZ   MODI210                                                          
MODI235  LR    R6,R5               COPY REST OF NEW ELEMENT                     
         LR    R0,R4                                                            
         SR    R7,R7                                                            
         B     MODI290                                                          
*                                                                               
MODI270  CP    REPEL.PTHCHRS,=P'0'                                              
         BNE   MODI273                                                          
         CP    REPEL.PTHTHRS,=P'0'                                              
         BER   RE                                 BOTH ZEROS, LEAVE             
MODI273  MVC   SUBEL.PTHMOA(L'PTHSLNQ),REPEL.PTHMOA                             
         LTR   R7,R7                                                            
         BZ    MODI275                                                          
         OI    SUBEL.PTHCHRS+L'PTHCHRS-1,X'0D'    NEGATE HOURS                  
         OI    SUBEL.PTHTHRS+L'PTHTHRS-1,X'0D'                                  
MODI275  LA    RF,L'PTHSLNQ(RF)                   BUMP END                      
         SR    R1,R1                                                            
         IC    R1,ELEMENT+1                                                     
         LA    R1,L'PTHSLNQ(R1)                                                 
         STC   R1,ELEMENT+1                                                     
         BR    RE                                                               
*                                                                               
MODI280  MVC   SUBEL.PTHMOA,PTHMOA            APPEND TO ELEMENT                 
         ZAP   DUB,NEWEL.PTHCHRS              SAVE DIFF IN HOURS                
         SP    DUB,PTHCHRS                                                      
         ZAP   SUBEL.PTHCHRS,DUB                                                
         ZAP   DUB,NEWEL.PTHTHRS                                                
         SP    DUB,PTHTHRS                                                      
         ZAP   SUBEL.PTHTHRS,DUB                                                
         LA    RF,L'PTHSLNQ(RF)               BUMP END                          
         SR    R1,R1                                                            
         IC    R1,ELEMENT+1                                                     
         LA    R1,L'PTHSLNQ(R1)                                                 
         STC   R1,ELEMENT+1                                                     
         B     MODI215                                                          
*                                                                               
MODI290  LTR   R0,R0                                                            
         BZ    MODI295                                                          
         BAS   RE,MODI270                                                       
         LA    R6,L'PTHSLNQ(R6)                                                 
         SH    R0,=Y(L'PTHSLNQ)                                                 
         B     MODI290                                                          
*                                                                               
MODI295  LA    RF,ELEMENT                                                       
         CLI   ELEMENT+1,PTHBLNQ             NOTHING TO ADD                     
         BNH   XIT                                                              
         GOTO1 AHELLO,LPARM,(C'P',ACCMST),(0,AIO5),(0,0(RF)),0                  
         CLI   LP4,0                                                            
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R3,NEWEL,SUBEL                                                   
         EJECT                                                                  
*-------------------------------------------------------------                  
* ADD AN ELEMENT TO IO5                                                         
*-------------------------------------------------------------                  
         USING PTHELD,R6                                                        
ADDL2IO5 NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    RF,ELEMENT                                                       
         MVC   ELEMENT(PTHBLNQ),PTHEL     GET ELEMENT HEADER                    
         SR    R1,R1                                                            
         IC    R1,PTHLN                                                         
         SH    R1,=Y(PTHBLNQ)                                                   
         MVI   ELEMENT+1,PTHBLNQ                                                
         LA    RF,ELEMENT+PTHBLNQ                                               
ADDL010  LTR   R1,R1                                                            
         BZ    ADDL020                                                          
         MVC   0(L'PTHSLNQ,RF),PTHMOA                                           
         CP    PTHCHRS,=P'0'                                                    
         BNE   ADDL011                                                          
         CP    PTHTHRS,=P'0'                                                    
         BE    ADDL018                                                          
ADDL011  LTR   R7,R7                      NEGATE HOURS?                         
         BZ    ADDL015                                                          
ADDL013  OI    PTHCHRS-PTHEL+L'PTHCHRS-1(RF),X'0D'                              
         OI    PTHTHRS-PTHEL+L'PTHTHRS-1(RF),X'0D'                              
*                                                                               
ADDL015  LA    RF,L'PTHSLNQ(RF)                                                 
         SR    R2,R2                                                            
         IC    R2,ELEMENT+1                                                     
         LA    R2,L'PTHSLNQ(R2)                                                 
         STC   R2,ELEMENT+1                                                     
ADDL018  LA    R6,L'PTHSLNQ(R6)                                                 
         SH    R1,=Y(L'PTHSLNQ)                                                 
         B     ADDL010                                                          
*                                                                               
ADDL020  LA    RF,ELEMENT                                                       
         CLI   ELEMENT+1,PTHBLNQ                                                
         BE    XIT                                                              
         GOTO1 AHELLO,LPARM,(C'P',ACCMST),(0,AIO5),(0,0(RF)),0                  
         CLI   LP4,0                                                            
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT RECORD CHANGES                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING PLD,R6                                                           
         USING BIGPRNTD,R7                                                      
         USING TTHRECD,R3                                                       
PRNT     NTR1  ,                                                                
         NI    PRNSW,ALL-PRNACT    TURNOFF PRINT ACTIVITY                       
         LA    R6,XP                                                            
         L     R3,APIO             R3=NEW/CHANGES                               
         MVC   PLACC,TTHKACT       ACCOUNT CODE                                 
         MVC   PLNME,PERNAM        ACCOUNT NAME                                 
         XC    LASTMOA,LASTMOA                                                  
         ZAP   CHAHRS,=P'0'                                                     
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TTHKTIME         TYPE OF TIME                                 
         MH    R1,=Y(L'TYPEL)                                                   
         LA    R1,TYPEL(R1)                                                     
         MVC   PLTYPE,1(R1)                                                     
         LA    R2,TTHRFST                                                       
         SR    R0,R0                                                            
*                                                                               
PRNT3    CLI   0(R2),PTHELQ        PERIOD ELEMENT                               
         BE    *+12                                                             
         CLI   0(R2),PTHELQA                                                    
         BNE   PRNT9                                                            
         TM    PRNSW,PRNPER        PRINT PERIOD DATA                            
         BNO   PRNT9                                                            
*                                                                               
         USING PTHELD,R4                                                        
         LR    R4,R2                                                            
         CLC   PTHMOA,MOASTR       TEST CURRENT PERIOD                          
         BL    PRNT9                                                            
         CLC   PTHMOA,MOAEND                                                    
         BH    PRNT9                                                            
         SR    R5,R5                                                            
         IC    R5,PTHLN                                                         
         SH    R5,=Y(PTHBLNQ)                                                   
         GOTO1 DATCON,DMCB,(1,PTHEDT),(8,PLDATE)                                
         CLI   0(R2),PTHELQA                                                    
         BNE   *+10                                                             
         MVC   PLTHRS,=CL10'**ADJSTD**'                                         
*                                                                               
PRNT5    CP    PTHCHRS,=P'0'                                                    
         BNE   PRNT7                                                            
         CP    PTHTHRS,=P'0'                                                    
         BE    PRNT8                                                            
*                                                                               
PRNT7    MVC   THISMOA,PTHMOA                                                   
         MVC   WORK(2),PTHMOA                                                   
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,PLFMOA)                                  
         EDIT  PTHCHRS,(10,PLFHRS),2,MINUS=YES                                  
         LA    R3,PLCHRS                                                        
*                                                                               
         AP    CHAHRS,PTHTHRS                                                   
         EDIT  PTHTHRS,(10,0(R3)),2,MINUS=YES                                   
         BAS   RE,ACRPT            PRINT IT                                     
         OI    PRNSW,PRNACT                                                     
*                                                                               
PRNT8    LA    R4,L'PTHSLNQ(R4)                                                 
         SH    R5,=Y(L'PTHSLNQ)                                                 
         BNZ   PRNT5                                                            
*                                                                               
PRNT9    MVC   PLDATE(PLR-PLDATE),XSPACES                                       
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   PRNT3                                                            
*                                                                               
         CP    CHAHRS,=P'0'        LAST MOA                                     
         BE    PRNT11                                                           
         MVI   LASTMOA,X'FF'                                                    
         OI    PRNSW,PRNACT                                                     
*                                                                               
PRNT11   MVC   XP,XSPACES                                                       
         TM    PRNSW,PRNACT                                                     
         BNO   XIT                                                              
*                                                                               
         USING BOXD,R4                                                          
         L     R4,ADBOX            DRAW A BOX                                   
         MVC   BOXROWS,XSPACES                                                  
         SR    RF,RF                                                            
         IC    RF,LINE                                                          
         LA    RF,BOXROWS-1(RF)                                                 
         MVI   0(RF),C'M'                                                       
         MVI   BOXINIT,0                                                        
         BAS   RE,ACRPT                                                         
         B     XIT                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ADD TIME ELEMENT TO RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
ADDL     NTR1  ,                                                                
         BAS   RE,MAXEL            TEST MAXIMUM LENGTH                          
         LR    R3,RF                                                            
         USING TTHRECD,R3                                                       
*                                                                               
ADDL5    GOTO1 AHELLO,LPARM,(C'P',ACCMST),(R3),(R4),0                           
         CLI   LP4,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
MAXEL    NTR1  ,                                                                
         LR    R3,RF                                                            
MAXEL1   SR    RE,RE               TEST RECORD LENGTH                           
         ICM   RE,3,TTHRLEN                                                     
         CH    RE,=Y(MAXLN)                                                     
         BL    XIT                                                              
         GOTO1 =A(DELZERO),(R3)    DELETE ZERO HOURS                            
         SR    RE,RE                                                            
         ICM   RE,3,TTHRLEN                                                     
         CH    RE,=Y(MAXLN)                                                     
         BL    XIT                                                              
         MVI   TTHRFST,X'FF'       DELETE OLDEST                                
         GOTO1 AHELLO,LPARM,(C'D',ACCMST),(X'FF',(R3)),0,0                      
         CLI   LP4,0                                                            
         BE    MAXEL1                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE ZERO HOURS                                                   *         
***********************************************************************         
DELZERO  NTR1                                                                   
         L     RE,AIO6             CLEAR WORKING AREA                           
         LA    RF,RLNMX                                                         
         XCEF                                                                   
*                                                                               
         USING TTHRECD,R3                                                       
DZ1      USING TTHRECD,RF                                                       
         LR    R3,R1               ADDRESS OF RECORD                            
         L     RF,AIO6                                                          
         MVC   DZ1.TTHKEY(TTHRFST-TTHKEY),TTHKEY                                
         LA    R4,TTHRFST          POINT TO FIRST ELEMENT                       
         LA    R5,DZ1.TTHRFST                                                   
         SR    R0,R0                                                            
*                                                                               
         USING PTHELD,R4                                                        
DZ2      USING PTHELD,R5                                                        
DELZRO10 CLI   PTHEL,0             END OF RECORD?                               
         BE    DELZRO80            YES, CALCULATE LENGTH, FINISHED              
         CLI   PTHEL,PTHELQ                                                     
         BE    DELZRO20                                                         
         CLI   PTHEL,PTHELQA                                                    
         BE    DELZRO20                                                         
DELZRO15 IC    R0,PTHLN                                                         
         AR    R4,R0                                                            
         B     DELZRO10                                                         
*                                                                               
DELZRO20 MVC   DZ2.PTHEL(PTHBLNQ),PTHEL                                         
         LR    R6,R4               SAVE POSITION                                
         LR    RE,R5                                                            
         SR    R1,R1                                                            
         IC    R1,PTHLN                                                         
         SH    R1,=Y(PTHBLNQ)                                                   
         LR    R2,R1               NEW ELEMENT LENGTH, MOA'S ONLY               
DELZRO25 CP    PTHCHRS,=P'0'                                                    
         BNE   DELZRO30                                                         
         CP    PTHTHRS,=P'0'                                                    
         BNE   DELZRO30                                                         
         SH    R2,=Y(L'PTHSLNQ)    REMOVE ONE MOA                               
         LA    R4,L'PTHSLNQ(R4)                                                 
         B     DELZRO50                                                         
*                                                                               
DELZRO30 MVC   DZ2.PTHMOA,PTHMOA   COPY OVER MOA AND HOURS                      
         MVC   DZ2.PTHCHRS,PTHCHRS                                              
         MVC   DZ2.PTHTHRS,PTHTHRS                                              
         LA    R4,L'PTHSLNQ(R4)                                                 
         LA    R5,L'PTHSLNQ(R5)                                                 
*                                                                               
DELZRO50 SH    R1,=Y(L'PTHSLNQ)                                                 
         BNZ   DELZRO25                                                         
         LR    R4,R6                                                            
         LR    R5,RE                                                            
         LTR   R2,R2               ANY MOA/HOURS COPIED?                        
         BNZ   DELZRO55                                                         
         XC    DZ2.PTHEL(PTHBLNQ),DZ2.PTHEL                                     
         B     DELZRO15                                                         
*                                                                               
DELZRO55 LA    R2,PTHBLNQ(R2)                                                   
         STC   R2,DZ2.PTHLN                                                     
         AR    R5,R2               BUMP TO NEXT POSITION                        
         B     DELZRO15                                                         
*                                                                               
DELZRO80 MVI   0(R5),0             MARK EOR ON NEW RECORD                       
         LA    R5,1(R5)                                                         
         SR    R5,RF                                                            
         STCM  R5,3,DZ1.TTHRLEN    SAVE NEW LENGTH                              
*                                                                               
         LR    R0,R3               COPY NEW RECORD ON TOP OF OLD ONE            
         LA    R1,RLNMX                                                         
         L     RE,AIO6                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EXTRACT COMPANY OPTIONS                                             *         
***********************************************************************         
         SPACE                                                                  
COMP     NTR1  ,                                                                
         MVC   ACURRMOA,AMOATAB                                                 
         MVI   FORCEHED,C'Y'                                                    
         MVI   CMPSW,0                                                          
         ZAP   DMPCNT,=P'0'        RESET DUMP COUNT                             
         LA    R2,DKEY                                                          
         USING CPYRECD,R2                                                       
         MVC   CPYKEY,SPACES       GET COMPANY RECORD                           
         MVC   CPYKCPY,COMPANY                                                  
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         BAS   RE,DMGETR                                                        
         BAS   RE,GETNM            GET COMPANY NAME                             
         MVC   CMPNME,WORK                                                      
         LA    R3,CPYRFST                                                       
         SR    R0,R0                                                            
*                                                                               
COMP3    IC    R0,1(R3)                                                         
         CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),CPYELQ                                                     
         BE    *+10                                                             
         AR    R3,R0                                                            
         B     COMP3                                                            
*                                                                               
         USING CPYELD,R3                                                        
         TM    CPYSTAT5,CPYSNCST   NEW COST                                     
         BNO   *+8                                                              
         OI    CMPSW,CMPNC         SET NEW METHOD                               
         TM    CPYSTAT7,CPYSTMSY                                                
         BNO   *+8                                                              
         OI    CMPSW,CMPTMS        TMS IN USE                                   
*                                                                               
         MVC   CMPFINYR,CPYSFST    START OF FISCAL YEAR                         
         CLI   CMPFINYR,0                                                       
         BZ    COMP5                                                            
         NI    CMPFINYR,X'0F'                                                   
         CLI   CPYSFST,X'F0'       JAN-SEP                                      
         BH    COMP5                                                            
         SR    RE,RE                                                            
         IC    RE,CMPFINYR         OCT-DEC, ADD 9                               
         LA    RE,9(RE)                                                         
         STC   RE,CMPFINYR                                                      
*                                                                               
COMP5    BAS   RE,LDGR             GET LEDGER RECORD                            
         BAS   RE,CLNDR            BUILD CALENDER TABLE                         
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET LEDGER RECORD                                                   *         
***********************************************************************         
         SPACE                                                                  
LDGR     NTR1  ,                                                                
         LA    R2,DKEY                                                          
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACES       BUILD LEDGER RECORD KEY                      
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),=C'1R'                                                
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR            TEST ANY 1R LEDGER                           
         BNE   XIT                                                              
         L     R2,AIO3                                                          
         BAS   RE,DMGETR           GET 1R LEDGER RECORD                         
*                                                                               
         XC    LVLS1R,LVLS1R                                                    
         LA    R4,LVLS1R                                                        
         LA    R3,LDGRFST                                                       
         SR    R0,R0                                                            
*                                                                               
LDGR3    CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),ACLELQ        LENGTHS ELEMENT                              
         BE    LDGR5                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     LDGR3                                                            
*                                                                               
         USING ACLELD,R3                                                        
LDGR5    LA    R5,ACLVALS                                                       
         LA    R0,4                                                             
         MVC   0(1,R4),0(R5)       SAVE THE LENGTHS                             
         LA    R4,1(R4)                                                         
         LA    R5,L'ACLVALS(R5)                                                 
         BCT   R0,*-14                                                          
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD LIST OF WEEK ENDING DATES                                     *         
***********************************************************************         
         SPACE 1                                                                
WKND     NTR1  ,                   BACK UP 10 YEARS                             
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         MVC   WORK(6),TODAY                                                    
         MVC   WORK+2(4),=C'0107'                                               
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-10                                
*                                  GET SATURDAY START                           
WKND3    GOTO1 GETDAY,DMCB,WORK+6,WORK+12                                       
         CLC   WORK+12(3),=C'SAT'                                               
         BE    WKND5                                                            
         MVC   WORK(6),WORK+6                                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,-1                                        
         B     WKND3                                                            
*                                                                               
WKND5    LA    R2,STRDTE                                                        
         LA    R0,NWKNDQ           NUMBER OF WEEKS                              
WKND7    GOTO1 DATCON,DMCB,(0,WORK+6),(1,0(R2))                                 
         MVC   WORK(6),WORK+6                                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,7                                         
         LA    R2,L'STRDTE(R2)                                                  
         BCT   R0,WKND7                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ CALENDAR RECORD - BUILD WEEK ENDING TABLE                      *         
***********************************************************************         
         SPACE 1                                                                
CLNDR    NTR1  ,                                                                
         NI    CMPSW,X'FF'-CMPCAL                                               
         L     R0,ACLNTAB          CLEAR CALENDAR                               
         L     R1,=AL4(CLNMX*CLNLNQ)                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R2,DKEY                                                          
         USING CASRECD,R2                                                       
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,COMPANY     COMPANY                                      
         BAS   RE,DMHGH                                                         
         CLC   DKEY(CASKEMOA-CASKEY),DIR                                        
         BNE   XIT                                                              
*                                                                               
CLNDR0   OI    CMPSW,CMPCAL        COMPANY HAS CALENDAR                         
         L     R2,AIO3                                                          
         BAS   RE,DMGETR           GET THE CALENDAR RECORD                      
         BAS   RE,ADDMOA           ADD CURRENT CALENDAR TO TABLE                
*                                                                               
         L     R5,ACLNTAB                                                       
         A     R5,=AL4((CLNMX-1)*CLNLNQ)  R5=LAST ENTRY(CORPORATE)              
         CLI   CASKOFC,C' '        TEST CORPORATE CALENDAR                      
         BE    CLNDR2                                                           
         LA    R0,CLNMX                                                         
         L     R5,ACLNTAB                                                       
*                                                                               
         USING CLND,R5                                                          
CLNDR1   CLC   CLNOFC,CASKOFC      TABLE OFFICE TO CALENDAR RECORD              
         BE    CLNDR2                                                           
         CLI   CLNOFC,0            NEW ENTRY                                    
         BE    CLNDR2                                                           
         A     R5,=A(CLNLNQ)       R5=NEXT CALENDAR ENTRY                       
         BCT   R0,CLNDR1                                                        
         DC    H'0'                TOO MANY CALENDARS                           
*                                                                               
CLNDR2   MVC   CLNOFC,CASKOFC      SAVE OFFICE                                  
         SR    R3,R3               R3 = NUMBER OF TABLE ENTRIES                 
         LA    R6,CLNDTE           R6 = START/END DATES                         
         ICM   R3,3,CLNNUM         NUMBER IN TABLE                              
         LR    RF,R3                                                            
         MH    RF,=Y(L'CLNWK)      NUMBER X LENGTH                              
         AR    R6,RF               R6=NEXT START/END ENTRY                      
*                                                                               
CLNDR3   LA    R4,CASRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING TMPELD,R4                                                        
CLNDR5   CLI   0(R4),TMPELQ        X'88' ELEMENT                                
         BE    CLNDR9                                                           
         CLI   0(R4),0                                                          
         BE    CLNDR15             EOR                                          
CLNDR7   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     CLNDR5                                                           
*                                                                               
CLNDR9   CLC   TMPSTART,STRDTE     TEST BEFORE START                            
         BL    CLNDR7                                                           
         CLC   TMPEND,ENDDTE       OR AFTER END                                 
         BH    CLNDR7                                                           
         MVC   0(3,R6),TMPSTART                                                 
         MVC   3(3,R6),TMPEND                                                   
         LA    R6,L'CLNWK(R6)      BUMP TO NEXT AVAILABLE ENTRY                 
         LA    R3,1(R3)            BUMP COUNT                                   
         STCM  R3,3,CLNNUM                                                      
         CH    R3,=Y(NCLNWQ)                                                    
         BL    CLNDR7                                                           
         B     XIT                 CALENDAR TABLE FULL                          
*                                                                               
CLNDR15  BAS   RE,DMSEQ            GET NEXT CALENDAR RECORD                     
         CLC   DKEY(CASKEMOA-CASKEY),DIR                                        
         BE    CLNDR0                                                           
         B     XIT                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* FIND CALENDER'S START/END MOA BASED ON PERIOD END DATE              *         
***********************************************************************         
         USING CASRECD,R2                                                       
         USING TMRELD,R3                                                        
         USING CALMD,R4                                                         
ADDMOA   NTR1                                                                   
         LA    R3,CASRFST                                                       
ADDMOA10 CLI   TMREL,0             EOR?                                         
         BE    XIT                                                              
         CLI   TMREL,TMRELQ                                                     
         BE    ADDMOA20                                                         
         SR    R0,R0               NEXT ELEMENT                                 
         IC    R0,TMRLN                                                         
         AR    R3,R0                                                            
         B     ADDMOA10                                                         
*                                                                               
ADDMOA20 L     R4,ACURRMOA                                                      
         MVC   CALMPSTR,TMRSTART   PUT IT INTO TABLE                            
         MVC   CALMPEND,TMREND                                                  
         MVC   CALMPOFF,CASKOFC                                                 
         MVC   CALMSMOA,CASKSMOA                                                
         MVC   CALMNMOA,CASKEMOA                                                
         LA    R4,CALMLNQ(R4)                                                   
         ST    R4,ACURRMOA                                                      
         MVI   0(R4),X'FF'         MARK EOT                                     
         B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* GET PERSON ACCOUNT NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
GETPER   NTR1  ,                                                                
         USING TTHRECD,R3                                                       
         MVC   DKEY,SPACES                                                      
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKCULA,TTHKCULA   VERIFY ACCOUNT RECORD                        
         BAS   RE,DMRD                                                          
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         BAS   RE,DMGETR           GET THE RECORD                               
*                                                                               
         BAS   RE,GETNM            GET ACCOUNT NAME                             
         MVC   PERNAM,WORK                                                      
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET NAME FROM RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
GETNM    NTR1  ,                                                                
         MVC   WORK,SPACES                                                      
         L     R2,AIO3                                                          
         LA    R3,ACTRFST-ACTRECD(R2)                                           
         SR    R1,R1                                                            
*                                                                               
GETNM3   IC    R1,1(R3)                                                         
         CLI   0(R3),0             EOR                                          
         BE    XIT                                                              
         CLI   0(R3),NAMELQ        NAME ELEMENT                                 
         BE    *+10                                                             
         AR    R3,R1                                                            
         B     GETNM3                                                           
*                                                                               
         USING NAMELD,R3                                                        
         IC    R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   WORK(0),NAMEREC                                                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SET UP HEADLINES AND GO TO ACREPORT                                 *         
***********************************************************************         
         SPACE 1                                                                
ACRPT    NTR1  ,                                                                
         MVC   XHEAD3+11(L'CMPNME),CMPNME                                       
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DUMP INPUT RECORDS                                                  *         
***********************************************************************         
         SPACE 1                                                                
DMPOLD   NTR1  ,                                                                
         CP    DMPCNT,DMPMAX       TEST MAXIMUM DUMPS                           
         BH    XIT                                                              
         L     R3,AIO1                                                          
         USING TTHRECD,R3                                                       
         LA    R1,PRNDMP                                                        
         CLI   TTHKTIME,TTHKTWKL   TEST PERIOD RECORD                           
         BE    *+8                                                              
         LA    R1,PRNDMC                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PRNSW,0             WANT DUMP OF RECORDS?                        
         BNO   XIT                                                              
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'132'                                                 
*                                                                               
         LA    R2,=CL6'INPUT'                                                   
         LA    R5,=C'2D'                                                        
         SR    R4,R4                                                            
         ICM   R4,3,TTHRLEN                                                     
         GOTO1 ,DMCB,(6,(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)               
         GOTO1 PRNTBL                                                           
         L     R4,ADBOX                                                         
         MVC   BOXWIDTH,=F'198'                                                 
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DUMP MODIFICATIONS                                                  *         
***********************************************************************         
         SPACE 1                                                                
DMPMOD   NTR1  ,                                                                
         CP    DMPCNT,DMPMAX       TEST MAXIMUM DUMPS                           
         BH    XIT                                                              
         L     R3,AIO5                                                          
         USING TTHRECD,R3                                                       
         LA    R1,PRNDMP                                                        
         CLI   TTHKTIME,TTHKTWKL   TEST PERIOD RECORD                           
         BE    *+8                                                              
         LA    R1,PRNDMC                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PRNSW,0             WANT DUMP OF RECORDS?                        
         BNO   XIT                                                              
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'132'                                                 
*                                                                               
         LA    R2,=CL6'CHANGES'                                                 
         LA    R5,=C'2D'                                                        
         SR    R4,R4                                                            
         ICM   R4,3,TTHRLEN                                                     
         GOTO1 ,DMCB,(6,(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)               
         GOTO1 PRNTBL                                                           
         L     R4,ADBOX                                                         
         MVC   BOXWIDTH,=F'198'                                                 
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DUMP OUTPUT RECORDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
DMPNEW   NTR1  ,                                                                
         CP    DMPCNT,DMPMAX       TEST MAXIMUM DUMPS                           
         BH    XIT                                                              
         L     R3,AIO2                                                          
         USING TTHRECD,R3                                                       
         LA    R1,PRNDMP                                                        
         CLI   TTHKTIME,TTHKTWKL   TEST PERIOD RECORD                           
         BE    *+8                                                              
         LA    R1,PRNDMC                                                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PRNSW,0             WANT DUMP OF RECORDS?                        
         BNO   XIT                                                              
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'132'                                                 
*                                                                               
         AP    DMPCNT,=P'1'                                                     
         LA    R2,=CL6'OUTPUT'                                                  
         CLI   ACTION,CHA                                                       
         BE    *+8                                                              
         LA    R2,=CL6'ADD'                                                     
         LA    R5,=C'2D'                                                        
         SR    R4,R4                                                            
         ICM   R4,3,TTHRLEN                                                     
         TM    PRCSW,PRCTAP        OUTPUT TAPE                                  
         BNO   *+12                                                             
         SH    R3,=H'4'                                                         
         ICM   R4,3,0(R3)                                                       
         GOTO1 ,DMCB,(6,(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)               
         GOTO1 PRNTBL                                                           
         L     R4,ADBOX                                                         
         MVC   BOXWIDTH,=F'198'                                                 
         B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
DMRD     LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',GETREC),ACCMST,DA,(R2),DMWORK                
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R2),DMWORK                        
         B     DMERR                                                            
*                                                                               
DMPUTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R2),DMWORK                        
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
*                                                                               
*ELLO    DC    V(HELLO)            HELLO                                        
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
*                                                                               
AIO1     DC    A(IO1)              IO AREA #1                                   
AIO2     DC    A(IO2)              IO AREA #2                                   
AIO3     DC    A(IO3)              IO AREA #3                                   
AIO4     DC    A(IO4)              IO AREA #4                                   
AIO5     DC    A(IO5)              IO AREA #5                                   
AIO6     DC    A(IO6)              IO AREA #5                                   
*                                                                               
AOUTFIL  DC    A(OUTFIL)           OUTFILE DCB                                  
ATSTAB   DC    A(TSTAB)            TIME SUMMARY TABLE                           
ACLNTAB  DC    A(CLNTAB)           CALENDAR TABLE                               
APRSNTAB DC    A(PRSNTAB)          PERSON TABLE                                 
AMOATAB  DC    A(CMOATAB)          CALENDAR MOA TABLE                           
*                                                                               
ACCFIL   DC    CL8'ACCOUNT '                                                    
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
PERSUL   DC    C'1R'                                                            
COSTUL   DC    C'1C'                                                            
NONCUL   DC    C'1N'                                                            
*                                                                               
SORTCARD DC    C'SORT FIELDS=(004,042,A),FORMAT=BI,WORK=1 '                     
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
*                                                                               
TYPEL    DS    0CL11               TYPES OF TIME                                
         DC    AL1(TTHKTWKL),CL10'PERIOD'                                       
         DC    AL1(TTHKTCLI),CL10'CLIENT'                                       
         DC    AL1(TTHKTTOT),CL10'TOTAL TIME'                                   
         DC    X'FF'                                                            
*                                                                               
CNTS     DS    0XL30               RECORD COUNTS                                
CNTADD   DC    PL5'0',CL25'RECORDS ADDED'                                       
CNTCHA   DC    PL5'0',CL25'RECORDS CHANGED'                                     
CNTOK    DC    PL5'0',CL25'RECORDS UNCHANGED'                                   
         DC    X'FF'                                                            
*                                                                               
*                                                                               
MSG1     DC    C'TIME TOTAL RECORD'                                             
DMPCNT   DC    PL2'0'              DUMP COUNT - ERRORS                          
DMPMAX   DC    PL2'10'             MAX DUMP COUNT                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DCB'S                                                               *         
***********************************************************************         
         SPACE 1                                                                
OUTFIL   DCB   DDNAME=OUTFIL,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
*                                                                               
TMWRK    DCB   DDNAME=TMWRK,DSORG=PS,MACRF=(PM,GM),                    X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760,EODAD=FRCVX                    
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+(PLL-PLD),C'L'                                           
         MVI   BOXCOLS+(PLC1-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC2-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC3-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC4-PLD),C'C'                                          
*                                                                               
         MVI   BOXCOLS+(PLC5-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC6-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC7-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC7-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC8-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC9-PLD),C'C'                                          
         MVI   BOXCOLS+(PLR-PLD),C'R'                                           
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS/TABLES/ETC.                                                 *         
***********************************************************************         
         SPACE 1                                                                
MAXLN    EQU   1900                                                             
RLNMX    EQU   2100+24+4                                                        
         DC    C'**IO1***'                                                      
         DC    F'0'                      IOAREA #1                              
IO1      DC    (RLNMX)X'00'                                                     
*                                                                               
         DC    C'**IO2***'                                                      
         DC    F'0'                      IOAREA #2                              
IO2      DC    (RLNMX)X'00'                                                     
*                                                                               
         DC    C'**IO3***'                                                      
         DC    F'0'                      IOAREA #3                              
IO3      DC    (RLNMX)X'00'                                                     
*                                                                               
         DC    C'**IO4***'                                                      
         DC    F'0'                      IOAREA #4                              
IO4      DC    (RLNMX)X'00'                                                     
*                                                                               
         DC    C'**IO5***'                                                      
         DC    F'0'                      IOAREA #5                              
IO5      DC    (RLNMX)X'00'                                                     
*                                                                               
         DC    C'**IO6***'                                                      
         DC    F'0'                      IOAREA #5                              
IO6      DC    (RLNMX)X'00'                                                     
*                                                                               
CMOAMAX  EQU   50                                                               
         DS    0D                                                               
         DC    CL8'*CMOATAB*'                                                   
CMOATAB  DS    (CMOAMAX*CALMLNQ)C                                               
*                                                                               
         DS    0D                                                               
TSNMX    EQU   2000                                                             
TABLEN   EQU   TSNMX*TSLNQ                                                      
         DS    0D                        PERIOD TABLE                           
         DC    CL8'**TSTAB**'                                                   
TSTAB    DS    (TABLEN)C                                                        
*                                                                               
CLNMX    EQU   20                  MAXIMUM NUMBER OF CALENDARS                  
CLNSIZE  EQU   (CLNMX*CLNLNQ)                                                   
         DS    0D                                                               
         DC    CL8'**CLNDR*'                                                    
CLNTAB   DS    (CLNSIZE)X                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'*PRSNTAB'                                                    
PRSNTAB  DS    30000C                                                           
         DS    30000C                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACTUD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
AHELLO   DS    A                   ADDRESS OF HELLO FROM MONACC                 
NUMTAB   DS    F                   NUMBER IN TABLE                              
*                                                                               
MOASTR   DS    XL2                 START MOA                                    
MOAEND   DS    XL2                 END MOA                                      
*                                                                               
TODAY    DS    CL6                 TODAY DATE                                   
TODAYC   DS    XL2                 TODAY COMPRESSED                             
PRYRS    DS    CL6                 TWO YEARS AGO DATE                           
PRYRSP   DS    PL3                 TWO YEARS AGO PACKED                         
PL8      DS    PL8                                                              
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
PRGEKEY  DS    CL(L'ACCKEY)                                                     
PRGECHG  DS    XL1                                                              
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
APIO     DS    AL4                 A(RECORD TO PRINT)                           
ACURRMOA DS    A                                                                
*                                                                               
ALL      EQU   X'FF'                                                            
PRCSW    DS    XL1                 PROCESS CONTROL SWITCH                       
PRCRCV   EQU   X'80'               RECOVERY FILE                                
PRCACC   EQU   X'40'               ACCOUNT FILE                                 
PRCBOTH  EQU   PRCRCV+PRCACC       RECOVERY + ACCOUNT                           
PRCTAP   EQU   X'20'               CREATE OUTPUT TAPE                           
PRCACT   EQU   X'08'               ACTIVITY SWITCH                              
PRCREV   EQU   X'04'               REVERSE TODAYS POSTINGS                      
PRCUPDT  EQU   X'02'               UPDATE MODE (PARM=UPDATE)                    
PRCSPCL  EQU   X'01'               SPECIAL                                      
*                                                                               
FLAG     DS    XL1                                                              
REVTODAY EQU   X'80'               REMOVE TODAYS ACTIVITY                       
*                                                                               
LNQAC    DS    XL1                 LENGTH OF ACCOUNT FILTER                     
*                                                                               
TYPSW    DS    XL1                 TYPE OF TIME RECORD                          
TYPTRN   EQU   X'80'               TRANSACTION                                  
TYPTIM   EQU   X'40'               TIME RECORD                                  
TYPCPY   EQU   X'08'               COPY                                         
TYPCHA   EQU   X'04'               CHANGE                                       
TYPADD   EQU   X'02'               ADD                                          
*                                                                               
PRNSW    DS    XL1                 PRINT CONTROL                                
PRNPER   EQU   X'80'               PRINT PERIOD RECORDS                         
PRNCLI   EQU   X'40'               PRINT CLIENT RECORDS                         
PRNDMP   EQU   X'20'               DUMP PERSON RECORDS                          
PRNDMC   EQU   X'10'               DUMP CLIENT RECORDS                          
PRNACT   EQU   X'08'               THERE HAS BEEN SOME PRINT ACTIVITY           
PRNCHA   EQU   X'04'               PRINT ONLY THE CHANGES                       
*                                                                               
ELEMENT  DS    XL255                                                            
COPYELEM DS    XL255                                                            
MOADEL   DS    XL1                                                              
*                                                                               
COMPANY  DS    CL1                 COMPANY CODE                                 
CMPSW    DS    XL1                 COMPANY OPTIONS                              
CMPNC    EQU   X'80'               ON NEW COST                                  
CMPTMS   EQU   X'40'               ON TMS                                       
CMPCAL   EQU   X'08'               COMPANY HAS CALENDAR                         
CMPNME   DS    CL36                COMPANY NAME                                 
CMPFINYR DS    XL1                 START OF FINANCIAL YEAR                      
PERACC   DS    CL15                ACCOUNT CODE                                 
PERNAM   DS    CL36                ACCOUNT NAME                                 
LVLS1R   DS    XL4                 LEVELS OF 1R                                 
OFC      DS    CL2                 OFFICE CODE                                  
*                                                                               
LPARM    DS    0F                  PARM LIST FOR HELLO                          
LP1      DS    F                   ACTION, A(FILE)                              
LP2      DS    F                   ELEMENT CODE, A(RECORD)                      
LP3      DS    F                   ARG. LEN, A(ELEMENT)                         
LP4      DS    F                   COMPLETION CODE, NEW LENGTH                  
LP5      DS    F                                                                
*                                                                               
ACTION   DS    CL1                 RECORD ACTION                                
ADD      EQU   C'A'                                                             
CHA      EQU   C'C'                                                             
*                                                                               
RECSW    DS    XL1                                                              
RECCHA   EQU   X'80'               RECORD CHANGED                               
KEEPELEM DS    CL1                                                              
*                                                                               
TS       DS    XL(TSLNQ)           TIME SUMMARY RECORD                          
*                                                                               
LASTMOA  DS    XL2                 LAST MONTH                                   
CHAHRS   DS    PL8                 NET HOURS CHANGED FOR MONTH                  
THISMOA  DS    XL2                 CURRENT MONTH                                
*                                                                               
NWKNDQ   EQU   683                                                              
STRDTE   DS    0XL3                WEEK ENDING DATES                            
         DS    XL(NWKNDQ*L'STRDTE)                                              
         ORG   *-L'STRDTE                                                       
ENDDTE   DS    XL(L'STRDTE)                                                     
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER REPORT PRINT LINE                                    *         
***********************************************************************         
         SPACE 1                                                                
PLD      DSECT                                                                  
PLL      DS    XL1                 LEFT COLUMN                                  
PLACC    DS    CL12                ACCOUNT CODE                                 
PLC1     DS    XL1                                                              
PLNME    DS    CL36                PERSON'S NAME                                
PLC2     DS    XL1                                                              
*                                                                               
PLTYPE   DS    CL10                TYPE OF DATA                                 
PLC3     DS    XL1                                                              
PLDATE   DS    CL9                 MMMDD/YY                                     
PLC4     DS    XL1                                                              
PLC5     DS    XL1                                                              
         DS    XL1                                                              
PLFMOA   DS    CL7                 MMM/YY                                       
PLC6     DS    XL1                                                              
PLFHRS   DS    CL10                FIRST HOURS                                  
PLC7     DS    XL1                                                              
PLCHRS   DS    CL10                CURRENT HOURS                                
PLC8     DS    XL1                                                              
PLC9     DS    XL1                                                              
PLPHRS   DS    CL10                PREVIOUS HOURS                               
         ORG   PLPHRS                                                           
PLTHRS   DS    CL10                TOTAL                                        
PLR      DS    XL1                                                              
         ORG   PLD+163                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR CALENDAR MOA ENTRIES                                      *         
***********************************************************************         
CALMD    DSECT                                                                  
CALMPSTR DS    PL3                 PERIOD START                                 
CALMPEND DS    PL3                 PERIOD END                                   
CALMPOFF DS    CL2                 OFFICE                                       
CALMSMOA DS    PL2                 START MOA                                    
CALMNMOA DS    PL2                 END MOA                                      
CALMLNQ  EQU   *-CALMD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT FOR TIME SUMMARY ENTRY                                        *         
***********************************************************************         
         SPACE 1                                                                
TSD      DSECT                                                                  
TSNMOA   DS    PL2                 ENDING MOA                                   
TSSMOA   DS    PL2                 START MOA                                    
TSTYPE   DS    XL1                 TYPE                                         
TSADJSTD EQU   X'80'               ADJUSTED                                     
TSPEDT   DS    XL3                 PERIOD END DATE                              
TSMOA    DS    XL2                 MOA                                          
TSKLNQ   EQU   *-TSD                                                            
TSTHRS   DS    PL5                 TOTAL HOURS                                  
TSCLTH   DS    PL5                 CLIENT HOURS                                 
TSLNQ    EQU   *-TSD                                                            
         EJECT                                                                  
***********************************************************************         
* DSECT FOR CALENDAR TABLE                                            *         
***********************************************************************         
         SPACE 1                                                                
NCLNWQ   EQU   2400                CALENDAR WEEKS                               
*                                                                               
CLND     DSECT                                                                  
CLNOFC   DS    CL2                 OFFICE OR SPACES FOR CORPORATE               
CLNNUM   DS    XL2                 NUMBER IN TABLE                              
CLNWK    DS    0CL6                WEEK ENTRY                                   
CLNDTE   DS    XL(NCLNWQ*L'CLNWK)                                               
CLNLNQ   EQU   *-CLND                                                           
         EJECT                                                                  
       ++INCLUDE ACRCVRECD                                                      
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPTU02 12/11/09'                                      
         END                                                                    
