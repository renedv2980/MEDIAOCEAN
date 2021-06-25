*          DATA SET ACREPE102  AT LEVEL 029 AS OF 05/01/02                      
*PHASE ACE102A,+0                                                               
*INCLUDE ACCEDIT                                                                
*INCLUDE ACLIST                                                                 
*INCLUDE CHOPCON                                                                
*INCLUDE SORTER                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'INTERAGENCY ESTIMATE STATEMENT'                                 
*----------------------------------------------------------------*              
*        INTERAGENCY ESTIMATE STATEMENT                          *              
*----------------------------------------------------------------*              
*                                                                               
*        REGISTER USAGE - RC - LOCAL WORKING STORAGE                            
*                         RB - FIRST BASE REGISTER                              
*                         RA - GLOBAL WORK STORAGE                              
*                         R9 - 2ND BASE REGISTER                                
*                         R8 - PRINT DSECT                                      
*                                                                               
ACE102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACE1**,R9       R9=2ND BASE REGISTER                         
         L     RA,0(R1)                                                         
*                                                                               
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
*                                                                               
         LA    RC,SPACEND                                                       
*                                                                               
         USING LWSD,RC             RC=A(SAVE W/S)                               
*                                                                               
         L     R8,VBIGPRNT                                                      
*                                                                               
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,REQFRST        REQ FIRST                                    
         BNE   EXIT                                                             
         BAS   RE,REQF                                                          
         BAS   RE,MAIN                                                          
         BAS   RE,REQL                                                          
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        INITIALIZE ROUTINE                                      *              
*----------------------------------------------------------------*              
*                                                                               
RUNF     DS    0H                                                               
         LA    RE,VTYPES           MOVE VTYPES TO W/S                           
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R2                                                      
*                                                                               
         L     R2,ADMASTD                                                       
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        INITIALIZE SORTER / PROLLER                             *              
*----------------------------------------------------------------*              
*                                                                               
REQF     NTR1                                                                   
         XC    ALSORT,ALSORT       INIT SORT RECORD ADDRESS                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'       NEW PAGE FOR EACH REQUEST                    
*                                                                               
         BAS   RE,BLDSRT           BUILD SORTCARD                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
         GOTO1 PROLLER,DMCB,DEFINE,ACCUMS,MAXROW,MAXCOL                         
         GOTO1 DATCON,DMCB,(0,QSTART),(1,STARTDT)                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,ENDDT)                                   
*                                                                               
         LA    R3,TOTTBTOT         NUMBER ENTRIES IN TABLE                      
         LA    R4,TOTTB                                                         
REQF1    CLC   QOPT1(1),0(R4)                                                   
         BE    REQF2                                                            
         LA    R4,TOTTBLNQ(R4)                                                  
         BCT   R3,REQF1                                                         
         DC    H'0'                                                             
*                                                                               
REQF2    LA    R4,1(R4)            BUMP TO START OF LIST                        
         ST    R4,TOTLIST          SAVE ADDRESS OF CORRECT LIST                 
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        BUILD SORTCARD                                          *              
*----------------------------------------------------------------*              
*                                                                               
BLDSRT   NTR1                                                                   
         LA    R5,OPTTB            INPUT OPTIONS                                
         LA    R3,OPTTBTOT         NUMBER OF TIMES TO LOOP                      
*                                                                               
BLDSRT1  DS    0H                                                               
         CLC   QOPT1(1),0(R5)      FIND CORRECT LINE TO MOVE IN                 
         BE    BLDSRT2                                                          
         LA    R5,OPTTBLNQ(R5)     BUMP TO NEXT LINE                            
         BCT   R3,BLDSRT1                                                       
         DC    H'0'                                                             
*                                                                               
BLDSRT2  DS    0H                                                               
         MVC   SORTCARD(13),=C'SORT FIELDS=('                                   
         LA    RF,SORTCARD+13                                                   
*                                                                               
BLDSRT3  LA    R3,SRTTBTOT         NUMBER OF TIMES TO LOOP                      
*                                                                               
BLDSRT4  DS    0H                                                               
         LA    R5,1(R5)            R5 POINTS AT WHAT OPTION IS WANTED           
         CLI   0(R5),X'FF'         END OF THE LINE                              
         BE    BLDSRT7                                                          
         LA    R1,SRTTB            R1 POINTS TO TRANSLATION TABLE               
*                                                                               
BLDSRT5  DS    0H                                                               
         CLC   0(1,R5),0(R1)                                                    
         BE    BLDSRT6                                                          
         LA    R1,SRTTBLNQ(R1)                                                  
         BCT   R3,BLDSRT5                                                       
*                                                                               
BLDSRT6  MVC   0(SRTTBLNQ-1,RF),1(R1)                                           
         MVC   7(1,RF),=C','                                                    
         LA    RF,8(RF)                                                         
         B     BLDSRT3                                                          
*                                                                               
BLDSRT7  DS    0H                                                               
         MVC   0(25,RF),=C'86,2,A),FORMAT=BI,WORK=1 '                           
         LA    R1,SORTCARD                                                      
         GOTO1 VSQUASHR,DMCB,SORTCARD,80                                        
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        READ INTERAGENCY RECS - PUT TO SORTER                   *              
*----------------------------------------------------------------*              
*                                                                               
         USING ACINKEY,R5                                                       
*                                                                               
MAIN     NTR1                                                                   
         XC    RKEY,RKEY                                                        
         LA    R5,RKEY                                                          
         MVC   ACINKEY,SPACES                                                   
         MVI   ACINCOD,ACINEQU     X'2D' RECORD TYPE                            
         MVI   ACINSREC,ACINSEQU   X'03' SUB RECORD                             
         MVC   ACINCOMP,QCOMPANY                                                
         MVC   ACINUL,QUNIT        U/L                                          
         MVC   ACINACC,QACCOUNT    ACCOUNT                                      
*                                                                               
         LA    R2,ACINACC-ACINKEY  DEFAULT TO LENGTH OF FRONT OF KEY            
         CLC   QACCOUNT,SPACES                                                  
         BE    MAIN1                                                            
         LA    R1,QACCOUNT+(L'QACCOUNT-1)                                       
         LA    R0,(L'QACCOUNT)                                                  
         CLI   0(R1),C' '          SCAN BACKWARDS FOR FIRST SIGNIFICANT         
         BNE   *+12                CHARACTER IN ACCOUNT CODE                    
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         DC    H'0'                                                             
         AR    R2,R0               ADD IN LENGTH OF FRONT OF KEY                
*                                                                               
MAIN1    BCTR  R2,0                DECREMENT FOR EXMVC                          
         STC   R2,ACCLEN           STORE LENGTH OF SR ACCT INPUT                
         BAS   RE,HIGH                                                          
         B     *+8                                                              
*                                                                               
MAIN2    BAS   RE,SEQ                                                           
         ZIC   R1,ACCLEN                                                        
         EXCLC R1,SAVEKEY,RKEY     SAME X'2D03' C/U/L                           
         BNE   EXIT                                                             
*                                                                               
         L     R6,ASRTREC          R6 COVERS SORT RECORD                        
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
         BAS   RE,CLEARSRT                                                      
         L     R5,AIOAREA1         R5 COVERS IO AREA                            
         MVC   SRTCUL,ACINCUL      C/U/L                                        
         MVC   SRTAOR,ACINACC      AOR AGENCY                                   
         MVC   SRTCLT,ACINCLT      CLIENT                                       
         MVC   SRTPRD,ACINPRD      PRODUCT                                      
*                                                                               
         MVC   SRTMED(2),=C'SI'    DEFAULT IS INCOME ACCOUNT                    
         MVC   SRTMED+2(2),ACINMED MEDIA                                        
         TM    ACSTATUS-ACKEYD(R5),X'02'                                        
         BZ    *+10                                                             
         MVC   SRTMED(2),=C'**'    TAKEN FROM MEDIA RECORD                      
         MVC   SRTEST,ACINEST      ESTIMATE NUMBER                              
         MVC   SRTCUL,ACINCUL      C/U/L                                        
         MVI   ELCODE,X'C6'                                                     
         BAS   RE,GETEL            GET PROFILE ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACINPRFD,R5                                                      
*                                                                               
         MVC   SRTDESC,ACIPFDES    ESTIMATE DESCRIPTION                         
         MVC   SRTPRS,ACIPFPRS     YYMM START EST PERIOD                        
         MVC   SRTPRE,ACIPFPRE     YYMM END EST PERIOD                          
         MVC   SRTNET,ACIPFNET     NET AOR NET PCT                              
*                                                                               
         MVI   ELCODE,X'C7'                                                     
*                                                                               
MAIN6    BAS   RE,NEXTEL                                                        
         BNE   MAIN2                                                            
*                                                                               
         USING ACINESTD,R5                                                      
*                                                                               
         MVC   SRTMTH,ACIESMTH     YYMM ADVERTISING MONTH                       
         MVC   SRTDAT,ACIESDAT     YMD POSTING DATE                             
         MVC   SRTFEE,ACIESFEE     AOR FEE PCT                                  
         MVC   SRTRCV,ACIESRCV     CREATIVE RECV PCT                            
         MVC   SRTGRS,ACIESGRS     GROSS BILLING EST                            
         MVC   SRTREC,ACIESREC     RECEIVABLE AMOUNT EST                        
         MVC   SRTPD,ACIESPD       PAID SO FAR                                  
*                                                                               
         CLI   QOPT3,C'Y'          USE PSTING DATE INSTEAD OF ADV MNTH?         
         BNE   MAIN8                                                            
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(2,SRTDAT),(1,WORK)                                  
         CLC   WORK(3),STARTDT                                                  
         BL    MAIN6                                                            
         CLC   WORK(3),ENDDT                                                    
         BH    MAIN6                                                            
         B     MAIN10                                                           
*                                                                               
MAIN8    CLC   SRTMTH,STARTDT      CHECK IF ADV MONTH IN RANGE                  
         BL    MAIN6                                                            
         CLC   SRTMTH,ENDDT                                                     
         BH    MAIN6                                                            
*                                                                               
MAIN10   GOTO1 VSORTER,DMCB,=C'PUT',(R6)                                        
         B     MAIN6                                                            
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        RETRIEVE RECORDS                                        *              
*----------------------------------------------------------------*              
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
REQL     NTR1                                                                   
         CLI   QOPT2,C'Y'          TAPE OUTPUT OPTION                           
         BNE   *+8                                                              
         BAS   RE,INITTAPE                                                      
         GOTO1 VSORTER,DMCB,=C'GET'     GET FIRST SORT RECORD                   
         L     R6,DMCB+4                                                        
         MVC   SAVESRT,0(R6)       SAVE FIRST RECORD                            
         BAS   RE,CLEARBLK         CLEAR PRINT BLOCK                            
         MVI   FIRST,C'Y'          FIRST TIME SWITCH                            
         MVI   HEADFLAG,C'Y'       INIT HEADER                                  
         B     REQL0A                                                           
*                                                                               
REQL0    GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R6,DMCB+4                                                        
*                                                                               
REQL0A   ST    R6,ALSORT           ADDRESS OF LAST SORT                         
         LTR   R6,R6                                                            
         BZ    REQX                END OF RECORDS FROM SORT                     
         BAS   RE,PRNTTOT          PRINT TOTALS IF NECESSARY                    
         MVC   CURCUL,SRTCUL       COMP U/L                                     
         MVC   CURAOR,SRTAOR                                                    
         MVC   CURCLT,SRTCLT                                                    
         MVC   CURPRD,SRTPRD                                                    
         MVC   CURMED,SRTMED                                                    
         MVC   CUREST,SRTEST                                                    
         MVC   CURESTNM,SRTDESC                                                 
         BAS   RE,GETNAMES         GET THE NAMES OF LEVELS                      
         CLI   HEADFLAG,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,HEADUP                                                        
*                                                                               
REQL1    DS    0H                                                               
         CLI   QOPT2,C'Y'          TAPE OPTION                                  
         BNE   *+8                                                              
         BAS   RE,BLDTAPE                                                       
*                                                                               
         L     R6,ALSORT           POINT TO SORT RECORD                         
*                                                                               
         USING PLINED,R7                                                        
*                                                                               
         LA    R7,PRNTBLOC                                                      
*                                                                               
REQL1A   MVC   PEST,SRTEST         ESTIMATE NUMBER                              
         MVC   PDESC,SRTDESC       DESCRIPTION                                  
         CLI   LINE,44                                                          
         BNH   *+8                                                              
         BAS   RE,HEADUP                                                        
*                                                                               
REQL2    MVC   PMED,SRTMED         MEDIA                                        
         MVC   WORK,SRTMTH                                                      
         MVI   WORK+2,X'01'        FORCE FIRST DAY OF MONTH                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,PADVMTH)                                 
         GOTO1 DATCON,DMCB,(2,SRTDAT),(5,PPSTDAT)                               
         EDIT  (P3,SRTFEE),(6,PFEE),2,ZERO=BLANK                                
         EDIT  (P3,SRTRCV),(6,PRCV),2,ZERO=BLANK                                
         EDIT  (P6,SRTGRS),(12,PGRS),2,MINUS=YES,ZERO=BLANK                     
         EDIT  (P6,SRTREC),(12,PREC),2,MINUS=YES,ZERO=BLANK                     
         EDIT  (P6,SRTPD),(12,PPD),2,MINUS=YES,ZERO=BLANK                       
         BAS   RE,PRNTOUT                                                       
         BAS   RE,ADDTOTS                                                       
         MVC   SAVESRT,0(R6)       SAVE LAST SORTRECORD                         
         B     REQL0                                                            
*                                                                               
REQX     DS    0H                                                               
         CLI   QOPT2,C'Y'          CLOSE OUTPUT TAPE                            
         BNE   REQX1                                                            
         CLOSE (TAPE01)                                                         
*                                                                               
REQX1    DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
         BAS   RE,ENDTOT                                                        
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        PRINT TOTALS                                            *              
*----------------------------------------------------------------*              
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
PRNTTOT  NTR1                                                                   
         L     R6,ALSORT           ADDRESS OF LAST SORTRECORD                   
         LA    R5,SAVESRT          SAVED SORT RECORD                            
         MVC   TOTSW(5),=C'NNNNN' INITIALIZE TOTALS FLAG                        
         L     R4,TOTLIST          HIERARCHY FOR TOTALS                         
         LA    R4,4(R4)                                                         
         LA    R3,TOTSW                                                         
         LA    R3,4(R3)                                                         
         B     PRNTTOTA                                                         
*                                                                               
PRNTTOT2 DS    0H                                                               
         SH    R3,=H'1'            NEXT TOTAL FLAG                              
         SH    R4,=H'1'                                                         
*                                                                               
PRNTTOTA DS    0H                                                               
         CLI   0(R4),AOR           AOR TOTAL?                                   
         BNE   PRNTTOTB                                                         
         CLC   SRTAOR-SRTRECD(L'SRTAOR,R5),SRTAOR                               
         BE    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         B     PRNTTOT2                                                         
*                                                                               
PRNTTOTB DS    0H                                                               
         CLI   0(R4),CLT           CLIENT TOTAL?                                
         BNE   PRNTTOTC                                                         
         CLC   SRTCLT-SRTRECD(L'SRTCLT,R5),SRTCLT                               
         BE    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         B     PRNTTOT2                                                         
*                                                                               
PRNTTOTC DS    0H                                                               
         CLI   0(R4),PRD           PRODUCT TOTAL?                               
         BNE   PRNTTOTD                                                         
         CLC   SRTPRD-SRTRECD(L'SRTPRD,R5),SRTPRD                               
         BE    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         B     PRNTTOT2                                                         
*                                                                               
PRNTTOTD DS    0H                                                               
         CLI   0(R4),MED           MEDIA TOTAL?                                 
         BNE   PRNTTOTE                                                         
         CLC   SRTMED-SRTRECD(L'SRTMED,R5),SRTMED                               
         BE    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         B     PRNTTOT2                                                         
*                                                                               
PRNTTOTE DS    0H                                                               
         CLI   0(R4),EST           ESTIMATE TOTAL?                              
         BNE   PRNTTOTF                                                         
         CLC   SRTEST-SRTRECD(L'SRTEST,R5),SRTEST                               
         BE    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         B     PRNTTOT2                                                         
*                                                                               
PRNTTOTF DS    0H                                                               
         CLC   TOTSW(5),=C'NNNNN'  ANY TOTALS THIS PASS?                        
         BE    EXIT                NO TOTALS THIS PASS                          
         GOTO1 ACREPORT            BLANK LINE BEFORE TOTALS                     
*                                                                               
         LA    R4,TOTSW                                                         
         LA    RF,=C'N'                                                         
         LA    R3,5                COUNTER                                      
*                                                                               
PRNTTOTI CLI   0(R4),C'Y'                                                       
         BNE   *+8                                                              
         LA    RF,=C'Y'                                                         
         MVC   0(1,R4),0(RF)       INSURE ALL LOWER LEVEL TOTALS PRINT          
         LA    R4,1(R4)                                                         
         BCT   R3,PRNTTOTI                                                      
*                                                                               
PRNTTOTK BAS   RE,FORMAT                                                        
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        FORMAT ACCUMS                                           *              
*----------------------------------------------------------------*              
*                                                                               
FORMAT   NTR1                                                                   
         L     R4,TOTLIST          HIERARCHY FOR TOTALS                         
         LA    R4,4(R4)                                                         
         LA    R3,TOTSW            FLAG SETTINGS                                
         LA    R3,4(R3)                                                         
         MVI   NOTOTS,C'Y'         SET TO ASSUME THAT NO TOTS PRINTED           
         LA    R7,PRNTBLOC                                                      
*                                                                               
         USING PLINED,R7                                                        
         USING PROLLD,R5                                                        
*                                                                               
FORMATA  CLI   0(R3),C'Y'          PRINT THIS TOTAL?                            
         BNE   FORMATX             NO MORE TOTALS                               
         ZIC   R2,0(R4)                                                         
         GOTO1 PROLLER,DMCB,GETADDR,ACCUMS,(R2)                                 
         L     R5,0(R1)                                                         
*                                                                               
FORMATB  EDIT  (P6,PRGRS),(12,PGRS),2,MINUS=YES,ZERO=BLANK                      
         EDIT  (P6,PRREC),(12,PREC),2,MINUS=YES,ZERO=BLANK                      
         EDIT  (P6,PRPD),(12,PPD),2,MINUS=YES,ZERO=BLANK                        
         MVI   NOTOTS,C'N'                                                      
*                                                                               
         MVC   WORK,XSPACES                                                     
         MVC   WORK(11),=C'TOTALS FOR '                                         
         LA    R6,WORK+11                                                       
*                                                                               
         CLI   0(R4),AOR                                                        
         BNE   FORMAT2                                                          
         MVC   0(L'CURAOR,R6),CURAOR                                            
         LA    R6,L'CURAOR(R6)                                                  
         MVC   1(L'CURAORNM,R6),CURAORNM                                        
         MVI   HEADFLAG,C'Y'                                                    
         B     FORMAT7                                                          
*                                                                               
FORMAT2  DS    0H                                                               
         CLI   0(R4),CLT                                                        
         BNE   FORMAT3                                                          
         MVC   0(L'CURCLT,R6),CURCLT                                            
         LA    R6,L'CURCLT(R6)                                                  
         MVC   1(L'CURCLTNM,R6),CURCLTNM                                        
         MVI   HEADFLAG,C'Y'                                                    
         B     FORMAT7                                                          
*                                                                               
FORMAT3  DS    0H                                                               
         CLI   0(R4),PRD                                                        
         BNE   FORMAT4                                                          
         MVC   0(L'CURPRD,R6),CURPRD                                            
         LA    R6,L'CURPRD(R6)                                                  
         MVC   1(L'CURPRDNM,R6),CURPRDNM                                        
         MVI   HEADFLAG,C'Y'                                                    
         B     FORMAT7                                                          
*                                                                               
FORMAT4  DS    0H                                                               
         CLI   0(R4),MED                                                        
         BNE   FORMAT5                                                          
         MVC   0(L'CURMED,R6),CURMED                                            
         LA    R6,L'CURMED(R6)                                                  
         MVC   1(L'CURMEDNM,R6),CURMEDNM                                        
         B     FORMAT7                                                          
*                                                                               
FORMAT5  DS    0H                                                               
         CLI   0(R4),EST                                                        
         BNE   FORMAT6                                                          
         MVC   0(L'CUREST,R6),CUREST                                            
         LA    R6,L'CUREST(R6)                                                  
         MVC   1(L'CURESTNM,R6),CURESTNM                                        
*                                                                               
FORMAT6  CLI   0(R4),BLK                                                        
         BE    FORMAT8                                                          
*                                                                               
FORMAT7  DS    0H                                                               
         GOTO1 VSQUASHR,DMCB,WORK,60                                            
         MVC   PTOTAL,WORK                                                      
         LA    R7,REPWIDTH(R7)     NEXT PRINT LINE                              
*                                                                               
FORMAT8  BCTR  R4,0                                                             
         BCTR  R3,0                                                             
         GOTO1 PROLLER,DMCB,CLEAR,ACCUMS,(R2)                                   
         B     FORMATA             LOOP BACK FOR NEXT TOTAL                     
*                                                                               
FORMATX  DS    0H                                                               
         CLI   LINE,48                                                          
         BNH   *+8                                                              
         BAS   RE,HEADUP           NEW PAGE IF ALL TOTALS WONT FIT              
         BAS   RE,PRNTOUT                                                       
         CLI   NOTOTS,C'Y'                                                      
         BE    EXIT                                                             
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        END TOTALS                                              *              
*----------------------------------------------------------------*              
*                                                                               
ENDTOT   NTR1                                                                   
         GOTO1 ACREPORT            BLANK LINE                                   
         MVC   TOTSW(5),=C'YYYYY'  PRINT ALL LEVEL TOTALS                       
         BAS   RE,FORMAT                                                        
         GOTO1 PROLLER,DMCB,GETADDR,ACCUMS,REP                                  
         LA    R7,PRNTBLOC                                                      
         L     R5,0(,R1)                                                        
*                                                                               
         USING PLINED,R7                                                        
         USING PROLLD,R5                                                        
*                                                                               
         MVC   PTOTAL(17),=C'TOTALS FOR REPORT'                                 
         EDIT  (P6,PRGRS),(12,PGRS),2,MINUS=YES,ZERO=BLANK                      
         EDIT  (P6,PRREC),(12,PREC),2,MINUS=YES,ZERO=BLANK                      
         EDIT  (P6,PRPD),(12,PPD),2,MINUS=YES,ZERO=BLANK                        
         BAS   RE,PRNTOUT                                                       
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        PRINT CONTENTS OF PRINTBLOC                             *              
*----------------------------------------------------------------*              
*                                                                               
PRNTOUT  NTR1                                                                   
         LA    R7,PRNTBLOC                                                      
*                                                                               
PRINT1   CLC   0(REPWIDTH,R7),XSPACES                                           
         BE    PRINTX                                                           
         MVC   XP(REPWIDTH),0(R7)  MOVE INTO PRINTLINE                          
         GOTO1 ACREPORT                                                         
         LA    R7,REPWIDTH(R7)     BUMP TO NEXT LINE                            
         B     PRINT1                                                           
*                                                                               
PRINTX   BAS   RE,CLEARBLK         CLEAR PRNTBLOC                               
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        ADD CURRENT SORT RECORD AMOUNTS TO ACCUMS               *              
*----------------------------------------------------------------*              
*                                                                               
*                       LAYOUT OF ACCUMS                                        
*                                                                               
*                       CNT               GRS   REC   PD                        
*        1. AOR  TOTAL   0     X     X     0     0     0                        
*        2. CLT  TOTAL   0     X     X     0     0     0                        
*        3. PRD  TOTAL   0     X     X     0     0     0                        
*        4. MED  TOTAL   0     X     X     0     0     0                        
*        5. EST  TOTAL   0     X     X     0     0     0                        
*        6. REP  TOTAL   0     X     X     0     0     0                        
*        7. WRK  LINE    0     X     X     0     0     0                        
*                                                                               
*        NOTE: X=COLUMN IS NOT USED AND WILL ALWAYS BE ZEROS.                   
*              THESE COLUMNS CAN BE CONSIDERED SPARE FOR FUTURE USE.            
*                                                                               
         USING SRTRECD,R6                                                       
*                                                                               
ADDTOTS  NTR1                                                                   
         L     R6,ALSORT           LAST SORT RECORD                             
         GOTO1 PROLLER,DMCB,GETADDR,ACCUMS,WRKLINE                              
         L     R5,DMCB                                                          
*                                                                               
         USING PROLLD,R5                                                        
*                                                                               
         ZAP   PRADDS,=P'1'        ADD AMOUNTS INTO WORKLINE                    
         ZAP   PRGRS,SRTGRS                                                     
         ZAP   PRREC,SRTREC                                                     
         ZAP   PRPD,SRTPD                                                       
*                                                                               
         GOTO1 PROLLER,DMCB,ADDPACK,ACCUMS,WRKLINE,AOR                          
         GOTO1 (RF),(R1),ADDPACK,ACCUMS,WRKLINE,CLT                             
         GOTO1 (RF),(R1),ADDPACK,ACCUMS,WRKLINE,PRD                             
         GOTO1 (RF),(R1),ADDPACK,ACCUMS,WRKLINE,MED                             
         GOTO1 (RF),(R1),ADDPACK,ACCUMS,WRKLINE,EST                             
         GOTO1 (RF),(R1),ADDPACK,ACCUMS,WRKLINE,REP                             
         GOTO1 (RF),(R1),CLEAR,ACCUMS,WRKLINE                                   
         B     EXIT                CLEAR OUT WRKLINE                            
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        GET THE NAMES OF LEVEL ACCTS                            *              
*----------------------------------------------------------------*              
*                                                                               
GETNAMES NTR1                                                                   
         MVC   RKEY,SPACES         LOOKUP A.O.R. NAME                           
         MVC   RKEY(15),CURCUL                                                  
         BAS   RE,HIGH                                                          
         CLC   SAVEKEY,RKEY                                                     
         BNE   GETNM1                                                           
         BAS   RE,GET20EL                                                       
         MVC   CURAORNM,WORK                                                    
*                                                                               
GETNM1   DS    0H                  LOOKUP CLIENT NAME                           
         MVC   RKEY,SPACES                                                      
         MVC   RKEY(1),QCOMPANY                                                 
         MVC   RKEY+1(2),=C'SJ'                                                 
         MVC   RKEY+3(3),CURCLT                                                 
         BAS   RE,HIGH                                                          
         CLC   SAVEKEY,RKEY                                                     
         BNE   GETNM2                                                           
         BAS   RE,GET20EL                                                       
         MVC   CURCLTNM,WORK                                                    
*                                                                               
GETNM2   DS    0H                  LOOKUP PRODUCT NAME                          
         MVC   RKEY,SPACES                                                      
         MVC   RKEY(1),QCOMPANY                                                 
         MVC   RKEY+1(2),=C'SJ'                                                 
         MVC   RKEY+3(3),CURCLT                                                 
         MVC   RKEY+6(3),CURPRD                                                 
         BAS   RE,HIGH                                                          
         CLC   SAVEKEY,RKEY                                                     
         BNE   GETNM3                                                           
         BAS   RE,GET20EL                                                       
         MVC   CURPRDNM,WORK                                                    
*                                                                               
GETNM3   DS    0H                                                               
         CLC   CURMED(2),=C'**'                                                 
         BE    GETNM3A                                                          
         MVC   RKEY,SPACES         READ SI ACCT (MEDIA= SIXX)                   
         MVC   RKEY(1),QCOMPANY                                                 
         MVC   RKEY+1(L'CURMED),CURMED                                          
         BAS   RE,HIGH                                                          
         CLC   SAVEKEY,RKEY                                                     
         BNE   GETNM4                                                           
         BAS   RE,GET20EL                                                       
         MVC   CURMEDNM,WORK                                                    
         B     GETNM4                                                           
*                                                                               
GETNM3A  DS    0H                  READ MEDIA RECORD                            
         MVC   RKEY,SPACES                                                      
         MVI   RKEY,X'09'                                                       
         MVC   RKEY+1(1),QCOMPANY                                               
         MVC   RKEY+2(2),CURMED+2                                               
         BAS   RE,HIGH                                                          
         CLC   SAVEKEY,RKEY                                                     
         BNE   GETNM4                                                           
         L     R5,AIOAREA1                                                      
         MVI   ELCODE,X'11'        MEDIA CATEGORY ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   GETNM4                                                           
*                                                                               
         USING ACMEDIAD,R5                                                      
*                                                                               
         MVC   CURMEDNM,SPACES                                                  
         MVC   CURMEDNM(15),ACMDDESC     DESCRIPTION                            
*                                                                               
GETNM4   DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        GET NAME ELEMENT                                        *              
*----------------------------------------------------------------*              
*                                                                               
GET20EL  NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         L     R5,AIOAREA1                                                      
         MVI   ELCODE,X'20'        NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING ACNAMED,R5                                                       
*                                                                               
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'            LENGTH=ACNMLEN-2 (-1 FOR EX)                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        GET PROFILE ELEMENT                                     *              
*----------------------------------------------------------------*              
*                                                                               
GET24EL  NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         L     R5,AIOAREA1                                                      
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING ACPROFD,R5                                                       
*                                                                               
         MVC   WORK(2),ACPROFFC    OFFICE CODE                                  
         B     EXIT                                                             
         SPACE 3                                                                
*----------------------------------------------------------------*              
*        GET FREE FORM NUMBER ELEMENT                            *              
*----------------------------------------------------------------*              
*                                                                               
GET25EL  NTR1                                                                   
         MVC   WORK,XSPACES                                                     
         L     R5,AIOAREA1                                                      
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BNE   EXIT                                                             
*                                                                               
         USING ACNOD,R5                                                         
*                                                                               
         ZIC   R1,ACNOLEN                                                       
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNO                                                     
         OC    WORK,SPACES                                                      
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        INITIALIZE TAPE OUTPUT                                  *              
*----------------------------------------------------------------*              
*                                                                               
INITTAPE NTR1                                                                   
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE01'),DSPARM                            
         OPEN  (TAPE01,OUTPUT)                                                  
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        BUILD TAPE RECORD                                       *              
*----------------------------------------------------------------*              
*                                                                               
         USING SRTRECD,R6                                                       
         USING TPRECD,R4                                                        
*                                                                               
BLDTAPE  NTR1                                                                   
         L     R6,ALSORT                                                        
         LA    R4,TAPEREC                                                       
         XC    TAPEREC,TAPEREC                                                  
*                                                                               
         MVC   RKEY,SPACES                                                      
         MVC   RKEY(1),QCOMPANY                                                 
         MVC   RKEY+1(2),=C'SJ'                                                 
         MVC   RKEY+3(3),CURCLT                                                 
         BAS   RE,HIGH                                                          
         CLC   SAVEKEY,RKEY                                                     
         BNE   BLDTAPE1                                                         
         BAS   RE,GET24EL          GET OFFICE                                   
         MVC   TPOFFICE(L'TPOFFICE),WORK                                        
         BAS   RE,GET25EL          GET COST ACCT                                
         MVC   TPCOST(L'TPCOST),WORK                                            
*                                                                               
BLDTAPE1 DS    0H                                                               
         MVC   RKEY,SPACES                                                      
         MVC   RKEY(1),QCOMPANY                                                 
         MVC   RKEY+1(2),=C'SJ'                                                 
         MVC   RKEY+3(3),CURCLT                                                 
         MVC   RKEY+6(3),CURPRD                                                 
         BAS   RE,HIGH                                                          
         CLC   SAVEKEY,RKEY                                                     
         BNE   BLDTAPE2                                                         
         BAS   RE,GET25EL          CHECK FOR COST OVERRIDE AT PRD LEV           
         CLC   WORK,SPACES                                                      
         BE    BLDTAPE2                                                         
         MVC   TPCOST(L'TPCOST),WORK                                            
*                                                                               
BLDTAPE2 DS    0H                                                               
         MVC   TPCLIENT,CURCLT     CLIENT                                       
         MVC   TPPRODCT,CURPRD     PRODUCT                                      
         MVC   TPMEDIA,CURMED+2    MEDIA                                        
         MVC   TPESTNUM,CUREST     ESTIMATE NUMBER                              
*                                                                               
         MVC   WORK,SPACES         ADVERTISING MONTH                            
         MVC   WORK(L'SRTMTH),SRTMTH                                            
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(6,TPADVMOS)                                
         GOTO1 DATCON,DMCB,(2,SRTDAT),(X'20',TPPSTDT)                           
*                                                                               
         ZAP   DUB,SRTGRS                                                       
         UNPK  TPESTAMT,DUB        GROSS BILLING ESTIMATE                       
*                                                                               
         ZAP   DUB,SRTREC                                                       
         UNPK  TPRECAMT,DUB        RECEIVABLE AMOUNT EST                        
*                                                                               
         ZAP   DUB,SRTRCV                                                       
         MP    DUB,=P'100'         ALLOW FOR 4 DEC PLACES                       
         UNPK  TPRECPCT,DUB        RECEIVABLE PERCENT                           
*                                                                               
         MVC   TPRCVACC,CURAOR     A.O.R ACCT                                   
         MVC   TPSPARE,XSPACES                                                  
         PUT   TAPE01,TAPEREC                                                   
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        CLEAR PRINT BLOCK                                       *              
*----------------------------------------------------------------*              
*                                                                               
CLEARBLK NTR1                                                                   
         LA    R7,PRNTBLOC                                                      
         LA    RE,20                                                            
         MVC   0(REPWIDTH,R7),XSPACES                                           
         LA    R7,REPWIDTH(R7)                                                  
         BCT   RE,*-10                                                          
         B     EXIT                                                             
         SPACE 5                                                                
*----------------------------------------------------------------*              
*       CLEAR SORT AREA                                          *              
*----------------------------------------------------------------*              
*                                                                               
CLEARSRT NTR1                                                                   
         L     R1,ASRTREC                                                       
         LA    RE,10                                                            
         XC    0(100,R1),0(R1)                                                  
         LA    R1,100(R1)                                                       
         BCT   RE,*-10                                                          
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*       PRINT HEADLINES                                          *              
*----------------------------------------------------------------*              
*                                                                               
HEADUP   NTR1                                                                   
         MVI   HEADFLAG,C'N'                                                    
         MVC   WORK,XSPACES        AOR NAME IN HEADLINES                        
         MVC   WORK(L'CURAOR),CURAOR                                            
         MVI   WORK+13,C'-'                                                     
         MVC   WORK+14(L'CURAORNM),CURAORNM                                     
         GOTO1 VSQUASHR,DMCB,WORK,50                                            
         MVC   XHEAD3+11(50),WORK                                               
*                                                                               
         MVC   WORK,XSPACES        CLIENT NAME IN HEADLINES                     
         MVC   WORK(L'CURCLT),CURCLT                                            
         MVI   WORK+6,C'-'                                                      
         MVC   WORK+7(L'CURCLTNM),CURCLTNM                                      
         GOTO1 VSQUASHR,DMCB,WORK,50                                            
         MVC   XHEAD4+11(50),WORK                                               
*                                                                               
         MVC   WORK,XSPACES        PRODUCT NAME IN HEADLINES                    
         MVC   WORK(L'CURPRD),CURPRD                                            
         MVI   WORK+6,C'-'                                                      
         MVC   WORK+7(L'CURPRDNM),CURPRDNM                                      
         GOTO1 VSQUASHR,DMCB,WORK,50                                            
         MVC   XHEAD5+11(50),WORK                                               
*                                                                               
         CLI   FIRST,C'Y'          IS THIS FIRST TIME THROUGH?                  
         BNE   HEADUP2                                                          
         MVI   FIRST,C'N'                                                       
         B     EXIT                                                             
*                                                                               
HEADUP2  MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        DATAMGR INTERFACE                                       *              
*----------------------------------------------------------------*              
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'        READ HIGH                              
         MVC   SAVEKEY,RKEY                                                     
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'        READ SEQUENTIAL                        
         MVC   SAVEKEY,RKEY                                                     
         B     GTREC                                                            
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'        A SPECIFIC READ                        
*                                                                               
GTREC    NTR1                                                                   
         L     R5,AIOAREA1                                                      
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',RKEY,(R5)                       
         MVC   RKEY,0(R5)                                                       
         CLI   DMCB+8,0                  TEST FOR ERRORS                        
         BE    *+6                                                              
         DC    H'0'                      DIE IF ERRORS FOUND                    
         B     EXIT                                                             
         EJECT ,                                                                
*----------------------------------------------------------------*              
*              CONSTANT DECLARATIONS                             *              
*----------------------------------------------------------------*              
*                                                                               
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=200 '                                     
SORTCARD DC    CL80' '                                                          
*                                                                               
TAPE01   DCB   DSORG=PS,                                               X        
               MACRF=PM,                                               X        
               DDNAME=TAPE01,                                          X        
               RECFM=FB,                                               X        
               BLKSIZE=256,                                            X        
               LRECL=256                                                        
*                                                                               
DSPARM   DC    CL20'ACCTAPE.AC0E1OM1'                                           
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        EQUATES                                                 *              
*----------------------------------------------------------------*              
*                                                                               
*        USED IN BUILDING SORTCARD & IN PROLLER REFERENCES                      
*                                                                               
BLK      EQU   0                   BLANK                                        
AOR      EQU   1                   AOR                                          
CLT      EQU   2                   CLIENT                                       
PRD      EQU   3                   PRODUCT                                      
MED      EQU   4                   MEDIA                                        
EST      EQU   5                   ESTIMATE                                     
REP      EQU   6                   REPORT TOTAL   (PROLLER LINE)                
*                                                                               
WRKLINE  EQU   7                   WORK LINE      (PROLLER LINE)                
*                                                                               
*        PROLLER ACTION EQUATES                                                 
*                                                                               
DEFINE   EQU   0                   DECLARE/INIT ACCUMS                          
GETADDR  EQU   1                   GET THE ADDRESS OF A LINE                    
CLEAR    EQU   2                   CLEAR A LINE OF ACCUMS                       
ADDPACK  EQU   4                   ADD WRKLINE INTO ANOTHER LINE                
*                                                                               
MAXROW   EQU   7                   NUMBER OF ROWS IN ACCUM BANK                 
MAXCOL   EQU   6                   NUMBER OF COLS IN ACCUM BANK                 
REPWIDTH EQU   165                 REPORT WIDTH = 165 PRINT COLS                
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        EXTERNAL ADDRESS LIST                                   *              
*----------------------------------------------------------------*              
*                                                                               
ADCONS   DS    0F                                                               
         DC    V(SORTER)                                                        
         DC    V(SQUASHER)                                                      
         DC    V(CHOPCON)                                                       
         DC    A(SORTREC)                                                       
         DC    A(SORTAREA)                                                      
         DC    A(IOAREA1)                                                       
         DC    A(IOAREA2)                                                       
         DC    A(CLITABLE)                                                      
         DC    A(PRDTABLE)                                                      
         DC    A(MEDTABLE)                                                      
         DC    X'FF'                                                            
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        TABLE OF SORT OPTIONS & DISPLACEMENTS                   *              
*----------------------------------------------------------------*              
*                                                                               
SRTTB    DS    0H                                                               
         DC    AL1(AOR),C'04,12,A'                                              
         DC    AL1(CLT),C'16,03,A'                                              
         DC    AL1(PRD),C'19,03,A'                                              
         DC    AL1(MED),C'22,04,A'                                              
         DC    AL1(EST),C'26,06,A'                                              
SRTTBLNQ EQU   8                                                                
SRTTBTOT EQU   (*-SRTTB)/SRTTBLNQ  NUMBER OF LINES IN TABLE                     
         SPACE 5                                                                
*----------------------------------------------------------------*              
*        TABLE OF INPUT OPTIONS                                  *              
*----------------------------------------------------------------*              
*                                                                               
OPTTB    DS    0H                                                               
         DC    C' ',AL1(AOR,CLT,PRD,MED,EST),X'FF'                              
OPTTBLNQ EQU   7                                                                
OPTTBTOT EQU   (*-OPTTB)/OPTTBLNQ  NUMBER OF ENTRIES IN TABLE                   
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        TOTALS LIST - USED IN PRINTING TOTALS                   *              
*----------------------------------------------------------------*              
*                                                                               
TOTTB    DS    0H                                                               
         DC    C' ',AL1(AOR,CLT,PRD,MED,EST),X'FF'                              
TOTTBLNQ EQU   7                                                                
TOTTBTOT EQU   (*-TOTTB)/TOTTBLNQ  NUMBER OF ENTRIES IN TABLE                   
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        LITERAL DECLARATIONS                                    *              
*----------------------------------------------------------------*              
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        BOX HOOK                                                *              
*----------------------------------------------------------------*              
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'            SET ROWS                               
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'              SET LH MARGIN                          
         MVI   BOXCOLS+47,C'C'                                                  
         MVI   BOXCOLS+54,C'C'                                                  
         MVI   BOXCOLS+63,C'C'                                                  
         MVI   BOXCOLS+74,C'C'                                                  
         MVI   BOXCOLS+83,C'C'                                                  
         MVI   BOXCOLS+92,C'C'                                                  
         MVI   BOXCOLS+107,C'C'                                                 
         MVI   BOXCOLS+122,C'C'                                                 
         MVI   BOXCOLS+137,C'R'                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*                                                                               
         DROP  R4                  KEEP IT CLEAN                                
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        STORAGE                                                 *              
*----------------------------------------------------------------*              
*                                                                               
SORTREC  DS    0D                  WHERE SORT RECORD IS BUILT                   
         DS    1000C                                                            
*                                                                               
IOAREA1  DS    0D                  IOAREA #1                                    
         DS    2000C                                                            
*                                                                               
IOAREA2  DS    0D                  IOAREA #2                                    
         DS    2000C                                                            
*                                                                               
SORTAREA DS    0D                  SORT BUFFER                                  
         DS    10000C                                                           
*                                                                               
CLITABLE DS    0D                  CLIENT TABLE                                 
*                                                                               
PRDTABLE DS    0D                  PRODUCT                                      
*                                                                               
MEDTABLE DS    0D                  MEDIA                                        
         EJECT ,                                                                
*------------------------------------------------------------------*            
*              DSECT TO COVER SAVE W/S                             *            
*------------------------------------------------------------------*            
*                                                                               
LWSD     DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
VSORTER  DS    A                   SORTER                                       
VSQUASHR DS    A                   SQUASHER                                     
VCHOPCON DS    A                   CHOPPER                                      
ASRTREC  DS    A                   A(SORT RECORD)                               
ASRTAREA DS    A                   A(SORT AREA)                                 
AIOAREA1 DS    A                   IO AREA #1                                   
AIOAREA2 DS    A                   IO AREA #2                                   
ACLIENT  DS    A                   CLIENT TABLE                                 
APRODUCT DS    A                   PRODUCT TABLE                                
AMEDIA   DS    A                   MEDIA TABLE                                  
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
ALSORT   DS    A                   ADDRESS OF LAST SORT RECORD                  
TOTLIST  DS    A                   ADDRESS OF TOTAL LIST                        
*                                                                               
NOTOTS   DS    CL1                 INDICATE THAT NO TOTALS PRNTED               
HEADFLAG DS    CL1                 NEW PAGE?  Y=NEW PAGE                        
FIRST    DS    CL1                 FIRST FLAG                                   
REPTOTFL DS    XL1                 REPORT TOTAL FLAG                            
TOTSW    DS    CL6                 TOTAL SWITCHES                               
STARTDT  DS    PL3                                                              
ENDDT    DS    PL3                 REQUEST CARD DATES YYMMDD                    
ACCLEN   DS    XL1                 LENGTH OF 2D03 KEY - 1                       
*                                                                               
ELCODE   DS    XL1                 USED IN GETEL ROUTINE                        
ACCUMS   DS    CL260               PROLLER TOTAL ACCUMULATOR                    
*                                  (7 ROWS * 6 COLS * 6 BYTES) +8               
COMMAND  DS    CL6                 USED IN DATAMGR INTERFACE                    
RKEY     DS    CL42                                                             
SAVEKEY  DS    CL42                                                             
SAVESRT  DS    CL(L'SRTKEY)        SAVE PREVIOUS SORT KEY                       
*                                                                               
CURNM    DS    0C                                                               
CURCUL   DS    CL3                                                              
CURAOR   DS    CL(L'SRTAOR)        CURRENT AOR                                  
CURAORNM DS    CL36                                                             
CURCLT   DS    CL(L'SRTCLT)        CURRENT CLIENT                               
CURCLTNM DS    CL36                                                             
CURPRD   DS    CL(L'SRTPRD)        CURRENT PRODUCT                              
CURPRDNM DS    CL36                                                             
CURMED   DS    CL(L'SRTMED)        CURRENT MEDIA                                
CURMEDNM DS    CL36                                                             
CUREST   DS    CL(L'SRTEST)        CURRENT ESTIMATE NUMBER                      
CURESTNM DS    CL36                                                             
CURNMLNQ EQU   *-CURNM                                                          
*                                                                               
TAPEREC  DS    CL(TPLENQ)          TAPE BUFFER                                  
*                                                                               
PRNTBLOC DS    20CL165             20 LINE PRINT BLOCK                          
         EJECT ,                                                                
*------------------------------------------------------------------*            
*        TAPE OUTPUT DSECT                                                      
*------------------------------------------------------------------*            
*                                                                               
TPRECD   DSECT                                                                  
TPOFFICE DS    CL2                 COMPANY/OFFICE CODE                          
TPCLIENT DS    CL3                 BUY CODE CLIENT                              
TPPRODCT DS    CL3                 BUY CODE PRODUCT                             
TPCOST   DS    CL4                 COST CODE FROM PRODUCT RECORD                
TPMEDIA  DS    CL2                 MEDIA CODE                                   
TPADVMOS DS    CL6                 ADV MONTH  EX/ C'890701'                     
TPESTNUM DS    CL6                 ESTIMATE NUMBER                              
TPPSTDT  DS    CL6                 POSTING DATE EX/ C'890703'                   
TPESTAMT DS    CL12                ESTIMATE AMOUNT                              
TPRECAMT DS    CL12                RECEIVABLE AMOUNT                            
TPRECPCT DS    CL12                RECEIVABLE PERCENT                           
TPRCVACC DS    CL12                                                             
TPSPARE  DS    CL176               SPARE                                        
TPLENQ   EQU   *-TPRECD            RECORD LENGTH                                
         ORG   TPRECD                                                           
TP00     DS    CL(TPLENQ)                                                       
         EJECT ,                                                                
*------------------------------------------------------------------*            
*        PROLLER DSECT ACCUMULATOR                                 *            
*------------------------------------------------------------------*            
*                                                                               
PROLLD   DSECT                                                                  
PRADDS   DS    PL6                 NUMBER OF ADDS TO THAT ROW                   
         DS    PL6                 SPARE                                        
         DS    PL6                 SPARE                                        
PRGRS    DS    PL6                 GROSS BILLING EST                            
PRREC    DS    PL6                 RECEIVALBE AMOUNT EST                        
PRPD     DS    PL6                 PAID TO DATE                                 
         EJECT ,                                                                
*------------------------------------------------------------------*            
*        SORT RECORD DSECT                                         *            
*------------------------------------------------------------------*            
*                                                                               
SRTRECD  DSECT                                                                  
SRTKEY   DS    0CL42                                                            
SRTCUL   DS    CL3                 CREATIVE COMP/ U/L                           
SRTAOR   DS    CL12                AOR AGENCY                                   
SRTCLT   DS    CL3                 CLIENT                                       
SRTPRD   DS    CL3                 PRODUCT                                      
SRTMED   DS    CL4                 MEDIA                                        
SRTEST   DS    CL6                 ESTIMATE NUMBER                              
         DS    CL11                SPARE                                        
SRTDESC  DS    CL36                ESTIMATE DESCRIPTION                         
SRTPRS   DS    PL2                 YYMM START EST PERIOD PACKED                 
SRTPRE   DS    PL2                 YYMM END EST PERIOD PACKED                   
SRTNET   DS    PL3                 AOR NET PCT (A/P FOR OTHER AGY)              
*                                                                               
SRTMTH   DS    PL2                 YYMM ADVERTISING MONTH PACKED                
SRTDAT   DS    XL2                 YMD POSTING DATE COMPRESSED                  
SRTFEE   DS    PL3                 AOR FEE PCT                                  
SRTRCV   DS    PL3                 CREATIVE RECV PCT 2 DEC PLACES               
SRTGRS   DS    PL6                 GROSS BILLING EST                            
SRTREC   DS    PL6                 RECEIVABLE AMOUNT EST                        
SRTPD    DS    PL6                 PAID SO FAR (UPDATED)                        
SRTLNGTH EQU   *-SRTRECD                                                        
         EJECT ,                                                                
*------------------------------------------------------------------*            
*        PRINT LINE DSECT                                          *            
*------------------------------------------------------------------*            
*                                                                               
PLINED   DSECT                                                                  
         DS    CL4                                                              
PTOTAL   DS    CL43                                                             
*                                                                               
         ORG   PLINED                                                           
         DS    CL1                                                              
PNAME    DS    CL40                                                             
*                                                                               
         ORG   PLINED                                                           
         DS    CL1                 OFFSET TO FIRST COLUMN                       
PEST     DS    CL6                 ESTIMATE NUMBER                              
         DS    CL2                                                              
PDESC    DS    CL36                ESTIMATE DESCRIPTION                         
*                                                                               
         ORG   PLINED                                                           
         DS    CL49                                                             
PMED     DS    CL4                 MEDIA                                        
*                                                                               
         ORG   PLINED                                                           
         DS    CL56                                                             
PADVMTH  DS    CL6                 ADVERTISING MONTH                            
*                                                                               
         ORG   PLINED                                                           
         DS    CL65                                                             
PPSTDAT  DS    CL8                 POSTING DATE                                 
*                                                                               
         ORG   PLINED                                                           
         DS    CL76                                                             
PFEE     DS    CL6                 AOR FEE PCT                                  
*                                                                               
         ORG   PLINED                                                           
         DS    CL85                                                             
PRCV     DS    CL6                 CREATIVE PCT                                 
*                                                                               
         ORG   PLINED                                                           
         DS    CL95                                                             
PGRS     DS    CL12                GROSS BILLING EST                            
*                                                                               
         ORG   PLINED                                                           
         DS    CL110                                                            
PREC     DS    CL12                RECEIVABLE AMOUNT EST                        
*                                                                               
         ORG   PLINED                                                           
         DS    CL124                                                            
PPD      DS    CL12                PAID SO FAR                                  
         EJECT ,                                                                
*----------------------------------------------------------------*              
*        OTHER INCLUDES                                          *              
*----------------------------------------------------------------*              
*                                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*DDCNTRL                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDCNTRL                                                        
         PRINT ON                                                               
*ACBIGPRNTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
*DDBOXEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDLOGOD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
*ACGENPOST                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*DDREMOTED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACREPE102 05/01/02'                                      
         END                                                                    
