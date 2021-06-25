*          DATA SET DDPHSCNTM  AT LEVEL 004 AS OF 05/08/20                      
*PHASE PHSCNTMA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE BINS31                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE QSORT                                                                  
*INCLUDE PHSCNT                                                                 
                                                                                
***********************************************************************         
         TITLE 'PHSCNTM - PHASE COUNT MAINTENANCE MODULE'                       
***********************************************************************         
         PRINT NOGEN                                                            
PHSCNTM  CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,**PHSM**,=V(REGSAVE),RA                              
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         LARL  R9,COMMON                                                        
         USING COMMON,R9                                                        
         USING PLINED,PLINE                                                     
         USING DMSPACED,DSPHSCNT                                                
*                                                                               
         BRAS  RE,INIT ----------- READ CARDS ECT                               
         BNE   XBASE                                                            
         BRAS  RE,MAIN ----------- MAIN LOOP                                    
         BRAS  RE,DONE ----------- CLOSE ALL                                    
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
                                                                                
***********************************************************************         
* READ DATA LINES                                                               
***********************************************************************         
MAIN     NTR1                                                                   
*                                                                               
         SELECT CLI,RMODE,EQ       COMPARE ACTION AND BRANCH TO ROUTINE         
         WHEN (C'I')               "I" INITIALIZE                               
           BRAS  RE,LOADTAB                                                     
         WHEN (C'R')               "R" RESET                                    
           BRAS  RE,LOADTAB                                                     
         WHEN (C'S')               "S" STATISTICAL REPORTING                    
           BRAS  RE,PRTREP                                                      
         WHEN (C'T')               "T" TEST                                     
           BRAS  RE,TESTTAB                                                     
         OTHRWISE                                                               
           CLI   RWRITE,C'Y'       KILL, and other commands                     
           BNE   MAINX                                                          
           GOTO1 VPHSCNT,DMCB,(RMODE,0),0                                       
         ENDSEL                                                                 
*                                                                               
MAINX    B     EXIT                                                             
                                                                                
***********************************************************************         
* INITIALIZE PROGRAM VALUES                                                     
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         XC    DSPHSCNT,DSPHSCNT                                                
*                                                                               
         L     R1,=A(IOAREA1-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO1                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA1**'                                             
*                                                                               
         L     R1,=A(IOAREA2-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,AIO2                                                          
         SHI   R1,8                                                             
         MVC   0(8,R1),=C'**IOA2**'                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAY)                                     
*                                                                               
         LA    R3,CARD                                                          
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT010                                                          
         CLC   =C'XX',0(R3)                                                     
         BE    INIT010                                                          
*                                                                               
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   XBASE               NEQ MEANS INVALID KEYWORD                    
         B     INIT02                                                           
*                                                                               
INIT010  CLI   DSPACE,C'?'         MAKE SURE WE HAVE A DSPACE CARD              
         BE    EXITNE                                                           
         L     RF,VSSB                                                          
         MVC   SSODSPAC-SSOOFF(1,RF),DSPACE                                     
*                                                                               
         CLI   RMODE,C'?'          MAKE SURE WE HAVE A MODE                     
         BE    EXITNE                                                           
*                                                                               
         MVC   TITLE,SPACES                                                     
         MVC   TITLE+40(40),=CL40'-- Phase Load Count Maintenance --'           
         BAS   RE,PRINTT                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
*                                                                               
         B     EXITEQ                                                           
INITNE   B     EXITNE                                                           
                                                                                
***********************************************************************         
* FINISHED PROCESSING                                                           
***********************************************************************         
DONE     NTR1                                                                   
         CLOSE SYSPRINT            CLOSE PRINT                                  
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* INITIALIZE AND LOAD THE PHASE LOAD TABLE                                      
***********************************************************************         
LOADTAB  NTR1                                                                   
*                                                                               
LT010    XR    R5,R5                                                            
         XC    PHASETAB(256),PHASETAB                                           
*                                                                               
         LA    R4,PHASETAB                                                      
         OPEN  (TAPEIN,INPUT)                                                   
LT020    GET   TAPEIN,IOAREA1                                                   
         MVC   0(L'PHASETAB,R4),IOAREA1                                         
         LA    R4,L'PHASETAB(,R4)                                               
         AHI   R5,1                                                             
         B     LT020                                                            
*                                                                               
LOADX    CLOSE TAPEIN                                                           
*                                                                               
         CLI   RWRITE,C'Y'                                                      
         BNE   EXITEQ                                                           
         GOTO1 VPHSCNT,DMCB,(RMODE,PHASETAB),(R5)                               
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
TESTTAB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,PHASELST                                                      
         ICM   R5,15,PHASECNT                                                   
         BZ    EXITEQ                                                           
*                                                                               
TT010    XC    PHASEINP,PHASEINP                                                
         MVC   PHASEINP(L'PHASELST),0(R4)                                       
*                                                                               
         CLI   RWRITE,C'Y'                                                      
         BNE   TT020                                                            
         GOTO1 VPHSCNT,DMCB,(C'A',PHASEINP),0                                   
*                                                                               
TT020    AHI   R4,L'PHASELST                                                    
         BCT   R5,TT010                                                         
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PRTREP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BTABCNT,BTABCNT                                                  
         XC    BTABCNT,BTABCNT                                                  
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),=AL4(DTPHSCNT)                                            
         OI    DUB,X'20'                                                        
         GOTO1 VLOCKSPC,DUB                                                     
         ICM   RF,15,4(R1)         PHASE LOAD TABLE DSPACE HEADER               
         BZ    PRX                 NO PHASE LOAD TABLE, STOP HERE               
         MVC   DSPHSCNT,0(RF)      SAVE HEADER FROM DSPACE                      
*                                                                               
         ICM   RF,15,VSSB          NEED THE SSB                                 
         BZ    PRX                 NO SSB, STOP HERE                            
         USING SSBOFFD,RF                                                       
         MVC   TBLET,SSOTBLET                                                   
         DROP  RF                                                               
*                                                                               
         OC    TBLET,TBLET                                                      
         BZ    PRX                 NO TABS ALET: STOP HERE                      
*                                                                               
         ICM   R2,15,DSPTFRST      GRAB THE TABLE ADDRESS                       
         BZ    PRX                 NO TABLE: STOP HERE                          
         STCM  R2,15,ATABHDR                                                    
*                                                                               
         SAM31                                                                  
         BRAS  RE,ARSR2                                                         
*                                                                               
         USING PHSTABD,R2                                                       
BT       USING BSPARA,PHSBBIN                                                   
BA       USING BSPARA,PHSIBIN                                                   
*                                                                               
         ICM   R1,15,BT.BSPSTRT                                                 
         BZ    PR040               NOT INITIALIZED                              
         STCM  R1,15,ABLDTAB                                                    
         MVC   BTABCNT,BT.BSPNOR                                                
*                                                                               
PR040    ICM   R1,15,BA.BSPSTRT                                                 
         BZ    PR050               NOT INITIALIZED                              
         STCM  R1,15,AINSTAB                                                    
         MVC   ITABCNT,BA.BSPNOR                                                
         DROP  BA,BT                                                            
                                                                                
*----------------------------------------------------------------------         
* PROCESS PREBUILT PHASE TABLE                                                  
*----------------------------------------------------------------------         
PR050    ICM   R3,15,BTABCNT                                                    
         BZ    PR070                                                            
         ICM   R2,15,ABLDTAB                                                    
         BZ    PR070                                                            
*                                                                               
         BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
         MVC   PHPHASE(5),=C'Phase'                                             
         MVC   PHON(17),=C'Online load count'                                   
         MVC   PHOFF(18),=C'Offline load count'                                 
         MVC   PLOFF+40(40),=CL40'*note: Phases preloaded into table'           
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
         SAM31                                                                  
         BRAS  RE,ARSR2                                                         
*                                                                               
         USING PHASED,R2                                                        
PR060    MVC   SOTPHS,PHASEKEY                                                  
         MVC   SOTON,PHASEON                                                    
         MVC   SOTOFF,PHASEOFF                                                  
*                                                                               
         BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
*                                                                               
         BRAS  RE,FILTER           FILTER FOR REPORTING                         
         BNE   PR068                                                            
*                                                                               
         MVC   PLPHASE,SOTPHS                                                   
         EDIT  SOTON,PLON,COMMAS=YES,ZERO=NOBLANK                               
         EDIT  SOTOFF,PLOFF,COMMAS=YES,ZERO=NOBLANK                             
         BRAS  RE,PRINTL                                                        
*                                                                               
PR068    SAM31                                                                  
         BRAS  RE,ARSR2                                                         
*                                                                               
         LA    R2,PHASELNQ(,R2)                                                 
         BCT   R3,PR060                                                         
                                                                                
*----------------------------------------------------------------------         
* PROCESS INSERTION TABLE                                                       
*----------------------------------------------------------------------         
PR070    ICM   R3,15,ITABCNT                                                    
         BZ    PRX                                                              
         ICM   R2,15,AINSTAB                                                    
         BZ    PRX                                                              
*                                                                               
         BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
         MVC   PHPHASE(5),=C'Phase'                                             
         MVC   PHON(17),=C'Online load count'                                   
         MVC   PHOFF(18),=C'Offline load count'                                 
         MVC   PLOFF+40(40),=CL40'*note: below inserted after build'            
         BRAS  RE,PRINTL                                                        
         MVC   PLINE,DASHES                                                     
         BRAS  RE,PRINTL                                                        
         SAM31                                                                  
         BRAS  RE,ARSR2                                                         
*                                                                               
         USING PHASED,R2                                                        
PR080    MVC   SOTPHS,PHASEKEY                                                  
         MVC   SOTON,PHASEON                                                    
         MVC   SOTOFF,PHASEOFF                                                  
         BRAS  RE,ARSOFF                                                        
         SAM24                                                                  
*                                                                               
         BRAS  RE,FILTER                                                        
         BNE   PR090                                                            
*                                                                               
         MVC   PLPHASE,SOTPHS                                                   
         EDIT  SOTON,PLON,COMMAS=YES,ZERO=NOBLANK                               
         EDIT  SOTOFF,PLOFF,COMMAS=YES,ZERO=NOBLANK                             
         MVC   PLINSERT,=C'INSERT'                                              
         BRAS  RE,PRINTL                                                        
*                                                                               
PR090    SAM31                                                                  
         BRAS  RE,ARSR2                                                         
*                                                                               
         LA    R2,PHASELNQ(,R2)                                                 
         BCT   R3,PR080                                                         
*                                                                               
PRX      BRAS  RE,ARSOFF                                                        
         B     EXITEQ                                                           
*----------------------------------------------------------------------         
* FILTER FOR STATISTICAL REPORT                                                 
*----------------------------------------------------------------------         
FILTER   NTR1                                                                   
*                                                                               
         CLI   SOTPHS,C' '         Convert name to EBCDIC if necessary          
         BH    FI002                                                            
         MVC   WORK(L'SOTPHS),SPACES                                            
         GOTO1 VHEXOUT,DMCB,SOTPHS,WORK,3                                       
         ICM   R1,15,DMCB+16                                                    
         BNZ   *+12                                                             
         MVI   WORK+7,C'?'                                                      
         B     *+8                                                              
         MVI   WORK,C'T'                                                        
         MVC   SOTPHS,WORK                                                      
*                                                                               
FI002    CLI   RCNT,X'FF'          Any universal count filter?                  
         BE    FI005               No                                           
         LLC   RF,RCNTC            Branch condition                             
         CLC   SOTON,RCNT          Online Value                                 
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FI005             Good: Continue                               
         CLC   SOTOFF,RCNT         Offline Value                                
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FI005             Good: Continue                               
         B     FINOX               Filter out                                   
*                                                                               
FI005    CLI   RONC,X'FF'          Any online count filter?                     
         BE    FI010               No                                           
         LLC   RF,RONCC            Branch condition                             
         CLC   SOTON,RONC          Value                                        
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FI010             Good: Continue                               
         B     FINOX               Filter out                                   
*                                                                               
FI010    CLI   ROFFC,X'FF'         Any offline count filter?                    
         BE    FI020               No                                           
         LLC   RF,ROFFCC           Branch condition                             
         CLC   SOTOFF,ROFFC        Value                                        
         EX    RF,*+8                                                           
         B     *+8                                                              
         BC    0,FI020             GOOD: CONTINUE                               
         B     FINOX               FILTER OUT                                   
*                                                                               
FI020    LA    R4,PHASELST                                                      
         ICM   R5,15,PHASECNT      ANY PHASE FILTERS?                           
         BZ    FI050                                                            
FI024    LHI   RE,L'PHASELST                                                    
         LA    RF,L'PHASELST-1(,R4)                                             
FI026    CLI   0(RF),C'*'                                                       
         BE    FI027                                                            
         AHI   RF,-1                                                            
         BCT   RE,FI026                                                         
         LHI   RE,L'PHASELST                                                    
         B     FI028                                                            
FI027    AHI   RE,-1                                                            
FI028    AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   SOTPHS(0),0(R4)     A MATCH ON A PHASE NAME FILTER?              
         BE    FI050               YES: KEEP THIS                               
         LA    R4,L'PHASELST(,R4)  NO: TRY NEXT                                 
         BCT   R5,FI024                                                         
         B     FINOX                                                            
*                                                                               
FI050    DS    0H                                                               
*                                                                               
FIOKX    B     EXITEQ                                                           
FINOX    B     EXITNE                                                           
                                                                                
***********************************************************************         
* PARAMETER HANDLING ROUTINE                                                    
***********************************************************************         
VALCARD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQ                                                           
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    VALC036                                                          
         BASR  RE,RF               GOTO ROUTINE                                 
         BNE   CERRPAR                                                          
         B     VALC500                                                          
*                                                                               
VALC036  TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BRAS  RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         ST    R1,0(RF)            SAVE FULLWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BRAS  RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         BZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         BNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALNSTA-PERVALD NEW CMPRSD DATE                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
         B     EXITEQ                                                           
*                                                                               
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
CERRPAR  LA    R1,=C'INVALID PARAM   '                                          
         B     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         BRAS  RE,PRINTL                                                        
         B     EXITLO                                                           
*----------------------------------------------------------------------         
* GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU                             
*----------------------------------------------------------------------         
VALTIME  NTR1  ,                                                                
*                                                                               
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BRAS  RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BRAS  RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BRAS  RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQ                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQ                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BRAS  RE,VALTADD                                                       
         B     EXITEQ                                                           
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
*                                                                               
       ++INCLUDE DDVALNUM                                                       
*----------------------------------------------------------------------         
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
* FLAGS  X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*----------------------------------------------------------------------         
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,10),X'0000',AL3(DDSIO)                          
         DC    C'DSPACE ',AL1(5,01),X'0000',AL3(DSPACE)                         
         DC    C'WRITE  ',AL1(4,03),X'0000',AL3(RWRITE)                         
         DC    C'MODE   ',AL1(3,10),X'8000',AL3(CMODE)                          
         DC    C'COUNT  ',AL1(4,10),X'6800',AL3(RCNTC)                          
         DC    C'ONC    ',AL1(2,10),X'6800',AL3(RONCC)                          
         DC    C'OFFC   ',AL1(3,10),X'6800',AL3(ROFFCC)                         
         DC    C'PHASE  ',AL1(4,08),X'8000',AL3(CPHSLIST)                       
         DC    C'NOTIFY ',AL1(5,60),X'0000',AL3(NOTYLIST)                       
         DC    X'0000'                                                          
                                                                                
***********************************************************************         
* ADD PHASE CARDS TO PHASE LIST FOR TEST LOAD                                   
***********************************************************************         
CMODE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'INIT',0(R2)                                                   
         BNE   CMODE1                                                           
         MVC   RMODE,=CL10'INITIALIZE'                                          
         B     EXITEQ                                                           
*                                                                               
CMODE1   CLC   =C'RESET',0(R2)                                                  
         BNE   CMODE2                                                           
         MVC   RMODE,=CL10'RESET'                                               
         B     EXITEQ                                                           
*                                                                               
CMODE2   CLC   =C'CLEAR',0(R2)                                                  
         BNE   CMODE3                                                           
         MVC   RMODE,=CL10'CLEAR'                                               
         B     EXITEQ                                                           
*                                                                               
CMODE3   CLC   =C'STAT',0(R2)                                                   
         BNE   CMODE4                                                           
         MVC   RMODE,=CL10'STATISTICS'                                          
         B     EXITEQ                                                           
*                                                                               
CMODE4   CLC   =C'REPORT',0(R2)                                                 
         BNE   CMODE5                                                           
         MVC   RMODE,=CL10'STATISTICS'                                          
         B     EXITEQ                                                           
*                                                                               
CMODE5   CLC   =C'TEST',0(R2)                                                   
         BNE   CMODE6                                                           
         MVC   RMODE,=CL10'TEST'                                                
         B     EXITEQ                                                           
*                                                                               
CMODE6   CLC   =C'KILL',0(R2)                                                   
         BNE   CMODE7                                                           
         MVC   RMODE,=CL10'KILL'                                                
         B     EXITEQ                                                           
*                                                                               
CMODE7   DS    0H                                                               
*                                                                               
CMODENEX B     EXITNE                                                           
                                                                                
***********************************************************************         
* ADD PHASE CARDS TO PHASE LIST FOR TEST LOAD                                   
***********************************************************************         
CPHSLIST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,PHASELST                                                      
         ICM   RF,15,PHASECNT                                                   
         MHI   RF,L'PHASELST                                                    
         AR    R1,RF                                                            
         MVC   0(L'PHASELST,R1),0(R2)                                           
         ICM   RF,15,PHASECNT                                                   
         AHI   RF,1                                                             
         STCM  RF,15,PHASECNT                                                   
         B     EXITEQ                                                           
                                                                                
***********************************************************************         
* COMMON STORAGE                                                                
***********************************************************************         
COMMON   DS    0D                                                               
*----------------------------------------------------------------------         
* EXITS                                                                         
*----------------------------------------------------------------------         
EXITNE   DS    0H                                                               
EXITLO   MVI   EXITCC,0                                                         
         J     EXITC                                                            
EXITHI   MVI   EXITCC,2                                                         
         J     EXITC                                                            
EXITEQ   MVI   EXITCC,1                                                         
EXITC    CLI   EXITCC,1                                                         
EXIT     XIT1                                                                   
*----------------------------------------------------------------------         
* PRINT ROUTINES                                                                
*----------------------------------------------------------------------         
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
ARSR2    LAM   AR0,ARF,ARZERO      SET AR2 AND TURN ON ACCESS REG MODE          
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         BR    RE                                                               
*                                                                               
ARSOFF   SAC   0                   TURN OFF ACCESS REGISTER MODE                
         SAM24                                                                  
         LAM   AR0,ARF,ARZERO      AND CLEAR ACCESS REGISTERS                   
         BR    RE                                                               
*----------------------------------------------------------------------         
* CONSTANTS AND LITERALS                                                        
*----------------------------------------------------------------------         
VDATAMGR DC    V(DATAMGR)                                                       
VLOCKSPC DC    V(LOCKSPC)                                                       
VCARDS   DC    V(CARDS)                                                         
VPERVAL  DC    V(PERVAL)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VDATCON  DC    V(DATCON)                                                        
VQSORT   DC    V(QSORT)                                                         
VPHSCNT  DC    V(PHSCNT)                                                        
*                                                                               
VSSB     DC    A(SSB)                                                           
TBLET    DC    A(0)                                                             
ATABHDR  DC    A(0)                                                             
ABLDTAB  DC    A(0)                                                             
AINSTAB  DC    A(0)                                                             
*                                  INPUT CARD AREAS                             
RCNTC    DC    XL1'00'             Universal count                              
RCNT     DC    XL4'FFFFFFFF'                                                    
RONCC    DC    XL1'00'             Online count                                 
RONC     DC    XL4'FFFFFFFF'                                                    
ROFFCC   DC    XL1'00'             Offline count                                
ROFFC    DC    XL4'FFFFFFFF'                                                    
RWRITE   DC    CL3'NO'                                                          
RMODE    DC    CL10'??????????'                                                 
NOTYLIST DC    CL60'AWIL'                                                       
*                                                                               
DSPACE   DC    CL1'?'                                                           
DDSIO    DC    CL10'DDSIO'                                                      
*                                                                               
MERROR   DC    CL10'**ERROR***'                                                 
MAXLINE  DC    P'60'                                                            
*                                                                               
SPACES   DC    166C' '                                                          
DASHES   DC    166C'-'                                                          
ARZERO   DC    16F'0'              USED TO CLEAR ACCESS REGISTERS               
*                                                                               
         LTORG                                                                  
*                                  DCBS & ADCONS                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=GM,RECFM=FB,               X        
               LRECL=80,BLKSIZE=80,EODAD=LOADX                                  
*                                                                               
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
         DC    0D                                                               
         DC    CL16'*UTL*UTL*UTL*UTL'                                           
UTL      DC    F'0',X'0A',XL3'00',XL252'00'                                     
         DC    0D                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
SSB      DC    X'0000'                                                          
         DC    X'FF'                                                            
         DC    AL1(SSOSNRCV)                                                    
         DC    1024X'00'                                                        
*                                                                               
         DS    16D                                                              
*                                                                               
PHASETAB DS    20000CL8                                                         
*                                                                               
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
AIO1     DS    A                   A(IO AREA 1)                                 
AIO2     DS    A                   A(IO AREA 2)                                 
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
EXITCC   DS    X                                                                
DMCB     DS    6F                                                               
PLIST    DS    6F                                                               
WORK     DS    CL64                                                             
LINE     DS    PL2                                                              
PAGE     DS    PL4                                                              
TODAY    DS    XL2                 TODAY                                        
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
KEY      DS    CL48                                                             
         DS    CL48                                                             
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
TOTCNT   DS    F                                                                
BTABCNT  DS    F                                                                
ITABCNT  DS    F                                                                
SOTPHS   DS    CL8                                                              
SOTON    DS    F                                                                
SOTOFF   DS    F                                                                
*                                                                               
ELEM     DS    XL256               GENERAL USE ELEMENT                          
*                                                                               
DSPHSCNT DC    XL64'00'            JOBS TABLE IN DATA SPACE                     
*                                                                               
PHASECNT DS    XL4                                                              
PHASELST DS    12CL8                                                            
*                                                                               
PHASEINP DS    CL16                                                             
*                                                                               
         DS    CL8                                                              
IOAREA1  DS    2048C               IO AREA 1                                    
         DS    CL8                                                              
IOAREA2  DS    2048C               IO AREA 1                                    
*                                                                               
SPARE    DS    1024X                                                            
WORKX    EQU   *                                                                
                                                                                
***********************************************************************         
* PRINT LINE                                                                    
***********************************************************************         
PLINED   DSECT                                                                  
         DS    CL4                                                              
PHPHASE  DS    CL8                                                              
         DS    CL8                                                              
PHON     DS    CL14                                                             
         DS    CL10                                                             
PHOFF    DS    CL14                                                             
         DS    CL4                                                              
PHINSERT DS    CL6                                                              
PHLNQ    EQU   *-PLINED                                                         
*                                                                               
         ORG   PLINED                                                           
         DS    CL4                                                              
PLPHASE  DS    CL8                                                              
         DS    CL8                                                              
PLON     DS    CL14                                                             
         DS    CL10                                                             
PLOFF    DS    CL14                                                             
         DS    CL4                                                              
PLINSERT DS    CL6                                                              
PLLNQ    EQU   *-PLINED                                                         
*                                                                               
         ORG   PLINED+30                                                        
PMSG     DS    CL44                                                             
PMLNQ    EQU   *-PLINED                                                         
                                                                                
***********************************************************************         
* OTHER INCUDED MEMBERS                                                         
***********************************************************************         
* DDPERVALD                                                                     
* CTGENFILE                                                                     
* SEACSFILE                                                                     
* DMSPACED                                                                      
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE FATABSDEQU                                                     
       ++INCLUDE DDBSPARA                                                       
       ++INCLUDE DDPHSCNTD                                                      
         PRINT ON                                                               
                                                                                
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DDPHSCNTM 05/08/20'                                      
         END                                                                    
