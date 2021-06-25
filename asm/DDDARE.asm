*          DATA SET DDDARE     AT LEVEL 005 AS OF 05/29/20                      
*PHASE DAREB                                                                    
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*INCLUDE XSORT                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE BINSRCH2  <<ONLY NEEDED TO TEST BINSRCH OF STATTABL>>                  
                                                                                
***********************************************************************         
*                                                                     *         
*  TITLE:        DARE DISPATCHER                                      *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- WORK                                           *         
*                R7 -- FOURTH PROGRAM BASE                            *         
*                R8 -- THIRD PROGRAM BASE                             *         
*                R9 -- SECOND PROGRAM BASE                            *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDDARE -- DARE DISPATCHER'                                      
                                                                                
***********************************************************************         
* PRNT MACRO                                                                    
***********************************************************************         
         MACRO                                                                  
&NAME    PRNT  &A                                                               
&NAME    DS    0H                                                               
         AIF   (T'&A EQ 'O').NOMOVE                                             
         MVC   P(17),=CL17'&A'                                                  
.NOMOVE  ANOP                                                                   
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   P+18(2),PRNTTIME                                                 
         MVI   P+20,C':'                                                        
         MVC   P+21(2),PRNTTIME+2                                               
         MVI   P+23,C':'                                                        
         MVC   P+24(2),PRNTTIME+4                                               
         MVI   P+26,C'.'                                                        
         MVC   P+27(2),PRNTTIME+6                                               
         GOTO1 =V(PRINTER)                                                      
         MEND                                                                   
                                                                                
***********************************************************************         
* DARE DISPATCHER                                                               
***********************************************************************         
DARE     CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**DARE**,=A(R13CHAIN),R9,R8,R7                                 
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RF,=V(PRNTER)       A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'MASTER  '  DDNAME=MASTER                
*                                                                               
         BAS   RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         MVI   UTL+4,X'0A'         CONTROL                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE NGENDIR NGENFIL X',A(IO),0                            
*                                                                               
         BAS   RE,INITIAL          INITIALIZE                                   
         BRAS  RE,OPENREP          OPEN REP FILES FOR XML ORDER                 
*                                                                               
         LA    R2,TSKTAB           A(SUBTASK TABLE)                             
         USING TSKTABD,R2                                                       
         CLI   TSKATTCH,C'Y'       ATTACH THIS SUBTASK?                         
         BNE   *+8                                                              
         BAS   RE,ATTACH           YES                                          
         LA    R2,TSKTABLQ(,R2)                                                 
         CLI   0(R2),X'FF'                                                      
         BNE   *-20                                                             
         DROP  R2                                                               
*                                                                               
         LA    R0,4                                                             
         LNR   R0,R0               R0 = -4                                      
         SVC   247                 SO DARE IS NOT SWAPPABLE                     
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   SVAOPECB+1(3),COMECBPT+1  SAVE A(OPERATOR'S ECB)                 
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS DARE BROUGHT UP WITH 'START'?            
         BNE   NOSTART                                                          
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
NOSTART  QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
         EJECT                                                                  
**********************************************************************          
* MAIN LOOP                                                                     
**********************************************************************          
MAINLOOP BAS   RE,WAIT             WAIT FOR SOMETHING TO HAPPEN                 
*                                                                               
         BAS   RE,CHKOPER          CHECK FOR OPERATOR INTERVENTION              
         CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP?                  
         BE    GOODBYE             YES -- IT'S ALL OVER                         
**********************************************************************          
* DETACH AND RE-ATTACH ANY SUBTASKS THAT HAVE ENDED UNEXPECTEDLY                
**********************************************************************          
         LA    R2,TSKTAB           TABLE OF SUBTASKS                            
         USING TSKTABD,R2                                                       
CHKTASK  CLI   TSKATTCH,C'N'       IS SUBTASK MEANT TO BE RE-ATTACHED?          
         BE    CHKNEXT             NO                                           
         TM    TSKECB,X'40'        IS THE SUBTASK RUNNING?                      
         BZ    CHKNEXT             YES                                          
         BAS   RE,DETACH           DETACH THE SUBTASK                           
         OC    TSKECB+1(3),TSKECB+1                                             
         BZ    REATTACH            SENDER ENDED CLEANLY                         
         MVC   OPMSG2(3),TSKNAME   SUBTASK ID                                   
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG2,OPMSG2)                     
REATTACH BAS   RE,ATTACH           RE-ATTACH THE SUBTASK                        
CHKNEXT  LA    R2,TSKTABLQ(,R2)    BUMP TO NEXT ENTRY                           
         CLI   0(R2),X'FF'                                                      
         BNE   CHKTASK                                                          
         B     MAINLOOP                                                         
*                                                                               
GOODBYE  BAS   RE,SHUTDOWN         DETACH ALL RUNNING SUBTASKS                  
         PRNT  ShutDownOK                                                       
         BRAS  RE,CLOSEREP         CLOSE REP FILES                              
         XBASE                                                                  
         EJECT                                                                  
**********************************************************************          
* SHUT DOWN PROCESS                                                             
**********************************************************************          
SHUTDOWN NTR1                                                                   
         LA    R2,TSKTAB           A(SUBTASK TABLE)                             
         USING TSKTABD,R2                                                       
SD10     CLI   TSKATTCH,C'N'       COULD SUBTASK BE ATTACHED NOW?               
         BE    SD20                NO                                           
         TM    TSKECB,X'40'        IS IT RUNNING NOW?                           
         BO    SD20                NO                                           
         BAS   RE,DETACH           DETACH SUBTASK                               
*                                                                               
SD20     LA    R2,TSKTABLQ(,R2)    CHECK OUT NEXT SUBTASK                       
         CLI   0(R2),X'FF'                                                      
         BNE   SD10                                                             
         DROP  R2                                                               
*                                                                               
         MVC   P(43),=C'ABOUT TO PERFORM ASYNC. MANAGER FOR CLEANUP'            
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         SAM31                                                                  
         L     RF,VATBAMR1                                                      
         CALL  (15),(FUNCTION,ASYNCHRONOUS_NUMBER,RETURN_CODE),VL               
         SAM24                                                                  
*                                                                               
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    SDX                                                              
         CLC   RETURN_CODE,=F'4'                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(13),=C'TP CLEANED UP'                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
SDX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                                    
***********************************************************************         
INITIAL  NTR1                                                                   
         MVC   TITLE(4),=C'DARE'                                                
         PRNT  *****************                                                
         PRNT  ***Initialize****                                                
         PRNT  *****************                                                
         L     R2,PSATOLD-PSA(,0)  TCB ADDRESS                                  
         GOTO1 =V(HEXOUT),DMCB,(R2),P+30,4,=C'TOG'                              
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         PRNT  TCBAddress                                                       
         BAS   RE,BLDTABLS         BUILD CONTROL TABLES                         
         BLDL  0,ENTRYPTS          BUILD TABLE OF APPC/MVS ENTRY POINTS         
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LOAD  DE=ATBAMR1                                                       
         ST    R0,VATBAMR1                                                      
         LOAD  DE=ATBEXAI                                                       
         ST    R0,VATBEXAI                                                      
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*======================================================================         
* READ CONTROL RECORDS AND BUILD TABLES.                                        
*======================================================================         
BLDTABLS NTR1                                                                   
         BAS   RE,BLDSTATB         BUILD STATION TABLE                          
         BAS   RE,BLDUIDTB         BUILD USERID TABLE                           
         BAS   RE,BLDOEXTB         BUILD OFFICE EXCEPTION TABLE                 
         BAS   RE,BLDMQTAB         BUILD MQ SERIES CONTROL TABLE                
         B     XIT                                                              
         EJECT                                                                  
*======================================================================         
* BUILD TABLE OF PRINT QUEUE INFORMATION.                                       
*======================================================================         
BLDPQTAB NTR1                                                                   
         PRNT  BuildPRTQTable                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'GLIST'),=C'PRTQUE',WORK,0,A(CXREC)            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R4,15,WORK+32       A(PRINT QUEUE LIST)                          
         ZIC   R3,0(R4)            NUMBER OF PRINT QUEUES                       
         L     R6,=A(CITABLE)      A(PRINT QUEUE TABLE)                         
         USING CIDATAD,R6                                                       
BP10     XC    0(CITBLLNQ,R6),0(R6)                                             
         LA    R4,8(R4)            BUMP TO NEXT PRINT QUEUE                     
         MVC   DUB(4),=C'PRTQ'     CONSTRUCT PRINT QUEUE NAME                   
         MVC   DUB+4(1),1(R4)                                                   
         MVC   CFPQENUM,4(R4)                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'BUFFER'),DUB,0,0,A(CXREC)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,=A(CXREC)                                                     
         MVC   CIDATA,12(RE)       CIDATA IS HERE                               
         L     RF,=V(DMENQDEQ)     V(DMENQDEQ)                                  
         ST    RF,CIENQDEQ                                                      
******************FOR BACKWARD COMPATIBLE**********************                 
         LHI   RF,24               LENGTH OF OLD PQ KEY                         
         CLI   CIDATA+16,0                                                      
         BNE   *+8                                                              
         LHI   RF,40               LENGTH OF NEW PQ KEY                         
         STH   RF,CINDXLN                                                       
         MVC   CFPQID,DUB                                                       
         MVC   CFPQINUM,0(R4)      PQ FILE INTERNAL NUMBER                      
******************FOR BACKWARD COMPATIBLE**********************                 
         MVC   CFPQSAVE,8(RE)      DISPLACEMENT TO PQ SAVE AREA                 
         LA    R6,CITBLLNQ(R6)                                                  
         BCT   R3,BP10             LOOK AT ALL PRINT QUEUES                     
         DROP  R6                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*=============================================================                  
* BUILD STATION TABLE                                                           
* +0 IS A(END OF TABLE)                                                         
* +4 IS NUMBER OF ENTRIES                                                       
* +8 IS FIRST ENTRY                                                             
*=============================================================                  
BLDSTATB NTR1                                                                   
*                                                                               
* READ GENFILE'S DSTA TO BUILD THE TABLE OF STATIONS/REPS                       
*                                                                               
         PRNT  BuildStationTable                                                
*                                                                               
         LA    R2,=C'DARE    '                                                  
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   ((2),(3),E,8)       ENQUEUE THE STATION TABLE                    
*                                                                               
         L     R3,=A(STATTABL)     A(STATION TABLE)                             
         XC    0(8,R3),0(R3)       CLEAR 8 BYTES                                
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(15,TODAY)                                 
         GOTO1 =V(DATCON),DMCB,(5,0),(3,BNGTODAY)                               
         XR    RF,RF                                                            
         ICM   RF,7,BNGTODAY                                                    
         LNR   RF,RF                                                            
         STCM  RF,7,BNGTODAY       WE NOW HAVE BINARY NEGATED TODAY             
*                                                                               
         MVC   DATADISP,=H'42'                                                  
         L     R3,=A(STATTABL)     A(STATION TABLE)                             
         USING STATTABD,R3                                                      
         LA    R3,8(R3)            POINT TO FIRST ENTRY                         
*                                                                               
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   STAKSYS,STAKSYSQ    SYSTEM                                       
         MVI   STAKTYP,STAKTYPQ    RECORD TYPE                                  
****     MVI   STAKMEDA,C'T'       READ TV RECORDS ONLY                         
         MVI   STAKMEDA,C'R'       READ ALL DARE STATIONS                       
*                                                                               
         XC    FULL,FULL           CLEAR COUNTER                                
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY,0                 
         B     BS20                                                             
*                                                                               
BS10     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'GENDIR',KEY,KEY,0                 
*                                                                               
BS20     CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,KEY                                                           
         CLI   0(R4),STAKSYSQ      DARE STATION RECORD?                         
         BNE   BS60                                                             
         CLI   1(R4),STAKTYPQ                                                   
         BNE   BS60                NO MORE RECORDS                              
*                                                                               
         CLI   STAKMEDA,0          DSTA WITH UNIQUE ID?                         
         BE    BS10                YES, NO LONGER SUPPORTING, SO SKIP           
*                                                                               
         CLI   STAKMEDA,C'R'       RADIO DSTA?                                  
         BNE   BS30                NO                                           
         CLI   STAKSTIN+4,C'S'     YES, STREAMING MEDIA (-SM)?                  
         BE    BS30                     YES, KEEP IT                            
         CLC   =C'YY',STAKSTIN          TEST STATION (BEGINS WITH YY)           
         BNE   BS10                     NO, SKIP IT                             
*                                                                               
BS30     CLC   STAKEFDA,BNGTODAY   DSTA EFF DATE 3 BYTE BINARY NEGATED          
         BL    BS10                EFF DATE IS IN THE FUTURE SO SKIP            
*                                                                               
         MVI   STATMED,X'FF'       PREFILL ENTRY                                
         MVC   STATMED+1(STATTBLQ-1),STATMED                                    
*                                                                               
         MVC   STATMED,STAKMEDA    MEDIA                                        
         MVC   STATSTN,STAKSTIN    STATION CALLS (WITH 1 BYTE OF C' ')          
         MVI   STATSTN+5,C' '        GETS RID OF 1ST BYTE OF DATE               
         MVC   STATEFDT,STAKEFDA   DSTA EFF DATE 3 BYTE BINARY NEGATED          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,A(IO),    +        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SAVEKEY,SAVEKEY     TO REESTABLISH READ SEQUENCE                 
         L     R4,=A(IO)                                                        
*                                                                               
         MVI   ELCODE,STASTACQ     NEW STATION CALL LETTER ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   BS40                NO NEW CALL LETTERS FOR THIS STATION         
*                                                                               
         USING STASTAD,R4                                                       
         CLC   STASTADT,TODAY      NEW CALL EFF DATE IN THE FUTURE?             
         BH    BS40                YES, SO WE DON'T CARE ABOUT IT               
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY SO WE CAN DO SEQ LATER              
         LA    RF,KEY              SAME KEY WITH NEW CALL LETTERS               
         MVC   STAKSTIN-STAKEYD(,RF),STASTA   MOVE STATION AND CLEAR            
         XC    STAKEFDA-STAKEYD(L'STAKEFDA,RF),STAKEFDA-STAKEYD(RF)             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 =V(DATCON),DMCB,(6,STASTADT),(3,DUB)                             
         XR    RE,RE               DSTA EFFECTIVE DATE IS NEGATED NOT           
         ICM   RE,7,DUB               FF'D SO WE'RE LOOKING FOR A DSTA          
         LNR   RE,RE                  WHOSE DATE IS OLDER THAN THE CALL         
         STCM  RE,7,DUB               SWITCH DATE                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEYSAVE,KEY,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY(STAKEFDA-STAKEYD),KEYSAVE  THE WANTED CALL LETTERS?          
         BNE   BS40                           NEW CALL LETTERS MISSING          
         CLC   KEY+STAKEFDA-STAKEYD(L'STAKEFDA),DUB                             
         BNL   BS35                & DSTA EFFECTIVE BEFORE CALL SWITCH          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'GENDIR',KEYSAVE,KEY,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BS35     MVC   STATNSTN,STASTA     PUT NEW STATION CALL IN TABLE                
         MVC   STATNSTD,STASTADT   PUT EFFECTIVE DATE IN TABLE                  
         DROP  R4                                                               
*                                                                               
BS40     L     R4,=A(IO)                                                        
         MVI   ELCODE,STAREPCQ     REP ELEMENT                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ELEMENT IS REQUIRED                          
*                                                                               
         USING STAREPD,R4                                                       
         MVC   STATREP,STAREPCR    CURRENT REPCODE                              
         CLC   STAREPED,TODAY                                                   
         BNH   *+10                                                             
         MVC   STATREP,STAREPPR    PREVIOUS REPCODE                             
         DROP  R4                                                               
*                                                                               
         XC    STATCITY,STATCITY   ASSUME NO HOME MARKET CITY                   
         XC    STATUID,STATUID     ASSUME NO HOME MARKET USERID                 
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,STAHOMCQ     HOME MARKET ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   BS45                                                             
*                                                                               
         USING STAHOMD,R4                                                       
         MVC   STATCITY,STAHOMCT   HOME MARKET CITY CODE                        
         MVC   STATUID,STAHOMIC    HOME MARKET RECEIVING DDS USERID             
         DROP  R4                                                               
*                                                                               
BS45     XC    STATFLG1,STATFLG1                                                
         L     R4,=A(IO)                                                        
         MVI   ELCODE,STAH2CQ      HIDDEN RECIEVING ID                          
         BAS   RE,GETEL                                                         
         BNE   BS50                                                             
*                                                                               
         USING STAH2D,R4                                                        
         MVC   STATUID,STAH2IC     HOME MARKET RECEIVING DDS USERID             
         OI    STATFLG1,SF1H2UID   ONLY ALLOWABLE TO SPECIAL AGENCIES           
         DROP  R4                                                               
*                                                                               
BS50     OC    SAVEKEY,SAVEKEY     RESTORE READ SEQUENCE IF NECESSARY           
         BZ    BS55                                                             
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'GENDIR',SAVEKEY,KEY,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                WE JUST READ THIS UP ABOVE!                  
*                                                                               
BS55     LA    R3,STATTBLQ(,R3)    BUMP TO NEXT SLOT IN TABLE                   
         L     RE,FULL                                                          
         AHI   RE,1                BUMP RECORD COUNT                            
         ST    RE,FULL                                                          
         C     R3,=A(STATTBLX)                                                  
         BNH   BS10                NEXT RECORD                                  
         DC    H'0'                MUST INCREASE MAXSTNS                        
                                                                                
***********************************                                             
* ANY ENTRY IN THE TABLE THAT HAS A NEW STATION FOR AN EFFECTIVE DATE           
* LESS THAN OR EQUAL TO TODAY, MEANS THAT THE CORRESPONDING STATION'S           
* ENTRY SHOULD HAVE THE OLD STATION IN NEW STATION POSITION WITHOUT             
* AN EFFECTIVE DATE.                                                            
***********************************                                             
                                                                                
BS60     MVI   STATMED,X'FF'       ADD A X'FF' ENTRY TO TBALE                   
         MVC   STATMED+1(STATTBLQ-1),STATMED                                    
         LA    R3,STATTBLQ(,R3)    POINT TO NEW END OF TABLE                    
*                                                                               
         L     RE,FULL             ADD 1 TO ENTRY COUNT                         
         LA    RE,1(RE)                                                         
         ST    RE,FULL                                                          
*                                                                               
         L     RE,=A(STATTABL)                                                  
         ST    R3,0(RE)            SET A(LAST ENTRY) AT TOP OF TABLE            
         MVC   4(4,RE),FULL        SAVE NUMBER OF ENTRIES                       
*                                                                               
         L     R3,=A(STATTABL)                                                  
         L     R4,4(R3)             GET NUMBER OF ENTRIES                       
         LA    R3,8(R3)             POINT TO FIRST ENTRY                        
*                                                                               
BS61     OC    0(STATTBLQ,R3),0(R3) ANY MORE STATION ENTRIES?                   
         BZ    BS70                 NO, NO MORE                                 
         CLC   STATNSTN,=5X'FF'     ANYTHING IN THE NEW STATION?                
         BE    BS66                 NO, CHECK NEXT ENTRY                        
*                                                                               
         CLC   STATNSTD,=5X'FF'    HAVE A NEW CALLS EFFECTIVE DATE?             
         BE    BS66                NO, THIS IS AN OLD CALLS                     
*                                                                               
         GOTO1 =V(DATCON),DMCB,(6,STATNSTD),(3,DUB)                             
         XR    RE,RE               DSTA EFFECTIVE DATE IS NEGATED NOT           
         ICM   RE,7,DUB               FF'D SO WE'RE LOOKING FOR A DSTA          
         LNR   RE,RE                  WHOSE DATE IS OLDER THAN THE CALL         
         STCM  RE,7,DUB               SWITCH DATE                               
*                                                                               
         L     RE,=A(STATTABL)                                                  
         L     RF,4(RE)               GET NUMBER OF ENTRIES                     
*                                                                               
         LA    RE,8(RE)               POINT TO FIRST ENTRY                      
BS60D    USING STATTABD,RE                                                      
*                                                                               
BS62     CLI   BS60D.STATSTN,X'FF'                                              
         BE    BS62DIE                                                          
         CLC   STATNSTN,BS60D.STATSTN   FOUND THE MATCHING ENTRY?               
         BE    BS64                                                             
*                                                                               
BS63     LA    RE,STATTBLQ(RE)                                                  
         BCT   RF,BS62                                                          
*                                                                               
BS62DIE  DC    H'0'                                                             
*                                                                               
BS64     CLC   BS60D.STATEFDT,DUB   NEW STATION DSTA EFFECTIVE BEFORE?          
         BL    BS63                 THIS DSTA IS NEWER THAN CALL SWITCH         
*                                                                               
         CLC   BS60D.STATNSTN,=5X'FF'   NEW STATION ALREADY IN DSTA?            
         BNE   BS66                     YES, SKIP IT                            
         MVC   BS60D.STATNSTN,STATSTN                                           
         DROP  BS60D                                                            
*                                                                               
BS66     LA    R3,STATTBLQ(R3)                                                  
         BCT   R4,BS61                                                          
         DROP  R3                                                               
                                                                                
*===========================================================                    
* SORT THE STATION TABLE SO BINSRCH WILL WORK                                   
*===========================================================                    
                                                                                
BS70     L     R3,=A(STATTABL)     A(STATION TABLE)                             
         LA    R0,8(R3)                                                         
         ST    R0,DMCB             SET A(FIRST ENTRY)                           
         MVC   DMCB+4(4),4(R3)     RECORD COUNT                                 
         LA    R0,STATTBLQ         RECLEN                                       
         ST    R0,DMCB+8                                                        
         ST    R0,DMCB+12          RECLEN=KEYLEN                                
         SR    R0,R0                                                            
         ST    R0,DMCB+16          KEY DSPL                                     
         GOTO1 =V(XSORT),DMCB                                                   
*                                                                               
BSX      LA    R2,=C'DARE    '                                                  
         LA    R3,=C'STATTABL'                                                  
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   ((2),(3),8)         DEQUEUE THE STATION TABLE                    
                                                                                
*==============================================================                 
* CODE BELOW ONLY USED TO TEST BINSRCH ON NEW TABLE                             
*==============================================================                 
                                                                                
*&&DO                                                                           
         XC    MYKEY,MYKEY                                                      
         MVI   MYKEY,C'T'                                                       
         MVC   MYKEY+1(6),MYSTA                                                 
*                                                                               
         L     R2,=A(STATTABL)     A(STATION TABLE)                             
         L     R0,4(R2)            RECORD COUNT                                 
         XC    DMCB,DMCB                                                        
         L     RF,=V(BINSRCH)                                                   
         GOTO1 (RF),DMCB,(X'02',MYKEY),8(R2),(R0),STATTBLQ,(0,10),(R0)          
         B     XIT                                                              
         DS    0D                                                               
MYKEY    DS    CL64                                                             
MYSTA    DC    CL6'ARWB  '                                                      
*&&                                                                             
         B     XIT                                                              
                                                                                
***********************************************************************         
* READ CTFILE TO BUILD THE TABLE OF DARE USERIDS                                
***********************************************************************         
BLDUIDTB NTR1                                                                   
         PRNT  BuildUserIDTable                                                 
*                                                                               
         LA    R2,=C'DARE    '                                                  
         LA    R3,=C'UIDTABL'                                                   
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(7),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   ((2),(3),E,7)       ENQUEUE THE USERID TABLE                     
*                                                                               
         L     R0,=A(UIDTABL)                                                   
         L     RE,=A(UIDTABLX)                                                  
         LR    R1,RE               R1 = L'AREA TO FILL WITH X'FF'S              
         SR    R1,R0                                                            
         XR    RF,RF               AT UIDTABLX WE HAVE A X'FF'                  
         ICM   RF,8,0(RE)          USE IT FOR THE FILL CHARACTER                
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DATADISP,=H'28'                                                  
         L     R3,=A(UIDTABL)      A(USERID TABLE)                              
         USING UIDTABD,R3                                                       
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ    ID RECORDS                                   
         MVI   CTIKID,X'01'        ONLY LOOK AT ALPHA-KEYED RECORDS             
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,A(IO)                 
         B     BU20                                                             
*                                                                               
BU10     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'CTFILE',KEY,A(IO)                 
*                                                                               
BU20     CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,=A(IO)                                                        
         CLI   0(R4),CTIKTYPQ      ID RECORD?                                   
         BNE   BUX                                                              
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTUSAELQ     DARE ELEMENT                                 
         BAS   RE,GETEL                                                         
         BNE   BU10                NOT THERE -- SKIP THIS RECORD                
*                                                                               
         CLI   CTUSADPI-CTUSAD(R4),0                                            
         BE    BU10                NO DARE PARTNER                              
         MVC   UIDPART,CTUSADPI-CTUSAD(R4)                                      
         MVC   UIDMQID,CTUSADMQ-CTUSAD(R4)                                      
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTDSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                ELEMENT IS REQUIRED                          
         MVC   UIDNUM,CTDSC-CTDSCD(R4)   USERID NUMBER                          
*                                                                               
         L     R4,=A(IO)                                                        
         MVC   UIDNAME,CTIKID-CTIREC(R4) ALPHA USERID                           
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTAGYELQ     ALPHA ID ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   BU50                                                             
         MVC   UIDREPCD,CTAGYID-CTAGYD(R4)                                      
*                                                                               
BU50     DS    0H                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,CTSYSELQ     SYS AUTH ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   BU80                                                             
BU60     CLI   CTSYSNUM-CTSYSD(R4),X'08' REP SYSTEM?                            
         BE    BU70                                                             
         BAS   RE,NEXTEL                                                        
         BE    BU60                                                             
         B     BU80                                                             
*                                                                               
BU70     DS    0H                                                               
         MVC   UIDREPFN,CTSYSSE-CTSYSD(R4)                                      
*                                                                               
BU80     DS    0H                                                               
         LA    R3,UIDTBLQ(,R3)     BUMP TO NEXT SLOT IN TABLE                   
         C     R3,=A(UIDTABLX)                                                  
         BNE   BU10                NEXT RECORD                                  
         DC    H'0'                MUST INCREASE MAXUIDS                        
         DROP  R3                                                               
*                                                                               
BUX      LA    R2,=C'DARE    '                                                  
         LA    R3,=C'UIDTABL'                                                   
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(7),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   ((2),(3),7)         DEQUEUE THE USERID TABLE                     
*                                                                               
         B     XIT                                                              
                                                                                
***********************************************************************         
* READ GENFILE TO BUILD THE TABLE OF OFFICE EXCEPTIONS                          
***********************************************************************         
BLDOEXTB NTR1                                                                   
         PRNT  BuildExceptionTab                                                
*                                                                               
         LA    R2,=C'DARE    '                                                  
         LA    R3,=C'OFEXTABL'                                                  
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   ((2),(3),E,8)       ENQUEUE THE OFFICE EXCEPTION TABLE           
*                                                                               
         L     R0,=A(OFEXTABL)                                                  
         L     RE,=A(OFEXTBLX)                                                  
         LR    R1,RE               R1 = L'AREA TO FILL WITH X'FF'S              
         SR    R1,R0                                                            
         XR    RF,RF               AT OFEXTBLX WE HAVE A X'FF'                  
         ICM   RF,8,0(RE)          USE IT FOR THE FILL CHARACTER                
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DATADISP,=H'42'                                                  
         L     R3,=A(OFEXTABL)     A(OFFICE EXCEPTION TABLE)                    
         USING OFEXTABD,R3                                                      
         LA    R4,KEY                                                           
         USING AGRKEYD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   AGRKSYS,AGRKSYSQ    SYSTEM                                       
         MVI   AGRKTYP,AGRKTYPQ    RECORD TYPE                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY,0                 
         B     OE20                                                             
*                                                                               
OE10     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'GENDIR',KEY,KEY,0                 
*                                                                               
OE20     CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,KEY                                                           
         CLI   0(R4),AGRKSYSQ      OFFICE EXCEPTION RECORD?                     
         BNE   OEX                                                              
         CLI   1(R4),AGRKTYPQ                                                   
         BNE   OEX                 NO MORE RECORDS                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,A(IO),    +        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,AGROVRCQ     OVERRIDE ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   OE10                NO OVERRIDES -- SKIP THIS RECORD             
*                                                                               
         USING AGROVRD,R4                                                       
OE30     MVC   OFEXREP,AGROVRCR    REP CODE                                     
         MVC   OFEXOFF,AGROVROF    OFFICE OVERRIDE                              
         DROP  R4                                                               
*                                                                               
         L     RF,=A(IO)                                                        
         MVC   OFEXMED,AGRKMEDA-AGRKEYD(RF)    MEDIA                            
         MVC   OFEXROUT,AGRKAGRT-AGRKEYD(RF)   ROUTING CODE                     
*                                                                               
         LA    R3,OFEXTBLQ(,R3)    BUMP TO NEXT SLOT IN TABLE                   
         C     R3,=A(OFEXTBLX)                                                  
         BNE   *+6                                                              
         DC    H'0'                MUST INCREASE MAXOFEXS                       
         DROP  R3                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    OE30                                                             
         B     OE10                NEXT RECORD                                  
*                                                                               
OEX      LA    R2,=C'DARE    '                                                  
         LA    R3,=C'OFEXTABL'                                                  
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(8),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   ((2),(3),8)         DEQUEUE THE OFFICE EXCEPTION TABLE           
*                                                                               
         B     XIT                                                              
                                                                                
***********************************************************************         
* BUILD MQ SERIES CONTROL TABLE                                                 
***********************************************************************         
BLDMQTAB NTR1                                                                   
         PRNT  BuildMQTable                                                     
*                                                                               
         LA    R2,=C'DARE    '                                                  
         LA    R3,=C'MQTABLE'                                                   
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(7),0(R3)                                                    
         PRNT  Enqueue                                                          
         ENQ   ((2),(3),E,7)       ENQUEUE THE MQ CONTROL TABLE                 
*                                                                               
         L     R0,=A(MQTABLE)                                                   
         L     RE,=A(MQTABLEX)                                                  
         LR    R1,RE               R1 = L'AREA TO FILL WITH X'FF'S              
         SR    R1,R0                                                            
         XR    RF,RF               AT MQTABLEX WE HAVE A X'FF'                  
         ICM   RF,8,0(RE)          USE IT FOR THE FILL CHARACTER                
         MVCL  R0,RE                                                            
*                                                                               
         MVC   DATADISP,=H'42'                                                  
         L     R3,=A(MQTABLE)      A(MQ CONTROL TABLE)                          
         USING MQTABLED,R3                                                      
         LA    R4,KEY                                                           
         USING MQDEFD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   MQDKSYS,MQDKSYSQ    MQ DEFINITION RECORD TYPE                    
         MVI   MQDKTYP,MQDKTYPQ                                                 
         MVC   MQDAPPL,=CL8'DARE'  APPLICATION: DARE                            
         MVI   MQDOTYP,MQDOTYPQ    OBJECT TYPE: QUEUE                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY,KEY,0                 
         B     MQ20                                                             
*                                                                               
MQ10     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'GENDIR',KEY,KEY,0                 
*                                                                               
MQ20     CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,KEY                                                           
         CLI   0(R4),MQDKSYSQ      MQ DEFINITION RECORD?                        
         BNE   MQX                                                              
         CLI   1(R4),MQDKTYPQ                                                   
         BNE   MQX                                                              
         CLC   MQDAPPL,=CL8'DARE'  ANY MORE FOR DARE?                           
         BNE   MQX                                                              
         CLI   MQDOTYP,MQDOTYPQ    ANY MORE QUEUES?                             
         BNE   MQX                                                              
*                                                                               
         MVC   MQQIDNUM,MQDID      MQ QUEUE ID NUMBER                           
         XC    MQQIDNUM,=X'FFFF'   TAKE 1'S COMPLEMENT                          
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'GENFIL',KEY+36,A(IO),    +        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,MQDGENCQ     GENERAL ELEMENT                              
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MISSING ELEMENT IN RECORD                    
         MVC   MQQOUTVR,MQDGENOV-MQDGEND(R4)   OUTPUT VERSION                   
         MVC   MQQOFLAG,MQDGENFL-MQDGEND(R4)   Q FLAGS                          
*                                                                               
         L     R4,=A(IO)                                                        
         MVI   ELCODE,MQDNAMCQ     QUEUE NAME ELEMENT                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MISSING ELEMENT IN RECORD                    
         MVC   MQQNAME,MQDNAME-MQDNAMD(R4)   MQ QUEUE NAME                      
*                                                                               
*********************************************************************           
         L     RF,=A(SSB)                                                       
         CLI   SSODSPAC-SSOOFF(RF),C'Q'      DARE FOR FQA                       
         BNE   MQ50                                                             
         CLC   MQQNAME,=CL48'MO.QA21.REMOTEQ'                                   
         BNE   MQ50                                                             
         MVC   MQQNAME,=CL48'MO.QA25.REMOTEQ' OVERRIDE QUEUE NAME               
MQ50     EQU   *                                                                
*********************************************************************           
*                                                                               
         LA    R3,MQTABLEQ(,R3)    BUMP TO NEXT SLOT IN TABLE                   
         C     R3,=A(MQTABLEX)                                                  
         BNE   *+6                                                              
         DC    H'0'                MUST INCREASE MAXMQS                         
         DROP  R3                                                               
*                                                                               
         B     MQ10                NEXT RECORD                                  
*                                                                               
MQX      LA    R2,=C'DARE    '                                                  
         LA    R3,=C'MQTABLE'                                                   
         MVC   P+30(8),0(R2)                                                    
         MVC   P+40(7),0(R3)                                                    
         PRNT  Dequeue                                                          
         DEQ   ((2),(3),7)         DEQUEUE THE MQ CONTROL TABLE                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
                                                                                
*======================================================================         
* READ ALL PRINT QUEUE INDEXES ONCE, SO THEY ARE TOUCHED.                       
* THIS CAN BE REMOVED ONCE DMENQDEQ IS RE-ENTRANT.                              
*======================================================================         
INITPQS  NTR1                                                                   
*                                                                               
         PRNT  InitPrintQueues                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GLIST'),=C'PRTQUE',WORK,0,A(CXREC)            
         ICM   R4,15,WORK+32                                                    
         LA    R4,8(R4)                                                         
*                                                                               
SCANPQ10 MVC   PQNM+4(1),1(R4)     SAVE EBCDIC PQ#                              
         L     RE,=A(CXREC)                                                     
         XCEFL (RE),14336          CLEAR PQ INDEX BUFFER                        
*                                                                               
         XC    PQINDEX,PQINDEX     START FROM BEGINNING OF INDEX                
         GOTO1 DATAMGR,DMCB,(0,=C'INDEX'),PQNM,PQINDEX,WORK,A(CXREC)            
*                                                                               
         LA    R4,8(R4)            BUMP TO NEXT PRINT QUEUE                     
         CLI   0(R4),0                                                          
         BNE   SCANPQ10                                                         
         B     XIT                                                              
*                                                                               
PQNM     DC    C'PRTQ '                                                         
PQINDEX  DS    XL40                                                             
         EJECT                                                                  
*======================================================================         
* OPEN/CLOSE ALL REP FILES FOR XML ORDER ROUTING VERIFICATION                   
*======================================================================         
OPENREP  MVI   OCFLAG,C'O'                                                      
         J     OPSYS                                                            
*                                                                               
CLOSEREP MVI   OCFLAG,C'C'                                                      
*                                                                               
OPSYS    NTR1                                                                   
         CLI   OCFLAG,C'C'                                                      
         BE    OPSYS50                                                          
         XC    KEY,KEY                                                          
         USING CTWREC,R2           READ CONTROL FILE                            
         LA    R2,KEY                                                           
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         MVI   CTWKSYSN,CTWKREP                                                 
         LARL  R2,IO                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,(R2)                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T READ SYSTEM LIST RECORD                
*                                                                               
         LA    R5,REPLIST          OPEN THESE                                   
         LA    R1,CTWDATA                                                       
         USING SYSELD,R1                                                        
OPSYS10  CLI   SYSEL,0             TEST EOR                                     
         BE    OPSYS50                                                          
         CLI   SYSEL,SYSELQ        TEST SYSTEM ELEMENT                          
         BNE   OPSYS18                                                          
*                                                                               
         LA    R4,NOOPREPS         IGNORE THESE                                 
OPSYS12  CLI   0(R4),C' '          END OF NOOP LIST?                            
         BE    OPSYS16                                                          
         CLC   SYSNAME+3(2),0(R4)  COMPARE FOR ONE/TWO CHARACTER SYSTEM         
         BE    OPSYS18             SKIP THIS ONE                                
         AHI   R4,2                CHECK FOR ANOTHER                            
         B     OPSYS12                                                          
*                                                                               
OPSYS16  MVC   0(1,R5),SYSSEN      MOVE IN SE# (REP) TO OPEN                    
         AHI   R5,1                NEXT AREA TO PUT NEW ONE                     
*                                                                               
OPSYS18  LLC   R0,SYSLEN           BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         B     OPSYS10                                                          
         DROP  R1,R2                                                            
*                                                                               
OPSYS50  MVC   SVUTL,UTL+4         SAVE OFF CURRENT UTL+4                       
         LA    R4,REPLIST                                                       
*                                                                               
OPSYS60  CLI   0(R4),X'FF'                                                      
         BE    OPSYSXIT                                                         
         MVC   UTL+4(1),0(R4)                                                   
*                                                                               
         LA    R5,=C'DMCLSE'                                                    
         SR    R6,R6                                                            
         CLI   OCFLAG,C'C'         OPEN OR CLOSE REQUEST?                       
         BE    OPSYS70                                                          
         LA    R5,=C'DMOPEN'                                                    
         LA    R6,REPFILES                                                      
         CLI   OCFLAG,C'O'                                                      
         BNE   OPSYSXIT                                                         
*                                                                               
OPSYS70  GOTO1 DATAMGR,DMCB,(R5),REPSYS,(R6),A(IO),0                            
         AHI   R4,1                NEXT SYSTEM TO OPEN OR CLOSE                 
         B     OPSYS60                                                          
*                                                                               
OPSYSXIT MVC   UTL+4(1),SVUTL      RESTORE UTL+4                                
         B     XIT                                                              
*                                                                               
REPSYS   DC    CL8'REP'                                                         
REPFILES DC    C'NREPFIL NREPDIR X'                                             
         DS    0F                  THIS IS FOR LARL INTRUCTION                  
NOOPREPS DC    CL40'  '                                                         
REPLIST  DC    16X'FF'                                                          
*                                                                               
OCFLAG   DS    C                                                                
SVUTL    DS    X                                                                
         EJECT                                                                  
**********************************************************************          
* ATTACH THE SUBTASK WHOSE TABLE ENTRY IS POINTED TO BY R2.                     
**********************************************************************          
ATTACH   NTR1                                                                   
         USING TSKTABD,R2                                                       
         MVC   P+30(3),TSKNAME                                                  
         PRNT  AttachingSubTask                                                 
         XC    TSKECB,TSKECB       CLEAR PRIMARY SUBTASK ECB                    
         XC    TSKECBSS,TSKECBSS   CLEAR ECB TO STOP SENDING SUBTASK            
         XC    TSKECBPQ,TSKECBPQ   CLEAR ECB TELLING SENDER TO SCAN PQS         
         CLI   TSKMETH,METHAPPC    APPC SUBTASK?                                
         BNE   *+10                NO                                           
         MVC   TSKLUID,LOCLLUID    THIS PROGRAM'S LUID                          
         MVC   TSKTSMF,TESTSMF     SET TEST SFM RECORD FLAG                     
         MVC   TSKDRTST,DRTEST     DISASTER RECOVERY TEST MODE FLAG             
         MVC   TSKATRCR,ATTRCVR    ATTACH RECEIVER=NO FLAG                      
         LARL  RF,REPLIST          PASS LIST OF REP FILES TO OPEN               
         ST    RF,TSKFILES                                                      
         L     RF,=A(SSB)                                                       
         MVC   TSKDSPC,SSODSPAC-SSOOFF(RF)  DSPACE                              
         LA    R3,TSKECB                                                        
         LA    R4,TSKSNDR          R4 = A(SENDING MODULE)                       
         LA    R1,TSKTAB           SUBTASK PARAMETER IS A(TASK TABLE)           
         ICM   R1,8,TSKSNAME       . . . HOB TELLS HIM WHICH ONE HE IS          
         ATTACH EPLOC=(R4),ECB=(3),SZERO=NO,ALCOPY=YES                          
         ST    R1,TSKTCB                                                        
         OC    TSKTCB,TSKTCB                                                    
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
         PRNT  AttachComplete                                                   
         MVI   TSKATTCH,C'Y'       SET ATTACH FLAG                              
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC SUBTASK?                                
         BNE   ATT10               NO                                           
         MVC   OPMSG3+25(3),TSKNAME                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG3,OPMSG3)                     
         B     ATTX                                                             
*                                                                               
ATT10    MVC   OPMSG5+8(3),TSKNAME                                              
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG5,OPMSG5)                     
*                                                                               
ATTX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* DETACH THE SUBTASK WHOSE TABLE ENTRY IS POINTED TO BY R2.                     
**********************************************************************          
DETACH   NTR1                                                                   
         USING TSKTABD,R2                                                       
         CLI   TSKATTCH,C'Y'       COULD THIS SUBTASK BE ATTACHED?              
         BNE   DETX                                                             
         MVC   P+30(3),TSKNAME                                                  
         PRNT  DetachingSubTask                                                 
         POST  TSKECBSS            TELL SENDING SUBTASK TO STOP                 
         PRNT  WaitingForStop                                                   
         WAIT  ECB=TSKECB          WAIT FOR ANSWER FROM SUBTASK                 
         TM    TSKECB,X'40'        SUBTASK TERMINATED?                          
         BO    *+6                                                              
         DC    H'0'                                                             
         PRNT  Detaching                                                        
         DETACH TSKTCB             DETACH SUBTASK                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         PRNT  DetachComplete                                                   
         MVI   TSKATTCH,C'N'       RESET ATTACH FLAG                            
*                                                                               
DETX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
* READ PARAMETER CARDS                                                          
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   DSPACE=X         DSPACE CONTROL CARD                                        
*   DDSIO=DDSIO?     DDSIO  CONTROL CARD                                        
*   LOCALLUID=CCCCCCCC   APPC/MVS LUID OF THIS PROGRAM                          
*   MQQMGRNAME=      MQ SERIES QUEUE MANAGER NAME                               
*   MQINPUTQ=        MQ SERIES INPUT QUEUE FROM DARE PARTNERS                   
*   MQREPLYQ=        MQ SERIES REPLY QUEUE FOR DARE DISPATCHER                  
*   MQLOGQ=          MQ LOG QUEUE FOR BOTH APPC/MQ PARTNERS                     
*   MQLOGQ2=         MQ LOG QUEUE FOR BOTH APPC/MQ PARTNERS (2ND)               
*   MQLOGQB=         MQ BLOCKCHAIN PROCESSING QUEUE                             
*   MQDEADQ=         MQ DEAD LETTER QUEUE FOR MQ PARTNERS                       
*   ATTACH=CCC       TASK ID TO ATTACH (CAN HAVE MULTIPLE OF THESE)             
*                                                                               
*   THE FOLLOWING PARAMETERS ARE REALLY FOR TESTING/DEBUGGING ONLY              
*                                                                               
*   ATTACHRCVR=NO    ATTACH RECEIVER BUT DON'T RECEIVE ANYTHING                 
*                                                                               
*   TESTTPNAME=YES   USE DDS INTERNAL TEST TPNAMES (DEFAULT = NO)               
*   TESTSMF=YES      SMF RECORDS GET MARKED AS TEST RECORDS (DEF. = NO)         
*   DRTEST=YES       DISASTER RECOVERY TEST MODE (DEFAULT = NO)                 
*   SETALLOCATETIMER=Y/N  HONOR ALLOCATE TIMEOUTS (DEFAULT = YES)               
*                                                                               
***********************************************************************         
READCRDS NTR1                                                                   
RC10     GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BE    RCX                                                              
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC10                YES                                          
*                                                                               
         LA    RE,CARD             BLANK DELIMITS A COMMENT                     
         LA    R0,C' '                                                          
         SRST  R0,RE               R0 = A(FIRST BLANK)                          
         LR    R1,R0                                                            
         LA    R1,1(R1)            BUMP TO NEXT CHARACTER IN CARD               
         C     R1,=A(CARD+79)      STOP AT END OF CARD                          
         BH    *+12                                                             
         MVI   0(R1),C' '          REPLACE COMMENT WITH BLANKS                  
         B     *-16                                                             
*                                                                               
         CLC   =C'DSPACE=',CARD    DSPACE=                                      
         BNE   RC15                                                             
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),CARD+7                                     
         B     RC10                                                             
*                                                                               
RC15     CLC   =C'DDSIO=',CARD     DDSIO=                                       
         BNE   RC18                                                             
         ICM   RF,15,VDDSIO                                                     
         MVC   0(8,RF),CARD+6                                                   
         B     RC10                                                             
*                                                                               
RC18     CLC   =C'ATTACH=',CARD    ATTACH=                                      
         BNE   RC30                                                             
*                                                                               
         MVI   WORK,C' '           FIRST 80 BYTES OF WORK CONTAIN INPUT         
         MVC   WORK+1(79),WORK                                                  
         LA    RE,WORK                                                          
         LA    RF,CARD                                                          
         MVC   0(1,RE),0(RF)       MOVE IN INPUT DATA                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNE   *-18                COMMENT NOW REPLACED BY BLANKS               
         XC    WORK+80(L'WORK-80),WORK+80                                       
         GOTO1 =V(SCANNER),DMCB,(C'C',WORK),WORK+80                             
         CLI   DMCB+4,0                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID CARD DATA                            
         LA    R4,WORK+80                                                       
         CLC   =C'ATTACH',12(R4)                                                
         BE    *+6                                                              
         DC    H'0'                SERIOUS BUG                                  
*                                                                               
         LA    RF,TSKTAB           LOOK UP TASK NAME IN TABLE                   
         USING TSKTABD,RF                                                       
RC20     CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN TASK NAME                            
         CLC   TSKNAME,22(R4)      FIND THE TABLE ENTRY FOR TASK ID             
         BE    *+12                                                             
         LA    RF,TSKTABLQ(,RF)    BUMP TO NEXT ENTRY                           
         B     RC20                                                             
         MVI   TSKATTCH,C'Y'       WE WILL ATTACH THIS PARTNER                  
*                                                                               
         LA    R4,32(R4)           BUMP TO NEXT SCANNER TABLE ENTRY             
         CLI   12(R4),C'S'         S= OPTION                                    
         BNE   *+14                                                             
         MVC   TSKSNDR,22(R4)      NAME OF SENDING SUBTASK                      
         LA    R4,32(R4)                                                        
         CLI   12(R4),C'R'         R= OPTION                                    
         BNE   RC10                                                             
         MVC   TSKRCVR,22(R4)      NAME OF RECEVING SUBTASK                     
         B     RC10                                                             
         DROP  RF                                                               
*                                                                               
RC30     CLC   =C'DRTEST=',CARD    DISASTER RECOVERY TEST MODE                  
         BNE   RC35                                                             
         CLC   =C'NO',CARD+7                                                    
         BE    RC10                DRTEST=NO IS THE DEFAULT                     
         CLC   =C'YES',CARD+7                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DRTEST,C'Y'                                                      
         B     RC10                                                             
*                                                                               
RC35     CLC   =C'ATTACHRCVR=',CARD    ATTACH RECEIVER TEST MODE                
         BNE   RC40                                                             
         CLC   =C'YES',CARD+11     ATTACHRCVR=YES IS THE DEFAULT                
         BE    RC10                                                             
         CLC   =C'NO',CARD+11                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ATTRCVR,C'N'                                                     
         B     RC10                                                             
*                                                                               
RC40     CLC   =C'TESTSMF=',CARD   TESTSMF=                                     
         BNE   RC50                                                             
         CLC   =C'NO',CARD+8                                                    
         BE    RC10                TESTSMF=NO IS THE DEFAULT                    
         CLC   =C'YES',CARD+8                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TESTSMF,C'Y'                                                     
         B     RC10                                                             
*                                                                               
RC50     CLC   =C'TESTTPNAME=',CARD TESTTPNAME=                                 
         BNE   RC60                                                             
         CLC   =C'NO',CARD+11                                                   
         BE    RC10                TESTTPNAME=NO IS THE DEFAULT                 
         CLC   =C'FQA',CARD+11                                                  
         BE    RC55                TESTTPNAME=FQA                               
         CLC   =C'YES',CARD+11                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EDATPNAM,=C'DDSEDA0'  USE TEST TPNAMES                           
         MVC   EDRTPNAM,=C'DDSEDR0'                                             
         B     RC10                                                             
RC55     MVC   EDATPNAM,=C'DDSEDAQ'  USE FQA TPNAMES                            
         MVC   EDRTPNAM,=C'DDSEDRQ'                                             
         B     RC10                                                             
*                                                                               
RC60     CLC   =C'NOOP=REP',CARD    REP SYSTEM NOT TO OPEN                      
         BNE   RC70                                                             
         LARL  RE,NOOPREPS                                                      
RC62     CLI   0(RE),C' '          FIND OPEN SLOT                               
         BE    RC65                                                             
         AHI   RE,2                                                             
         B     RC62                                                             
*                                                                               
RC65     MVC   0(2,RE),CARD+8      MOVE IN SYSTEM CHARACTERS                    
         B     RC10                                                             
*                                                                               
RC70     CLC   =C'LOCALLUID=',CARD LOCALLUID=                                   
         BNE   *+14                                                             
         MVC   LOCLLUID,CARD+10    THIS PROGRAM'S LUID                          
         B     RC10                                                             
*                                                                               
         CLC   =C'MQQMGRNAME=',CARD MQ SERIES QUEUE MANAGER                     
         BNE   *+14                                                             
         MVC   MQMGRNAM,CARD+11    OVERRIDE QUEUE MANAGER                       
         B     RC10                                                             
*                                                                               
         CLC   =C'MQINPUTQ=',CARD  NAME OF INPUT QUEUE FROM PARTNERS            
         BNE   *+14                                                             
         MVC   MQINPUTQ,CARD+9                                                  
         B     RC10                                                             
*                                                                               
         CLC   =C'MQREPLYQ=',CARD  NAME OF REPLY QUEUE FOR DARE                 
         BNE   *+14                                                             
         MVC   MQREPLYQ,CARD+9                                                  
         B     RC10                                                             
*                                                                               
         CLC   =C'MQLOGQ=',CARD  NAME OF LOG QUEUE FOR DARE                     
         BNE   *+14                                                             
         MVC   MQLOGQ,CARD+7                                                    
         B     RC10                                                             
*                                                                               
         CLC   =C'MQLOGQ2=',CARD  NAME OF 2ND LOG QUEUE FOR DARE                
         BNE   *+14                                                             
         MVC   MQLOGQ2,CARD+8                                                   
         B     RC10                                                             
*                                                                               
         CLC   =C'MQLOGQB=',CARD   NAME OF DARE BLOCKCHAIN QUEUE                
         BNE   *+14                                                             
         MVC   MQLOGQB,CARD+8                                                   
         B     RC10                                                             
*                                                                               
         CLC   =C'MQDEADQ=',CARD  NAME OF DEAD LETTER QUEUE FOR DARE            
         BNE   *+14                                                             
         MVC   MQDEADQ,CARD+8                                                   
         B     RC10                                                             
*                                                                               
         CLC   =C'SETALLOCATETIMER=',CARD   SETALLOCATETIMER=                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'YES',CARD+17                                                  
         BE    RC10                YES IS THE DEFAULT                           
         CLC   =C'NO',CARD+17                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,TSKTAB           LOOP THROUGH ENTIRE TASK TABLE               
         USING TSKTABD,RF                                                       
RC90     CLI   0(RF),X'FF'                                                      
         BE    RC10                                                             
         CLI   TSKMETH,METHAPPC    APPC SUBTASK?                                
         BNE   *+10                NO                                           
         XC    TSKTIMER,TSKTIMER   CLEAR ALLOCATE TIMEOUT VALUE                 
         LA    RF,TSKTABLQ(,RF)    BUMP TO NEXT ENTRY                           
         B     RC90                                                             
         DROP  RF                                                               
*                                                                               
RCX      GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
***********************************************************************         
***NOTE: COMMENT OUT FROM 6/7/12, REP FILE IS WRITABLE NOW ON SUNDAY.**         
*                                                                     *         
*DON'T ATTACH EDR TASK WHEN IT'S SUNDAY BECAUSE CAN'T WRITE TO REP FILE         
*                                                                     *         
*        GOTO1 =V(DATCON),DMCB,(5,0),(0,CARD)    TODAY'S DATE         *         
*        GOTO1 =V(GETDAY),DMCB,(0,CARD),(0,FULL) GET DAY OF WEEK      *         
*                                                                     *         
*        CLC   =C'SUN',FULL        IS THIS SUNDAY?                    *         
*        BNE   RCXX                                                   *         
*                                                                     *         
*        LA    RF,TSKTAB           LOOK UP TASK 'EDR' IN TABLE        *         
*        USING TSKTABD,RF                                             *         
*RCX20    CLI   0(RF),X'FF'                                           *         
*        BNE   *+6                                                    *         
*        DC    H'0'                IMPOSSIBLE!                        *         
*        CLC   TSKNAME,=C'EDR'     FIND THE TABLE ENTRY FOR TASK 'EDR'*         
*        BE    *+12                                                   *         
*        LA    RF,TSKTABLQ(,RF)    BUMP TO NEXT ENTRY                 *         
*        B     RCX20                                                  *         
*        MVI   TSKATTCH,C'N'       DON'T ATTACH 'EDR' ON SUNDAY       *         
*        DROP  RF                                                     *         
***********************************************************************         
RCXX     B     XIT                                                              
                                                                                
***********************************************************************         
* BUILD AN ECBLIST DYNAMICALLY.  THE FIRST ECB IS THE OPERATOR'S.               
* THE REST ARE THE ECBS OF THE SUBTASKS WHICH HAVE BEEN ATTACHED.               
* WAIT FOR EITHER AN OPERATOR INTERRUPT, OR FOR ONE OF THE SUBTASKS             
* TO END UNEXPECTEDLY.                                                          
***********************************************************************         
WAIT     NTR1                                                                   
*                                                                               
         PRNT  WaitForSomething                                                 
*                                                                               
         MVC   AOPERECB,SVAOPECB   A(OPERATOR ECB)                              
*                                                                               
         LA    R3,ATSKECBS         PUT SUBTASK ECBS HERE                        
         LA    R1,NUMTASKS                                                      
         XC    0(4,R3),0(R3)       CLEAR AREA ENTIRELY                          
         LA    R3,4(R3)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         LA    R3,ATSKECBS                                                      
         LA    R2,TSKTAB           TABLE OF SUBTASKS                            
         USING TSKTABD,R2                                                       
WAIT10   CLI   TSKATTCH,C'N'       IS SUBTASK MEANT TO BE ATTACHED?             
         BE    *+16                                                             
         LA    R1,TSKECB           YES -- PUT A(ECB) INTO THE LIST              
         ST    R1,0(R3)                                                         
         LA    R3,4(R3)                                                         
         LA    R2,TSKTABLQ(,R2)    BUMP TO NEXT ENTRY                           
         CLI   0(R2),X'FF'                                                      
         BNE   WAIT10                                                           
         DROP  R2                                                               
*                                                                               
         AHI   R3,-4               BACK UP TO LAST ECB IN LIST                  
         MVI   0(R3),X'80'         INDICATE END OF ECBLIST                      
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR OPERATOR OR SUBTASK END             
*                                                                               
         PRNT  HeresSomething                                                   
*                                                                               
         B     XIT                                                              
                                                                                
**********************************************************************          
* SEE IF THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' (P) OR               
* 'MODIFY' (F) COMMAND.  EXAMINE THE COMMAND AND TAKE CORRECT ACTION.           
**********************************************************************          
CHKOPER  NTR1                                                                   
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    CHKOPX              NO                                           
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R4,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R4                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CHKOPMOD                                                         
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'STOP COMMAND ACCEPTED'             
         B     CHKOPOK                                                          
*                                                                               
CHKOPMOD CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         JNE   *+2                 NO: WHAT IS GOING ON?                        
*----------------------------------------------------------------------         
CHKEIGHT CLC   CIBDATLN,=H'8'      LENGTH OF INPUT STRING                       
         BNE   CHKSEVEN                                                         
         CLC   =C'BLOCKYES',CIBDATA ALLOW BLOCKCHAIN OUTPUT                     
         BE    CHKSEV01                                                         
         B     CHKBAD                                                           
*----------------------------------------------------------------------         
CHKSEVEN CLC   CIBDATLN,=H'7'      LENGTH OF INPUT STRING                       
         BNE   CHKSIX                                                           
         CLC   =C'BLOCKNO',CIBDATA SUSPEND BLOCKCHAIN OUTPUT                    
         BE    CHKSEV02                                                         
         CLC   =C'REFRESH',CIBDATA REFRESH CONTROL TABLES                       
         BE    CHKSEV10                                                         
         B     CHKBAD                                                           
*                                                                               
CHKSEV01 MVI   BYTE,C'Y'                                                        
         MVC   P+30(30),=CL30'Blockchain Transmission ON'                       
         B     CHKSEV03                                                         
CHKSEV02 MVI   BYTE,C'N'                                                        
         MVC   P+30(30),=CL30'Blockchain Transmission OFF'                      
*                                                                               
CHKSEV03 L     RF,=A(SSB)                                                       
         LAM   AR2,AR2,SSOTBLET-SSOOFF(RF)                                      
         XR    R2,R2                                                            
         SAM31                                                                  
         SAC   512                                                              
         ICM   R2,15,TABSCOMM-TABSADDR(R2)                                      
         USING TCOMMOND,R2                                                      
         MVC   TCOBCMQ,BYTE        SET BLOCKCHAIN OUTPUT                        
         DROP  R2                                                               
         SAM24                                                                  
         SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
         PRNT  BlockchainOpCmd                                                  
         B     CHKOPOK                                                          
*                                                                               
CHKSEV10 BAS   RE,BLDTABLS                                                      
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG4,OPMSG4)                     
         B     CHKOPOK                                                          
*----------------------------------------------------------------------         
CHKSIX   CLC   CIBDATLN,=H'6'      LENGTH OF INPUT STRING                       
         BNE   CHKTHREE                                                         
         CLC   =C'STATUS',CIBDATA  SHOW PROGRAM STATUS ON CONSOLE               
         BNE   CHKBAD                                                           
         BAS   RE,STATUS                                                        
         B     CHKOPOK                                                          
*----------------------------------------------------------------------         
CHKTHREE CLC   CIBDATLN,=H'3'      LENGTH OF INPUT STRING                       
         BNE   CHKBAD                                                           
         LA    R2,TSKTAB           LOOK UP TASK NAME IN TABLE                   
         USING TSKTABD,R2                                                       
CHKLIST  CLI   0(R2),X'FF'                                                      
         BE    CHKBAD              OPERATOR ENTERED INVALID SUBTASK ID          
         CLC   TSKNAME,CIBDATA     FIND THE TABLE ENTRY FOR TASK ID             
         BE    *+12                                                             
         LA    R2,TSKTABLQ(,R2)    BUMP TO NEXT ENTRY                           
         B     CHKLIST                                                          
         BAS   RE,TOGGLE                                                        
         B     CHKOPOK                                                          
         DROP  R2                                                               
*----------------------------------------------------------------------         
CHKBAD   GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'*INVALID DARE COMMAND*'            
*                                                                               
CHKOPOK  L     RF,ACOMM                                                         
         LA    R3,COMCIBPT-COMLIST(RF) A(A(CIB))                                
         QEDIT ORIGIN=(R3),BLOCK=(R4)  FREE THE CIB                             
         DROP  R4                                                               
*                                                                               
CHKOPX   B     XIT                                                              
                                                                                
**********************************************************************          
* SHOW THE PROGRAM STATUS ON THE CONSOLE.                                       
**********************************************************************          
STATUS   NTR1                                                                   
         LA    R2,TSKTAB           TABLE OF SUBTASKS                            
         USING TSKTABD,R2                                                       
*                                                                               
ST10     MVC   OPMSG1(3),TSKNAME   SUBTASK ID                                   
         CLI   TSKATTCH,C'N'       COULD THIS SUBTASK BE ATTACHED?              
         BE    ST40                NO                                           
         TM    TSKECB,X'40'        IS THE SUBTASK RUNNING?                      
         BZ    *+14                YES                                          
         MVC   OPMSG1+18(8),=C'DISABLED'                                        
         B     *+10                                                             
         MVC   OPMSG1+18(8),=C'RUNNING '                                        
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG1,OPMSG1)                     
*                                                                               
         CLI   TSKMETH,METHAPPC    APPC SUBTASK?                                
         BNE   ST40                NO                                           
*                                                                               
         MVC   OPMSG1S+33(18),=CL18'NONE'                                       
         OC    TSKSCONV,TSKSCONV   IS THE SENDING CONVERSATION ACTIVE?          
         BZ    ST20                NO                                           
*                                                                               
         SAM31                                                                  
         MVC   QUALIFIER_VALUE,TSKSCONV                                         
         MVC   BUFFER_LENGTH,=A(ATBEXCOS_LEN)                                   
         L     RF,VATBEXAI                                                      
         CALL  (15),(EXTRACT_CODE,QUALIFIER_TYPE,QUALIFIER_VALUE,      +        
               ACCESS_TOKEN,BUFFER_LENGTH,BUFFER,RETURN_CODE),VL                
         SAM24                                                                  
*                                                                               
         CLC   RETURN_CODE,=F'48'  INVALID CONVERSATION ID?                     
         BE    ST20                YES -- CONVERSATION WENT AWAY                
         CLC   RETURN_CODE,=F'4'   BUFFER LENGTH TOO SMALL?                     
         BE    *+16                NOT A PROBLEM: WE DON'T NEED MORE            
         OC    RETURN_CODE,RETURN_CODE                                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER ERROR IS LEGITIMATE                 
*                                                                               
         LA    RF,BUFFER                                                        
         USING ATBEXCOS,RF                                                      
         MVC   OPMSG1S+33(18),=CL18'RESET'                                      
         CLC   EXCOS_CONVERSATION_STATE,=F'1'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'INITIALIZE'                                 
         CLC   EXCOS_CONVERSATION_STATE,=F'2'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'SEND'                                       
         CLC   EXCOS_CONVERSATION_STATE,=F'3'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'RECEIVE'                                    
         CLC   EXCOS_CONVERSATION_STATE,=F'4'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'SEND-PENDING'                               
         CLC   EXCOS_CONVERSATION_STATE,=F'5'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'CONFIRM'                                    
         CLC   EXCOS_CONVERSATION_STATE,=F'6'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'CONFIRM-SEND'                               
         CLC   EXCOS_CONVERSATION_STATE,=F'7'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'CONFIRM-DEALLOCATE'                         
         CLC   EXCOS_CONVERSATION_STATE,=F'8'                                   
         BE    ST20                                                             
         MVC   OPMSG1S+33(18),=CL18'*** INVALID ***'                            
         DROP  RF                                                               
*                                                                               
ST20     GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG1S,OPMSG1S)                   
*                                                                               
         MVC   OPMSG1R+33(18),=CL18'NONE'                                       
         OC    TSKRCONV,TSKRCONV   IS THE RECEIVING CONV. ACTIVE?               
         BZ    ST30                NO                                           
*                                                                               
         SAM31                                                                  
         MVC   QUALIFIER_VALUE,TSKRCONV                                         
         MVC   BUFFER_LENGTH,=A(ATBEXCOS_LEN)                                   
         L     RF,VATBEXAI                                                      
         CALL  (15),(EXTRACT_CODE,QUALIFIER_TYPE,QUALIFIER_VALUE,      +        
               ACCESS_TOKEN,BUFFER_LENGTH,BUFFER,RETURN_CODE),VL                
         SAM24                                                                  
*                                                                               
         CLC   RETURN_CODE,=F'48'  INVALID CONVERSATION ID?                     
         BE    ST30                YES -- CONVERSATION WENT AWAY                
         CLC   RETURN_CODE,=F'4'   BUFFER LENGTH TOO SMALL?                     
         BE    *+16                NOT A PROBLEM: WE DON'T NEED MORE            
         OC    RETURN_CODE,RETURN_CODE                                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER ERROR IS LEGITIMATE                 
*                                                                               
         LA    RF,BUFFER                                                        
         USING ATBEXCOS,RF                                                      
         MVC   OPMSG1R+33(18),=CL18'RESET'                                      
         CLC   EXCOS_CONVERSATION_STATE,=F'1'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'INITIALIZE'                                 
         CLC   EXCOS_CONVERSATION_STATE,=F'2'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'SEND'                                       
         CLC   EXCOS_CONVERSATION_STATE,=F'3'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'RECEIVE'                                    
         CLC   EXCOS_CONVERSATION_STATE,=F'4'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'SEND-PENDING'                               
         CLC   EXCOS_CONVERSATION_STATE,=F'5'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'CONFIRM'                                    
         CLC   EXCOS_CONVERSATION_STATE,=F'6'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'CONFIRM-SEND'                               
         CLC   EXCOS_CONVERSATION_STATE,=F'7'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'CONFIRM-DEALLOCATE'                         
         CLC   EXCOS_CONVERSATION_STATE,=F'8'                                   
         BE    ST30                                                             
         MVC   OPMSG1R+33(18),=CL18'*** INVALID ***'                            
         DROP  RF                                                               
*                                                                               
ST30     GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG1R,OPMSG1R)                   
*                                                                               
ST40     LA    R2,TSKTABLQ(,R2)    BUMP TO NEXT ENTRY                           
         CLI   0(R2),X'FF'                                                      
         BNE   ST10                                                             
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
                                                                                
***********************************************************************         
* TURN A PARTNER ON OR OFF, VIA ATTACH/DETACH.                                  
* R2 POINTS TO TSKTAB ENTRY TO TOGGLE.                                          
***********************************************************************         
TOGGLE   NTR1                                                                   
         USING TSKTABD,R2                                                       
         MVC   OPMSG1(3),TSKNAME                                                
         CLI   TSKATTCH,C'N'       COULD THE SUBTASK BE ATTACHED?               
         BE    TOGGLE5             NO                                           
         TM    TSKECB,X'40'        IS THE SUBTASK RUNNING?                      
         BO    TOGGLE5             NO                                           
         MVC   OPMSG1+18(8),=C'DISABLED'                                        
         BAS   RE,DETACH           DETACH SUBTASK                               
         B     TOGGLEX                                                          
*                                                                               
TOGGLE5  MVC   OPMSG1+18(8),=C'RUNNING '                                        
         BAS   RE,ATTACH           ATTACH SUBTASK                               
*                                                                               
TOGGLEX  GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMSG1,OPMSG1)                     
*                                                                               
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
         ATBSERV                                                                
                                                                                
***********************************************************************         
* SSB                                                                           
***********************************************************************         
         DS    0D                                                               
         DC    CL8'***SSB**'                                                    
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
                                                                                
***********************************************************************         
* UTL                                                                           
***********************************************************************         
         DC    CL8'***UTL**'                                                    
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
                                                                                
***********************************************************************         
* STORAGE                                                                       
***********************************************************************         
DMWORK   DS    12D                                                              
DMCB     DS    10F                                                              
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
DATADISP DS    H                                                                
ELCODE   DS    X                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
WORK     DS    CL256                                                            
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
KEY      DS    XL40                FOR CTFILE/GENDIR READS                      
KEYSAVE  DS    XL40                FOR CTFILE/GENDIR READS                      
SAVEKEY  DS    XL40                FOR CTFILE/GENDIR READS                      
*                                                                               
DATAMGR  DC    V(DATAMGR)          A(DATAMGR)                                   
VDDSIO   DC    V(DDSIO)            DDSIO VALUE                                  
ACOMM    DS    A                   A(COMMUNICATIONS PARAMETER LIST)             
SVAOPECB DC    A(0)                OPERATOR ECB IS SAVED HERE                   
MQMGRNAM DC    CL48' '             MQ SERIES QUEUE MANAGER NAME                 
MQINPUTQ DS    CL48                NAME OF INPUT QUEUE FROM PARTNERS            
MQREPLYQ DS    CL48                NAME OF REPLY QUEUE FOR DARE                 
MQLOGQ   DS    CL48                NAME OF LOG QUEUE FOR APPC/MQ PRTNRS         
MQLOGQ2  DS    CL48                NAME OF LOG QUEUE FOR APPC/MQ PRTNRS         
MQLOGQB  DS    CL48                NAME OF BLOCKCHAIN QUEUE FOR DARE            
MQDEADQ  DC    CL48'DARE.DEAD.LETTER'   NAME OF DEAD LETTER Q MQ PRTNRS         
LOCLLUID DC    C'        '         DARE'S LOCAL LUID                            
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
TESTSMF  DC    C'N'                'Y' = SMF RECORDS GET MARKED AS TEST         
DRTEST   DC    C'N'                'Y' = DISASTER RECOVERY TEST MODE            
ATTRCVR  DC    C'Y'                'N' = ATTACH RECIEVER BUT DON'T SEND         
TODAY    DS    XL4                 0CYYDDDF                                     
BNGTODAY DS    XL3                 3 BYTE BINARY DATE NEGATED                   
CARD     DS    CL80                FOR CONTROL CARDS                            
OPMSG1   DC    C'XXX TRANSMISSIONS XXXXXXXX'                                    
OPMSG1S  DC    C'    SENDING CONVERSATION STATE = XXXXXXXXXXXXXXXXXX'           
OPMSG1R  DC    C'    RECVING CONVERSATION STATE = XXXXXXXXXXXXXXXXXX'           
OPMSG2   DC    C'XXX SENDER DIED'                                               
OPMSG3   DC    C'WAITING FOR DARE PARTNER XXX'                                  
OPMSG4   DC    C'DARE CONTROL TABLES REFRESHED'                                 
OPMSG5   DC    C'PARTNER XXX ATTACHED'                                          
*                                                                               
VATBAMR1 DS    V                   A(APPC/MVS ASYNCHRONOUS MANAGER RTN)         
VATBEXAI DS    V                   A(APPC/MVS EXTRACT_INFORMATION RTN)          
         SPACE 2                                                                
FUNCTION                       DC    F'2' ASYNC_MANAGER CLEANUP                 
ASYNCHRONOUS_NUMBER            DS    F                                          
CONVERSATION_ID                DS    CL8                                        
RETURN_CODE                    DS    F                                          
EXTRACT_CODE                   DC    F'1' SPECIFIC CONVERSATION INFO            
QUALIFIER_TYPE                 DS    F                                          
QUALIFIER_VALUE                DS    CL8                                        
ACCESS_TOKEN                   DC    F'0' BUFFER IS IN MY ADDRESS SPACE         
BUFFER_LENGTH                  DS    F                                          
BUFFER                         DS    (ATBEXCOS_LEN)C                            
         SPACE 2                                                                
         DS    0D                                                               
         DC    C'*ECBLST*'                                                      
ECBLST   DS    0A                  DYNAMICALLY BUILT ECBLIST                    
AOPERECB DS    A                   OPERATOR ECB IS STORED HERE                  
ATSKECBS DS    (NUMTASKS)A         SUBTASK ECBS ARE STORED HERE                 
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*TSKTAB*'                                                      
TSKTAB   EQU   *                                                                
*                                                                               
         DC    C'A'                EDICTA SHORT SUBTASK NAME                    
         DC    C'EDA'              SUBTASK ID FOR EDICTA                        
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'Y'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
EDATPNAM DC    C'DDSEDA1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'R'                EDICTR SHORT SUBTASK NAME                    
         DC    C'EDR'              SUBTASK ID FOR EDICTR                        
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'Y'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
EDRTPNAM DC    C'DDSEDR1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'T'                JDS/TELEREP SHORT SUBTASK NAME               
         DC    C'TEL'              SUBTASK ID FOR JDS/TELEREP (REP)             
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSTEL1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'O'                COXREPS  SHORT SUBTASK NAME                  
         DC    C'COX'              SUBTASK ID FOR COXREPS (REP)                 
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'DARECOX'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'H'                JDS/HRP SHORT SUBTASK NAME                   
         DC    C'HRP'              SUBTASK ID FOR JDS/HRP (REP)                 
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSHRP1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'J'                JDS/TEST SHORT SUBTASK NAME                  
         DC    C'JDT'              SUBTASK ID FOR JDS/TEST (REP)                
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSJDT1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'W'                JDS/RS1 SHORT SUBTASK NAME                   
         DC    C'JD1'              SUBTASK ID FOR JDS/RS1 (AGENCY)              
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSJD11'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'X'                JDS/RS2 SHORT SUBTASK NAME                   
         DC    C'JD2'              SUBTASK ID FOR JDS/RS2 (SVC BUREAU)          
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSJD21'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'S'                JDS/PSGWHNY SHORT SUBTASK NAME               
         DC    C'PSW'              SUBTASK ID FOR JDS/PSGWHNY (REP)             
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSPSW1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'Z'                JDS/PSGALNY SHORT SUBTASK NAME               
         DC    C'PSA'              SUBTASK ID FOR JDS/PSGALNY (AGENCY)          
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSPSA1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'U'                JDS/TELEREP SHORT SUBTASK NAME               
         DC    C'TLU'              SUBTASK ID FOR JDS/TELEREP (AGENCY)          
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSTLU1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'4'                JDS/TELEREP SHORT SUBTASK NAME               
         DC    C'JD4'              SUBTASK ID FOR JDS/RS4 (AGY/REP)             
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSJD41'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'G'                JDS/GREY SHORT SUBTASK NAME                  
         DC    C'GRE'              SUBTASK ID FOR JDS/GREY (AGENCY)             
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSGRE1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'C'                JDS/CPM SHORT SUBTASK NAME                   
         DC    C'CPM'              SUBTASK ID FOR JDS/CPM (AGENCY)              
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSCPM1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'5'                JDS/RS3 SHORT SUBTASK NAME                   
         DC    C'RSA'              SUBTASK ID FOR JDS/RS3A (AGENCY)             
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSRS3A'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'6'                JDS/RS3 SHORT SUBTASK NAME                   
         DC    C'RSR'              SUBTASK ID FOR JDS/RS3R (REP)                
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSRS3R'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'3'                JDS/SB3 SHORT SUBTASK NAME                   
         DC    C'SB3'              SUBTASK ID FOR JDS/RS3R (REP)                
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'0'                NO TIMER SET FOR ALLOCATES                   
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSSB31'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'Q'                JDS/HRP SHORT SUBTASK NAME                   
         DC    C'AMY'              SUBTASK ID FOR JDS/HRP (REP)                 
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSAMY1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'V'                JDS/HRP SHORT SUBTASK NAME                   
         DC    C'VSG'              SUBTASK ID FOR JDS/HRP (REP)                 
         DC    C'N'                ATTACH FLAG                                  
         DC    C'A'                COMMUNICATIONS METHOD (APPC/MVS)             
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    H'10'               # OF MINUTES PARTNER HAS TO ALLOCATE         
         DC    CL8' '              LOCAL LUNAME                                 
         DC    XL8'00'             SENDING CONVERSATION ID                      
         DC    XL8'00'             RECEIVING CONVERSATION ID                    
         DC    C'JDSVSG1'          TPNAME PREFIX                                
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                                                               
*                                                                               
         DC    C'M'                MQ SERIES SHORT SUBTASK NAME                 
         DC    C'MQ1'              SUBTASK ID FOR MQ SERIES                     
         DC    C'N'                ATTACH FLAG                                  
         DC    C'M'                COMMUNICATIONS METHOD (MQ SERIES)            
         DC    C'N'                DDS PARTNER FLAG                             
         DS    C                   TEST SMF RECORD FLAG                         
         DS    C                   DISASTER RECOVERY TEST MODE                  
         DS    C                   DSPACE CHAR                                  
         DS    C                   'N' = ATTACH RECEIVER BUT DON'T SEND         
         DS    XL1                 SPARE                                        
         DS    F                   TCB FOR SUBTASK                              
         DS    F                   ECB FOR SUBTASK                              
         DS    F                   ECB TO STOP SENDING                          
         DS    F                   ECB TO STOP RECEIVING                        
         DS    F                   ECB TO TELL SENDER TO SCAN PRTQUES           
         DC    V(DATAMGR)          A(DATAMGR)                                   
         DC    A(STATTABL)         A(STATION TABLE)                             
         DC    A(UIDTABL)          A(USERID TABLE)                              
         DC    A(OFEXTABL)         A(OFFICE EXCEPTION TABLE)                    
         DC    A(CITABLE)          A(PRINT QUEUE INFO TABLE)                    
         DC    A(UTL)              A(UTL)                                       
         DC    C'DARESND '         SUBTASK SENDS WITH THIS PROGRAM              
         DC    C'DARERCV '         SUBTASK RECEIVES WITH THIS PROGRAM           
         DC    A(MQTABLE)          A(MQ SERIES CONTROL TABLE)                   
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME)                        
         DC    A(MQINPUTQ)         A(NAME OF INPUT QUEUE FROM PARTNERS)         
         DC    A(MQREPLYQ)         A(NAME OF REPLY QUEUE FOR DARE)              
         DC    A(MQDEADQ)          A(NAME OF DEAD LETTER QUEUE)                 
         DS    XL13                N/A                                          
         DC    X'00'               SPARE                                        
         DC    A(MQMGRNAM)         A(QUEUE MANAGER NAME FOR LOGGING)            
         DC    A(MQLOGQ)           A(MQ LOG QUEUE NAME)                         
         DC    V(DDSIO)            A(DDSIO CARD)                                
         DC    A(MQLOGQ2)          A(2ND MQ LOG QUEUE NAME)                     
         DC    A(0)                A(LIST OF SE FILES TO OPEN)                  
         DC    A(MQLOGQB)          A(DARE BLOCKCHAIN QUEUE)                     
         DC    XL2'00'             SPARE                                        
         DS    0D                  *NOTE: QMGR IS THE SAME FOR DARE             
*                                                                               
*                                                                               
NUMTASKS EQU   (*-TSKTAB)/TSKTABLQ NUMBER OF SUPPORTED SUBTASKS                 
*                                                                               
         DC    X'FF'               END OF TABLE MARKER                          
         EJECT                                                                  
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60) NUMBER OF TABLE ENTRIES                
         DC    H'60'                     MUST REMAIN AS 60                      
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBAMR1  DC    CL8'ATBAMR1'                                                     
         DC    XL52'00'                                                         
ATBEXAI  DC    CL8'ATBEXAI'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DC    2000X'00'           CTFILE/GENFIL I/O AREA                       
*                                                                               
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PQ INDEX BUFFER                              
*                                                                               
         DS    0D                                                               
         DC    C'CITABLE*'                                                      
CITABLE  DC    (MAXPQS*CITBLLNQ)X'FF'                                           
         DC    X'FF'                                                            
MAXPQS   EQU   16                  MAXIMUM OF 16 PRINT QUEUES                   
*                                                                               
         DS    0D                                                               
         DC    C'*MQTABLE'         MQ SERIES CONTROL TABLE                      
MQTABLE  DS    (MAXMQS)XL(MQTABLEQ)                                             
MQTABLEX EQU   *                                                                
         DC    X'FF'                                                            
MAXMQS   EQU   250                                                              
*                                                                               
         DS    0D                                                               
         DC    C'*STATTAB'         STATION TABLE                                
STATTABL DS    (MAXSTNS)XL(STATTBLQ)                                            
STATTBLX EQU   *                                                                
         DC    X'FF'                                                            
MAXSTNS  EQU   36000               Was 22000                                    
*                                                                               
         DS    0D                                                               
         DC    C'*UIDTAB*'         USERID TABLE                                 
UIDTABL  DS    (MAXUIDS)XL(UIDTBLQ)                                             
UIDTABLX EQU   *                                                                
         DC    X'FF'                                                            
MAXUIDS  EQU   8000                                                             
*                                                                               
         DS    0D                                                               
         DC    C'*OFEXTB*'         OFFICE EXCEPTION TABLE                       
OFEXTABL DS    (MAXOFEXS)XL(OFEXTBLQ)                                           
OFEXTBLX EQU   *                                                                
         DC    X'FF'                                                            
MAXOFEXS EQU   5000                                                             
*                                                                               
       ++INCLUDE DDDAREWRKD                                                     
*                                                                               
       ++INCLUDE CTGENSTAD                                                      
*                                                                               
       ++INCLUDE CTGENAGRD                                                      
*                                                                               
       ++INCLUDE CTGENMQDEF                                                     
*                                                                               
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         IHAPSA                                                                 
         ATBEXCOS                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
* DDSYSELD                                                                      
       ++INCLUDE DDSYSELD                                                       
*                                                                               
         PRINT OFF                                                              
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
* FATABSDEQU                                                                    
       ++INCLUDE FATABSDEQU                                                     
* FATABSD                                                                       
       ++INCLUDE FATABSD                                                        
* FATABSCOM                                                                     
       ++INCLUDE FATABSCOM                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDDARE    05/29/20'                                      
         END                                                                    
