*          DATA SET PEMAP16    AT LEVEL 034 AS OF 05/01/02                      
*PHASE TE1B16A                                                                  
         TITLE 'TE1B16 - CALENDAR REPORT'                                       
TE1B16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CLDR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE 1                                                                
         ST    R2,RELO                                                          
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LIST                                                             
         EJECT                                                                  
*              VALIDATE KEY FOR LIST OR REPORTS                                 
         SPACE 3                                                                
VKEY     DS    0H                                                               
         XC    QSYS,QSYS           PRESET FIELDS                                
         XC    QPROJ,QPROJ                                                      
         XC    QTYPE,QTYPE                                                      
         XC    QTASK,QTASK                                                      
         XC    QPERS,QPERS                                                      
         XC    QSTAT,QSTAT                                                      
         XC    QDATE,QDATE                                                      
         SPACE 1                                                                
         LA    R2,TSLSYSH          SYSTEM                                       
         CLI   5(R2),0                                                          
         BE    LKEY2               NO SYSTEM, NO PROJECT                        
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QSYSL                                                         
         GOTO1 VALISYS                                                          
         MVC   QSYS,SAVSYSCD                                                    
         SPACE 1                                                                
         LA    R2,TSLPRJH          PROJECT                                      
         CLI   5(R2),0                                                          
         BE    LKEY2                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QPROJL                                                        
         MVC   QPROJ,TSLPRJ                                                     
         SPACE 1                                                                
LKEY2    LA    R2,TSLTYPEH         TYPE                                         
         CLI   5(R2),0                                                          
         BE    LKEY3                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QTYPEL                                                        
         GOTO1 VALITYPE                                                         
         MVC   QTYPE,SAVTYPE                                                    
         SPACE 1                                                                
LKEY3    LA    R2,TSLTASKH         TASK                                         
         CLI   5(R2),0                                                          
         BE    LKEY4                                                            
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,QTASKL                                                        
         GOTO1 ANY                                                              
         MVC   QTASK,WORK                                                       
         SPACE 1                                                                
LKEY4    LA    R2,TSLSTATH         OPEN?                                        
         CLI   5(R2),0                                                          
         BE    LKEY6                                                            
         MVC   QSTAT,8(R2)                                                      
         CLI   QSTAT,C'Y'          S/B Y OR N                                   
         BE    LKEY6                                                            
         CLI   QSTAT,C'N'                                                       
         BE    LKEY6                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
LKEY6    LA    R2,TSLPERSH         PERSON                                       
         CLI   5(R2),0                                                          
         BE    LKEY8                                                            
         GOTO1 VALIPERS                                                         
         MVC   QPERS,WORK                                                       
         SPACE 1                                                                
LKEY8    LA    R2,TSLDATEH         DATE                                         
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 VALIDATE,DMCB,QDATE                                              
         B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     L     R2,=A(SORTC)                                                     
         A     R2,RELO                                                          
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(41,(R2))                           
         MVI   SORTSW,C'N'                                                      
         BAS   RE,WEEKSET          SET UP WEEKS FOR CALENDAR                    
         LA    R4,KEY              SET UP KEY                                   
         XC    KEY,KEY                                                          
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'06'                                                    
         MVC   TSKSYS,QSYS                                                      
         MVC   TSKPROJ,QPROJ                                                    
         MVC   TSKTYPE,QTYPE                                                    
         GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   LISTEND                                                          
         CLI   QSYS,0              IF SYSTEM WAS REQUESTED                      
         BE    LIST6                                                            
         ZIC   R1,QSYSL                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY+3(0),KEYSAVE+3  SHOULD MATCH ON THIS                         
         BNE   LISTEND                                                          
         CLI   QPROJ,0             IF PROJECT WAS ALSO REQUESTED                
         BE    LIST6                                                            
         ZIC   R1,QPROJL                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY+11(0),KEYSAVE+11 THIS SHOULD ALSO MATCH                      
         BNE   LIST2                                                            
         SPACE 1                                                                
LIST6    CLI   QTYPE,0             TYPE SELECTED?                               
         BE    LIST7                                                            
         ZIC   R1,QTYPEL                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QTYPE(0),TSKTYPE                                                 
         BNE   LIST2                                                            
LIST7    CLI   QTASK,0             TASK NAME FILTERING?                         
         BE    LIST8                                                            
         ZIC   R1,QTASKL                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QTASK(0),TSKCODE                                                 
         BNE   LIST2                                                            
         SPACE 1                                                                
LIST8    GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO              GET TASK DETAILS                             
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TSKELD,R6                                                        
         SPACE 1                                                                
         CLI   QPERS,0             MAY BE FILTERING ON PERSON                   
         BE    LIST9                                                            
         CLC   QPERS,TSKWHO                                                     
         BNE   LIST2                                                            
         SPACE 1                                                                
LIST9    CLI   TSKOKPCT,100        DONE PERCENT                                 
         BL    LIST11                                                           
         CLI   QSTAT,C'N'          DONE FILTER                                  
         BE    LIST2                                                            
         B     LIST12                                                           
         SPACE 1                                                                
LIST11   CLI   QSTAT,C'Y'                                                       
         BE    LIST2                                                            
         SPACE 1                                                                
LIST12   XC    SORTREC,SORTREC                                                  
         BAS   RE,WEEKANL                                                       
         OC    SORTTOT,SORTTOT                                                  
         BZ    LIST2                                                            
         MVC   SORTPERS,TSKWHO                                                  
         MVC   SORTSYST,TSKSYS                                                  
         MVC   SORTPROJ,TSKPROJ                                                 
         MVC   SORTTYPE,TSKTYPE                                                 
         MVC   SORTTASK,TSKCODE                                                 
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTSW,C'Y'                                                      
         B     LIST2                                                            
         SPACE 2                                                                
LISTEND  BAS   RE,REPS                                                          
         B     XIT                                                              
         EJECT                                                                  
REPS     NTR1                                                                   
         MVC   PAGE,=H'1'          FOR MULTIPLE REQUESTS                        
         MVI   FORCEHED,C'Y'                                                    
         XC    PERSTOTS,PERSTOTS                                                
         XC    LASTREC,LASTREC                                                  
REP4     GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BNZ   REP6                                                             
         CLI   SORTSW,C'Y'         IF THERE WAS NO DATA, JUST END               
         BNE   REP5                                                             
         BAS   RE,TTOT             TASK TOTALS                                  
         BAS   RE,PTOT             PERSON TOTALS                                
REP5     GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         SPACE 2                                                                
REP6     MVC   SORTREC,0(R3)                                                    
         OC    LASTREC,LASTREC                                                  
         BZ    REP12                                                            
         CLC   SORTREC(8),LASTREC            PERSON C/B                         
         BE    REP8                                                             
         BAS   RE,TTOT                                                          
         BAS   RE,PTOT                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     REP12                                                            
         SPACE 1                                                                
REP8     CLC   SORTREC(39),LASTREC           TASK                               
         BE    REP12                                                            
         BAS   RE,TTOT                                                          
         SPACE 1                                                                
REP12    MVC   LASTREC,SORTREC               SAVE THIS RECORD                   
         LA    R2,SORTWEEK                                                      
         LA    R4,PERSTOTS                                                      
         LA    R0,13                                                            
         SPACE 1                                                                
REP14    L     R1,0(R2)            ADD IN PERSON ACCUMS                         
         A     R1,0(R4)                                                         
         ST    R1,0(R4)                                                         
         LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,REP14                                                         
         B     REP4                                                             
         EJECT                                                                  
*              PRINTING LINE                                                    
TTOT     NTR1                      TASK TOTAL                                   
         SPACE 2                                                                
         BAS   RE,SPLAT                                                         
         MVC   P+1(8),LASTREC+8    SYSTEM                                       
         MVC   P2+1(8),LASTREC+16  PROJECT                                      
         MVC   P+10(7),LASTREC+25  TYPE                                         
         MVC   P2+10(8),LASTREC+32 TASK                                         
         SPACE 1                                                                
         LA    R4,KEY              GET DONE PCT, DONE DATE, AND                 
         XC    KEY,KEY             TASK DESCRIPTION                             
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   TSKKTYP,X'06'                                                    
         MVC   TSKSYS(32),LASTREC+8                                             
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'60'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING TSKELD,R6                                                        
         SPACE 1                                                                
         CLI   TSKOKPCT,0          SKIP IF NOTHING THERE                        
         BE    TTOT1                                                            
         LA    R3,P+50             DONE PERCENT                                 
         EDIT  (1,TSKOKPCT),(3,(R3))                                            
         MVI   3(R3),C'%'                                                       
         GOTO1 DATCON,DMCB,(1,TSKOKDAT),(7,P2+50)      DONE DATE                
         SPACE 1                                                                
TTOT1    MVI   ELCODE,X'62'        TASK DESCRIPTION                             
         LA    R3,P+19                                                          
         BAS   RE,CHATOUT                                                       
         SPACE 2                                                                
TTOT2    LA    R2,LASTWEEK                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 3                                                                
PTOT     NTR1                                PERSON TOTALS                      
         L     R5,ABOX             UNDERLINE BEFORE TOTAL LINE                  
         LTR   R5,R5                                                            
         BZ    PTOT1                                                            
         USING BOXD,R5                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS(R1)                                                   
         BCTR  R1,0                                                             
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R5                                                               
PTOT1    BAS   RE,SPLAT                                                         
         SPACE 1                                                                
         MVC   P+50(6),=C'TOTALS'                                               
         LA    R2,PERSTOTS                                                      
         BAS   RE,FORMAT                                                        
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         LA    R4,P+57                                                          
         LA    R5,13                                                            
         SPACE 1                                                                
FOR2     L     RF,0(R2)                                                         
         LTR   RF,RF                                                            
         BZ    FOR4                                                             
         EDIT  (RF),(3,0(R4))                                                   
         SPACE 1                                                                
FOR4     XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,FOR2                                                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR WEEKLY ANALYSIS                                     
         SPACE 3                                                                
WEEKSET  NTR1                                                                   
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         OC    QDATE,QDATE         REQUESTABLE START OR TODAY                   
         BZ    *+10                                                             
         MVC   WORK(6),QDATE                                                    
         GOTO1 GETDAY,DMCB,WORK,DUB  GET MONDAY DATE INTO WORK                  
         CLI   DMCB,1                                                           
         BE    WEEK2                                                            
         ZIC   R2,DMCB                                                          
         LA    R3,1                                                             
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,WORK,DUB,(R3)                                         
         MVC   WORK(6),DUB                                                      
         SPACE 1                                                                
WEEK2    LA    R2,WEEKLST                                                       
         LA    R3,13                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(5,PERSTRT)  FOR HEADLINE                   
         B     WEEK6                                                            
         SPACE 1                                                                
WEEK4    LA    R2,6(R2)                                                         
WEEK6    MVC   0(6,R2),WORK        FIRST MONDAY OF PERIOD                       
         GOTO1 ADDAY,DMCB,WORK,DUB,7                                            
         MVC   WORK(6),DUB                                                      
         BCT   R3,WEEK4                                                         
         GOTO1 DATCON,DMCB,(0,(R2)),(5,PEREND)   FOR HEADLINE                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES FOR WEEKLY ANALYSIS                                     
         SPACE 3                                                                
WEEKANL  NTR1                                                                   
         MVC   WORK(6),WEEKLST     DEFAULT FOR OPEN ITEMS IS START              
         OC    TSKSTART,TSKSTART                                                
         BZ    WEEK12                                                           
         GOTO1 DATCON,DMCB,(1,TSKSTART),(0,WORK)     OR USE MONDAY              
         GOTO1 GETDAY,DMCB,WORK,DUB               OF TASK START WEEK            
         CLI   DMCB,1                                                           
         BE    WEEK12                                                           
         ZIC   R2,DMCB                                                          
         LA    R3,1                                                             
         SR    R3,R2                                                            
         GOTO1 ADDAY,DMCB,WORK,DUB,(R3)                                         
         MVC   WORK(6),DUB                                                      
         SPACE 1                                                                
WEEK12   ZIC   R0,TSKWEEKS                                                      
         LTR   R0,R0               NUMBER OF WEEKS                              
         BNZ   *+8                                                              
         LA    R0,13                                                            
         SPACE 1                                                                
WEEK13   LA    R2,WEEKLST          FIND THE WEEK                                
         LA    R3,13                                                            
         LA    R4,SORTWEEK                                                      
         SPACE 1                                                                
WEEK14   CLC   WORK(6),0(R2)                                                    
         BE    WEEK16                                                           
         LA    R2,6(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,WEEK14                                                        
         B     WEEK18                                                           
         SPACE 1                                                                
WEEK16   ZIC   R1,TSKPCT           GOT A MATCH                                  
         A     R1,0(R4)            ADD INTO MONTH SLOT                          
         ST    R1,0(R4)                                                         
         ZIC   R1,TSKPCT                                                        
         ICM   RE,15,SORTTOT          AND INTO TOTALS                           
         AR    R1,RE                                                            
         STCM  R1,15,SORTTOT                                                    
         SPACE 1                                                                
WEEK18   GOTO1 ADDAY,DMCB,WORK,DUB,7                                            
         MVC   WORK(6),DUB                                                      
         BCT   R0,WEEK13                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
CHATOUT  NTR1                                                                   
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         B     CHAT4                                                            
         SPACE 1                                                                
CHAT2    BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
CHAT4    BNE   XIT                                                              
         USING CHATELD,R6                                                       
         ZIC   R4,CHATSEQ          POSITION TO RIGHT LINE                       
         CH    R4,=H'4'                                                         
         BH    CHAT2                                                            
         BCTR  R4,0                                                             
         MH    R4,=H'132'                                                       
         AR    R4,R3                                                            
         ZIC   R1,CHATLEN          PICK UP LENGTH                               
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     CHAT2                                                            
         MVC   0(0,R4),CHAT                                                     
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   PERSSAVE,LASTREC    NEED PERSON NAME                             
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   PERKTYP,X'02'                                                    
         MVC   PERCODE,PERSSAVE                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'20'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING CHATELD,R6                                                       
         ZIC   R1,CHATLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     HOOK2                                                            
         MVC   H4+10(0),CHAT                                                    
         SPACE 2                                                                
HOOK2    MVC   H5+10(8),PERSTRT    PERIOD START AND END DATES                   
         MVI   H5+18,C'-'                                                       
         MVC   H5+19(8),PEREND                                                  
         SPACE 1                                                                
         LA    R2,WEEKLST          WEEKLY HEADLINES                             
         LA    R3,H8+57                                                         
         LA    R4,H9+57                                                         
         LA    R0,13                                                            
HOOK3    GOTO1 DATCON,DMCB,(0,(R2)),(7,WORK)                                    
         MVC   0(3,R3),WORK                                                     
         MVC   0(2,R4),WORK+3                                                   
         LA    R2,6(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,HOOK3                                                         
         SPACE 1                                                                
         L     R4,ABOX             BOXES, IF AROUND                             
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+06,C'T'                                                  
         MVI   BOXROWS+09,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   00(R2),C'L'                                                      
         MVI   09(R2),C'C'                                                      
         MVI   18(R2),C'C'                                                      
         MVI   49(R2),C'C'                                                      
         MVI   56(R2),C'C'                                                      
         MVI   60(R2),C'C'                                                      
         MVI   64(R2),C'C'                                                      
         MVI   68(R2),C'C'                                                      
         MVI   72(R2),C'C'                                                      
         MVI   76(R2),C'C'                                                      
         MVI   80(R2),C'C'                                                      
         MVI   84(R2),C'C'                                                      
         MVI   88(R2),C'C'                                                      
         MVI   92(R2),C'C'                                                      
         MVI   96(R2),C'C'                                                      
         MVI   100(R2),C'C'                                                     
         MVI   104(R2),C'C'                                                     
         MVI   108(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS SSPEC H1,2,C'MAP SYSTEM'                                               
         SSPEC H2,2,C'----------'                                               
         SSPEC H1,40,C'PERSONAL CALENDAR'                                       
         SSPEC H2,40,C'-----------------'                                       
         SSPEC H1,77,REPORT                                                     
         SSPEC H1,96,REQUESTOR                                                  
         SSPEC H2,77,RUN                                                        
         SSPEC H2,103,PAGE                                                      
         SSPEC H4,2,C'PERSON -'                                                 
         SSPEC H5,2,C'PERIOD -'                                                 
         SSPEC H8,02,C'SYSTEM   TYPE     DESCRIPTION OF TASK'                   
         SSPEC H9,02,C'PROJECT  TASK'                                           
         SSPEC H8,51,C'PCT'                                                     
         SSPEC H9,51,C'DONE'                                                    
         DC    X'00'               END MARKER FOR SPECS                         
         EJECT                                                                  
*              CONSTANTS, LITERAL POOL, ETC.                                    
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,40,A),FORMAT=BI,WORK=1 '                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=96'                                    
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         ENTRY SORTC                                                            
SORTC    DS    0D                                                               
*&&DO*&& DS    44000C                                                           
         EJECT                                                                  
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* PEMAPFILE                                                                     
* PEMAPFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPF6D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE6D                                                       
* PEMAPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
QSYSL    DS    CL1                                                              
QPROJL   DS    CL1                                                              
QTYPEL   DS    CL1                                                              
QTASKL   DS    CL1                                                              
QSYS     DS    CL8                                                              
QPROJ    DS    CL8                                                              
QTYPE    DS    CL8                                                              
QTASK    DS    CL8                                                              
QPERS    DS    CL8                                                              
QDATE    DS    CL6                                                              
QSTAT    DS    CL8                                                              
         SPACE 1                                                                
LASTREC  DS    0CL96                                                            
         DS    CL40                                                             
LASTWEEK DS    CL52                                                             
         DS    CL4                                                              
WEEKLST  DS    CL78                                                             
PERSTOTS DS    CL52                                                             
PERSTRT  DS    CL8                                                              
PEREND   DS    CL8                                                              
PERSSAVE DS    CL8                                                              
SORTSW   DS    CL1                                                              
         SPACE 1                                                                
         DS    0D                                                               
SORTREC  DS    0CL96                                                            
SORTPERS DS    CL8                 PERSON                                       
SORTSYST DS    CL8                 SYSTEM                                       
SORTPROJ DS    CL8                 PROJECT                                      
SORTTYPE DS    CL8                 TYPE                                         
SORTTASK DS    CL8                 TASK                                         
SORTWEEK DS    CL52                                                             
SORTTOT  DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034PEMAP16   05/01/02'                                      
         END                                                                    
