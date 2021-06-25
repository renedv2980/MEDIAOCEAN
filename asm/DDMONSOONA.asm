*          DATA SET DDMONSOONA AT LEVEL 029 AS OF 05/01/02                      
*PHASE MONSOONA                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE GETRET                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
*                                                                               
* MONSOON W/ LONG SOON JOB RECORD IN A GIVEN DS                                 
*                                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        MONSOON -- LOCATE AND ATTACH SOON REQUESTS           *         
*                                                                     *         
*  COMMENTS:     FIND SOON JCL ON ALL PRINT QUEUES AND ATTACH IT.     *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- WORK                                           *         
*                R3 -- WORK                                           *         
*                R4 -- WORK (AND GETEL REGISTER)                      *         
*                R5 -- REQUEST TABLE                                  *         
*                R6 -- PRINT QUEUE TABLE                              *         
*                R7 -- WORK                                           *         
*                R8 -- WORK                                           *         
*                R9 -- S P A R E                                      *         
*                RA -- CPRINT                                         *         
*                RB -- PROGRAM BASE                                   *         
*                RC -- COMMON STORAGE AREA                            *         
*                RD -- REGISTER SAVE AREA                             *         
*                RE -- WORK                                           *         
*                RF -- WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'MONSOON - LOCATE AND ATTACH SOON REQUESTS'                      
         MACRO                                                                  
&NAME    PRNT  &A,&PRINT=                                                       
&NAME    MVC   P(17),=CL17'&A'                                                  
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
         AIF   ('&PRINT' EQ 'ALWAYS').SKIP                                      
         CLI   TRACEFLG,C'Y'                                                    
         BNE   *+10                                                             
.SKIP    L     RF,=V(PRINTER)                                                   
         BASR  RE,RF                                                            
         MVI   P,C' '                                                           
         MVC   P+1(L'P-1),P                                                     
         MEND                                                                   
         EJECT                                                                  
MONSOON  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*MONSOON,=A(R13CHAIN)                                          
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         USING COMMWORK,RC                                                      
*                                                                               
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         USING CIDATAD,R6                                                       
         USING REQTABLD,R5                                                      
*                                                                               
         LA    R0,4                                                             
         LNR   R0,R0               R0 = -4                                      
         SVC   247                 SO MONSOON IS NOT SWAPPABLE                  
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         MVC   ECBLST,AOPERECB     A(ECB)                                       
         MVC   ECBLST2,AOPERECB                                                 
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS MONSOON BROUGHT UP WITH 'START'?         
         BNE   NOSTART                                                          
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
NOSTART  QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
*                                                                               
         EXTRACT FULL,'S',FIELDS=(ASID)                                         
         L     R3,FULL             MVS JOB ACCOUNTING INFORMATION               
         LOCASCB ASID=(R3)         PUTS A(ASCB) INTO R1                         
         ST    R1,AASCB            SAVE A(ASCB)                                 
*                                                                               
         EXTRACT FULL,FIELDS=TIOT                                               
         L     R3,FULL                                                          
         MVC   MNSOONID,TIOCPSTN-TIOT(R3)  EXTRACT STEPNAME                     
*                                                                               
         BRAS  RE,INITIAL          INITIALIZE                                   
         EJECT                                                                  
* FIND JCL ON ALL PRINT QUEUES, AND UPDATE THE REQUEST TABLE WITH THEM.         
* ATTACH ANY JCL WHICH IS FOR US, WAIT (IF NECESSARY), THEN GO BACK             
* AND UPDATE THE TABLE AGAIN.  THIS PROGRAM RUNS FOREVER UNTIL STOPPED          
* BY THE OPERATOR.                                                              
*                                                                               
LOOP     CLI   LDACTION,LDREFQ     TEST DATASPACE REFRESH REQUIRED              
         BNE   LOOP1                                                            
         LOAD  EPLOC=LOADCARD,ERRET=RCERR01A                                    
         GOTO1 (R0),LOADPARM                                                    
         DELETE EPLOC=LOADCARD                                                  
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS7,OPMS7)                       
*                                                                               
LOOP1    BRAS  RE,UPDTABLE         UPDATE THE REQUEST TABLE                     
*                                                                               
NEXT     CLI   OPERSTOP,C'Y'       DOES OPERATOR WANT TO STOP MONSOON?          
         BE    GOODBYE             YES                                          
         CLI   OPERKILL,C'Y'       DOES OPERATOR WANT TO KILL MONSOON?          
         BNE   ANYJCL              NO                                           
*                                                                               
GOODBYE  BRAS  RE,SHUTDOWN         WRAP UP                                      
         XBASE                                                                  
*                                                                               
ANYJCL   CLI   ATTCHFLG,C'Y'       ARE WE ATTACHING JCL?                        
         BNE   WAIT                NO                                           
*                                                                               
         OC    NEXTREQ,NEXTREQ     ANY JCL FOUND TO ATTACH?                     
         BZ    WAIT                NO                                           
*                                                                               
         CLI   LOPRTYF2,C'Y'       FORCE REQUEST TABLE REFRESH NOW?             
         BNE   *+12                                                             
         MVI   LOPRTYF2,C'N'       YES                                          
         B     LOOP                                                             
*                                                                               
         BRAS  RE,ATTACH           ATTACH THE JCL (IF NECESSARY)                
         OC    REFFILT,REFFILT     WAS MONSOON FOR ONE REPORT ONLY?             
         BZ    NEXT                NO -- LOOK FOR MORE JCL                      
         B     GOODBYE             YES -- STOP NOW                              
*                                                                               
WAIT     CLI   ATTCHFLG,C'X'       LET OPERATOR KNOW WAIT COMMAND SET           
         BNE   WAIT1                                                            
         MVI   ATTCHFLG,C'N'       SET NOT TO ATTACH                            
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS8,OPMS8)                       
WAIT1    BRAS  RE,WAITABIT         WE MIGHT WAIT A LITTLE WHILE                 
         B     LOOP                NOW LOOK FOR MORE REQUESTS                   
         SPACE 5                                                                
OPMS7    DC    C'DATASPACE REFRESHED'                                           
OPMS8    DC    C'MONSOON NOW IN WAIT STATE'                                     
RCERR01A DC    H'0'                CANT LOAD PHASE                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* PRINT SOME TOTALS AND WRAP UP.                                                
*                                                                               
SHUTDOWN NTR1  BASE=*                                                           
*                                                                               
         GOTO1 =V(DMENQDEQ),DMCB,(C'D',=C'ALL')                                 
         CLC   MINORNAM,SPACES                                                  
         BE    SH02                                                             
         DEQ   (MAJORNAM,MINORNAM,8,SYSTEMS)                                    
*                                                                               
SH02     CLI   LOADCARD,0                                                       
         BE    SH05                                                             
         LOAD  EPLOC=LOADCARD,ERRET=RCERR01B                                    
         MVI   LDACTION,LDDELQ     REMOVE DATASPACE                             
         GOTO1 (R0),LOADPARM                                                    
         DELETE EPLOC=LOADCARD                                                  
*                                                                               
SH05     OC    NUMCLASS,NUMCLASS   ANY CLASSES IN CLASS TABLE?                  
         BZ    SH20                                                             
         L     R3,NUMCLASS         YES -- PRINT TOTALS                          
         L     R4,ACLASSTB         A(CLASS TABLE)                               
         USING CLASSD,R4                                                        
SH10     MVC   P(39),=C'REQUESTS ATTACHED FOR CLASS XX = NNNNNN'                
         MVC   P+28(L'CLSNAME),CLSNAME                                          
         EDIT  CLSTOTAL,(6,P+33),ZERO=NOBLANK                                   
         GOTO1 =V(PRINTER)                                                      
         LA    R4,CLSTABLQ(R4)     BUMP TO NEXT CLASS                           
         BCT   R3,SH10                                                          
         DROP  R4                                                               
*                                                                               
SH20     MVC   P(39),=C'TOTAL REQUESTS ATTACHED =        NNNNNN'                
         EDIT  TOTREQS,(6,P+33),ZERO=NOBLANK                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P+30(43),=C'TOTAL LOADS AVOIDED: XXXXXXX OUT OF XXXXXXX'         
         EDIT  LOADAVD,(7,P+51),ZERO=NOBLANK,ALIGN=LEFT                         
         EDIT  LOADASK,(7,P+66),ZERO=NOBLANK,ALIGN=LEFT                         
         PRNT  TERMINATE,PRINT=ALWAYS                                           
*                                                                               
         CLOSE (FILE)              CLOSE LONG SOON JOBS RECORD FILE             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
RCERR01B DC    H'0'                CANT LOAD PHASE                              
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* INITIALIZE                                                                    
*                                                                               
INITIAL  NTR1  BASE=*                                                           
*                                                                               
         MVC   TITLE(7),=C'MONSOON'                                             
         PRNT  INITIALIZE,PRINT=ALWAYS                                          
*                                                                               
         BRAS  RE,LOADMODS         LOAD CORE-RESIDENT PHASES                    
*                                                                               
         BRAS  RE,READCRDS         READ PARAMETER CARDS                         
*                                                                               
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
         XC    KEY,KEY             BUILD HIGH-ID RECORD KEY                     
         MVI   KEY,C'I'                                                         
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,AIO,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                WHAT HAPPENED TO THE HIGH-ID RECORD?         
         L     R4,AIO                                                           
         BAS   RE,GETEL            FIND HIGH-ID ELEMENT (X'02')                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,2(R4)          HIGHEST ID NUMBER                            
         MHI   R2,USERTBLQ         R2 = AMOUNT OF SPACE NEEDED IN TABLE         
         LR    R3,R2               SAVE LENGTH OF TABLE                         
         LA    R2,8(R2)            ADD ROOM FOR LABEL                           
         GETMAIN EU,LV=(2),A=AUSERTAB                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL GET OF USERTAB STORAGE          
         L     R2,AUSERTAB                                                      
         MVC   0(8,R2),=C'*USERTAB'                                             
         LA    R2,8(R2)            BUMP PAST LABEL                              
         ST    R2,AUSERTAB         A(START OF USERTAB)                          
         LR    RE,R2                                                            
         LR    RF,R3               CLEAR USERTAB                                
         XCEFL                                                                  
*                                                                               
         BRAS  RE,BLDREQTB         INITIALIZE THE REQTABLE AND PQTABLE          
*                                                                               
         MVC   P(60),=C'PRTQU    # CI''S    #INDX    #RPTS    TRKS/CI  +        
                 A(PARTITION)'                                                  
         GOTO1 =V(PRINTER)                                                      
         MVC   P(60),=C'-----    ------    -----    -----    -------   +        
                ------------'                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     R6,ACITABLE                                                      
INIT10   MVC   P(5),CFPQID         PRINT QUEUE NAME                             
         EDIT  CICITOT,(5,P+10)    TOTAL # OF PART 1 CONTROL INTERVALS          
         EDIT  CICINDX,(3,P+21)    NUMBER OF INDEX CONTROL INTERVALS            
         EDIT  CIMAXRPT,(5,P+28)   NUMBER OF REPORT CONTROL INTERVALS           
         EDIT  CITRKS,(3,P+41)     NO. OF TRACKS/CI                             
         GOTO1 =V(HEXOUT),DMCB,CIAPART,P+52,4,=C'TOG'                           
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(PRINTER)         PRINT INFO FOR ONE PRINT QUEUE               
         LA    R6,CITBLLNQ(R6)                                                  
         CLI   0(R6),X'FF'         ANY MORE PRINT QUEUES?                       
         BNE   INIT10                                                           
*                                                                               
         GOTO1 =V(PRINTER)         YES -- SKIP A LINE                           
         MVC   P(36),=C'TOTAL NUMBER OF REPORTS SUPPORTED:  '                   
         EDIT  REQTBLSZ,(6,P+36),ALIGN=LEFT                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         L     R1,X'10'            POINT TO CPU ID                              
         L     R1,X'C4'(R1)                                                     
         LA    R1,X'10'(R1)        A(FOUR CHARACTER CPU ID)                     
         ST    R1,ACPUID                                                        
         MVC   P(8),=C'CPU ID: '                                                
         MVC   P+8(4),0(R1)                                                     
*                                                                               
         LA    R1,CPUTABLE         LIST OF CPU IDS                              
INIT20   CLI   0(R1),X'FF'         END OF LIST?                                 
         BE    INIT30              YES -- DON'T ADJUST TCB PERCENTAGE           
         CLC   0(4,R1),P+8         MATCH ON CPU ID?                             
         BE    *+12                YES, R1 = A(CPU ID)                          
         LA    R1,L'CPUTABLE(R1)   BUMP TO NEXT CPU ID                          
         B     INIT20                                                           
         MVC   TCBPCT,4(R1)        SAVE TCB ADJUSTMENT PERCENTAGE               
*                                                                               
INIT30   GOTO1 =V(PRINTER)                                                      
         MVC   P(6),=C'ASID: '                                                  
         L     R1,AASCB            A(ASCB)                                      
         MVC   HALF,ASCBASID-ASCB(R1)                                           
         GOTO1 =V(HEXOUT),DMCB,HALF,P+6,2,=C'TOG'                               
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         ZAP   LINE,=PL2'75'       EJECT PAGE NEXT TIME                         
*                                                                               
         OPEN  (FILE,OUTPUT)       OPEN DS FOR RECORD LONG SOON JOBS            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
CPUTABLE DS    0CL6                CPU ID TABLE                                 
*                                  FIRST 4 BYTES = CPU ID                       
*                                  NEXT 2 BYTES = TCB %AGE ADJUSTMENT           
*&&US                                                                           
         DC    C'SYI ',AL2(50)                                                  
         DC    C'SYJ ',AL2(50)     ADDED 2/23/98                                
*                                   FROE SAYS SYJ MIGHT GET LESS LATER          
*&&                                                                             
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* LOAD A BUNCH OF CORE-RESIDENT (E.G., T00A) PHASES, AND BUILD A LIST           
* OF THEIR ADDRESSES.  THE ADDRESS OF THIS LIST WILL BE PASSED VIA A            
* CONTROL CARD TO MASTER.                                                       
*                                                                               
LOADMODS NTR1  BASE=*                                                           
*                                                                               
         LA    R3,CORETAB          A(CORE-RESIDENT PHASE TABLE)                 
*                                                                               
LM10     CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    LOADMODX            YES                                          
*                                                                               
         MVC   DUB,SPACES          PHASE NAMES MUST BE BLANK-PADDED             
         GOTO1 =V(HEXOUT),DMCB,1(R3),DUB,3,=C'TOG'                              
         CLC   =F'6',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   DUB,C'T'            PHASE NAMES ALL BEGIN WITH C'T'              
*                                                                               
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         ICM   RF,15,DMCB+4        A(PHASE)                                     
         BZ    LM20                PHASE NOT FOUND                              
         ST    RF,4(R3)            PUT A(PHASE) IN TABLE                        
*                                                                               
         TM    0(R3),X'80'         SHOULD WE GO TO THIS ROUTINE NOW?            
         BZ    LM20                                                             
         SR    R1,R1               YES -- CLEAR R1 SO ROUTINE. . .              
         BASR  RE,RF               . . . KNOWS WE CAME FROM MONSOON             
*                                                                               
LM20     LA    R3,8(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         B     LM10                                                             
*                                                                               
LOADMODX XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* READ PARAMETER CARDS                                                          
*                                                                               
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD                        
*   CLASS=ALL        ATTACH REQUESTS OF ANY CLASS                               
*   CLASS=S1=2,A3... ATTACH REQUESTS OF CLASS SPOT1, ACC3, ETC.                 
*                     WITH OPTIONAL REQUEST LIMITS ON EACH CLASS                
*                     (CAN HAVE > 1 OF THESE CARDS)                             
*   WAITSECS=N       WAIT N SECONDS BETWEEN PQ SEARCHES (DEFAULT = 120)         
*   REQTIMER=N       WARN OPER OF LONG REQUEST EVERY N MINS (DEFAULT=5)         
*   REQTIMER=NONE    DO NOT GENERATE OPERATOR WARNINGS ON LONG REQUESTS         
*   USERMAXREQ=N     MAX REQS PER USERID (DEFAULT=5)                            
*   JESMSG=YES       GIVE CONSOLE MSGS AT REQ START/END (DEFAULT NO)            
*   TYPE=UPDATE      ATTACH ONLY UPDATIVE SOONS                                 
*   TYPE=NOUPDATE    ATTACH ONLY NON-UPDATIVE SOONS                             
*   MINTIME=N        ATTACH ONLY SOONS OF AGE N MINUTES OR OLDER                
*   CLASSPRTY=YES    CLASS LIST IS PRIORITIZED (DEFAULT NO)                     
*   REFRESH=N        REFRESH REQUEST LIST AFTER EVERY N ATTACHES                
*   REFRESH=PRTY<N   REFRESH REQUEST LIST UPON FINDING PRTY<N                   
*   USERID=CCCCCC    ATTACH REQUESTS FOR THIS USERID ONLY                       
*   SUBID=CCC        ATTACH REQUESTS FOR THIS SUB-ID ONLY                       
*   REFNUM=N         ATTACH REQUESTS FOR THIS REFERENCE NUMBER ONLY             
*   DATE=YESTERDAY   ATTACH REQUESTS FROM PREVIOUS BUSINESS DAY ONLY            
*   MINPRTY=N        ATTACH REQUESTS WITH PRIORITY >= N (DEFAULT = 0)           
*   MAXPRTY=N        ATTACH REQUESTS WITH PRIORITY <= N (DEFAULT = 9)           
*   FACPAK=X         ATTACH REQUESTS FROM FACPAKX ONLY (SEE FASPOON)            
*   FACPAK=-X        EXCLUDE REQUESTS FROM FACPAKX (SEE FASPOON)                
*   EXUSERID=CCCCCC  EXCLUDE REQUESTS FOR THIS USERID                           
*                     (CAN HAVE > 1 OF THESE CARDS)                             
*   EXREPORT=SPP     EXCLUDE REQUESTS FOR THIS SYSTEM/PROGRAM                   
*                     (E.G.: SI2 (DON'T RUN SPOT I2 REQUESTS))                  
*   MONSOONID=CCCCCCCC  ENQ THIS NAME GLOBALLY                                  
*   ENQJOBNAME=YES   ONLY ALLOW ONE "JOBNAME" TO RUN AT ONCE (VIA ENQ)          
*   DSPACE=X         PASS A DSPACE CONTROL CARD TO MASTER                       
*                                                                               
*   THE FOLLOWING PARAMETERS ARE REALLY FOR TESTING/DEBUGGING ONLY              
*                                                                               
*   ATTACH=NO        DON'T ATTACH REQUESTS; JUST FIND THEM (DEF. = YES)         
*   DDSIO=DDSIOX     ALTERNATE DDSIO (FOR MONSOON AND ATTACHED TASKS)           
*   DUPDUMPWARN=NO   DON'T WARN CONSOLE ABOUT DUPLICATE DUMPS                   
*   DATE=ANY         ATTACH REQUESTS FROM ANY DATE                              
*   TCBTIME=N        MAX TASK TCB TIME ALLOWED IN SEC                           
*                                                                               
READCRDS NTR1  BASE=*                                                           
*                                                                               
         OPEN  (MONSYSIN)          OPEN PARAMETER CARD FILE                     
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RC10     GET   MONSYSIN,CARD       READ A MONSOON PARAMETER CARD                
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC10                YES                                          
*                                                                               
         CLC   =C'TRACE=',CARD     TRACE CARD?                                  
         BNE   RC15                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC10                TRACE=NO IS THE DEFAULT                      
         CLC   =C'YES',CARD+6                                                   
         BNE   INVCARD                                                          
         MVI   TRACEFLG,C'Y'       TRACE=YES                                    
         B     RC10                                                             
*                                                                               
RC15     CLC   =C'CLASS=',CARD     CLASS=                                       
         BNE   RC30                                                             
         CLC   =C'ALL',CARD+6      CLASS=ALL?                                   
         BNE   *+12                                                             
         MVI   ALLCLASS,C'Y'       YES - SET FLAG                               
         B     RC10                                                             
*                                                                               
         MVC   CARD(74),CARD+6     LEFT JUSTIFY THE CLASSES                     
         MVC   CARD+74(6),SPACES                                                
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),AIO                                 
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    INVCARD                                                          
*                                                                               
         L     R4,AIO              START OF SCANNER BLOCK                       
RC20     LA    R3,CLASS                                                         
         USING CLASSD,R3                                                        
         XC    CLASS,CLASS                                                      
         MVC   CLSNAME,12(R4)      PUT CLASS CODE INTO 'RECORD'                 
         GOTO1 =V(BINSRCH),DMCB,(1,CLASS),ACLASSTB,NUMCLASS,CLSTABLQ,  +        
               L'CLSNAME,MAXCLASS                                               
         SR    R3,R3                                                            
         ICM   R3,7,DMCB+1                                                      
         BNZ   *+6                                                              
         DC    H'0'                CLASS TABLE IS NOT BIG ENOUGH                
         TM    DMCB,X'01'                                                       
         BZ    INVCARD             DUPLICATE CLASS SPECIFIED                    
         MVC   CLSMAX,=X'FFFF'     DEFAULT CLSMAX IS HIGH VALUE                 
         CLI   1(R4),0             ANY CLASS LIMIT GIVEN?                       
         BE    *+18                                                             
         TM    3(R4),X'80'         YES -- IS IT NUMERIC?                        
         BZ    INVCARD                                                          
         MVC   CLSMAX,10(R4)       YES -- SAVE IT                               
         MVC   NUMCLASS,DMCB+8     UPDATE TABLE SIZE                            
         LH    RF,SVCLSPRT         LAST CLASS PRIORITY VALUE                    
         LA    RF,1(RF)                                                         
         STH   RF,SVCLSPRT                                                      
         STH   RF,CLSPRTY          SAVE CLASS PRIORITY                          
         LA    R4,32(R4)           BUMP TO NEXT CLASS                           
         BCT   R0,RC20                                                          
         B     RC10                                                             
         DROP  R3                                                               
*                                                                               
RC30     CLC   =C'MINPRTY=',CARD   MINPRTY=N                                    
         BNE   RC35                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+8,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   MINPRTY,CARD+8      SAVE MINIMUM PRIORITY                        
         B     RC10                                                             
*                                                                               
RC35     CLC   =C'MAXPRTY=',CARD   MAXPRTY=N                                    
         BNE   RC40                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+8,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   MAXPRTY,CARD+8      SAVE MAXIMUM PRIORITY                        
         B     RC10                                                             
*                                                                               
RC40     CLC   =C'DATE=YESTERDAY',CARD DATE=YESTERDAY                           
         BNE   *+12                                                             
         MVI   YESTERDY,C'Y'       ATTACH YESTERDAY'S REQUESTS ONLY             
         B     RC10                                                             
*                                                                               
         CLC   =C'DATE=ANY',CARD   DATE=ANY                                     
         BNE   *+12                                                             
         MVI   ANYDATE,C'Y'        ATTACH REQUESTS FROM ANY DATE                
         B     RC10                                                             
*                                                                               
         CLC   =C'DSPACE=',CARD    DSPACE=X                                     
         BNE   RC45                                                             
         MVC   DSPACE,CARD+7       PASS DSPACE CARD TO MASTER                   
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),DSPACE                                     
         B     RC10                                                             
*                                                                               
RC45     CLC   =C'DDSIO=',CARD     DDSIO=                                       
         BNE   RC50                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      THIS ONE IS FOR MONSOON ONLY                 
         MVC   DDSIOMOD,CARD+6     THIS ONE IS FOR ATTACHED TASKS               
         B     RC10                                                             
*                                                                               
RC50     CLC   =C'WAITSECS=',CARD  WAITSECS=N                                   
         BNE   RC60                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         MHI   R1,100              SCALE THE TIME INTERVAL FOR STIMER           
         ST    R1,WAITSECS                                                      
         B     RC10                                                             
*                                                                               
RC60     CLC   =C'USERMAXREQ=',CARD  USERMAXREQ=N                               
         BNE   RC70                                                             
         CLC   CARD+11(2),=C'NO'                                                
         BNE   *+14                                                             
         MVC   USERMAXR,=X'FFFF'   SET TO FFS FOR NONE                          
         B     RC10                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+11,(2,0)                                    
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   USERMAXR,DMCB+6                                                  
         B     RC10                                                             
*                                                                               
RC70     CLC   =C'USERID=',CARD    USERID=XXX                                   
         BNE   RC80                                                             
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),CARD+7   USER-ID                                      
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,AIO,0             
         CLI   DMCB+8,0                                                         
         BNE   INVCARD             ID RECORD NOT FOUND                          
         L     R4,AIO                                                           
         BAS   RE,GETEL            FIND ID ELEMENT (X'02')                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   USERFILT,2(R4)      HEX USERID FILTER                            
         B     RC10                                                             
*                                                                               
RC80     CLC   =C'JESMSG=',CARD    JESMSG=                                      
         BNE   RC90                                                             
         CLC   =C'NO',CARD+7                                                    
         BE    RC10                JESMSG=NO IS THE DEFAULT                     
         CLC   =C'YES',CARD+7                                                   
         BNE   INVCARD                                                          
         MVI   JESMSG,C'Y'         JESMSG=YES                                   
         B     RC10                                                             
*                                                                               
RC90     CLC   =C'SUBID=',CARD     SUBID=                                       
         BNE   *+14                                                             
         MVC   SUBFILT,CARD+6                                                   
         B     RC10                                                             
*                                                                               
         CLC   =C'EXREPORT=',CARD  EXREPORT=                                    
         BNE   *+14                                                             
         MVC   EXREPORT,CARD+9                                                  
         B     RC10                                                             
*                                                                               
         CLC   =C'ATTACH=',CARD    ATTACH=                                      
         BNE   RC100                                                            
         CLC   =C'YES',CARD+7                                                   
         BE    RC10                ATTACH=YES IS THE DEFAULT                    
         CLC   =C'NO',CARD+7                                                    
         BNE   INVCARD                                                          
         MVI   ATTCHFLG,C'N'       ATTACH=NO                                    
         B     RC10                                                             
*                                                                               
RC100    CLC   =C'REFNUM=',CARD    REFNUM=                                      
         BNE   RC110                                                            
         GOTO1 =V(NUMVAL),DMCB,CARD+7,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   REFFILT,DMCB+6      REFERENCE NUMBER                             
         B     RC10                                                             
*                                                                               
RC110    CLC   =C'REQTIMER=',CARD  REQTIMER=N                                   
         BNE   RC120                                                            
         CLC   =C'NONE',CARD+9                                                  
         BNE   *+14                                                             
         MVC   REQTIMER,=C'NONE'                                                
         B     RC10                                                             
         GOTO1 =V(NUMVAL),DMCB,CARD+9,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         L     R1,DMCB+4           TIME IN MINUTES                              
         MHI   R1,6000                                                          
         ST    R1,REQTIMER                                                      
         B     RC10                                                             
*                                                                               
RC120    CLC   =C'TYPE=',CARD      TYPE=                                        
         BNE   RC130                                                            
         CLC   =C'UPDATE',CARD+5                                                
         BNE   *+12                                                             
         MVI   TYPUPD,C'Y'                                                      
         B     RC10                                                             
         CLC   =C'NOUPDATE',CARD+5                                              
         BNE   INVCARD                                                          
         MVI   TYPNOUPD,C'Y'                                                    
         B     RC10                                                             
*                                                                               
RC130    CLC   =C'MINTIME=',CARD   MINTIME=N                                    
         BNE   RC140                                                            
         GOTO1 =V(NUMVAL),DMCB,CARD+8,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         L     R1,DMCB+4           TIME IN MINUTES                              
         MHI   R1,6000                                                          
         ST    R1,MINTIME                                                       
         B     RC10                                                             
*                                                                               
RC140    CLC   =C'CLASSPRTY=',CARD CLASSPRTY=                                   
         BNE   RC150                                                            
         CLC   =C'NO',CARD+10                                                   
         BE    RC10                CLASSPRTY=NO IS THE DEFAULT                  
         CLC   =C'YES',CARD+10                                                  
         BNE   INVCARD                                                          
         MVI   CLSPRTYF,C'Y'       CLASSPRTY=YES                                
         B     RC10                                                             
*                                                                               
RC150    CLC   =C'FACPAK=',CARD    FACPAK=                                      
         BNE   RC152                                                            
         MVI   FACPAKFL,C'P'       ASSUME POSITIVE FILTER                       
         MVC   FACPAK,CARD+7                                                    
         CLI   CARD+7,C'-'                                                      
         BNE   RC10                                                             
         MVI   FACPAKFL,C'N'       IT'S A NEGATIVE FILTER                       
         MVC   FACPAK,CARD+8                                                    
         B     RC10                                                             
*                                                                               
RC152    CLC   =C'REFRESH=',CARD   REFRESH=                                     
         BNE   RC160                                                            
         CLC   =C'PRTY<',CARD+8                                                 
         BE    RC155                                                            
         GOTO1 =V(NUMVAL),DMCB,CARD+8,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   REFRESH,DMCB+6      REFRESH VALUE                                
         B     RC10                                                             
*                                                                               
RC155    GOTO1 =V(NUMVAL),DMCB,CARD+13,(2,0)                                    
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   REFPRTY,CARD+13     REFRESH PRIORITY VALUE                       
         B     RC10                                                             
*                                                                               
RC160    CLC   =C'LOAD=',CARD      LOAD=   GENERATE AND LOAD DATASPACE          
         BNE   RC170                                                            
         MVC   LOADCARD,CARD+5     SAVE MODULE NAME                             
         LOAD  EPLOC=CARD+5,ERRET=RCERR01                                       
         GOTO1 (R0),LOADPARM                                                    
         BNE   RCERR02                                                          
         DELETE EPLOC=CARD+5                                                    
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS6,OPMS6)                       
         B     RC10                                                             
*                                                                               
RC170    CLC   =C'MONSOONID=',CARD   MONSOONID=                                 
         BNE   RC180                                                            
         MVC   MINORNAM,CARD+10                                                 
         MVC   MNSOONID,MINORNAM   OVERRIDE STEPNAME WITH UNIQUE NAME           
         ENQ   (MAJORNAM,MINORNAM,E,8,SYSTEMS),RET=USE                          
         LTR   RF,RF                                                            
         BZ    RC10                                                             
         MVC   OPMS9+10(8),MINORNAM                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS9,OPMS9)                       
         DC    H'0'                THIS MONSOON IS ALREADY RUNNING              
*                                                                               
RC180    CLC   =C'EXUSERID=',CARD  EXUSERID=CCCCCC                              
         BNE   RC190                                                            
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),CARD+9   USER-ID                                      
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,AIO,0             
         CLI   DMCB+8,0                                                         
         BNE   INVCARD             ID RECORD NOT FOUND                          
         L     R4,AIO                                                           
         BAS   RE,GETEL            FIND ID ELEMENT (X'02')                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,EXUSRIDS         ARRAY OF EXCLUDED USERIDS                    
         LA    R0,EXUSRMXQ         MAXIMUM NUMBER OF EXUSERID CARDS             
RC185    OC    0(2,RF),0(RF)       IS THIS ARRAY SLOT FREE?                     
         BZ    *+14                                                             
         LA    RF,2(RF)                                                         
         BCT   R0,RC185                                                         
         DC    H'0'                TOO MANY EXUSERID CARDS                      
         MVC   0(2,RF),2(R4)       HEX USERID NEGATIVE FILTER                   
         B     RC10                                                             
*                                                                               
RC190    CLC   =C'ENQJOBNAME=',CARD   ENQJOBNAME=                               
         BNE   RC200                                                            
         CLC   =C'NO',CARD+11                                                   
         BE    RC10                ENQJOBNAME=NO IS THE DEFAULT                 
         CLC   =C'YES',CARD+11                                                  
         BNE   INVCARD                                                          
         MVI   ENQJOBNM,C'Y'       ENQJOBNAME=YES                               
         B     RC10                                                             
*                                                                               
RC200    CLC   =C'DUPDUMPWARN=',CARD   WARN CONSOLE ABOUT DUP. DUMPS?           
         BNE   RC210                                                            
         CLC   =C'YES',CARD+12                                                  
         BE    RC10                YES BY DEFAULT                               
         CLC   =C'NO',CARD+12                                                   
         BNE   INVCARD                                                          
         MVI   DUMPWARN,C'N'       NO                                           
         B     RC10                                                             
*                                                                               
RC210    CLC   =C'TCBTIME=',CARD   MAX TCB TIME ALLOWED                         
         BNE   INVCARD                                                          
         GOTO1 =V(NUMVAL),DMCB,CARD+8,(2,0)                                     
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   MAXTCBTM,DMCB+4                                                  
         B     RC10                                                             
*                                                                               
INVCARD  MVC   P+20(22),=C'- INVALID CONTROL CARD'                              
         GOTO1 =V(PRINTER)                                                      
         DC    H'0'                                                             
*                                                                               
RCX      CLOSE (MONSYSIN)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
         XIT1                                                                   
RCERR01  DC    H'0'                CANT LOAD PHASE                              
RCERR02  DC    H'0'                CANT LOAD DATASPACE                          
         SPACE 3                                                                
MONSYSIN DCB   DDNAME=MONSYSIN,MACRF=GM,DSORG=PS,RECFM=FB,LRECL=80,    +        
               EODAD=RCX                                                        
         SPACE 3                                                                
OPMS6    DC    C'DATASPACE ACQUIRED AND LOADED'                                 
OPMS9    DC    C'MONSOONID XXXXXXXX ALREADY RUNNING'                            
         LTORG                                                                  
         EJECT                                                                  
BLDREQTB NTR1  BASE=*                                                           
*                                                                               
* BUILD THE PRINT QUEUE TABLE, AND FROM THIS DATA,                              
* FORMAT THE REQUEST TABLE.                                                     
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'GLIST'),PRTQUE,AIO,0,CXREC                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         L     R4,32(R4)           SAVE A(PRINT QUEUE LIST)                     
*                                                                               
         ZIC   R3,0(R4)            NUMBER OF PRINT QUEUES                       
         SR    R2,R2               REPORT COUNTER                               
         L     R6,ACITABLE         A(PRINT QUEUE TABLE)                         
BJ10     LA    R4,8(R4)            BUMP TO NEXT PRINT QUEUE                     
         XC    0(CITBLLNQ,R6),0(R6)                                             
         MVC   CFPQENUM,4(R4)      PRINT QUEUE EXTERNAL FILE NUMBER             
         MVC   PRTQUE+4(1),1(R4)   CONSTRUCT C'PRTQ#'                           
         GOTO1 =V(DATAMGR),DMCB,(0,=C'BUFFER'),PRTQUE,0,0,CXREC                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CIDATA,CXREC+12     CIDATA IS HERE                               
         L     RF,=V(DMENQDEQ)     V(DMENQDEQ)                                  
         ST    RF,CIENQDEQ                                                      
         LA    RF,L'PQINDEX        LENGTH OF PQ KEY                             
         STH   RF,CINDXLN                                                       
         L     RF,CXREC+8          DISPLACEMENT TO PQ SAVE AREA                 
         LA    RF,CXREC(RF)                                                     
         ST    RF,APQSAVE          A(PQ SAVE AREA)                              
         LH    R0,CICITOT          NUMBER OF PART 1 CI'S. . .                   
         SH    R0,CICINDX          . . .MINUS INDEX ENTRIES. . .                
         STH   R0,CIMAXRPT         . . .EQUALS NUMBER OF REPORTS                
         AR    R2,R0               ADD UP NUMBER OF POSSIBLE REPORTS            
         LA    R6,CITBLLNQ(R6)                                                  
         BCT   R3,BJ10             LOOK AT ALL PRINT QUEUES                     
*                                                                               
         ST    R2,REQTBLSZ         SAVE NUMBER OF ENTRIES IN REQTABLE           
         MHI   R2,REQTABLQ         R2 = TOTAL SIZE OF REQTABLE                  
         LA    R2,8(R2)            ADD ROOM FOR LABEL                           
         GETMAIN EU,LV=(2),A=AREQTAB                                            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL GET OF REQTABLE STORAGE         
         A     R2,AREQTAB                                                       
         ST    R2,AREQTABX         A(END OF REQTABLE)                           
         L     R2,AREQTAB                                                       
         MVC   0(8,R2),=C'*REQTAB*'                                             
         LA    R2,8(R2)            BUMP PAST LABEL                              
         ST    R2,AREQTAB          A(START OF REQTABLE)                         
*                                                                               
         L     R6,ACITABLE         A(PRINT QUEUE TABLE)                         
         L     R5,AREQTAB                                                       
         ST    R5,CIAPART          PQ#1 PARTITION IS AT BEGINNING               
*                                                                               
BJ20     LH    R0,CIMAXRPT         MAX NUMBER OF REPORTS ON THIS PQ             
         LA    R5,REQTABLQ(R5)     BUMP REQTABLE POINTER UP 1 PARTITION         
         BCT   R0,*-4                                                           
*                                                                               
         LA    R6,CITBLLNQ(R6)                                                  
         CLI   0(R6),X'FF'         ANY MORE PRINT QUEUES?                       
         BE    *+12                NO                                           
         ST    R5,CIAPART          A(THIS PQ PARTITION)                         
         B     BJ20                                                             
*                                                                               
         BRAS  RE,CLRREQTB         CLEAR THE REQUEST TABLE                      
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
UPDTABLE NTR1  BASE=*                                                           
*                                                                               
* EVERY 30 MINUTES, CLEAR THE REQUEST TABLE (TO GET AROUND A HIDEOUS            
* BUG IN WHICH SOME JOBS SEEM TO BE FOUND, BUT IGNORED).  THEN READ             
* THE PRINT QUEUES TO FIND UNATTACHED JCL, AND PUT RELEVANT REQUEST             
* INFORMATION IN THE TABLE.                                                     
*                                                                               
         PRNT  UPDATETABLE,PRINT=ALWAYS                                         
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(2,WORK)                                   
*                                                                               
         SR    R0,R0                                                            
UP10     TIME  BIN                 GET THE CURRENT TIME                         
         LTR   R0,R0                                                            
         BZ    UP10                BAD RETURN FROM MACRO                        
         ST    R0,PQSRCHTM         TIME WE BEGAN SEARCHING PRINT QUEUES         
*                                                                               
         S     R0,REQCLRTM         R0=ELAPSED TIME SINCE REQTAB CLEARED         
         C     R0,=F'180000'       HAVE 30 MINUTES ELAPSED?                     
         BL    *+8                                                              
         BRAS  RE,CLRREQTB         YES -- CLEAR THE REQUEST TABLE               
*                                                                               
         L     R6,ACITABLE         PRINT QUEUE INFO                             
*                                                                               
UP30     XC    KEY,KEY                                                          
         GOTO1 =V(DATAMGR),DMCB,(0,=C'SEQ'),CFPQID,KEY,R,CXREC                  
         TM    DMCB+8,X'80'        END OF FILE?                                 
         BO    UP60                YES                                          
*                                                                               
         LA    R3,R                                                             
         USING PQPLD,R3                                                         
         CLI   QLSTAT+1,1          *** FUCKING HELL ***                         
         BH    UP30                *** IGNORE THIS REPORT ***                   
         TM    QLATTB,QLATJOBI     DOES REPORT CONTAIN JCL?                     
         BZ    UP30                NO                                           
         TM    QLATTB,QLATJOBO     IS THE REPORT CURRENTLY RUNNING?             
         BO    UP30                YES                                          
         TM    QLSTAT,PQSTHO+PQSTTE  HOLD OR TEMPORARY STATUS?                  
         BNZ   UP30                YES                                          
*                                                                               
         OC    USERFILT,USERFILT   IS THERE A USERID FILTER?                    
         BZ    *+14                NO                                           
         CLC   QLSRCID,USERFILT    YES - DOES THIS REPORT MATCH FILTER?         
         BNE   UP30                NO                                           
         LA    RF,EXUSRIDS         ARRAY OF EXCLUDED USERIDS                    
UP33     OC    0(2,RF),0(RF)                                                    
         BZ    UP37                                                             
         CLC   QLSRCID,0(RF)       IS THIS USERID IN THE ARRAY?                 
         BE    UP30                YES - IGNORE IT                              
         LA    RF,2(RF)                                                         
         B     UP33                                                             
UP37     CLC   SUBFILT,SPACES      IS THERE A SUBID FILTER?                     
         BE    *+14                NO                                           
         CLC   QLSUBID,SUBFILT     YES - DOES THIS REPORT MATCH FILTER?         
         BNE   UP30                NO                                           
         OC    REFFILT,REFFILT     IS THERE A REFERENCE NUMBER FILTER?          
         BZ    *+14                NO                                           
         CLC   QLREPNO,REFFILT     YES - DOES THIS REPORT MATCH FILTER?         
         BNE   UP30                NO                                           
*                                                                               
         CLI   TYPUPD,C'Y'         ATTACH ONLY UPDATIVE SOONS?                  
         BNE   *+12                NO                                           
         TM    QLTYPE,PQTYUPDT     YES - IS THIS JOB UPDATIVE?                  
         BZ    UP30                NO                                           
         CLI   TYPNOUPD,C'Y'       ATTACH ONLY NON-UPDATIVE SOONS?              
         BNE   *+12                NO                                           
         TM    QLTYPE,PQTYUPDT     YES - IS THIS JOB UPDATIVE?                  
         BO    UP30                NO                                           
         TM    QLTYPE,PQTYRSND+PQTYRRCV   LUNATIC REPORT OK TO RUN?             
         BO    UP30                NO                                           
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,QLREPNO                                                     
         SH    R5,CICINDX                                                       
         MHI   R5,REQTABLQ         DISPLACEMENT FROM START OF PARTITION         
         A     R5,CIAPART          R5 = A(REQUEST TABLE ENTRY)                  
*                                                                               
         C     R5,AREQTABX                                                      
         BL    *+6                                                              
         DC    H'0'                HOW CAN ENTRY BE AFTER TABLE END?            
         C     R5,CIAPART                                                       
         BL    UP30                D/A IS TOO LOW                               
         LA    R1,CITBLLNQ(R6)     A(NEXT PQ INFO)                              
         CLI   0(R1),X'FF'                                                      
         BE    *+12                IT'S OK -- JCL IS ON THE LAST PQ             
         C     R5,CIAPART-CIDATAD(R1)                                           
         BNL   UP30                D/A IS TOO HIGH                              
*                                                                               
         CLC   QLAGELD,WORK        REQUEST MADE YESTERDAY?                      
         BL    UP40                YES -- MUST BE OLD ENOUGH TO ATTACH          
         SR    R0,R0                                                            
         ICM   R0,3,QLAGELT        TIME OF REQUEST SUBMISSION                   
         MHI   R0,400              TIMES 4*100                                  
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         D     R0,=F'3'            R1 = SUBMIT TIME IN 1/100THS OF SEC.         
         L     R0,PQSRCHTM         R0 = CURRENT TIME                            
         SR    R0,R1                                                            
         C     R0,MINTIME                                                       
         BL    UP30                REQUEST ISN'T OLD ENOUGH TO ATTACH           
*                                                                               
UP40     CLC   REQREPUI,QLSRCID    IS IT THE SAME REPORT?                       
         BNE   UP50                                                             
         LR    R1,R5                                                            
         S     R1,CIAPART                                                       
         SR    R0,R0                                                            
         D     R0,=A(REQTABLQ)                                                  
         AH    R1,CICINDX                                                       
         CLM   R1,3,QLREPNO                                                     
         BNE   UP50                                                             
         CLC   REQTIME,QLAGELT                                                  
         BE    UP30                YES -- WE'VE SEEN IT AND SKIPPED IT          
*                                                                               
UP50     MVC   REQREPUI,QLSRCID    SAVE USER-ID                                 
         MVC   REQTIME,QLAGELT     TIME CREATED (4/3 SEC UNITS)                 
         OI    REQFLAGS,REQLOOKD   WE MUST LOOK AT THIS JCL                     
         B     UP30                                                             
         DROP  R3                                                               
*                                                                               
UP60     L     R5,CIAPART          A(START OF PARTITION)                        
         LH    R2,CIMAXRPT         MAX NUMBER OF REPORTS FOR THIS PQ            
*                                                                               
UP70     TM    REQFLAGS,REQLOOKD   SHOULD WE READ THE JCL?                      
         BZ    UP80                NO                                           
*                                                                               
         BRAS  RE,GETTTTT          PUTS TRACK NUMBER IN HALF                    
         L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKFCTRL,SKFCTRL     START READING FROM SCRATCH                   
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKFSTCI(2),HALF     USE TTTT FROM ABOVE                          
         MVI   SKFSTCI+2,1         ALWAYS START WITH BLOCK 1                    
         DROP  RF                                                               
         BRAS  RE,FILTJOB          CHECK OUT JCL -- SHOULD WE RUN IT?           
*                                                                               
UP80     LA    R5,REQTABLQ(R5)     INCREMENT REQUEST TABLE POINTER              
         BCT   R2,UP70                                                          
*                                                                               
         L     RF,APQSAVE          NO MORE CONTROL INTERVALS TO LOOK AT         
         USING SKBUFFD,RF                                                       
         XC    SKBCTRL,SKBCTRL     CLEAR PQ                                     
         XC    SKFCTRL,SKFCTRL                                                  
         XC    SKXCTRL,SKXCTRL                                                  
         DROP  RF                                                               
*                                                                               
         LA    R6,CITBLLNQ(R6)     BUMP TO NEXT PQ                              
         CLI   0(R6),X'FF'         END-OF-LIST?                                 
         BNE   UP30                NO                                           
*                                                                               
         LA    R1,UP100            EODAD                                        
         LA    RF,JCLFILE          A(DCB) FOR JCLFILE                           
         ST    R1,DCBEODAD-IHADCB(RF)                                           
*                                                                               
         OPEN  (JCLFILE,(INPUT))   FILE MAY CONTAIN LEFTOVER REQUEST            
         LTR   RF,RF                                                            
         BNZ   UP100                                                            
*                                                                               
         GET   JCLFILE,CARD                                                     
         CLC   =C'*JCLHDR*',CARD   IF WE GET THIS FAR. . .                      
         BE    *+6                 . . . THEN DATASET MUST HAVE JCL             
         DC    H'0'                                                             
*                                                                               
         L     R6,ACITABLE         PRINT QUEUE INFO                             
UP90     CLC   CFPQID,CARD+50      FIND TABLE ENTRY FOR THIS PRTQ               
         BE    *+18                GOT IT                                       
         LA    R6,CITBLLNQ(R6)     BUMP TO NEXT PQ                              
         CLI   0(R6),X'FF'         END-OF-LIST?                                 
         BNE   UP90                NO                                           
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXIN),DMCB,CARD+40,HALF,4                                    
         CLC   =F'2',DMCB+12       REFERENCE NUMBER                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R5,HALF                                                          
         SH    R5,CICINDX                                                       
         MHI   R5,REQTABLQ         DISPLACEMENT FROM START OF PARTITION         
         A     R5,CIAPART          R5 = A(REQUEST TABLE ENTRY)                  
*                                                                               
         MVC   REQCLASS,CARD+27    CLASS                                        
         MVC   REQPRTY,CARD+48     PRIORITY                                     
         MVC   REQSYPGM,CARD+56    SYSTEM/PROGRAM                               
         MVC   REQFACPK,CARD+60    FACPAK                                       
*                                                                               
         GOTO1 =V(HEXIN),DMCB,CARD+35,REQREPUI,4                                
         CLC   =F'2',DMCB+12       HEX USERID NUMBER                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXIN),DMCB,CARD+30,REQTIME,4                                 
         CLC   =F'2',DMCB+12       SUBMIT TIME                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(HEXIN),DMCB,CARD+45,REQFLAGS,2                                
         CLC   =F'1',DMCB+12       FLAGS                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    REQFLAGS,REQFILED   REQUEST CAME FROM JCLFILE, NOT PRTQ          
*                                                                               
         CLI   ALLCLASS,C'Y'       CLASS=ALL?                                   
         BNE   UP100               NO -- JOB WAS OK BEFORE, SO OK NOW           
         XC    CLASS,CLASS         MIGHT HAVE TO ADD CLASS TO TABLE             
         LA    RF,CLASS                                                         
         USING CLASSD,RF                                                        
         MVC   CLSNAME,REQCLASS                                                 
         MVC   CLSMAX,=X'FFFF'     DEFAULT CLSMAX IS HIGH VALUE                 
         DROP  RF                                                               
         GOTO1 =V(BINSRCH),DMCB,(1,CLASS),ACLASSTB,NUMCLASS,CLSTABLQ,  +        
               L'CLSNAME,MAXCLASS                                               
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                CLASS TABLE IS NOT BIG ENOUGH                
         MVC   NUMCLASS,DMCB+8     UPDATE TABLE SIZE                            
*                                                                               
UP100    CLOSE (JCLFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,SORTTABL         CHAIN REQUEST TABLE ENTRIES                  
         MVI   BYTE,C'B'           PARAMETER TO STATUS SUBROUTINE               
         BRAS  RE,STATUS           SHOW ANY CLASSES WHICH ARE BACKED UP         
         EJECT                                                                  
* PRINT TABLE INFORMATION                                                       
*                                                                               
         OC    NEXTREQ,NEXTREQ     ANY REQUESTS IN CHAIN AT ALL?                
         BNZ   UP110               YES                                          
         MVC   P(21),=C'NO REQUESTS TO ATTACH'                                  
         GOTO1 =V(PRINTER)                                                      
         B     UPX                                                              
*                                                                               
UP110    MVC   P(8),=C'NEXTREQ='   PRINT START OF CHAIN                         
         EDIT  NEXTREQ,(8,P+8),ALIGN=LEFT                                       
         GOTO1 =V(PRINTER)                                                      
         MVC   P(71),=C'A(ENTRY) PRTQ# TTTT USERID,REF#       TIME   PR+        
               TY CL SPP FAC#    NEXT  '                                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P(71),=C'-------- ----- ---- -----------     -------- --+        
               -- -- --- ----  --------'                                        
         GOTO1 =V(PRINTER)                                                      
         L     R5,NEXTREQ                                                       
*                                                                               
UP120    BCTR  R5,0                                                             
         MHI   R5,REQTABLQ         DISPLACEMENT TO NEXT ENTRY IN CHAIN          
         A     R5,AREQTAB          R5 = A(NEXT ENTRY IN CHAIN)                  
*                                                                               
         L     R6,ACITABLE         FIND OUT WHICH PQ THE JCL IS ON              
UP130    LA    R6,CITBLLNQ(R6)                                                  
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    *+12                YES                                          
         C     R5,CIAPART                                                       
         BNL   UP130                                                            
         AHI   R6,-CITBLLNQ        R6 = A(CITABLE ENTRY FOR THIS JCL)           
*                                                                               
         ST    R5,FULL             A(REQUEST TABLE ENTRY)                       
         GOTO1 =V(HEXOUT),DMCB,FULL,P,4,=C'TOG'                                 
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+9(5),CFPQID       C'PRTQN'                                     
         BRAS  RE,GETTTTT                                                       
         GOTO1 =V(HEXOUT),DMCB,HALF,P+15,2,=C'TOG'                              
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LH    RF,REQREPUI         USERID NUMBER                                
         BCTR  RF,0                                                             
         MHI   RF,USERTBLQ                                                      
         A     RF,AUSERTAB         RF = A(THIS USERID'S TABLE ENTRY)            
         MVC   P+20(8),USERNAME-USERTABD(RF)                                    
         LA    RF,P+27             END OF USERNAME                              
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                BACK UP TO LAST CHAR OF USERNAME             
         B     *-10                                                             
         MVI   1(RF),C','                                                       
         LR    R1,R5                                                            
         S     R1,CIAPART                                                       
         SR    R0,R0                                                            
         D     R0,=A(REQTABLQ)                                                  
         AH    R1,CICINDX          R1 = REPORT NUMBER                           
         EDIT  (R1),(5,2(RF)),ALIGN=LEFT                                        
*                                                                               
         TM    REQFLAGS,REQYESTD   YESTERDAY?                                   
         BZ    *+8                                                              
         MVI   P+35,C'Y'           C'Y' = YESTERDAY                             
         MVC   THREE(2),REQTIME                                                 
         BRAS  RE,CONVTHMS         CONVERT TIME TO X'HHMMSS'                    
         ZIC   R0,THREE            SUBMIT TIME (HOURS)                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+36(2),DUB                                                      
         MVI   P+38,C':'                                                        
         ZIC   R0,THREE+1          SUBMIT TIME (MINUTES)                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+39(2),DUB                                                      
         MVI   P+41,C':'                                                        
         ZIC   R0,THREE+2          SUBMIT TIME (SECONDS)                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+42(2),DUB                                                      
         MVC   P+45(1),REQPRTY     PRIORITY                                     
         MVC   P+50(2),REQCLASS    CLASS                                        
         MVC   P+53(3),REQSYPGM    SYSTEM/PROGRAM                               
         MVC   P+60(1),REQFACPK    FACPAK NUMBER                                
         OC    REQCHAIN,REQCHAIN   IS THIS THE LAST REQUEST?                    
         BZ    UP140                                                            
         EDIT  REQCHAIN,(8,P+63)   NO -- NEXT ENTRY IN REQUEST CHAIN            
         GOTO1 =V(PRINTER)         PRINT INFO FOR THIS REQUEST                  
         L     R5,REQCHAIN         POINT TO NEXT ENTRY IN CHAIN                 
         B     UP120                                                            
*                                                                               
UP140    GOTO1 =V(PRINTER)         PRINT INFO FOR LAST REQUEST                  
         ZAP   LINE,=PL2'75'       EJECT PAGE NEXT TIME                         
*                                                                               
UPX      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* CLEAR REQUEST TABLE                                                           
*                                                                               
CLRREQTB NTR1  BASE=*                                                           
*                                                                               
* THIS ROUTINE SIMPLY CLEARS THE ENTIRE REQUEST TABLE.  THIS IS                 
* NECESSARY BECAUSE OF A HIDEOUS BUG.  ONCE IN A WHILE, A JOB IS FOUND,         
* IT DOES NOT GET ATTACHED FOR SOME UNKNOWN REASON, AND UNLESS WE CLEAR         
* ITS REQUEST TABLE ENTRY, IT WILL NEVER RUN.  SO UNTIL WE FIND THE             
* BUG, WE SOLVE THE PROBLEM WITH A SLEDGEHAMMER BY CLEARING THE ENTIRE          
* TABLE.  SINCE THIS ONLY HAPPENS ONCE EVERY 30 MINUTES, THE COST IS            
* MIMINAL.                                                                      
*                                                                               
         CLI   ATTCHFLG,C'Y'       ARE WE ATTACHING JCL?                        
         BNE   CLX                 NO                                           
*                                                                               
         PRNT  CLEARREQTABLE                                                    
*                                                                               
         SR    R0,R0                                                            
CL10     TIME  BIN                 GET THE CURRENT TIME                         
         LTR   R0,R0                                                            
         BZ    CL10                BAD RETURN FROM MACRO                        
         ST    R0,REQCLRTM         TIME WE LAST CLEARED THE REQTABLE            
*                                                                               
         L     RF,AREQTAB          A(REQUEST TABLE)                             
         SR    R1,R1               TABLE INDEX NUMBER                           
CL20     LA    R1,1(R1)            INCREMENT INDEX                              
         XC    0(REQTABLQ,RF),0(RF) CLEAR REQUEST TABLE ENTRY                   
         LA    RF,REQTABLQ(RF)     BUMP TO NEXT ENTRY                           
         C     R1,REQTBLSZ         ANY MORE ENTRIES TO CLEAR?                   
         BL    CL20                YES                                          
*                                                                               
CLX      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
FILTJOB  NTR1  BASE=*                                                           
*                                                                               
* READ THE CONTENTS OF THE PRINT QUEUE REPORT AND EXTRACT THE TIME              
* SUBMITTED, REQUEST PRIORITY, CLASS, AND FACPAK.  IF THE REQUEST WAS           
* SUBMITTED YESTERDAY OR TODAY, THE CLASS IS ONE OF OURS, THE PRIORITY          
* IS HIGH ENOUGH FOR THIS MONSOON, AND IT'S THE RIGHT FACPAK, THEN PUT          
* THE SUBMIT TIME INTO THE REQUEST TABLE ENTRY.                                 
*                                                                               
* R6 = A(CITABLE ENTRY)                                                         
* R5 = A(REQUEST TABLE ENTRY)                                                   
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 =V(DATAMGR),DMCB,(0,=C'RANDOM'),CFPQID,0,R,CXREC                 
         CLI   DMCB+8,0            READ PQ HEADER INFORMATION                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,R                                                             
         USING PQPLD,R4                                                         
         CLC   REQREPUI,QLSRCID    IS IT THE SAME REPORT?                       
         BNE   FJSKIP                                                           
         LR    R1,R5                                                            
         S     R1,CIAPART                                                       
         SR    R0,R0                                                            
         D     R0,=A(REQTABLQ)                                                  
         AH    R1,CICINDX          R1 = REPORT NUMBER                           
         CLM   R1,3,QLREPNO                                                     
         BNE   FJSKIP                                                           
         CLC   REQTIME,QLAGELT                                                  
         BNE   FJSKIP              NO -- IT'S BEEN PURGED                       
         TM    QLATTB,QLATJOBI     YES -- DOES IT STILL CONTAIN JCL?            
         BZ    FJSKIP              NOT ANY MORE -- IT COMPLETED                 
         TM    QLATTB,QLATJOBO     IS THE REPORT CURRENTLY RUNNING?             
         BO    FJSKIP              YES                                          
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(2,WORK)                                   
         CLC   QLDATEL,WORK        WAS REPORT SUBMITTED TODAY?                  
         BNE   *+16                NO                                           
         CLI   YESTERDY,C'Y'       ATTACH YESTERDAY'S REQUESTS ONLY?            
         BE    FJSKIP              YES -- IGNORE                                
         B     FJ20                                                             
*                                                                               
         CLI   ANYDATE,C'Y'        ATTACH REQUESTS FROM ANY DATE?               
         BE    FJ20                YES -- ACCEPT IT                             
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(3,THREE)                                  
         LA    R3,DMCB                                                          
         USING GETRETD,R3                                                       
         XC    DMCB(24),DMCB                                                    
         MVC   GRDIDY(3),THREE     TODAY'S DATE (BINARY YMD)                    
         MVI   GRDFLAG,GRDFBACK+GRDFHLOK                                        
         MVC   GRDHRS,=H'24'                                                    
         GOTO1 =V(GETRET),DMCB                                                  
         CLI   GRDRETC,0                                                        
         BE    *+6                                                              
         DC    H'0'                INVALID RETURN CODE FROM GETRET              
         MVC   THREE,GRDODY        RETURNED DATE (BINARY YMD)                   
         DROP  R3                                                               
         GOTO1 =V(DATCON),DMCB,(3,THREE),(2,WORK)                               
         CLC   QLDATEL,WORK        WHEN WAS REPORT REQUESTED?                   
         BL    FJSKIP              PRIOR TO YESTERDAY -- IGNORE                 
         OI    REQFLAGS,REQYESTD   FLAG REPORT AS CREATED YESTERDAY             
         DROP  R4                                                               
*                                                                               
FJ20     GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                   
         CLI   DMCB+8,0            READ A CARD                                  
         BNE   FJSKIP              JOB CARD NOT FOUND                           
         CLC   =C'$$MONSOON',R+1   NEW-STYLE "JCL"?                             
         BE    FJ95                                                             
*                                                                               
         CLC   =C'//',R+1          NO -- LOOK FOR JOB CARD                      
         BNE   FJ20                                                             
         CLC   =C' JOB ',R+11                                                   
         BNE   FJ20                                                             
*                                                                               
         MVC   REQSYPGM,R+7        SYSTEM/PROGRAM                               
         CLC   EXREPORT,SPACES     ANY REPORT EXCLUSION FILTER?                 
         BE    *+14                                                             
         CLC   REQSYPGM,EXREPORT   YES: EXCLUDE THIS REQUEST?                   
         BE    FJSKIP              YES                                          
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                   
         CLI   DMCB+8,0            READ THE JOB CONTINUATION CARD               
         BNE   FJSKIP                                                           
         CLC   =C',CLASS=',R+26    CLASS PARAMETER MUST BE HERE                 
         BNE   FJSKIP              IT ISN'T                                     
         CLC   =C',PRTY=',R+34     PRIORITY PARAMETER MUST BE HERE              
         BNE   FJSKIP              IT ISN'T                                     
*                                                                               
         MVC   REQPRTY,R+40        SAVE PRIORITY IN REQUEST TABLE               
         CLC   REQPRTY,MINPRTY                                                  
         BL    FJSKIP              PRIORITY IS TOO LOW FOR THIS MONSOON         
         CLC   REQPRTY,MAXPRTY                                                  
         BH    FJSKIP              PRIORITY TOO HIGH FOR THIS MONSOON           
*&&US                                                                           
FJ30     GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                   
         CLI   DMCB+8,0            READ A CARD                                  
         BNE   FJSKIP              CLASS= CARD NOT FOUND                        
         CLC   =C'//*MAIN CLASS=',R+1                                           
         BNE   FJ30                                                             
         MVC   HALF,R+15           SAVE CLASS                                   
*&&                                                                             
*&&UK                                                                           
         MVC   HALF(1),R+33        USE CLASS FROM JOB CARD                      
         MVI   HALF+1,C' '                                                      
*&&                                                                             
FJ35     GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                   
         CLI   DMCB+8,0            READ A CARD                                  
         BNE   FJSKIP              FACPAK= CARD NOT FOUND                       
         CLC   =C'FACPAK=',R+1                                                  
         BNE   FJ35                                                             
         MVC   REQFACPK,R+8        FACPAK ID                                    
         CLI   FACPAK,C' '         ANY FACPAK FILTER?                           
         BE    FJ38                NO                                           
         CLC   FACPAK,R+8          MATCH ON FACPAK FILTER?                      
         BNE   *+16                                                             
         CLI   FACPAKFL,C'P'       YES, TAKE IT IF FILTER IS POSITIVE           
         BNE   FJSKIP                                                           
         B     FJ38                                                             
         CLI   FACPAKFL,C'P'       NO, TAKE IT IF FILTER IS NEGATIVE            
         BE    FJSKIP                                                           
*                                                                               
FJ38     XC    CLASS,CLASS                                                      
         LA    RF,CLASS                                                         
         USING CLASSD,RF                                                        
         MVC   CLSNAME,HALF                                                     
         CLI   ALLCLASS,C'Y'       CLASS=ALL?                                   
         BNE   FJ40                NO -- SEE IF WE WANT THIS CLASS              
         MVC   CLSMAX,=X'FFFF'     DEFAULT CLSMAX IS HIGH VALUE                 
         DROP  RF                                                               
         GOTO1 =V(BINSRCH),DMCB,(1,CLASS),ACLASSTB,NUMCLASS,CLSTABLQ,  +        
               L'CLSNAME,MAXCLASS                                               
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                CLASS TABLE IS NOT BIG ENOUGH                
         MVC   NUMCLASS,DMCB+8     UPDATE TABLE SIZE                            
         B     FJ50                                                             
*                                                                               
FJ40     GOTO1 =V(BINSRCH),DMCB,CLASS,ACLASSTB,NUMCLASS,CLSTABLQ,      +        
               L'CLSNAME,MAXCLASS                                               
         TM    DMCB,X'01'          CLASS FOUND?                                 
         BO    FJSKIP              NO -- WE WILL NOT RUN THIS REQUEST           
*                                                                               
FJ50     L     RF,DMCB             R2 = A(CLASS TABLE ENTRY)                    
         USING CLASSD,RF                                                        
         MVC   REQCLASS,CLSNAME    PUT CLASS NAME IN REQUEST TABLE              
         B     FJX                                                              
         DROP  RF                                                               
         EJECT                                                                  
FJ95     XC    REQSYPGM,REQSYPGM   CLEAR FILTER FIELDS                          
         MVI   REQPRTY,0                                                        
         MVI   REQFACPK,0                                                       
         XC    REQCLASS,REQCLASS                                                
*                                                                               
FJ100    GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                   
         CLI   DMCB+8,0            READ A CARD                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =C'SYSTEM=',R+1                                                  
         BNE   *+14                                                             
         MVC   REQSYST,R+8         SAVE SYSTEM                                  
         B     FJ100                                                            
*                                                                               
         CLC   =C'PROGRAM=',R+1                                                 
         BNE   *+14                                                             
         MVC   REQPROG,R+9         SAVE PROGRAM                                 
         B     FJ100                                                            
*                                                                               
         CLC   =C'PRIORITY=',R+1                                                
         BNE   FJ110                                                            
         MVC   REQPRTY,R+10        SAVE PRIORITY                                
         CLC   REQPRTY,MINPRTY                                                  
         BL    FJSKIP              PRIORITY IS TOO LOW FOR THIS MONSOON         
         CLC   REQPRTY,MAXPRTY                                                  
         BH    FJSKIP              PRIORITY TOO HIGH FOR THIS MONSOON           
         B     FJ100                                                            
*                                                                               
FJ110    CLC   =C'FACPAK=',R+1                                                  
         BNE   FJ120                                                            
         MVC   REQFACPK,R+8        SAVE FACPAK NUMBER                           
         CLI   FACPAK,C' '         ANY FACPAK FILTER?                           
         BE    FJ100                                                            
         CLC   FACPAK,REQFACPK     MATCH ON FACPAK FILTER?                      
         BNE   *+16                                                             
         CLI   FACPAKFL,C'P'       YES, TAKE IT IF FILTER IS POSITIVE           
         BNE   FJSKIP                                                           
         B     FJ100                                                            
         CLI   FACPAKFL,C'P'       NO, TAKE IT IF FILTER IS NEGATIVE            
         BE    FJSKIP                                                           
         B     FJ100                                                            
*                                                                               
FJ120    CLC   =C'CLASS=',R+1      CLASS= CARD?                                 
         BNE   FJ150                                                            
         XC    CLASS,CLASS                                                      
         LA    RF,CLASS            SAVE THE CLASS                               
         USING CLASSD,RF                                                        
         MVC   CLSNAME,R+7                                                      
         CLI   ALLCLASS,C'Y'       CLASS=ALL?                                   
         BNE   FJ130               NO -- SEE IF WE WANT THIS CLASS              
         MVC   CLSMAX,=X'FFFF'     DEFAULT CLSMAX IS HIGH VALUE                 
         DROP  RF                                                               
         GOTO1 =V(BINSRCH),DMCB,(1,CLASS),ACLASSTB,NUMCLASS,CLSTABLQ,  +        
               L'CLSNAME,MAXCLASS                                               
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                CLASS TABLE IS NOT BIG ENOUGH                
         MVC   NUMCLASS,DMCB+8     UPDATE TABLE SIZE                            
         B     FJ140                                                            
*                                                                               
FJ130    GOTO1 =V(BINSRCH),DMCB,CLASS,ACLASSTB,NUMCLASS,CLSTABLQ,      +        
               L'CLSNAME,MAXCLASS                                               
         TM    DMCB,X'01'          CLASS FOUND?                                 
         BO    FJSKIP              NO -- WE WILL NOT RUN THIS REQUEST           
*                                                                               
FJ140    L     RF,DMCB             R2 = A(CLASS TABLE ENTRY)                    
         USING CLASSD,RF                                                        
         MVC   REQCLASS,CLSNAME    PUT CLASS NAME IN REQUEST TABLE              
         DROP  RF                                                               
         B     FJ100                                                            
*                                                                               
FJ150    CLC   =C'$$MONSOONEND',R+1                                             
         BE    FJX                 WE MAY ATTACH THIS REQUEST                   
         B     FJ100               IGNORE THIS CARD                             
*                                                                               
FJSKIP   NI    REQFLAGS,X'FF'-REQLOOKD  WE WILL NOT RUN THIS REQUEST            
*                                                                               
FJX      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
SORTTABL NTR1  BASE=*                                                           
*                                                                               
* CREATE SORT RECORDS FOR EACH REQUEST TO BE ATTACHED.  THE REQUESTS            
* ARE SORTED ON PRIORITY AND TIME SUBMITTED.  OPTIONALLY, THE REQUESTS          
* MAY ALSO BE SORTED ON CLASS (IF CLASSPRIORITY IS SET).  WHEN THE              
* REQUESTS ARE READ BACK IN FROM THE SORT, USE THE CHAIN FIELD IN THE           
* REQUEST TABLE TO HOLD A POINTER TO THE NEXT ENTRY IN THE CHAIN.  THIS         
* PRODUCES A LINKED LIST OF REQUESTS TO BE ATTACHED DURING THIS                 
* GO-ROUND.  ALSO, ACCUMULATE STATISTICS BY CLASS.                              
*                                                                               
         OC    NUMCLASS,NUMCLASS   ANY CLASSES IN CLASS TABLE?                  
         BZ    SO20                                                             
         L     R0,NUMCLASS         YES -- CLEAR THEM                            
         L     R4,ACLASSTB         A(CLASS TABLE)                               
         USING CLASSD,R4                                                        
SO10     XC    CLSREQW,CLSREQW     CLEAR CLASS ACCUMULATORS                     
         XC    CLSREQI,CLSREQI                                                  
         XC    CLSTIME,CLSTIME                                                  
         LA    R4,CLSTABLQ(R4)     BUMP TO NEXT CLASS                           
         BCT   R0,SO10                                                          
         DROP  R4                                                               
*                                                                               
SO20     LA    R0,SRTKEYLQ         SOFT SORT KEY LENGTH                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(2),DUB                                               
         LA    R0,SRTRECLQ         SOFT SORT RECORD LENGTH                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(2),DUB                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
         L     R5,AREQTAB          A(REQUEST TABLE)                             
         SR    R3,R3               TABLE INDEX NUMBER                           
*                                                                               
SO30     LA    R3,1(R3)            INCREMENT INDEX                              
         TM    REQFLAGS,REQLOOKD   IS THIS TABLE ENTRY BEING USED?              
         BZ    SO50                NO                                           
*                                                                               
         LH    R2,REQREPUI         USERID NUMBER                                
         BCTR  R2,0                                                             
         MHI   R2,USERTBLQ                                                      
         A     R2,AUSERTAB         R2 = A(THIS USERID'S TABLE ENTRY)            
         OC    0(USERTBLQ,R2),0(R2) HAVE WE ALREADY BEGUN THIS ENTRY?           
         BNZ   SO40                YES, DON'T REREAD CTFILE ID RECORD           
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),REQREPUI  USER-ID NUMBER                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),=C'CTFILE',KEY,AIO,0             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                WHAT HAPPENED TO THE ID RECORD?              
*                                                                               
         L     R4,AIO                                                           
         BAS   RE,GETEL            FIND ID ELEMENT (X'02')                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING USERTABD,R2                                                      
         MVC   USERNAME,2(R4)      SIGNON ID                                    
*                                                                               
SO40     MVC   CLASS(2),REQCLASS   PUT CLASS CODE INTO 'RECORD'                 
         GOTO1 =V(BINSRCH),DMCB,CLASS,ACLASSTB,NUMCLASS,CLSTABLQ,      +        
               L'CLSNAME,MAXCLASS                                               
         TM    DMCB,X'01'          CLASS FOUND?                                 
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         L     R4,DMCB             R4 = A(CLASS TABLE ENTRY)                    
         USING CLASSD,R4                                                        
         LH    R1,CLSREQW                                                       
         LA    R1,1(R1)                                                         
         STH   R1,CLSREQW          INCREMENT NUMBER OF REQUESTS WAITING         
         MVC   THREE(2),REQTIME                                                 
         BRAS  RE,CONVTHMS         CONVERT TIME TO X'HHMMSS'                    
         ZIC   R0,THREE            NUMBER OF HOURS                              
         MHI   R0,60               HOURS CONVERTED TO MINUTES                   
         ZIC   R1,THREE+1          NUMBER OF MINUTES                            
         AR    R1,R0               TOTAL NUMBER OF MINUTES                      
         L     R0,CLSTIME                                                       
         AR    R1,R0                                                            
         ST    R1,CLSTIME          INCREMENT TOTAL WAITING TIME                 
*                                                                               
         XC    SRTREC(SRTRECLQ),SRTREC                                          
         TM    REQFLAGS,REQFILED   REQUEST CAME FROM JCLFILE?                   
         BZ    *+8                                                              
         MVI   SRTFORCE,X'FF'      YES                                          
         XI    SRTFORCE,X'FF'      SORT MONSOON FORCE DESCENDING                
         CLI   ALLCLASS,C'Y'                                                    
         BE    *+18                                                             
         CLI   CLSPRTYF,C'Y'                                                    
         BNE   *+10                                                             
         MVC   SRTCLASS,CLSPRTY    WE DO HAVE CLASS PRIORITY                    
         DROP  R4                                                               
         MVC   SRTPRTY,REQPRTY     REQUEST PRIORITY                             
         XI    SRTPRTY,X'FF'       SORT PRIORITY DESCENDING                     
         TM    REQFLAGS,REQYESTD   REQUEST MADE YESTERDAY?                      
         BZ    *+8                                                              
         MVI   SRTYESTD,X'FF'      YES                                          
         XI    SRTYESTD,X'FF'      SORT 'YESTERDAY' DESCENDING                  
         MVC   SRTTIME,REQTIME     REPORT CREATION TIME                         
         STCM  R3,15,SRTINDEX      TABLE INDEX NUMBER                           
         GOTO1 =V(SORTER),DMCB,=C'PUT',SRTREC                                   
*                                                                               
SO50     LA    R5,REQTABLQ(R5)     BUMP TO NEXT ENTRY                           
         C     R3,REQTBLSZ         ANY MORE ENTRIES TO LOOK AT?                 
         BL    SO30                YES                                          
*                                                                               
         XC    NEXTREQ,NEXTREQ                                                  
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)         ANY REQUESTS TO ATTACH AT ALL?               
         BZ    SOX                 NO                                           
*                                                                               
         MVC   SRTREC(SRTRECLQ),0(RF)                                           
         MVC   NEXTREQ,SRTINDEX    FIRST INDEX IS START OF CHAIN                
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,15,SRTINDEX                                                   
         BCTR  R3,0                                                             
         MHI   R3,REQTABLQ         DISPLACEMENT TO THIS REQTABLE ENTRY          
         A     R3,AREQTAB          R3 = A(THIS REQTABLE ENTRY)                  
         LH    R2,REQREPUI-REQTABLD(R3)   USERID NUMBER                         
         BCTR  R2,0                                                             
         MHI   R2,USERTBLQ                                                      
         A     R2,AUSERTAB         R2 = A(THIS USERID'S TABLE ENTRY)            
         LH    R1,USERREQI         NO. OF REQUESTS CURRENTLY FOR USER           
         LA    R1,1(R1)                                                         
         STH   R1,USERREQI         INCREMENT NO. OF CURRENT USER REQS           
*                                                                               
         MVC   CLASS(2),REQCLASS-REQTABLD(R3)                                   
         GOTO1 =V(BINSRCH),DMCB,CLASS,ACLASSTB,NUMCLASS,CLSTABLQ,      +        
               L'CLSNAME,MAXCLASS                                               
         TM    DMCB,X'01'          CLASS FOUND?                                 
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,DMCB             A(CLASS TABLE ENTRY)                         
         USING CLASSD,RF                                                        
         LH    R1,CLSREQI                                                       
         LA    R1,1(R1)                                                         
         STH   R1,CLSREQI          INCREMENT NO. OF REQUESTS IN TABLE           
         LH    R1,CLSTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,CLSTOTAL         INCREMENT TOTAL NO. OF REQUESTS              
         DROP  RF                                                               
         LA    R4,1                ONE ITEM IN TABLE SO FAR                     
*                                                                               
SO60     SR    R5,R5                                                            
         ICM   R5,15,SRTINDEX                                                   
         BCTR  R5,0                                                             
         MHI   R5,REQTABLQ         DISPLACEMENT TO THIS ENTRY IN CHAIN          
         A     R5,AREQTAB          R5 = A(THIS ENTRY IN CHAIN)                  
*                                                                               
SO70     GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   RF,15,4(R1)         ANY MORE REQUESTS TO ATTACH?                 
         BZ    *+12                                                             
         CH    R4,REFRESH          HAVE WE REACHED THE REFRESH LIMIT?           
         BL    *+14                                                             
         XC    REQCHAIN,REQCHAIN   NOTHING ELSE IN TABLE                        
         B     SOX                                                              
*                                                                               
         MVC   SRTREC(SRTRECLQ),0(RF)                                           
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,15,SRTINDEX                                                   
         BCTR  R3,0                                                             
         MHI   R3,REQTABLQ         DISPLACEMENT TO THIS REQTABLE ENTRY          
         A     R3,AREQTAB          R3 = A(THIS REQTABLE ENTRY)                  
         LH    R2,REQREPUI-REQTABLD(R3)   USERID NUMBER                         
         BCTR  R2,0                                                             
         MHI   R2,USERTBLQ                                                      
         A     R2,AUSERTAB         R2 = A(THIS USERID'S TABLE ENTRY)            
*                                                                               
         CLC   USERMAXR,=X'FFFF'   TEST FOR USERMAX=NO                          
         BE    SO80                                                             
         CLC   USERREQI,USERMAXR                                                
         BL    *+12                                                             
         MVI   JOBSLEFT,C'Y'       USER MAXIMUM EXCEEDED                        
         B     SO70                                                             
*                                                                               
SO80     MVC   CLASS(2),REQCLASS-REQTABLD(R3)                                   
         GOTO1 =V(BINSRCH),DMCB,CLASS,ACLASSTB,NUMCLASS,CLSTABLQ,      +        
               L'CLSNAME,MAXCLASS                                               
         TM    DMCB,X'01'          CLASS FOUND?                                 
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,DMCB             A(CLASS TABLE ENTRY)                         
         USING CLASSD,RF                                                        
         CLC   CLSREQI,CLSMAX                                                   
         BL    *+12                                                             
         MVI   JOBSLEFT,C'Y'       CLASS MAXIMUM EXCEEDED                       
         B     SO70                                                             
*                                                                               
         LH    R1,CLSREQI          NO. OF REQUESTS CURRENTLY FOR CLASS          
         LA    R1,1(R1)                                                         
         STH   R1,CLSREQI          INCREMENT NO. CURRENT REQS FOR CLASS         
         LH    R1,CLSTOTAL                                                      
         LA    R1,1(R1)                                                         
         STH   R1,CLSTOTAL         INCREMENT TOTAL NO. OF REQUESTS              
         DROP  RF                                                               
*                                                                               
         LH    R1,USERREQI         NO. OF REQUESTS CURRENTLY FOR USER           
         LA    R1,1(R1)                                                         
         STH   R1,USERREQI         INCREMENT NO. OF CURRENT USER REQS           
         DROP  R2                                                               
*                                                                               
         MVC   REQCHAIN,SRTINDEX   INDEX IS NEXT ENTRY IN CHAIN                 
         LA    R4,1(R4)            ONE MORE ENTRY IN TABLE                      
         B     SO60                                                             
*                                                                               
SOX      GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,XX,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XX'                                    
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
STATUS   NTR1  BASE=*                                                           
*                                                                               
* DISPLAYS THE CLASS TABLE STATUS ON THE CONSOLE.  IF BYTE=C'B', ONLY           
* DISPLAY BACKED UP CLASSES.  IF BYTE=C'A', DISPLAY ALL CLASSES.                
*                                                                               
         OC    NUMCLASS,NUMCLASS   ANY CLASSES IN CLASS TABLE?                  
         BZ    STX                                                              
         L     R3,NUMCLASS         YES -- EXAMINE THEM                          
         XC    FULL,FULL                                                        
         L     R2,ACLASSTB         A(CLASS TABLE)                               
         USING CLASSD,R2                                                        
*                                                                               
         SR    R0,R0                                                            
ST10     TIME  BIN                 GET THE CURRENT TIME                         
         LTR   R0,R0                                                            
         BZ    ST10                BAD RETURN FROM MACRO                        
*                                                                               
         SRDL  R0,32                                                            
         D     R0,=F'6000'                                                      
         ST    R1,FULL2            CURRENT TIME IN MINUTES                      
*                                                                               
ST20     OC    CLSREQW,CLSREQW     ANY REQUESTS AWAITING EXECUTION?             
         BZ    ST30                                                             
         MVC   FULL+2(2),CLSREQW   YES                                          
         L     RF,CLSTIME          TOTAL SUBMIT TIME FOR CLASS (MINS)           
         SR    RE,RE                                                            
         D     RE,FULL             RF = AVG WAIT TIME FOR CLASS (MINS)          
         L     RE,FULL2            CURRENT TIME IN MINUTES                      
         SR    RE,RF               AVERAGE WAIT TIME                            
         CLI   BYTE,C'A'           DISPLAY ALL CLASSES?                         
         BE    *+12                YES                                          
         CHI   RE,CLSWAIT          IS MAX AVERAGE WAIT TIME EXCEEDED?           
         BNH   ST30                NO                                           
*                                                                               
         MVC   OPMS2+16(2),CLSNAME CLASS NAME                                   
         EDIT  CLSREQW,(3,OPMS2+23)                                             
         SR    R0,R0                                                            
         LR    R1,RE               AVERAGE WAIT TIME IN MINUTES                 
         D     R0,=F'60'                                                        
         CVD   R1,DUB              WAIT TIME IN HOURS                           
         OI    DUB+7,X'0F'                                                      
         UNPK  OPMS2+56(2),DUB                                                  
         CVD   R0,DUB              WAIT TIME IN MINUTES                         
         OI    DUB+7,X'0F'                                                      
         UNPK  OPMS2+59(2),DUB                                                  
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2,OPMS2)                       
*                                                                               
         XC    PRTYS(40),PRTYS     CLEAR PRIORITY ACCUMLATORS                   
         L     R5,AREQTAB          A(REQUEST TABLE)                             
ST23     CLC   REQCLASS,CLSNAME    LOOK FOR REQUESTS FOR THIS CLASS             
         BNE   ST25                                                             
         ZIC   RE,REQPRTY          R0 = PRIORITY IN EBCDIC                      
         N     RE,=X'0000000F'     R0 = PRIORITY IN BINARY                      
         SLL   RE,2                RE = DISPLACEMENT TO ACCUMULATOR             
         L     RF,PRTYS(RE)        RF = ACCUMULATOR VALUE                       
         LA    RF,1(RF)                                                         
         ST    RF,PRTYS(RE)        UPDATE ACCUMULATOR                           
*                                                                               
ST25     LA    R5,REQTABLQ(R5)     BUMP TO NEXT ENTRY                           
         C     R5,AREQTABX         ANY MORE ENTRIES?                            
         BL    ST23                YES                                          
*                                                                               
         LA    R5,PRTYS                                                         
         SR    R4,R4               MAXIMUM OF 10 PRIORITY VALUES                
ST27     OC    0(4,R5),0(R5)       IF ACCUMULATOR IS ZERO, IGNORE IT            
         BZ    ST28                                                             
         EDIT  (B4,0(R5)),(5,OPMS2A+10)                                         
         EDIT  (R4),(1,OPMS2A+33)                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS2A,OPMS2A)                     
ST28     LA    R5,4(R5)            BUMP TO NEXT ACCUMLATOR                      
         LA    R4,1(R4)                                                         
         CHI   R4,9                HIGHEST PRIORITY IS 9                        
         BNH   ST27                                                             
*                                                                               
ST30     LA    R2,CLSTABLQ(R2)     BUMP TO NEXT CLASS                           
         BCT   R3,ST20                                                          
         DROP  R2                                                               
*                                                                               
STX      XIT1                                                                   
         SPACE 3                                                                
*&&US                                                                           
CLSWAIT  EQU   15                  MAX WAIT BEFORE CONSOLE MSG (MINS)           
*&&                                                                             
*&&UK                                                                           
CLSWAIT  EQU   60                  MAX WAIT BEFORE CONSOLE MSG (MINS)           
*&&                                                                             
         SPACE 3                                                                
OPMS2    DC    C'MONSOON:  CLASS CC HAS NNN JOBS WAITING, AVERAGE WAIT +        
               = HH.MM'                                                         
OPMS2A   DC    C'MONSOON:  NNNNN JOBS OF PRIORITY N'                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
ATTACH   NTR1  BASE=*                                                           
         USING ATTACH+4096,R8                                                   
         LR    R8,RB                                                            
         AHI   R8,4096                                                          
*                                                                               
* READ THE PRINT QUEUE FOR THE FIRST REQUEST IN THE CHAIN.  MAKE SURE           
* IT HAS NOT YET BEEN ATTACHED BY ANOTHER MONSOON.  IF NOT, TURN ON             
* THE JOB OUTPUT FLAG AND ATTACH IT.                                            
*                                                                               
         MVC   JOBNAME,SPACES                                                   
         L     R5,NEXTREQ                                                       
         BCTR  R5,0                                                             
         MHI   R5,REQTABLQ         DISPLACEMENT TO FIRST ENTRY IN CHAIN         
         A     R5,AREQTAB          R5 = A(FIRST ENTRY IN CHAIN)                 
*                                                                               
         CLC   REQPRTY,REFPRTY     FORCE REFRESH DUE TO LOW PRIORITY?           
         BNL   ATT5                                                             
         CLI   LOPRTYF1,C'Y'       MAYBE -- DID WE JUST COME FROM HERE?         
         BE    ATT5                                                             
         MVI   LOPRTYF1,C'Y'       NO -- WE'LL REFRESH LIST NOW                 
         MVI   LOPRTYF2,C'Y'                                                    
         BRAS  RE,CLRREQTB         CLEAR THE REQUEST TABLE                      
         B     ATT200                                                           
*                                                                               
ATT5     MVI   LOPRTYF1,C'N'                                                    
         L     R6,ACITABLE         FIND OUT WHICH PQ THE JCL IS ON              
         C     R5,CIAPART                                                       
         BNL   *+6                                                              
         DC    H'0'                HOW CAN ENTRY BE BEFORE TABLE START?         
*                                                                               
ATT10    LA    R6,CITBLLNQ(R6)                                                  
         CLI   0(R6),X'FF'         END OF TABLE?                                
         BE    *+16                YES                                          
         C     R5,CIAPART                                                       
         BNL   ATT10                                                            
         B     *+14                                                             
         C     R5,AREQTABX                                                      
         BL    *+6                                                              
         DC    H'0'                HOW CAN ENTRY BE AFTER TABLE END?            
         AHI   R6,-CITBLLNQ        R6 = A(CITABLE ENTRY FOR THIS JCL)           
*                                                                               
         LH    R2,REQREPUI         USERID NUMBER                                
         BCTR  R2,0                                                             
         MHI   R2,USERTBLQ                                                      
         A     R2,AUSERTAB         R2 = A(THIS USERID'S TABLE ENTRY)            
         USING USERTABD,R2                                                      
*                                                                               
         MVC   OPMS1,SPACES        FORMAT THE OPERATOR MESSAGE                  
         MVC   OPMS1+8(8),USERNAME                                              
         MVI   OPMS1+16,C','                                                    
         MVI   OPMS1+20,C','                                                    
         LR    R1,R5                                                            
         S     R1,CIAPART                                                       
         SR    R0,R0                                                            
         D     R0,=A(REQTABLQ)                                                  
         AH    R1,CICINDX          R1 = REPORT NUMBER                           
         EDIT  (R1),(5,OPMS1+21)                                                
*                                                                               
         MVC   FJOBUSER-FKEYD+FKEY,USERNAME    USERID                           
         MVC   FJBSYPGM-FKEYD+FKEY,REQSYPGM    SYSTEM/PROGRAM                   
*                                                                               
         CLI   ENQJOBNM,C'Y'       ENQ THE JOB NAME?                            
         BNE   ATT15                                                            
         MVC   JOBUSER,USERNAME    USERID                                       
         MVC   JOBSYPGM,REQSYPGM   SYSTEM/PROGRAM                               
         MVC   P+30(8),MAJORNAM                                                 
         MVC   P+40(L'JOBNAME),JOBNAME                                          
         PRNT  ENQUEUE                                                          
         ENQ   (MAJORNAM,JOBNAME,,,SYSTEMS),RET=USE                             
         LTR   RF,RF                                                            
         BZ    ATT15                                                            
         MVC   P+30(12),=C'REPORT ID:  '  DON'T ATTACH IT NOW                   
         MVC   P+42(18),OPMS1+8    UUUUUUUU,   ,NNNNN                           
         PRNT  DELAYATTACH                                                      
         MVC   JOBNAME,SPACES                                                   
         B     ATT190                                                           
*                                                                               
* ENQUEUE THE APPROPRIATE PRINT QUEUE                                           
*                                                                               
ATT15    BAS   RE,PQLOCK           ENQUEUE THE PRINT QUEUE                      
*                                                                               
         BRAS  RE,GETTTTT                                                       
         MVC   CIADDR(2),HALF      USE THIS TTTT                                
         MVI   CIADDR+2,1          ALWAYS START WITH BLOCK 1                    
         MVI   CIADDR+3,0          RECORD 0                                     
         BAS   RE,GETXPE           FIND INDEX PAGE/ENTRY                        
         BAS   RE,GETXAD           FIND INDEX DISK ADDRESS                      
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),CFPQID,CXADDR,CXREC              
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    *+10                                                             
         BAS   RE,PQUNLK           YES -- DEQUEUE THE PRINT QUEUE               
         DC    H'0'                                                             
*                                                                               
         LH    RE,CXENTRY          INDEX PAGE ENTRY                             
         MH    RE,CINDXLN          LENGTH OF PQ KEY                             
         LA    R4,CXREC(RE)        R4 = A(INDEX ENTRY)                          
         USING PQRECD,R4                                                        
*                                                                               
         CLC   REQREPUI,PQSRCID    IS IT THE SAME REPORT?                       
         BNE   ATT20                                                            
         LR    R1,R5                                                            
         S     R1,CIAPART                                                       
         SR    R0,R0                                                            
         D     R0,=A(REQTABLQ)                                                  
         AH    R1,CICINDX          R1 = REPORT NUMBER                           
         CLM   R1,3,PQREPNO                                                     
         BNE   ATT20                                                            
         CLC   REQTIME,PQAGELT                                                  
         BNE   ATT20               NO  -- IT'S BEEN PURGED                      
         TM    REQFLAGS,REQFILED   REQUEST IS IN JCLFILE, NOT PRTQ?             
         BZ    *+12                                                             
         BAS   RE,PQUNLK           YES -- DEQUEUE THE PRINT QUEUE               
         B     ATT30                                                            
         TM    PQATTB,PQATJOBI     DOES REPORT STILL CONTAIN JCL?               
         BZ    ATT20               NOT ANY MORE -- IT COMPLETED                 
         TM    PQATTB,PQATJOBO     IS THE REPORT CURRENTLY RUNNING?             
         BZ    ATT25               NO                                           
*                                                                               
ATT20    BAS   RE,PQUNLK           DEQUEUE THE PRINT QUEUE                      
         B     ATT180                                                           
*                                                                               
ATT25    OI    PQATTB,PQATJOBO     TURN ON 'JOB OUTPUT' FLAG                    
         TM    PQTYPE,PQTYRRCV     LUNATIC REPORT?                              
         BZ    *+8                                                              
         OI    PQTYPE,PQTYRSND     YES -- IT'S ABOUT TO RUN                     
         LH    RF,CXENTRY          SET DISP TO ENTRY IN INDEX PAGE              
         MH    RF,CINDXLN                                                       
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),CFPQID,CXADDR,CXREC,(RF)          
*                                                                               
         BRAS  RE,GETTTTT          RETURNS TRACK NUMBER IN HALF                 
         L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKFCTRL,SKFCTRL     START READING FROM SCRATCH                   
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKLABEL,=C'*PQSAVE*'                                             
         MVC   SKINTNO,CFPQINUM    INTERNAL FILE NUMBER                         
         MVC   SKEXTNO,CFPQENUM    EXTERNAL FILE NUMBER                         
         MVC   SKFSTCI(2),HALF     USE TTTT FROM ABOVE                          
         MVI   SKFSTCI+2,1         ALWAYS START WITH BLOCK 1                    
         DROP  RF                                                               
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 =V(DATAMGR),DMCB,(0,=C'RANDOM'),CFPQID,0,R,CXREC                 
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    *+10                                                             
         BAS   RE,PQUNLK           YES -- DEQUEUE THE PRINT QUEUE               
         DC    H'0'                                                             
*                                                                               
         LA    R4,CXREC            A(FIRST CONTROL INTERVAL)                    
         OI    PQATTB,PQATJOBO     TURN ON 'JOB OUTPUT' FLAG                    
         TM    PQTYPE,PQTYRRCV     LUNATIC REPORT?                              
         BZ    *+8                                                              
         OI    PQTYPE,PQTYRSND     YES -- IT'S ABOUT TO RUN                     
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),CFPQID,CIADDR,CXREC               
*                                                                               
         BAS   RE,PQUNLK           DEQUEUE THE PRINT QUEUE                      
         CLI   DMCB+8,0            ANY ERROR ON THE DMWRT?                      
         BE    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
* BY THIS POINT, THE PRINT QUEUE HAS BEEN DEQUEUED                              
*                                                                               
ATT30    MVC   OPMS1+17(3),PQSUBID NOW WE KNOW THE SUB-ID                       
*                                                                               
         BRAS  RE,GETTTTT          RETURNS TRACK NUMBER IN HALF                 
         L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKBCTRL,SKBCTRL     START READING FROM SCRATCH                   
         XC    SKFCTRL,SKFCTRL                                                  
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKLABEL,=C'*PQSAVE*'                                             
         MVC   SKINTNO,CFPQINUM    INTERNAL FILE NUMBER                         
         MVC   SKEXTNO,CFPQENUM    EXTERNAL FILE NUMBER                         
         MVC   SKFSTCI(2),HALF     USE TTTT FROM ABOVE                          
         MVI   SKFSTCI+2,1         ALWAYS START WITH BLOCK 1                    
         DROP  RF                                                               
*                                                                               
         BRAS  RE,GETJCL           EXTRACT DATA FROM JCL BOOK                   
         BE    ATT40               JCL IS OK                                    
         MVC   OPMS1(7),=C'BAD*JCL'                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
         B     ATT160              JCL ERROR                                    
*                                                                               
ATT40    MVC   P+30(12),=C'REPORT ID:  '                                        
         MVC   P+42(18),OPMS1+8    UUUUUUUU,XXX,NNNNN                           
         MVC   P+65(8),ATTCHPGM    NAME OF ATTACHED PROGRAM                     
         MVC   P+75(10),=C'DIRECT ID='                                          
         MVC   P+85(18),DIRECTID   DIRECT CARD ID=                              
         PRNT  ATTACH,PRINT=ALWAYS                                              
*                                                                               
         XC    AVOIDLD,AVOIDLD     CLEAR LOAD COUNTERS                          
         XC    TOTALLD,TOTALLD                                                  
*                                                                               
         MVC   OPMS1(7),=C'RUNNING'                                             
         CLI   JESMSG,C'Y'         ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   ATT45               NO                                           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
ATT45    L     R1,TOTREQS          INCREMENT TOTAL NUMBER OF REQUESTS           
         LA    R1,1(R1)                                                         
         ST    R1,TOTREQS                                                       
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         ST    R0,STRTTMTC                                                      
*                                                                               
         TIME  BIN                                                              
         ST    R0,STRTTMRL         STARTING EXECUTION TIME                      
*                                                                               
ATT50    GOTO1 =V(DATAMGR),DMCB,=C'LOCKSPC',X'20008005',WORK                    
*                                                                               
         XC    AJOBNTRY,AJOBNTRY                                                
         L     RF,=A(SSB)                                                       
         LAM   R3,R3,SSOTBLET-SSOOFF(RF)                                        
         SAC   512                                                              
         L     R3,8(R1)            GET A(JOBTAB HEADER)                         
         L     R3,60(R3)           GET A(JOB TABLE)                             
         LR    R1,R2                                                            
         SR    RE,RE                                                            
         ICM   R0,15,0(R3)         SET R0 TO NUMBER OF ENTRIES                  
         BZ    ATT56                                                            
         LA    R3,32(,R3)          BUMP PAST HEADER                             
         USING TBJOBTAB,R3                                                      
ATT51    OC    0(4,R3),0(R3)       IGNORE BLANK ENTRIES                         
         BNZ   *+12                                                             
         LA    R3,L'TBJNTRY(,R3)                                                
         B     ATT51                                                            
*                                                                               
         CLC   TBJPQKEY,PQKEY      TEST THIS KEY                                
         BE    ATT55                                                            
         LA    R3,L'TBJNTRY(,R3)   NEXT                                         
         BCT   R0,ATT51                                                         
         B     ATT56               NOT FOUND                                    
*                                                                               
ATT55    MVC   TBJMONS,MNSOONID    SET MONSOON NAME                             
         L     R1,STRTTMRL                                                      
         STCM  R1,7,TBJRTIME       SET RUN TIME                                 
         ST    R3,AJOBNTRY                                                      
*                                                                               
ATT56    SAC   0                                                                
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK+1,10           L'PARAMS WITHOUT LENGTH ITSELF               
         MVC   THREE(2),REQTIME                                                 
         BRAS  RE,CONVTHMS         CONVERT TIME TO X'HHMMSS'                    
         MVC   WORK+2(3),THREE     SUBMIT TIME                                  
         MVC   WORK+5(1),CFPQINUM  PRINT QUEUE NUMBER                           
         MVC   WORK+6(2),REQCLASS  CLASS                                        
         L     R1,AASCB            A(ASCB)                                      
         MVC   WORK+8(2),ASCBASID-ASCB(R1)  ASID                                
         MVC   WORK+10(1),REQPRTY  PRIORITY                                     
         MVC   WORK+11(1),REQFACPK FACPAK ID                                    
         LA    R1,WORK             R1 = A(PARAMETERS TO SVC)                    
         LA    R0,14                                                            
         LNR   R0,R0               R0 = -14                                     
         SVC   247                 FOR JOB ACCOUNTING                           
*                                                                               
         LA    R3,ATTCHPGM                                                      
         XC    TASKECB,TASKECB                                                  
*&&US*&& ATTACH EPLOC=(R3),ECB=TASKECB,SZERO=NO                                 
*&&UK*&& ATTACH EPLOC=(R3),ECB=TASKECB,SZERO=NO,ALCOPY=YES                      
         ST    R1,TASKTCB          SAVE TCB                                     
         OC    TASKTCB,TASKTCB                                                  
         BNZ   *+6                                                              
         DC    H'0'                IF TCB IS ZERO, THEN ATTACH FAILED           
*                                                                               
ATT60    XC    TIMERECB,TIMERECB                                                
         CLC   REQTIMER,=C'NONE'                                                
         BE    ATT70               DON'T SET THE TIMER                          
         STIMERM SET,ID=STIMERID,BINTVL=REQTIMER,EXIT=TIMERXIT                  
         LTR   RF,RF                                                            
         BZ    ATT70                                                            
         DC    H'0'                                                             
*                                                                               
ATT70    WAIT  1,ECBLIST=ECBLST    WAIT FOR REQ COMPLETION OR OPERATOR          
*                                                                               
         TM    TIMERECB,X'40'      DID THE TIMER POP?                           
         BZ    ATT100                                                           
         CLI   UPDATIVE,C'Y'       YES - UPDATIVE SOON?                         
         BNE   *+12                                                             
         BRAS  RE,PUTWARN          YES - PUT WARNING MESSAGE TO CONSOLE         
         B     ATT60               START TIMER AGAIN, THEN WAIT                 
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         S     R0,STRTTMTC         R0 = ELAPSED TCB TIME                        
         AHI   R0,-2               ALLOW TWO SECOND GRACE PERIOD                
         CH    R0,TCBTIME          HAS THE JOB TIMED OUT?                       
         BH    *+12                                                             
         BRAS  RE,PUTWARN          NO - PUT WARNING MESSAGE TO CONSOLE          
         B     ATT60               START TIMER AGAIN, THEN WAIT                 
*                                                                               
         MVC   OPMS5+16(18),OPMS1+8  UUUUUUUU,XXX,NNNNN                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS5,OPMS5)                       
         B     ATT120                                                           
*                                                                               
ATT100   TM    TASKECB,X'40'       DID THE REQUEST COMPLETE?                    
         BZ    ATT110                                                           
         CLC   REQTIMER,=C'NONE'   YES -- WAS TIMER SET?                        
         BE    ATT120              NO, SO DON'T CANCEL                          
         STIMERM CANCEL,ID=STIMERID                                             
         LTR   RF,RF                                                            
         BZ    ATT120                                                           
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
*                                                                               
ATT110   L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BO    *+6                 YES                                          
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
         CLI   OPERKILL,C'Y'       KILL THIS REQUEST AND STOP MONSOON?          
         BNE   ATT70               WAIT SOME MORE                               
*                                                                               
ATT120   MVC   OPMS1(7),=C'ENDED  '                                             
         DETACH TASKTCB            DETACH THE TASK                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
*                                                                               
         TM    TASKECB,X'40'       DID REQUEST COMPLETE?                        
         BO    ATT125              YES                                          
         WAIT  ECB=TASKECB         WAIT FOR REQUEST COMPLETION                  
*                                                                               
ATT125   XC    WORK,WORK                                                        
         MVI   WORK+1,4            L'PARAMS WITHOUT LENGTH ITSELF               
         MVC   WORK+2(4),TASKECB   TASK ECB UPON COMPLETION                     
         NI    WORK+2,X'FF'-X'C0'  TURN OFF HIGH-ORDER TWO BITS OF ECB          
         LA    R1,WORK             R1 = A(PARAMETERS TO SVC)                    
         LA    R0,15                                                            
         LNR   R0,R0               R0 = -15                                     
         SVC   247                 FOR JOB ACCOUNTING                           
*                                                                               
         OC    TASKECB+1(3),TASKECB+1                                           
         BZ    ATT130              NO SYSTEM OR USER ABEND                      
         GOTO1 =V(DMENQDEQ),DMCB,(C'D',=C'ALL')                                 
*                                                                               
*        CODE FOR RECOVERING AND RELEASING UPDATIVE LOCKS                       
*                                                                               
**NOP    GOTO1 =V(DATAMGR),DMCB,=C'RECOVR'                                      
**NOP    GOTO1 =V(DATAMGR),DMCB,=C'LOCKUP',(C'C',0),0                           
*                                                                               
ATT130   MVC   P+30(4),=C'ECB='                                                 
         GOTO1 =V(HEXOUT),DMCB,TASKECB,P+34,4,=C'TOG'                           
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TIME  BIN                                                              
         S     R0,STRTTMRL         R0 = ELAPSED REAL TIME                       
         BNM   *+8                                                              
         A     R0,=A(24*3600*100)  ADD 24HRS, JUST WENT PAST MIDNIGHT           
*                                                                               
         ICM   R3,15,AJOBNTRY                                                   
         BZ    ATT131                                                           
         SAC   512                 DIP INTO THE DATASPACE                       
         STCM  R0,7,TBJETIME       SET ELAPSED                                  
         SAC   0                                                                
*                                                                               
ATT131   BRAS  RE,PRTTIME1         PUTS FORMATTED TIME IN WORK                  
         MVC   P+58(3),=C'ET='                                                  
         MVC   P+61(8),WORK                                                     
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         S     R0,STRTTMTC         R0 = ELAPSED TCB TIME                        
         ST    R0,FULL             SAVE TCB TIME FOR LATER CHECK                
         BRAS  RE,PRTTIME2         PUTS FORMATTED TIME IN WORK                  
         MVC   P+44(4),=C'TCB='                                                 
         MVC   P+48(8),WORK                                                     
*                                                                               
         L     RE,FULL                                                          
         C     RE,MAXTCBTM         EXCESS MAX TCB TIME ALLOWED?                 
         BL    *+8                                                              
         BRAS  RE,LONGSOON                                                      
*                                                                               
         MVC   P+71(31),=C'LOADS AVOIDED: XXXX OUT OF XXXX'                     
         L     R1,AVOIDLD          # LOADS AVOIDED DUE TO MONSOON               
         EDIT  (R1),(4,P+86),ZERO=NOBLANK,ALIGN=LEFT                            
         A     R1,LOADAVD                                                       
         ST    R1,LOADAVD                                                       
         L     R1,TOTALLD          # LOADS REQUESTED FOR THIS REQUEST           
         EDIT  (R1),(4,P+98),ZERO=NOBLANK,ALIGN=LEFT                            
         A     R1,LOADASK                                                       
         ST    R1,LOADASK                                                       
         PRNT  COMPLETED,PRINT=ALWAYS                                           
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,7,TASKECB+1      RE = 00SSSUUU (ABEND CODES)                  
         BZ    ATT140              TASK DID NOT ABEND                           
         SR    RF,RF                                                            
         SRDL  RE,12               RE = SYSTEM ABEND CODE                       
         SRL   RF,20               RF = USER ABEND CODE                         
         MVC   P+11(34),=C'*** ERROR ***   ABEND = SXXX UXXXX'                  
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+41(4),DUB         PUT USER ABEND CODE IN PRINT LINE            
         STH   RE,HALF                                                          
         GOTO1 =V(HEXOUT),DMCB,HALF,FULL,2,=C'TOG'                              
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P+36(3),FULL+1      PUT SYSTEM ABEND CODE IN PRINT LINE          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         OC    USRPSW,USRPSW       DID MASTER RETURN A VALID PSW?               
         BZ    ATT140              NO -- DON'T BOTHER PRINTING PSW+REGS         
*                                                                               
         MVC   P(4),=C'PSW='       PSW AT ABEND                                 
         GOTO1 =V(HEXOUT),DMCB,USRPSW,P+5,4,=C'TOG'                             
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HEXOUT),DMCB,USRPSW+4,P+14,4,=C'TOG'                          
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(6),=C'GR 0-7'                                                  
         LA    R3,USRREGS          REGISTERS R0 - R7 AT ABEND                   
ATT133   LA    R0,8                                                             
         LA    R7,P+8              START OF PRINT LINE                          
ATT135   GOTO1 =V(HEXOUT),DMCB,(R3),(R7),4,=C'TOG'                              
         LA    R3,4(R3)            BUMP TO NEXT REGISTER                        
         LA    R7,9(R7)            BUMP TO NEXT PRINT LINE POSITION             
         BCT   R0,ATT135                                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         BC    0,*+18              ** SELF-MODIFYING CODE **                    
         MVI   *-3,X'F0'           NOW DO R8 - RF                               
         MVC   P(6),=C'GR 8-F'                                                  
         B     ATT133                                                           
         MVI   *-17,0              ** SELF-MODIFYING CODE **                    
*                                                                               
         CLI   DUMPWARN,C'Y'       WARN ON DUPLICATE DUMPS?                     
         BNE   ATT140              NO                                           
         CLC   SAVEPSW,USRPSW      PSW SAME AS LAST TIME?                       
         BNE   ATT138              NO                                           
         CLC   SAVERB,USRRB        RB SAME AS LAST TIME?                        
         BNE   ATT138              NO                                           
*                                                                               
         LA    RE,ABENDCDS         TABLE OF ABEND CODES                         
ATT136   CLC   =X'FFFFFF',0(RE)                                                 
         BE    ATT137              NOT FOUND: WARN OPERATOR TO RESTART          
         CLC   0(L'ABENDCDS,RE),TASKECB+1                                       
         BE    ATT138              MATCH FOUND: NO RESTART NEEDED               
         LA    RE,L'ABENDCDS(,RE)                                               
         B     ATT136              NEXT TABLE ENTRY                             
*                                                                               
ATT137   GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS11,OPMS11)                     
*                                                                               
ATT138   MVC   SAVEPSW,USRPSW      REMEMBER LAST PSW AND REGISTERS. . .         
         MVC   SAVEREGS,USRREGS    . . . AT ABEND                               
*                                                                               
ATT140   CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   ATT145              NO                                           
         GOTO1 =V(PRINTER)         SKIP A LINE                                  
*                                                                               
ATT145   CLI   JESMSG,C'Y'         ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   ATT150              NO                                           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
*                                                                               
ATT150   BRAS  RE,GETTTTT                                                       
         MVC   CIADDR(2),HALF      USE THIS TTTT                                
         MVI   CIADDR+2,1          ALWAYS START WITH BLOCK 1                    
         MVI   CIADDR+3,0          RECORD 0                                     
         BAS   RE,GETXPE           FIND INDEX PAGE/ENTRY                        
         BAS   RE,GETXAD           FIND INDEX DISK ADDRESS                      
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),CFPQID,CXADDR,CXREC              
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    *+6                                                              
         DC    H'0'                YES                                          
*                                                                               
         LH    RE,CXENTRY          INDEX PAGE ENTRY                             
         MH    RE,CINDXLN          LENGTH OF PQ KEY                             
         LA    R4,CXREC(RE)        R4 = A(INDEX ENTRY)                          
*                                                                               
         CLI   OPERKILL,C'Y'       DID OPERATOR KILL THIS REQUEST?              
         BNE   *+16                                                             
         TM    PQATTB,PQATJOBI     IS REPORT 'SUBMITTED' OR 'RUNNING'?          
         BO    ATT170              YES -- GOOD (USER CAN'T SEE IT YET)          
         B     ATT160                                                           
         TM    PQATTB,PQATJOBI     IS REPORT 'SUBMITTED' OR 'RUNNING'?          
         BZ    ATT170              NO                                           
         EJECT                                                                  
* IF A JCL ERROR WAS FOUND, OR IF THE REQUEST COMPLETED                         
* BUT WAS NOT IN READY OR ERROR STATUS, THEN                                    
* ENQUEUE THE PRINT QUEUE ONCE MORE, AND SET THE ERROR FLAG                     
* IN THE PRINT QUEUE INDEX AND IN THE FIRST CONTROL INTERVAL.                   
*                                                                               
ATT160   BAS   RE,PQLOCK           ENQUEUE THE PRINT QUEUE                      
*                                                                               
         BRAS  RE,GETTTTT                                                       
         MVC   CIADDR(2),HALF      USE THIS TTTT                                
         MVI   CIADDR+2,1          ALWAYS START WITH BLOCK 1                    
         MVI   CIADDR+3,0          RECORD 0                                     
         BAS   RE,GETXPE           FIND INDEX PAGE/ENTRY                        
         BAS   RE,GETXAD           FIND INDEX DISK ADDRESS                      
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMREAD'),CFPQID,CXADDR,CXREC              
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    *+10                                                             
         BAS   RE,PQUNLK           YES -- DEQUEUE THE PRINT QUEUE               
         DC    H'0'                                                             
*                                                                               
         LH    RE,CXENTRY          INDEX PAGE ENTRY                             
         MH    RE,CINDXLN          LENGTH OF PQ KEY                             
         LA    R4,CXREC(RE)        R4 = A(INDEX ENTRY)                          
*                                                                               
         CLI   OPERKILL,C'Y'                                                    
         BE    *+16                                                             
         OI    PQATTB,PQATERR      SET ERROR FLAG, RESET JCL FLAG               
         NI    PQATTB,X'FF'-PQATJOBI                                            
         B     *+16                                                             
         OI    PQATTB,PQATJOBI     RESET ERROR FLAG, SET JCL FLAG               
         NI    PQATTB,X'FF'-PQATERR                                             
         OI    PQSTAT,PQSTTE       SET 'CREATION IN PROGRESS' (RUNNING)         
*                                                                               
         LH    RF,CXENTRY          SET DISP TO ENTRY IN INDEX PAGE              
         MH    RF,CINDXLN                                                       
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),CFPQID,CXADDR,CXREC,(RF)          
*                                                                               
         BRAS  RE,GETTTTT          RETURNS TRACK NUMBER IN HALF                 
         L     RF,APQSAVE                                                       
         USING SKBUFFD,RF                                                       
         XC    SKFCTRL,SKFCTRL     START READING FROM SCRATCH                   
         XC    SKXCTRL,SKXCTRL                                                  
         MVC   SKFSTCI(2),HALF     USE TTTT FROM ABOVE                          
         MVI   SKFSTCI+2,1         ALWAYS START WITH BLOCK 1                    
         DROP  RF                                                               
*                                                                               
         XC    R,R                                                              
         MVI   R+4,C'L'            SILLY PARAMETER FOR RANDOM READ              
         GOTO1 =V(DATAMGR),DMCB,(0,=C'RANDOM'),CFPQID,0,R,CXREC                 
         CLI   DMCB+8,0            ANY DMGR ERRORS?                             
         BE    *+10                                                             
         BAS   RE,PQUNLK           YES -- DEQUEUE THE PRINT QUEUE               
         DC    H'0'                                                             
*                                                                               
         LA    R4,CXREC            A(FIRST CONTROL INTERVAL)                    
         CLI   OPERKILL,C'Y'                                                    
         BE    *+16                                                             
         OI    PQATTB,PQATERR+PQATJOBO  SET JOB OUTPUT AND ERROR FLAGS          
         NI    PQATTB,X'FF'-PQATJOBI    RESET JCL FLAG                          
         B     *+16                                                             
         OI    PQATTB,PQATJOBI     RESET ERROR FLAG, SET JCL FLAG               
         NI    PQATTB,X'FF'-PQATERR                                             
         OI    PQSTAT,PQSTTE       SET 'CREATION IN PROGRESS' (RUNNING)         
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMWRT'),CFPQID,CIADDR,CXREC               
         DROP  R4                                                               
*                                                                               
* DEQUEUE THE PRINT QUEUE                                                       
*                                                                               
         BAS   RE,PQUNLK           DEQUEUE THE PRINT QUEUE                      
         CLI   DMCB+8,0            ANY ERROR ON THE DMWRT?                      
         BE    *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
ATT170   CLI   OPERKILL,C'Y'       WILL WE BE RE-RUNNING THIS REQUEST?          
         BE    ATT200              YES -- DON'T ERASE JCLFILE                   
*                                                                               
         OPEN  (JCLFILE,(OUTPUT))  ERASE THE JCLFILE. . .                       
         LTR   RF,RF               . . . SO WE DON'T RUN THIS ONE AGAIN         
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE (JCLFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ATT180   LH    R0,USERREQI                                                      
         BCTR  R0,0                                                             
         STH   R0,USERREQI         DECREMENT NO. OF CURRENT USER REQS           
         DROP  R2                                                               
*                                                                               
ATT190   MVC   OPMS1,SPACES                                                     
         MVC   OPMS1(22),=C'NO REQUEST RUNNING NOW'                             
         MVC   NEXTREQ,REQCHAIN    FORWARD POINTER BECOMES NEW START            
         XC    0(REQTABLQ,R5),0(R5) REMOVE FROM REQUEST TABLE                   
*                                                                               
ATT200   CLC   JOBNAME,SPACES      DID WE ENQ A "JOBNAME"?                      
         BE    ATTX                                                             
         MVC   P+30(8),MAJORNAM    YES -- RELEASE IT                            
         MVC   P+40(L'JOBNAME),JOBNAME                                          
         PRNT  DEQUEUE                                                          
         DEQ   (MAJORNAM,JOBNAME,,SYSTEMS)                                      
*                                                                               
ATTX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DMPRTQR                                                        
         SPACE 3                                                                
         DROP  R8                                                               
         EJECT                                                                  
* THE TIMER EXIT ROUTINE.  IT POSTS THE TIMERECB.                               
*                                                                               
         SPACE 3                                                                
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
TIMERECB DC    F'0'                ECB OF ATTACHED TASK TIMER                   
         EJECT                                                                  
OPMS5    DC C'*** WARNING *** UUUUUUUU,XXX,NNNNN  TIMED OUT BY MONSOON'         
OPMS11   DC C'*** WARNING *** DUPLICATE SOON DUMP: CONSIDER RECYCLING'          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
GETJCL   NTR1  BASE=*                                                           
*                                                                               
* LOCATE THE EXEC CARD, AND FIGURE OUT WHICH PROGRAM TO ATTACH.                 
* ALSO, LOCATE THE TIME PARAMETER ON THE EXEC CARD AND CREATE AN                
* "RTPTCB=" CARD.  THEN CREATE A SYSIN FILE WITH ALL CONTROL AND                
* REQUEST CARDS.                                                                
*                                                                               
         OPEN  (SYSIN,(OUTPUT))    WRITE ALL SYSIN CARDS TO SYSIN FILE          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    REQFLAGS,REQFILED   REQUEST IS ALREADY IN JCLFILE?               
         BO    GJ30                                                             
*                                                                               
         OPEN  (JCLFILE,(OUTPUT))  WRITE ALL CARDS TO REQUEST FILE              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK,SPACES         BUILD HEADER RECORD                          
         MVC   WORK(8),=C'*JCLHDR*'                                             
         MVC   WORK+8(18),OPMS1+8  UUUUUUUU,SSS,NNNNN                           
         MVC   WORK+27(2),REQCLASS CLASS                                        
         GOTO1 =V(HEXOUT),DMCB,REQTIME,WORK+30,2,=C'TOG'                        
         CLC   =F'4',DMCB+16       SUBMIT TIME (4/3 SECOND UNITS)               
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HEXOUT),DMCB,REQREPUI,WORK+35,2,=C'TOG'                       
         CLC   =F'4',DMCB+16       HEX USERID                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R1,R5               A(THIS REQUEST TABLE ENTRY)                  
         S     R1,CIAPART          R1 = DISPLACEMENT INTO PARTITION             
         SR    R0,R0                                                            
         D     R0,=A(REQTABLQ)                                                  
         AH    R1,CICINDX          R1 = REPORT NUMBER                           
         STH   R1,HALF                                                          
         GOTO1 =V(HEXOUT),DMCB,HALF,WORK+40,2,=C'TOG'                           
         CLC   =F'4',DMCB+16       REFERENCE NUMBER                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(HEXOUT),DMCB,REQFLAGS,WORK+45,1,=C'TOG'                       
         CLC   =F'2',DMCB+16       FLAGS                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+48(1),REQPRTY  PRIORITY                                     
         MVC   WORK+50(5),CFPQID   PQ NAME                                      
         MVC   WORK+56(3),REQSYPGM SYSTEM/PROGRAM                               
         MVC   WORK+60(1),REQFACPK FACPAK ID                                    
         PUT   JCLFILE,WORK                                                     
*                                                                               
GJ10     GOTO1 =V(DATAMGR),DMCB,(0,=C'READ'),CFPQID,0,R,CXREC                   
         CLI   DMCB+8,0                                                         
         BNE   GJ20                NO MORE RECORDS                              
         PUT   JCLFILE,R+1                                                      
         B     GJ10                                                             
*                                                                               
GJ20     CLOSE (JCLFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GJ30     LA    R1,GJBADX           EODAD                                        
         LA    RF,JCLFILE          A(DCB) FOR JCLFILE                           
         ST    R1,DCBEODAD-IHADCB(RF)                                           
*                                                                               
         OPEN  (JCLFILE,(INPUT))   READ REQUEST BACK FROM FILE                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AGENCY,=C'??'       ASSUME NO AGENCY CODE                        
*                                                                               
         MVC   OPMS1+27(14),=C'*NO EXEC CARD*'                                  
GJ40     GET   JCLFILE,R                                                        
*                                                                               
         CLC   =C'$$MONSOON',R     NEW-STYLE "JCL"?                             
         BNE   GJ45                                                             
*                                                                               
GJ42     GET   JCLFILE,R                                                        
*                                                                               
         CLC   =C'EXEC=',R                                                      
         BNE   *+14                                                             
         MVC   ATTCHPGM,R+5        SAVE PROGRAM NAME                            
         B     GJ42                                                             
*                                                                               
         CLC   =C'TIME=',R                                                      
         BNE   *+20                                                             
         MVC   RTPTCB+7(73),SPACES                                              
         MVC   RTPTCB+7(6),R+5     SAVE ATTACH TIME                             
         B     GJ42                                                             
*                                                                               
         CLC   =C'$$CONTROLLER',R                                               
         BE    GJ142                                                            
         B     GJ42                                                             
*                                                                               
GJ45     CLC   =C'//',R            NO -- IS THIS A JCL STATEMENT?               
         BNE   GJ40                NO -- READ THE NEXT CARD                     
*                                                                               
         LA    RF,R+1              SKIP PAST THE OPTIONAL NAME FIELD...         
         LA    RF,1(RF)            AND LOCATE THE OPERATION FIELD               
         CLI   0(RF),C' '                                                       
         BNE   *-8                                                              
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BE    *-8                                                              
         CLC   =C'EXEC ',0(RF)     IS THIS THE EXEC CARD?                       
         BNE   GJ40                NO -- READ THE NEXT CARD                     
*                                                                               
         MVC   AGENCY,R+2          SAVE AGENCY CODE                             
*                                                                               
         LA    RF,4(RF)                                                         
         LA    RF,1(RF)            FIGURE OUT WHAT TO ATTACH                    
         CLI   0(RF),C' '                                                       
         BE    *-8                                                              
*                                                                               
         MVI   BYTE,0              ASSUME PROCEDURE NAME WILL BE FOUND          
         CLC   =C'PGM=',0(RF)      IS THE LOAD MODULE NAMED?                    
         BNE   *+16                                                             
         LA    RF,4(RF)            YES -- POINT RF TO LOAD MODULE NAME          
         MVI   BYTE,X'FF'          A PROGRAM NAME WAS FOUND                     
         B     GJ60                                                             
*                                                                               
         CLC   =C'PROC=',0(RF)     IS A PROCEDURE NAMED?                        
         BNE   *+8                 NO -- RF ALREADY POINTS TO PROCEDURE         
         LA    RF,5(RF)            YES -- POINT RF TO PROCEDURE NAME...         
         ST    RF,FULL             ... AND SAVE IT                              
*                                                                               
GJ50     CLI   0(RF),C' '          END OF CARD?                                 
         BNE   *+12                                                             
         L     RF,FULL             YES -- RESTORE RF                            
         B     GJ60                                                             
         CLI   0(RF),C','          END OF NAME?                                 
         BE    *+12                YES                                          
         LA    RF,1(RF)                                                         
         BNE   GJ50                                                             
*                                                                               
         CLC   =C',PROG=',0(RF)    IS OVERRIDE PROGRAM (&PROG) GIVEN?           
         BE    *+12                                                             
         L     RF,FULL             RESTORE RF                                   
         B     GJ60                                                             
         LA    RF,6(RF)            YES -- POINT RF TO THE NAME                  
         MVI   BYTE,X'FF'          A PROGRAM NAME WAS FOUND                     
*                                                                               
GJ60     MVC   ATTCHPGM,SPACES     SYMBOL MUST BE BLANK-PADDED                  
         LA    R1,ATTCHPGM                                                      
GJ70     MVC   0(1,R1),0(RF)       PUT SYMBOL INTO WORK AREA                    
         LA    RF,1(RF)                                                         
         CLI   0(RF),C' '                                                       
         BE    *+20                END OF CARD                                  
         CLI   0(RF),C','                                                       
         BE    *+12                END OF NAME                                  
         LA    R1,1(R1)                                                         
         B     GJ70                                                             
*                                                                               
         CLI   BYTE,X'FF'          WAS A PROGRAM NAME FOUND?                    
         BE    GJ90                YES -- IT'S NOT A PROCEDURE                  
*                                                                               
         LA    R2,CONTRLRS         LIST OF CONTROLLERS                          
GJ80     CLI   0(R2),X'FF'         END OF LIST?                                 
         BNE   *+14                                                             
         MVC   OPMS1+27(14),=C'*UNKNOWN PROC*'                                  
         B     GJBADX              YES -- UNKNOWN CONTROLLER                    
         ZIC   RE,8(R2)            NUMBER OF SIGNIFICANT CHARACTERS - 1         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),ATTCHPGM                                                 
         BE    *+12                WE HAVE A MATCH, R2 = A(CONTROLLER)          
         LA    R2,L'CONTRLRS(R2)   BUMP TO NEXT CONTROLLER                      
         B     GJ80                                                             
         MVC   ATTCHPGM,0(R2)      SAVE PROGRAM NAME                            
*                                                                               
GJ90     MVC   RTPTCB+7(73),SPACES CLEAR OUT THE TIME                           
*                                                                               
GJ100    CLI   0(RF),C' '          END OF CARD?                                 
         BE    GJ130                                                            
         CLC   =C',TIME=',0(RF)    IS THIS THE TIME PARAMETER?                  
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     GJ100                                                            
*                                                                               
         CLI   6(RF),C'('          IS TIME GIVEN IN MINUTES ONLY?               
         BE    GJ110               NO                                           
         LA    R2,6(RF)            R2 = A(NUMBER OF MINUTES)                    
         BAS   RE,CONVTIME                                                      
         L     R1,DMCB+4                                                        
         CHI   R1,1440             TIME=1440 ? (UNLIMITED?)                     
         BE    GJ130               YES -- IGNORE THIS (NOT ALLOWED)             
         MHI   R1,60               R1 = NUMBER OF SECONDS                       
         B     GJ120                                                            
*                                                                               
GJ110    LA    R2,7(RF)            R2 = A(NUMBER OF MINUTES)                    
         BAS   RE,CONVTIME                                                      
         L     R1,DMCB+4                                                        
         MHI   R1,60               R1 = NUMBER OF SECONDS                       
         L     R2,DMCB             A(NEXT CHARACTER)                            
         CLI   0(R2),C')'          ANY SECONDS?                                 
         BE    GJ120               NO                                           
         CLI   0(R2),C','                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,1(R2)                                                         
         BAS   RE,CONVTIME                                                      
         L     R2,DMCB             A(NEXT CHARACTER)                            
         CLI   0(R2),C')'          ANY SECONDS?                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         A     R1,DMCB+4           ADD SECONDS TO MINUTES                       
*                                                                               
GJ120    EDIT  (R1),(6,RTPTCB+7),ALIGN=LEFT                                     
*                                                                               
GJ130    MVC   OPMS1+27(15),=C'*NO SYSIN CARD*'                                 
GJ140    GET   JCLFILE,R           SYSIN CARD NOT FOUND                         
         CLC   =C'//SYSIN',R                                                    
         BNE   GJ140               IT IS NOT THE SYSIN CARD                     
*                                                                               
GJ142    MVC   CARD,SPACES         THIS MUST BE *FIRST* CONTROL CARD            
         MVC   CARD(9),=C'NOCONTROL' NEVER PRINT CONTROL CARDS                  
         PUT   SYSIN,CARD          PUT NOCONTROL CARD TO SYSIN FILE             
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ143               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GJ143    CLI   DSPACE,C' '         DSPACE= CARD PRESENT?                        
         BE    GJ145                                                            
         MVC   CARD,SPACES         YES -- GENERATE A DSPACE= CARD               
         MVC   CARD(7),=C'DSPACE='                                              
         MVC   CARD+7(1),DSPACE                                                 
         PUT   SYSIN,CARD          PUT DSPACE= CARD TO SYSIN FILE               
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ145               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GJ145    XC    TCBTIME,TCBTIME                                                  
         CLI   RTPTCB+7,C' '       WAS A TIME PARAMETER FOUND?                  
         BE    GJ150                                                            
         LA    R2,RTPTCB           YES -- ADJUST IT IF NECESSARY                
         BRAS  RE,TCBCHNGE                                                      
         PUT   SYSIN,RTPTCB        PUT RTPTCB CARD TO SYSIN FILE                
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ150               NO                                           
         MVC   P(80),RTPTCB                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GJ150    MVC   CARD,SPACES                                                      
         MVC   CARD(6),=C'DDSIO='  ALWAYS GENERATE A DDSIO= CARD                
         MVC   CARD+6(8),DDSIOMOD                                               
         PUT   SYSIN,CARD          PUT DDSIO= CARD TO SYSIN FILE                
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ152               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GJ152    MVC   CARD,SPACES                                                      
         MVC   CARD(8),=C'CORETAB='  ALWAYS GENERATE A CORETAB= CARD            
         GOTO1 =V(HEXOUT),DMCB,=A(CORETAB),CARD+8,4,=C'TOG'                     
         CLC   =F'8',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         PUT   SYSIN,CARD          PUT CORETAB= CARD TO SYSIN FILE              
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ153               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GJ153    CLI   LOADCARD,0          GENERATE STOKEN= CARD IF REQUIRED            
         BE    GJ155                                                            
         MVC   CARD,SPACES                                                      
         MVC   CARD(7),=C'STOKEN='                                              
         MVC   CARD+7(L'LDSTOKEN),LDSTOKEN                                      
         MVC   CARD+7+L'LDSTOKEN(L'LDORIGIN),LDORIGIN                           
         PUT   SYSIN,CARD          PUT STOKEN= CARD TO SYSIN FILE               
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ155               NO                                           
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GJ155    XC    DIRECTID,DIRECTID                                                
         MVI   UPDATIVE,C'N'       ASSUME NOT AN UPDATIVE SOON                  
*                                                                               
         MVC   OPMS1+27(15),=C'*NO "**" (EOF)*'                                 
GJ160    GET   JCLFILE,R                                                        
         CLC   =C'**',R            END OF CARDS?                                
         BE    GJ200               YES                                          
*&&UK                                                                           
         MVC   WORK(80),R          IGNORE BLANK REQUEST/CONTROL CARDS           
         OC    WORK(80),SPACES                                                  
         CLC   WORK(80),SPACES                                                  
         BE    GJ160                                                            
*&&                                                                             
         CLC   =C'DIRECT=',R       IS THIS THE DIRECT= CARD?                    
         BNE   GJ170                                                            
         CLC   =C'ID=',R+34        YES -- DO WE HAVE THE PQ INFO HERE?          
         BE    *+14                                                             
         MVC   OPMS1+27(15),=C'*NO DIRECT ID=*'                                 
         B     GJBADX              NO -- BUT ALL SOON REQS MUST HAVE IT         
         MVC   DIRECTID,R+37       SAVE PQ INFO                                 
         B     GJ190                                                            
*                                                                               
GJ170    CLC   =C'RTPTCB=',R       IS THIS AN RTPTCB CARD?                      
         BNE   *+16                                                             
         LA    R2,R                YES -- ADJUST IT IF NECESSARY                
         BRAS  RE,TCBCHNGE                                                      
         B     GJ190                                                            
*                                                                               
         CLC   =C'RECOVER=W',R     IS THIS AN UPDATIVE SOON?                    
         BNE   *+8                                                              
         MVI   UPDATIVE,C'Y'       YES                                          
*                                                                               
GJ190    PUT   SYSIN,R             PUT REQUEST CARD TO SYSIN FILE               
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ160               NO                                           
         MVC   P(80),R                                                          
         GOTO1 =V(PRINTER)                                                      
         B     GJ160                                                            
*                                                                               
GJ200    MVI   R,C'/'              REPLACE '**' WITH '/*' TO DENOTE EOF         
         PUT   SYSIN,R             PUT FINAL CARD TO SYSIN FILE                 
         CLI   TRACEFLG,C'Y'       PRINT DETAILED TRACE?                        
         BNE   GJ205               NO                                           
         MVC   P(80),R                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GJ205    OC    TCBTIME,TCBTIME     WAS RTPTCB CARD GENERATED?                   
         BNZ   GJ210                                                            
*&&UK                                                                           
         MVC   TCBTIME,=X'7FFF'    SET TCBTIME TO HIGH VALUES                   
         B     GJ210                                                            
*&&                                                                             
         MVC   OPMS1+27(15),=C'*NO TIME GIVEN*'                                 
         B     GJBADX              NO                                           
*                                                                               
GJ210    MVC   OPMS1+27(15),SPACES NO ERRORS FOUND                              
         CLOSE (JCLFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE (SYSIN)                                                          
         LTR   RF,RF                                                            
         BZ    YES                 SET CC EQUAL                                 
         DC    H'0'                UNSUCCESSFUL CLOSE                           
*                                                                               
*                                                                               
GJBADX   CLOSE (JCLFILE,)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLOSE (SYSIN)                                                          
         LTR   RF,RF                                                            
         BZ    NO                  SET CC NOT EQUAL                             
         DC    H'0'                UNSUCCESSFUL CLOSE                           
*                                                                               
         ANSR                                                                   
         EJECT                                                                  
         SPACE 2                                                                
CONTRLRS DS    0CL9                CONTROLLER TABLE                             
*                                  FIRST 8 BYTES IS CONTROLLER NAME             
*                                  NINTH BYTE IS NO. CHARACTERS MINUS 1         
*                                                                               
*&&US                                                                           
         DC    C'SPOOF   ',AL1(4)  SPOOFXXX                                     
         DC    C'SPONSOR ',AL1(6)  SPONSOR                                      
         DC    C'REPORTER',AL1(7)  REPORTER                                     
         DC    C'PPG     ',AL1(2)  PPG                                          
         DC    C'MONACC  ',AL1(5)  MONACC                                       
*&&                                                                             
*&&UK                                                                           
         DC    C'SPOOF   ',AL1(4)  SPOOF                                        
         DC    C'MONACC  ',AL1(5)  MONACC                                       
         DC    C'MERLIN  ',AL1(5)  MERLIN                                       
*&&                                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
CONVTIME NTR1                      R2 = A(NUMBER)                               
*                                                                               
         XC    DUB,DUB                                                          
         LA    RF,DUB                                                           
CT10     CLI   0(R2),C'0'          IS CHARACTER NUMERIC?                        
         BL    CT20                                                             
         CLI   0(R2),C'9'                                                       
         BH    CT20                                                             
         MVC   0(1,RF),0(R2)       YES -- SAVE IT                               
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CT10                                                             
*                                                                               
CT20     GOTO1 =V(NUMVAL),DMCB,DUB,(2,0)                                        
         CLI   DMCB,0              DID WE GET VALUE?                            
         BE    *+10                                                             
         XC    DMCB+4(4),DMCB+4    NO -- RETURN ZERO                            
         ST    R2,DMCB             A(STRING END)                                
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
GETTTTT  NTR1  BASE=*                                                           
*                                                                               
* R5 = A(REQUEST TABLE ENTRY)                                                   
* R6 = A(CITABLE ENTRY)                                                         
* UPON EXIT, FIELD 'HALF' CONTAINS THE TRACK NUMBER TTTT                        
*                                                                               
         LR    R1,R5               A(THIS REQUEST TABLE ENTRY)                  
         S     R1,CIAPART          R1 = DISPLACEMENT INTO PARTITION             
         SR    R0,R0                                                            
         D     R0,=A(REQTABLQ)                                                  
         AH    R1,CICINDX          R1 = REPORT NUMBER                           
*                                                                               
         LA    R1,1(R1)                                                         
         MH    R1,CITRKS                                                        
         LH    R0,CITRKS                                                        
         BCTR  R0,0                                                             
         SR    R1,R0               R1 = TTTT                                    
         STH   R1,HALF                                                          
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
WAITABIT NTR1  BASE=*                                                           
*                                                                               
* IF LESS THAN THE REQUESTED TIME INTERVAL HAS ELAPSED SINCE THE                
* LAST SEARCH OF THE PRINT QUEUES, THEN WAIT UNTIL THE FULL INTERVAL            
* HAS EXPIRED BEFORE SEARCHING AGAIN.  ALSO, CHECK FOR AN OPERATOR              
* INTERRUPT.                                                                    
*                                                                               
         SR    R0,R0               R0 = CLOCK TIME                              
         L     R2,WAITSECS         R2 = THE WAIT INTERVAL                       
         CLI   ATTCHFLG,C'Y'       WAIT FULL TIME IF NOT ATTACHING              
         BNE   WB12                                                             
         CLI   ENQJOBNM,C'Y'       IF PREVENTING MULTIPLE JOBNAMES. . .         
         BE    WB12                . . . THEN ALWAYS WAIT THE INTERVAL          
         CLI   JOBSLEFT,C'Y'       ANY REQUESTS REMAINING TO RUN?               
         BE    WB20                YES -- RUN THEM NOW, DON'T WAIT              
*                                                                               
WB10     TIME  BIN                 GET THE CURRENT TIME                         
         LTR   R0,R0                                                            
         BZ    WB10                BAD RETURN FROM MACRO                        
*                                                                               
         S     R0,PQSRCHTM         R0 = ELASPED TIME SINCE PQ SEARCH            
         CR    R0,R2               HAVE WE WAITED THE FULL INTERVAL?            
         BNL   WB20                YES                                          
*                                                                               
WB12     SR    R2,R0               R2 = REMAINING TIME TO WAIT                  
         ST    R2,FULL                                                          
         XC    TIMRECB2,TIMRECB2                                                
         STIMERM SET,ID=STIMERID,BINTVL=FULL,EXIT=TIMRXIT2                      
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  1,ECBLIST=ECBLST2   WAIT FOR TIMER POP OR OPERATOR               
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    WB15                NO                                           
         STIMERM CANCEL,ID=STIMERID  YES, SO CANCEL THE TIMER                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
         B     WB20                                                             
*                                                                               
WB15     TM    TIMRECB2,X'40'      DID THE TIMER POP?                           
         BO    *+6                 YES                                          
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
*                                                                               
WB20     MVI   JOBSLEFT,C'N'       SO FAR, NO REQUESTS LEFT OVER                
*                                                                               
WBX      XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* THE TIMER EXIT ROUTINE.  IT POSTS AN ECB.                                     
*                                                                               
         SPACE 3                                                                
TIMRXIT2 SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMRXIT2,RB                                                      
         POST  TIMRECB2                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         SPACE 2                                                                
TIMRECB2 DC    F'0'                ECB OF ATTACHED TASK TIMER                   
         EJECT                                                                  
TCBCHNGE NTR1  BASE=*                                                           
*                                                                               
* AN RTPTCB= CARD IS ABOUT TO BE WRITTEN TO SYSIN.  HOWEVER, THE                
* TIME ON THE CARD MAY NEED TO BE ADJUSTED DEPENDING UPON WHICH CPU             
* IS RUNNING THIS MONSOON.  IF SO, CHANGE THE CARD AND RETURN.                  
* UPON ENTRY, R2 = A(RTPTCB CARD).                                              
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,7(R2),(2,0)                                      
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R0,DMCB+4           TCB TIME ON CARD                             
*                                                                               
         MH    R0,TCBPCT           (TCB TIME) X (&AGE ADJUSTMENT). . .          
         SRDL  R0,32               . . . DIVIDED BY 100. . .                    
         D     R0,=F'100'          . . . GIVES NEW TCB TIME IN R1               
         LTR   R0,R0               WAS THERE A REMAINDER?                       
         BZ    *+8                                                              
         LA    R1,1(R1)            YES -- ALWAYS ROUND UP                       
         STH   R1,TCBTIME          SAVE THE TCB TIME ALLOWED                    
         EDIT  (R1),(6,7(R2)),ALIGN=LEFT                                        
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
PUTWARN  NTR1  BASE=*                                                           
*                                                                               
* PUT WARNING MESSAGE ON CONSOLE                                                
*                                                                               
         TIME  BIN                                                              
         S     R0,STRTTMRL         R0 = ELAPSED REAL TIME                       
         BNM   *+8                                                              
         A     R0,=A(24*3600*100)  ADD 24HRS, JUST WENT PAST MIDNIGHT           
         BRAS  RE,PRTTIME1         PUTS FORMATTED TIME IN WORK                  
         MVC   OPMS4+53(8),WORK                                                 
*                                                                               
         L     R1,AASCB            A(ASCB)                                      
         L     R0,ASCBEJST-ASCB(R1)                                             
         S     R0,STRTTMTC         R0 = ELAPSED TCB TIME                        
         BRAS  RE,PRTTIME2         PUTS FORMATTED TIME IN WORK                  
         MVC   OPMS4+40(8),WORK                                                 
*                                                                               
         MVC   OPMS4+16(18),OPMS1+8  UUUUUUUU,XXX,NNNNN                         
*                                                                               
         MVC   OPMS4+63(16),SPACES TELL OPERATOR IF IT'S UPDATIVE               
         CLI   UPDATIVE,C'Y'                                                    
         BNE   *+10                                                             
         MVC   OPMS4+63(16),=C'*** UPDATIVE ***'                                
*                                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS4,OPMS4)                       
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
OPMS4    DC    C'*** WARNING *** UUUUUUUU,XXX,NNNNN  TCB=HH.MM.SS  ET=H+        
               H.MM.SS  *** UPDATIVE ***'                                       
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
OPENCTFL NTR1  BASE=*                                                           
*                                                                               
         BC    0,OPENCTX           FIRST TIME THROUGH, DON'T EXIT               
         MVI   *-3,X'F0'           ONLY OPEN CONTROL SYSTEM ONCE                
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(0,=C'DMOPEN'),=C'CONTROL',            +        
               =C'NCTFILE X',AIO,0                                              
*                                                                               
OPENCTX  XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
CONVTHMS NTR1  BASE=*                                                           
*                                                                               
* CONVERT THE BINARY 4/3 SECOND UNIT TIME IN THE FIRST TWO BYTES OF             
* 'THREE' INTO FORMAT X'HHMMSS', AND PUT THE RESULT BACK INTO THREE.            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,THREE          BINARY UNIT TIME IN FIRST TWO BYTES          
         SLL   R0,2                TIMES 4                                      
         SRDL  R0,32               PREPARE FOR DIVIDE                           
         D     R0,=F'3'            R1 = TIME IN SECONDS ONLY                    
         SR    R0,R0                                                            
         D     R0,=F'60'           R1 = TIME IN MINUTES ONLY                    
         STC   R0,THREE+2          NUMBER OF SECONDS                            
         SR    R0,R0                                                            
         D     R0,=F'60'           R1 = TIME IN MINUTES ONLY                    
         STC   R0,THREE+1          NUMBER OF MINUTES                            
         STC   R1,THREE            NUMBER OF HOURS                              
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
CHKOPER  NTR1  BASE=*                                                           
*                                                                               
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'                 
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.                
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH10                                                             
         MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'STOP COMMAND ACCEPTED'             
         B     CHX                                                              
*                                                                               
CH10     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         CLC   CIBDATLN,=H'6'      LENGTH OF 'MODIFY' STRING                    
         BNE   CH30                                                             
         CLC   =C'JESMSG',CIBDATA  TOGGLE JESMSG SWITCH?                        
         BNE   CH20                                                             
         CLI   JESMSG,C'Y'         YES                                          
         BNE   *+18                                                             
         MVI   JESMSG,C'N'                                                      
         MVC   OPMS3+33(3),=C'OFF'                                              
         B     *+14                                                             
         MVI   JESMSG,C'Y'                                                      
         MVC   OPMS3+33(3),=C'ON '                                              
         L     R1,AASCB            A(ASCB)                                      
         MVC   HALF,ASCBASID-ASCB(R1)                                           
         GOTO1 =V(HEXOUT),DMCB,HALF,OPMS3+7,2,=C'TOG'                           
         CLC   =F'4',DMCB+16                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS3,OPMS3)                       
         B     CHX                                                              
*                                                                               
CH20     CLC   =C'STATUS',CIBDATA  DISPLAY STATUS?                              
         BNE   CHBAD                                                            
         MVI   BYTE,C'A'           PARAMETER TO STATUS SUBROUTINE               
         BRAS  RE,STATUS           DISPLAY STATUS OF ALL CLASSES                
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS1,OPMS1)                       
         B     CHX                                                              
*                                                                               
CH30     CLC   CIBDATLN,=H'4'      LENGTH OF 'MODIFY' STRING                    
         BNE   CH50                                                             
         CLC   =C'KILL',CIBDATA    DETACH CURRENT REQUEST AND STOP?             
         BNE   CH40                                                             
         CLI   UPDATIVE,C'Y'       CAN'T KILL AN UPDATIVE SOON                  
         BNE   CH35                                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'CANNOT KILL NOW: MONSOON R+        
               UNNING UPDATIVE SOON'                                            
         B     CHX                                                              
CH35     MVI   OPERKILL,C'Y'                                                    
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'KILL COMMAND ACCEPTED'             
         B     CHX                                                              
*                                                                               
CH40     CLC   =C'WAIT',CIBDATA    STOP ATTACHING REQUESTS                      
         BNE   CHBAD                                                            
         MVI   ATTCHFLG,C'X'       SET FOR 1ST LOOP TO INFORM OPERATOR          
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'WAIT COMMAND ACCEPTED'             
         B     CHX                                                              
*                                                                               
CH50     CLC   CIBDATLN,=H'2'      LENGTH OF 'MODIFY' STRING                    
         BNE   CH60                                                             
         CLC   =C'GO',CIBDATA      START ATTACHING REQUESTS                     
         BNE   CHBAD                                                            
         MVI   ATTCHFLG,C'Y'                                                    
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'GO COMMAND ACCEPTED'               
         CLI   LOADCARD,0          NEED TO REFRESH DATASPACE IF ACTIVE          
         BE    *+8                                                              
         MVI   LDACTION,LDREFQ                                                  
         B     CHX                                                              
*                                                                               
CH60     CLC   CIBDATLN,=H'5'      LENGTH OF 'MODIFY' STRING                    
         BNE   CHBAD                                                            
         CLC   =C'TRACE',CIBDATA   TOGGLE TRACE FLAG?                           
         BNE   CHBAD                                                            
         CLI   TRACEFLG,C'Y'       YES                                          
         BNE   *+18                                                             
         MVI   TRACEFLG,C'N'                                                    
         MVC   OPMS10+15(3),=C'OFF'                                             
         B     *+14                                                             
         MVI   TRACEFLG,C'Y'                                                    
         MVC   OPMS10+15(3),=C'ON '                                             
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPMS10,OPMS10)                     
         B     CHX                                                              
*                                                                               
CHBAD    GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'*INVALID MONSOON COMMAND*'         
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  R2                                                               
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
OPMS3    DC    C'ASID = XXXX:  START/END MESSAGES XXX'                          
OPMS10   DC    C'DETAILED TRACE XXX'                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* FORMAT TIME FOR PRINTOUT                                                      
*                                                                               
PRTTIME1 NTR1  BASE=*                                                           
*                                                                               
         SRDL  R0,32               CONVERT SEC*100 TO SEC                       
         D     R0,=F'100'                                                       
         SR    R0,R0               LEAVES SECONDS IN R1                         
         D     R0,=F'3600'         GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVI   WORK+2,C':'                                                      
         SRDL  R0,32                                                            
         D     R0,=F'60'           GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         MVI   WORK+5,C':'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* FORMAT TIME FOR PRINTOUT                                                      
*                                                                               
PRTTIME2 NTR1  BASE=*                                                           
*                                                                               
         SRDL  R0,32                                                            
         D     R0,=F'3600'         GIVES HOURS IN R1                            
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(2),DUB                                                      
         MVI   WORK+2,C':'                                                      
         SRDL  R0,32                                                            
         D     R0,=F'60'           GIVES MINUTES IN R1/SECS IN R0               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         MVI   WORK+5,C':'                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
LONGSOON NTR1  BASE=*                                                           
*                                                                               
         OPEN  (SYSIN,INPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,FKEY                                                          
         USING FKEYD,R2                                                         
*                                                                               
         MVC   FKEY(2),=C'**'                                                   
         MVC   FAGENCY,AGENCY                                                   
         MVC   FATTPGM,ATTCHPGM                                                 
         GOTO1 =V(DATCON),DMCB,(5,0),(11,FDATE)                                 
         MVI   FDATE+L'FDATE,C'|'                                               
         TIME  DEC                                                              
         ST    R0,PRNTDUB                                                       
         MVI   PRNTDUB+4,X'0F'                                                  
         UNPK  PRNTTIME,PRNTDUB(5)                                              
         MVC   FHOUR,PRNTTIME                                                   
         MVI   FHOUR+L'FHOUR,C':'                                               
         MVC   FMIN,PRNTTIME+2                                                  
         MVI   FMIN+L'FMIN,C':'                                                 
         MVC   FSEC,PRNTTIME+4                                                  
         DROP  R2                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'FKEY),FKEY                                                
         B     LS20                                                             
*                                                                               
LS10     GET   SYSIN,WORK                                                       
LS20     PUT   FILE,WORK                                                        
         B     LS10                GET NEXT LINE                                
*                                                                               
EOSYSIN  CLOSE (SYSIN)                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
COMMWORK DS    0D                  COMMON STORAGE AREA                          
         SPACE 2                                                                
SYSIN    DCB   DDNAME=SYSIN,MACRF=(GM,PM),DSORG=PS,RECFM=FB,LRECL=80,  +        
               BLKSIZE=800,EODAD=EOSYSIN                                        
JCLFILE  DCB   DDNAME=JCLFILE,MACRF=(GM,PM),DSORG=PS,RECFM=FB,LRECL=80,+        
               BLKSIZE=800                                                      
FILE     DCB   DDNAME=FILE,MACRF=PM,DSORG=PS,RECFM=FB,LRECL=80                  
         SPACE 2                                                                
         GETEL R4,28,ELCODE                                                     
         SPACE 2                                                                
ECBLST   DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    A(TIMERECB)         A(TASK TIMER ECB)                            
         DC    X'80',AL3(TASKECB)  A(ATTACHED TASK ECB)                         
         SPACE 2                                                                
ECBLST2  DC    A(0)                A(OPERATOR ECB IS STORED HERE)               
         DC    X'80',AL3(TIMRECB2) A(TIMER ECB)                                 
         SPACE 2                                                                
ELCODE   DC    X'02'               ID ELEMENT                                   
         SPACE 2                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         SPACE 3                                                                
UTL      DC    F'0',X'0A'          FOR DATAMGR (CONTROL SYSTEM)                 
         EJECT                                                                  
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
DUB      DS    D                                                                
PRNTDUB  DS    D                   FOR PRNT MACRO                               
FULL     DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
THREE    DS    XL3                                                              
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
WORK     DS    CL80                                                             
FKEY     DS    CL(FKEYLQ)                                                       
         SPACE 2                                                                
AIO      DC    A(IO)               A(IO AREA)                                   
ACITABLE DC    A(CITABLE)          A(PRINT QUEUE CI TABLE)                      
ACLASSTB DC    A(CLASSTAB)         A(CLASS TABLE)                               
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
AREQTAB  DS    A                   A(REQUEST TABLE)                             
AREQTABX DS    A                   A(END OF REQUEST TABLE)                      
AUSERTAB DS    A                   A(INDEXED USERID TABLE)                      
APQSAVE  DS    A                   A(PQ BUFFER SAVE AREA)                       
ACOMM    DS    A                   A(COMMUNICATIONS PARAMETER LIST)             
AASCB    DS    A                   A(ASCB) FROM EXTRACT                         
ACPUID   DS    A                   A(FOUR CHARACTER CPU ID)                     
AJOBNTRY DS    A                   A(JOBTAB ENTRY IN DSPACE)                    
         SPACE 2                                                                
LOADCARD DC    XL8'00'             LOAD= MODULE NAME                            
LOADPARM DS    0XL(LDPARMLQ)                                                    
LDCPRINT DC    V(CPRINT)                                                        
LDVPRINT DC    V(PRINTER)                                                       
LDSTOKEN DC    XL8'00'             STOKEN OF DATASPACE CREATED BY LOAD=         
LDALET   DC    XL4'0'              ALET DITTO                                   
LDORIGIN DC    XL4'0'              ORIGIN DITTO                                 
LDACTION DC    X'00'                                                            
LDINITQ  EQU   X'00'               INITIALISE                                   
LDREFQ   EQU   X'01'               REFRESH                                      
LDDELQ   EQU   X'FF'               DELETE                                       
LDPARMLQ EQU   *-LDCPRINT                                                       
         SPACE 2                                                                
MAXTCBTM DC    F'60'               MAX TCB TIME ALLOWED IN SEC                  
TASKTCB  DS    F                   TCB OF ATTACHED TASK                         
TASKECB  DC    F'0'                ECB OF ATTACHED TASK                         
STRTTMTC DS    F                   STARTING TCB TIME OF ATTACHED TASK           
STRTTMRL DS    F                   STARTING REAL TIME OF ATTACHED TASK          
PQSRCHTM DS    F                   TIME WE LAST SEARCHED PRINT QUEUES           
REQCLRTM DS    F                   TIME WE LAST CLEARED REQUEST TABLE           
MINTIME  DC    F'0'                ATTACH REQS ONLY >= MINTIME MINUTES          
WAITSECS DC    F'12000'            DEFAULT WAIT TIME = 120 SECONDS              
REQTIMER DC    F'30000'            WAIT 5 MINUTES BETWEEN WARNINGS              
LOADAVD  DC    F'0'                TOTAL # OF LOADS AVOIDED                     
LOADASK  DC    F'0'                TOTAL # OF LOAD REQUESTS                     
*                                                                               
SAVEPSW  DC    XL8'00'             SAVED PSW (FROM SDWA IN DDMASTER)            
SAVEREGS DS    0XL64                                                            
SAVER0   DC    F'0'                SAVED R0 (FROM SDWA IN DDMASTER)             
SAVER1   DC    F'0'                SAVED R1 (FROM SDWA IN DDMASTER)             
SAVER2   DC    F'0'                SAVED R2 (FROM SDWA IN DDMASTER)             
SAVER3   DC    F'0'                SAVED R3 (FROM SDWA IN DDMASTER)             
SAVER4   DC    F'0'                SAVED R4 (FROM SDWA IN DDMASTER)             
SAVER5   DC    F'0'                SAVED R5 (FROM SDWA IN DDMASTER)             
SAVER6   DC    F'0'                SAVED R6 (FROM SDWA IN DDMASTER)             
SAVER7   DC    F'0'                SAVED R7 (FROM SDWA IN DDMASTER)             
SAVER8   DC    F'0'                SAVED R8 (FROM SDWA IN DDMASTER)             
SAVER9   DC    F'0'                SAVED R9 (FROM SDWA IN DDMASTER)             
SAVERA   DC    F'0'                SAVED RA (FROM SDWA IN DDMASTER)             
SAVERB   DC    F'0'                SAVED RB (FROM SDWA IN DDMASTER)             
SAVERC   DC    F'0'                SAVED RC (FROM SDWA IN DDMASTER)             
SAVERD   DC    F'0'                SAVED RD (FROM SDWA IN DDMASTER)             
SAVERE   DC    F'0'                SAVED RE (FROM SDWA IN DDMASTER)             
SAVERF   DC    F'0'                SAVED RF (FROM SDWA IN DDMASTER)             
*                                                                               
ABENDCDS DS    0XL3                ABEND CODES WHICH DO NOT REQUIRE...          
*                                   ...A RESTART OF MONSOON                     
         DC    AL3(666)            OPERATOR STOP COMMAND                        
         DC    AL3(667)            RUN-TIME PARAMETER EXCEEDED                  
         DC    AL3(669)            BAD PATCH CARD                               
         DC    AL3(670)            BAD CPUID OR CONCURRENT FILE UPDATE          
         DC    AL3(899)            FRED ROE SPECIAL                             
         DC    X'222000'           OPERATOR CANCEL                              
         DC    X'13E000'           TIMED OUT BY MONSOON                         
         DC    X'FFFFFF'           EOT MARKER                                   
*                                                                               
NUMCLASS DC    A(0)                NUMBER OF CLASSES FOR THIS MONSOON           
REQTBLSZ DS    F                   NUMBER OF ENTRIES IN REQUEST TABLE           
TOTREQS  DC    F'0'                TOTAL NUMBER OF REQUESTS                     
NEXTREQ  DS    F                   INDEX TO NEXT REQUEST TO BE ATTACHED         
TCBPCT   DC    H'100'              PERCENTAGE ADJUSTMENT ON TCB TIME            
TCBTIME  DS    H                   TCB TIME ALLOWED FOR THIS REQUEST            
SVCLSPRT DC    H'0'                LAST CLASS PRIORITY VALUE                    
REFRESH  DC    X'7FFF'             REFRESH NUMBER (INIT TO HIGH VALUE)          
*&&US                                                                           
USERMAXR DC    H'5'                MAXIMUM REQS PER USER IN REQTABLE            
*&&                                                                             
*&&UK                                                                           
USERMAXR DC    H'5'                MAXIMUM REQS PER USER IN REQTABLE            
*&&                                                                             
EXUSRIDS DC    (EXUSRMXQ+1)H'0'    OPTIONAL HEX USERID NEGATIVE FILTERS         
EXUSRMXQ EQU   20                   MAXIMUM NUMBER OF EXCLUDED USERIDS          
USERFILT DC    H'0'                OPTIONAL HEX USERID FILTER                   
SUBFILT  DC    CL3'   '            OPTIONAL SUBID FILTER                        
REFFILT  DC    XL2'00'             OPTIONAL REFERENCE NUMBER FILTER             
EXREPORT DC    CL3' '              EXCLUDE THESE REPORTS (SPP)                  
CLASS    DS    XL(CLSTABLQ)        CLASS TABLE ENTRY                            
MAJORNAM DC    CL8'MONSOON '       MAJOR NAME TO GLOBALLY ENQ                   
MINORNAM DC    CL8' '              MINOR NAME TO GLOBALLY ENQ                   
MNSOONID DC    CL8' '              IDENTIFIES THE MONSOON IN DATASPACE          
DSPACE   DC    C' '                DATASPACE IDENTIFIER                         
*                                                                               
PRTYS    DS    10F                 PRIORITY ACCUMULATORS                        
*                                   (INDEXED: WORD 1  = PRIORITY 0...           
*                                             WORD 10 = PRIORITY 9)             
*                                                                               
JOBNAME  DS    0CL11               MIMICS MVS "JOBNAME"                         
JOBUSER  DS    CL8                 USERID                                       
JOBSYPGM DS    0CL3                                                             
JOBSYST  DS    C                   SYSTEM                                       
JOBPROG  DS    CL2                 PROGRAM                                      
*                                                                               
AGENCY   DS    CL2                 AGENCY CODE                                  
ATTCHPGM DS    CL8                 NAME OF PROGRAM TO ATTACH                    
DIRECTID DS    CL18                DIRECT CARD ID=                              
MINPRTY  DC    C'0'                LOWEST ACCEPTABLE PRIORITY TO ATTACH         
MAXPRTY  DC    C'9'                HIGHEST PRIORITY TO ATTACH                   
REFPRTY  DC    C'0'                LOWEST PRIORITY BEFORE REFRESH               
FACPAK   DC    C' '                FACPAK FILTER                                
FACPAKFL DS    C                   'P' = FACPAK FILTER IS POSITIVE              
*                                  'N' = FACPAK FILTER IS NEGATIVE              
TRACEFLG DC    C'N'                'Y' = PRINT DETAILED TRACE                   
ALLCLASS DC    C'N'                'Y' = ACCEPT ALL SOON CLASSES                
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
OPERKILL DC    C'N'                'Y' = OPERATOR ENTERED MODIFY 'KILL'         
ATTCHFLG DC    C'Y'                'Y' = ATTACH REQUESTS                        
TYPUPD   DC    C'N'                'Y' = ATTACH ONLY UPDATIVE SOONS             
TYPNOUPD DC    C'N'                'Y' = ATTACH ONLY NON-UPDATIVE SOONS         
YESTERDY DC    C'N'                'Y' = ATTACH ONLY YESTERDAY'S JOBS           
ANYDATE  DC    C'N'                'Y' = ATTACH REQUESTS FROM ANY DATE          
UPDATIVE DS    C                   'Y' = RECOVER=W SPECIFIED (UPDATIVE)         
JOBSLEFT DC    C'N'                'Y' = REQUESTS ARE REMAINING TO RUN          
JESMSG   DC    C'N'                'Y' = DO CONSOLE MSGS FOR START/END          
CLSPRTYF DC    C'N'                'Y' = CLASS LIST IS PRIORITIZED              
LOPRTYF1 DC    C'N'                'Y' = WE'VE FOUND A LOW PRTY REQUEST         
LOPRTYF2 DC    C'N'                'Y' = UPDATE THE REQUEST TABLE               
ENQJOBNM DC    C'N'                'Y' = ONLY ONE REQUEST PER JOBNAME           
DUMPWARN DC    C'Y'                'N' = NO WARNING FOR DUPLICATE DUMPS         
STIMERID DS    XL4                 FOR TIMER POPS                               
PRTQUE   DC    C'PRTQU'            PRTQ NAME                                    
OPMS1    DC    CL42'NO REQUEST RUNNING NOW'                                     
CARD     DS    CL80                FOR INPUT/OUTPUT OF CONTROL CARDS            
RTPTCB   DC    CL80'RTPTCB='       RTPTCB= CARD FOR MASTER SYSIN                
DDSIOMOD DC    CL8'DDSIO   '       DDSIO LOAD MODULE NAME                       
KEY      DS    XL40                FOR PQ INDEX AND CTFILE READS                
         DS    XL4                 PQ RECORD LENGTH                             
R        DS    CL150               PQ RECORD                                    
         EJECT                                                                  
SRTREC   DS    0D                  SORT RECORD                                  
SRTFORCE DS    X                   MONSOON INTERNAL      (DESCENDING)           
SRTCLASS DS    XL2                 CLASS NUMBER (OR 0)   (ASCENDING)            
SRTPRTY  DS    C                   REQ PRIORITY (0..9)   (DESCENDING)           
SRTYESTD DS    X                   X'FF' IF YESTERDAY    (DESCENDING)           
SRTTIME  DS    XL2                 REPORT CREATION TIME  (ASCENDING)            
SRTKEYLQ EQU   *-SRTREC                                                         
SRTINDEX DS    XL4                 INDEX INTO REQUEST TABLE                     
SRTRECLQ EQU   *-SRTREC                                                         
         SPACE 3                                                                
         IEZCOM                                                                 
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CORETAB'         CORE-RESIDENT PHASE TABLE                    
CORETAB  DS    0X                                                               
*                                                                               
*  TABLE FORMAT:                                                                
*    BYTE  1:     FLAGS                                                         
*                 X'FF' = END OF TABLE                                          
*                 X'80' = CALL THIS ROUTINE AT INITIALIZATION                   
*    BYTES 2..4:  PHASE NAME (E.G., 000A30 = T00A30 = GENCON)                   
*    BYTES 5..8:  A(PHASE)                                                      
*                                                                               
*&&US                                                                           
         DC    X'00000A',AL1(QBOOKVAL),A(0)                                     
         DC    X'00000A',AL1(QCENTER),A(0)                                      
         DC    X'00000A',AL1(QCHOPPER),A(0)                                     
         DC    X'00000A',AL1(QDAYVAL),A(0)                                      
         DC    X'00000A',AL1(QDEMCON),A(0)                                      
         DC    X'00000A',AL1(QDEMEX),A(0)                                       
         DC    X'00000A',AL1(QDEMOTAB),A(0)                                     
         DC    X'00000A',AL1(QDEMVAL),A(0)                                      
         DC    X'00000A',AL1(QREDEMUP),A(0)                                     
         DC    X'00000A',AL1(QINVEDIT),A(0)                                     
         DC    X'00000A',AL1(QPAVCOND),A(0)                                     
         DC    X'00000A',AL1(QPAVEXPL),A(0)                                     
         DC    X'00000A',AL1(QSPOOL),A(0)                                       
         DC    X'00000A',AL1(QSQUASH),A(0)                                      
         DC    X'00000A',AL1(QTIMVAL),A(0)                                      
         DC    X'00000A',AL1(QUNDAY),A(0)                                       
         DC    X'00000A',AL1(QUNDRLIN),A(0)                                     
         DC    X'00000A',AL1(QUNTIME),A(0)                                      
         DC    X'00000A',AL1(QXSORT),A(0)                                       
         DC    X'00000A',AL1(QUPVAL),A(0)                                       
         DC    X'00000A',AL1(QCLPACK),A(0)                                      
         DC    X'00000A',AL1(QCLUNPK),A(0)                                      
         DC    X'00000A',AL1(QNETUNIV),A(0)                                     
         DC    X'00000A',AL1(QNETWEEK),A(0)                                     
         DC    X'00000A',AL1(QNETGTSP),A(0)                                     
         DC    X'00000A',AL1(QNEUTIL),A(0)                                      
         DC    X'00000A',AL1(QPARSNIP),A(0)                                     
         DC    X'00000A',AL1(QMSPACK),A(0)                                      
         DC    X'00000A',AL1(QMSUNPK),A(0)                                      
*********DC    X'00000A',AL1(QSTAPACK),A(0)                                     
         DC    X'00000A',AL1(QBLDMGA),A(0)                                      
         DC    X'00000A',AL1(QGETBROD),A(0)                                     
         DC    X'00000A',AL1(QNETUNBK),A(0)                                     
         DC    X'00000A',AL1(QGETDEM1),A(0)                                     
         DC    X'00000A',AL1(QGETDEM2),A(0)                                     
         DC    X'00000A',AL1(QSPDEMUP),A(0)                                     
         DC    X'00000A',AL1(QNETSPVL),A(0)                                     
         DC    X'00000A',AL1(QSPOON),A(0)                                       
         DC    X'00000A',AL1(QDEFINE),A(0)                                      
         DC    X'00000A',AL1(QNETIO),A(0)                                       
         DC    X'00000A',AL1(QNETVALU),A(0)                                     
         DC    X'00000A',AL1(QGENCON),A(0)                                      
         DC    X'00000A',AL1(QDICTCON),A(0)                                     
         DC    X'00000A',AL1(QGETNUN),A(0)                                      
         DC    X'00000A',AL1(QGETHUT),A(0)                                      
         DC    X'00000A',AL1(QNETGOAL),A(0)                                     
         DC    X'00000A',AL1(QDRIVAL),A(0)                                      
         DC    X'00000A',AL1(QGENPRG),A(0)                                      
         DC    X'00000A',AL1(QOFFICER),A(0)                                     
         DC    X'00000A',AL1(QDRONE),A(0)                                       
         DC    X'00000A',AL1(QBUFFOON),A(0)                                     
         DC    X'00000A',AL1(QMEDGEN),A(0)                                      
         DC    X'00000A',AL1(QSPACNVL),A(0)                                     
         DC    X'00000A',AL1(QSPOTIO),A(0)                                      
         DC    X'00000A',AL1(QSPOTDRV),A(0)                                     
         DC    X'00000A',AL1(QRANSID),A(0)                                      
         DC    X'00000A',AL1(QSPOTBUY),A(0)                                     
         DC    X'00000A',AL1(QSPOTBK),A(0)                                      
         DC    X'00000A',AL1(QNEWRIGN),A(0)                                     
*********DC    X'00000A',AL1(QPRNTIO),A(0) BOBY SAYS NOT RE-ENTRANT             
         DC    X'00000A',AL1(QQSORT),A(0)                                       
         DC    X'00000A',AL1(QSPOTGL),A(0)                                      
         DC    X'00000A',AL1(QSPWRIGN),A(0)                                     
         DC    X'00000A',AL1(QPODDRIV),A(0)                                     
         DC    X'00000A',AL1(QPDWRIGN),A(0)                                     
         DC    X'00000A',AL1(QTSAR),A(0)                                        
         DC    X'00000A',AL1(QOFFAL),A(0)                                       
         DC    X'00000A',AL1(QADDTRN),A(0)                                      
         DC    X'00000A',AL1(QNODIO),A(0)                                       
         DC    X'00000A',AL1(QEDITOR),A(0)                                      
         DC    X'00000A',AL1(QMOBILE),A(0)                                      
         DC    X'00000A',AL1(QACCGEN),A(0)                                      
         DC    X'00000A',AL1(QPROGEN),A(0)                                      
         DC    X'00000A',AL1(QGETOPT),A(0)                                      
         DC    X'00000A',AL1(QJOBBER),A(0)                                      
         DC    X'00000A',AL1(QTASYSIO),A(0)                                     
         DC    X'00000A',AL1(QTASYSVL),A(0)                                     
         DC    X'00000A',AL1(QTASYSTB),A(0)                                     
         DC    X'00000A',AL1(QTAREPGN),A(0)                                     
         DC    X'00000A',AL1(QDROOL),A(0)                                       
         DC    X'00000A',AL1(QTASYSCA),A(0)                                     
         DC    X'00000A',AL1(QTACONCU),A(0)                                     
         DC    X'00000A',AL1(QDDISP),A(0)                                       
         DC    X'00000A',AL1(QDEMOVAL),A(0)                                     
         DC    X'00000A',AL1(QDEMOMTH),A(0)                                     
         DC    X'00000A',AL1(QDEMEL),A(0)                                       
         DC    X'00000A',AL1(QDEMAINT),A(0)                                     
         DC    X'00000A',AL1(QDEMAND),A(0)                                      
         DC    X'00000A',AL1(QDEMAND1),A(0)                                     
         DC    X'80000A',AL1(QDEMADDR),A(0)                                     
         DC    X'00000A',AL1(QDEMOUT),A(0)                                      
         DC    X'00000A',AL1(QDEMOCON),A(0)                                     
         DC    X'00000A',AL1(QGENERAL),A(0)                                     
         DC    X'00000A',AL1(QREPORT),A(0)                                      
         DC    X'00000A',AL1(QGETIDS),A(0)                                      
         DC    X'00000A',AL1(QPERVAL),A(0)                                      
         DC    X'00000A',AL1(QGENIDS),A(0)                                      
         DC    X'00000A',AL1(QGETRATE),A(0)                                     
*&&                                                                             
* DDMONSPARM MUST FOLLOW THE LAST CORERES TABLE ENTRY                           
       ++INCLUDE DDMONSPARM                                                     
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    5000D               WORKING STORAGE                              
*                                                                               
         DS    0D                                                               
         DC    C'***IO***'                                                      
IO       DC    2000X'00'           CTFILE I/O AREA (FOR ID RECORDS)             
         EJECT                                                                  
         DS    0D                                                               
         DC    C'CITABLE*'                                                      
CITABLE  DC    (MAXPQS*CITBLLNQ)X'FF'                                           
         DC    X'FF'               END OF TABLE MARKER                          
MAXPQS   EQU   16                  MAXIMUM OF 16 PRINT QUEUES                   
         SPACE 2                                                                
CIDATAD  DSECT                                                                  
CIAPART  DS    F                   A(THIS PARTITION IN REQUEST TABLE)           
CIMAXRPT DS    H                   MAX NUM REPORTS = CICITOT - CICINDX          
CFPQENUM DS    X                   PRINT QUEUE EXTERNAL FILE NUMBER             
         DS    X                   SPARE                                        
       ++INCLUDE DMPRTQW                                                        
CITBLLNQ EQU   *-CIDATAD                                                        
         EJECT                                                                  
MONSOON  CSECT                                                                  
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'CLASSTAB'                                                      
CLASSTAB DC    (MAXCLASS)XL(CLSTABLQ)'00'    LIST OF CLASSES TO ATTACH          
MAXCLASS EQU   256                 MAXIMUM NUMBER OF CLASSES                    
         SPACE 3                                                                
CLASSD   DSECT                                                                  
CLSNAME  DS    CL2                 CLASS NAME (E.G. A3, S4, ETC.)               
*                                   *** THIS FIELD MUST BE FIRST ***            
CLSMAX   DS    H                   MAX. NO. OF REQS IN THIS GO-ROUND            
CLSPRTY  DS    H                   PRIORITY OF THIS CLASS (1..N)                
CLSTOTAL DS    H                   TOTAL REQS ATTACHED FOR THIS CLASS           
CLSTIME  DS    F                   TOTAL SUBMIT TIME FOR CLASS (MINS)           
CLSREQW  DS    H                   NO. OF REQS AWAITING EXECUTION               
CLSREQI  DS    H                   NO. OF REQS IN THE REQUEST TABLE             
CLSTABLQ EQU   *-CLASSD                                                         
         EJECT                                                                  
REQTABLD DSECT                                                                  
REQTIME  DS    H                   REPORT CREATION TIME                         
REQREPUI DS    H                   REPORT USER-ID                               
REQCHAIN DS    F                   INDEX OF NEXT ENTRY IN CHAIN                 
REQCLASS DS    CL2                 SUBMIT CLASS                                 
REQPRTY  DS    C                   REQUEST PRIORITY (C'0'..C'9')                
REQSYPGM DS    0CL3                                                             
REQSYST  DS    C                   SYSTEM                                       
REQPROG  DS    CL2                 PROGRAM                                      
REQFACPK DS    C                   FACPAK ID                                    
REQFLAGS DS    X                   VARIOUS FLAGS                                
REQYESTD EQU   X'80'               REPORT WAS CREATED YESTERDAY                 
REQLOOKD EQU   X'40'               REPORT WAS EXAMINED AND NOT ATTACHED         
REQFILED EQU   X'20'               REPORT CAME FROM JCLFILE, NOT PRTQ           
REQTABLQ EQU   *-REQTABLD                                                       
         SPACE 5                                                                
USERTABD DSECT                                                                  
USERNAME DS    CL8                 EBCDIC USERID                                
USERREQI DS    H                   NUMBER OF REQS IN REQTABLE CURRENTLY         
USERTBLQ EQU   *-USERTABD                                                       
         EJECT                                                                  
FKEYD    DSECT                                                                  
         DC    C'**'                                                            
FAGENCY  DS    CL(L'AGENCY)        AGENCY CODE                                  
FJOBNAM  DS    0CL(L'JOBNAME)      USERID, SYSTEM, PROGRAM                      
FJOBUSER DS    CL(L'JOBUSER)       USERID                                       
FJBSYPGM DS    CL(L'JOBSYPGM)      SYS/PROG                                     
FATTPGM  DS    CL(L'ATTCHPGM)      ATTACH TASK NAME                             
FDATE    DS    XL8                 TODAY'S DATE, MMMDD/YY                       
         DC    C'|'                                                             
FTIME    DS    0CL8                TIME HH:MM:SS                                
FHOUR    DS    CL2                                                              
         DC    C':'                                                             
FMIN     DS    CL2                                                              
         DC    C':'                                                             
FSEC     DS    CL2                                                              
FKEYLQ   EQU   *-FKEYD                                                          
         EJECT                                                                  
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQS                                                        
         EJECT                                                                  
* ++INCLUDE DDDPRINT                                                            
* ++INCLUDE DDGETRETD                                                           
* ++INCLUDE DDCOREQUS                                                           
* ++INCLUDE FATABSJOB                                                           
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDGETRETD                                                      
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE FATABSJOB                                                      
         EJECT                                                                  
         PRINT ON                                                               
         SPACE 2                                                                
         IHAASCB                                                                
         SPACE 2                                                                
         DCBD  DSORG=PS,DEVD=DA                                                 
         SPACE 2                                                                
         DSECT                                                                  
         IEZCIB                                                                 
         SPACE 2                                                                
TIOT     DSECT                                                                  
         IEFTIOT1                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029DDMONSOONA05/01/02'                                      
         END                                                                    
