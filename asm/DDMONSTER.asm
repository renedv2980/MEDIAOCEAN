*          DATA SET DDMONSTER  AT LEVEL 084 AS OF 09/21/18                      
*&&      SET   CL=Y,AG=N                                                        
*PHASE MONSTERC                                                                 
*INCLUDE BINSR31                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*&&UK                                                                           
*INCLUDE LOADER                                                                 
*&&                                                                             
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE QSORT                                                                  
         TITLE 'MONSTER - SOON REQUEST SCHEDULER'                               
***********************************************************************         
* THIS PROGRAM SCHEDULE THE SOON JOBS USING ATTACH MACRO TO START               
* MONSTEM1. THIS USES MULTI-TRANS ASCH. THE TP (TRANSCTION PROFILES)            
* CONTROL HOW THEY ARE RUNNING AND DICATE WHAT JCL THEY RUN.                    
***********************************************************************         
MONSTER  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*MONSTER,AR13CHN                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                                                              
*                                                                               
         L     RC,ACOMMON        COMMON STORAGE AREA                            
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
         USING COMMWORK,RC,R9                                                   
         USING DMSPACED,DSPHD      DATASPACE SOON TABLE HEADER                  
C        USING DMSPACED,CSPHD      DATASPACE CLASS TABLE HEADER                 
         USING BSPARA,BSP                                                       
*                                                                               
         L     RA,VCPRINT                                                       
         USING DPRINT,RA                                                        
         USING APPCD,R7                                                         
*                                                                               
         LHI   R0,4                                                             
         LNR   R0,R0               R0 = -4                                      
         SVC   247                 SO MONSTER IS NOT SWAPPABLE                  
*                                                                               
         BRAS  RE,INIT             INITIALIZE                                   
         CLI   ESHTDOWN,YES        SHUTDOWN FLAG IS ON?                         
         BE    GOODBYE                                                          
         EJECT                                                                  
***********************************************************************         
* USE RUN TABLE IN TABS DATASPACE TO BUILD A SCHEDULED JOBS LIST      *         
* ATTACH ANY ELIGIBLE JOBS, WAIT (IF NECESSARY), RINSE AND REPEAT...  *         
* THIS PROGRAM RUNS FOREVER UNTIL STOPPED BY THE OPERATOR.            *         
***********************************************************************         
MAIN     DS    0H                                                               
         CLI   LDACTION,LDREFQ     TEST DATASPACE REFRESH REQUIRED              
         BNE   MAIN02                                                           
         LOAD  EPLOC=LOADCARD,ERRET=RCERR01A                                    
         GOTO1 (R0),LOADPARM                                                    
         DELETE EPLOC=LOADCARD                                                  
         WTO   TEXT=MNMS1H                                                      
*                                                                               
MAIN02   CLI   ATTCHFLG,C'X'       LET OPERATOR KNOW WAIT COMMAND SET           
         BNE   MAIN04                                                           
         MVI   ATTCHFLG,NO         SET NOT TO ATTACH                            
         WTO   TEXT=MNMS2H                                                      
*                                                                               
MAIN04   BRAS  RE,SCHEDULE         UPDATE INTERNAL LIST OF JOBS TO RUN          
*                                                                               
         CLI   ATTCHFLG,YES        ARE WE ATTACHING JCL?                        
         BNE   MAIN08              NO                                           
         BRAS  RE,SETWAIT                                                       
*                                                                               
         OC    NUMINQ,NUMINQ       ANYTHING TO ATTACH?                          
         BZ    MAIN08              NO - WAIT FOR TIMER OR COMPLETION            
*                                                                               
MAIN06   BRAS  RE,ATTACH           START AS MANY OF THE JOBS AS YOU CAN         
*                                                                               
MAIN08   MVC   P(14),=CL14'ABOUT TO WAIT'                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R3,AECBLIST                                                      
         WAIT  1,ECBLIST=(3)       WAIT FOR TIMER/OPERATOR/APPC_CALLS           
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BO    MAIN10              YES                                          
*                                                                               
         CLI   OPERSTOP,YES        OPERATOR WANTS TO STOP?                      
         BE    CHKAPPC             YES - DON'T BOTHER CHECKING TIMER            
         CLI   ESHTDOWN,YES        SHUTDOWN FLAG IS ON?                         
         BE    CHKAPPC             YES - DON'T BOTHER CHECKING TIMER            
*                                                                               
         TM    TIMERECB,X'40'      DID THE TIMER POP?                           
         BZ    CHKAPPC             NO                                           
         XC    TIMERECB,TIMERECB                                                
         B     MAIN                RUN SCHEDULING AGAIN                         
*                                                                               
MAIN10   BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
*                                                                               
         CLI   FREECONV,NO                                                      
         BNE   MAIN12                                                           
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   MAIN08                                                           
         MVC   P(15),=CL15'NO CONVERSATIONS'                                    
         BRAS  RE,PRNT                                                          
         B     MAIN08                                                           
*                                                                               
MAIN12   CLI   ATTCHFLG,C'R'       RESUME ATTACHING REQUESTS                    
         BNE   *+12                                                             
         MVI   ATTCHFLG,YES        SET TO ATTACH                                
         B     MAIN                                                             
*                                                                               
         CLI   OPERSTOP,YES        OPERATOR WANTS TO STOP?                      
         BNE   MAIN08              NO                                           
*                                                                               
         STIMERM CANCEL,ID=STIMERID   CANCEL THE TIMER                          
         XC    TIMERECB,TIMERECB                                                
*                                                                               
         MVC   P(14),=CL14'OPERATOR STOP'                                       
         BRAS  RE,PRNT                                                          
*                                                                               
WINDDOWN L     R7,AAPPCTAB         APPC/MVS CONTROL TABLE                       
*                                                                               
ALLFREE  OC    APPCANXT,APPCANXT                                                
         BNZ   MAIN08              CONVERSATION NOT YET COMPLETED               
         AHI   R7,APPCDLEN         BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R7)       END OF TABLE?                                
         BNE   ALLFREE                                                          
*                                                                               
GOODBYE  BRAS  RE,SHUTDOWN                                                      
         MVC   P(16),=CL16'MONSTER EXITING'                                     
         BRAS  RE,PRNT                                                          
         XBASE                                                                  
         EJECT ,                                                                
***********************************************************************         
* ERROR FOR LOADCARD=PHASE                                                      
***********************************************************************         
RCERR01A MVC   P(17),=CL17'CANNOT LOAD PHASE'                                   
         BRAS  RE,PRNT                                                          
         MVI   ESHTDOWN,YES        CANCEL THE TIMER                             
         STIMERM CANCEL,ID=STIMERID                                             
         XC    TIMERECB,TIMERECB                                                
         B     WINDDOWN            WRAP UP MONSTER                              
         EJECT                                                                  
***********************************************************************         
* CHECK COMPLETIONS ON INTERRUPT                                      *         
***********************************************************************         
CHKAPPC  L     R3,FIRSTECB                                                      
*                                                                               
CHKAPPC1 L     RF,0(R3)            RF NOW CONTAINS ECB                          
         TM    0(RF),X'40'         WAS THIS CONVERSATION POSTED?                
         BO    CHKAPPC3            NO                                           
*                                                                               
CHKAPPC2 TM    0(R3),X'80'         WAS I THE LAST ECB ON THE ECBLIST?           
         BO    CHKSD               NO                                           
         LA    R3,4(R3)            GO ON TO NEXT ECB                            
         B     CHKAPPC1                                                         
*                                                                               
CHKAPPC3 ST    R3,CHKDECB           SAVE THE ECB JUST CHECKED                   
         ST    RF,CHKDAPPC          SAVE THE APPC ENTRY TO RUN                  
         LR    R7,RF                APPC CONTROL TABLE ENTRY ADDRESS            
         MVC   RETURN_CODE,APPC_ECB RETURN CODE IS IN ECB                       
         MVI   RETURN_CODE,0        CLEAR HOB                                   
         XC    APPC_ECB,APPC_ECB    CLEAR ECB                                   
*                                                                               
         L     RF,APPCANXT                                                      
         BASR  RE,RF               PERFORM NEXT APPC/MVS FUNCTION               
         B     CHKAPPC2                                                         
*                                                                               
CHKSD    CLI   OPERSTOP,YES        OPERATOR WANTS TO STOP?                      
         BE    WINDDOWN                                                         
         CLI   ESHTDOWN,YES        SHUTDOWN FLAG IS ON?                         
         BE    WINDDOWN                                                         
*                                                                               
         CLI   JOBSLFTP,YES        ANY JOBS LEFT TO RUN?                        
         BNE   MAIN08              NO                                           
         MVI   JOBSLFTP,NO                                                      
         B     MAIN06              YES - RUN THEM                               
*                                                                               
AR13CHN  DC    A(R13CHAIN)                                                      
ACOMMON  DC    A(COMMWORK)                                                      
*                                                                               
MNMS1H   DC    AL2(L'MNMS1)                                                     
MNMS1    DC    C'DATASPACE REFRESHED'                                           
MNMS2H   DC    AL2(L'MNMS2)                                                     
MNMS2    DC    C'MONSTER NOW IN WAIT STATE - NOT ATTACHING'                     
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT SOME TOTALS AND WRAP UP.                                     *          
**********************************************************************          
SHUTDOWN NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VENQDEQ,DMCB,(C'D',=C'ALL')                                      
         CLC   MINORNAM,SPACES                                                  
         BE    SH02                                                             
         DEQ   (MAJORNAM,MINORNAM,8,SYSTEMS)                                    
                                                                                
SH02     CLI   LOADCARD,0                                                       
         BE    SH06                                                             
         LOAD  EPLOC=LOADCARD,ERRET=SH04                                        
         MVI   LDACTION,LDDELQ     REMOVE DATASPACE                             
         GOTO1 (R0),LOADPARM                                                    
         DELETE EPLOC=LOADCARD                                                  
         B     SH06                                                             
*                                                                               
SH04     MVC   P(17),=CL17'CANNOT DEL PHASE '                                   
         BRAS  RE,PRNT             CONTINUE THE SHUTDOWN PROCESS                
*                                                                               
SH06     OC    BSPNOR,BSPNOR       ANY CLASSES IN CLASS TABLE?                  
         BZ    SH10                                                             
*                                                                               
         L     R3,BSPNOR           YES -- PRINT TOTALS                          
         L     R4,ACLASSTB         A(CLASS TABLE)                               
         USING CLASSD,R4                                                        
SH08     MVC   P(39),=C'REQUESTS ATTACHED FOR CLASS XX = NNNNNN'                
         MVC   P+28(L'CLSNAME),CLSNAME                                          
         EDIT  CLSTOTAL,(6,P+33),ZERO=NOBLANK                                   
         GOTO1 VPRINTER                                                         
         AHI   R4,CLSTABLQ         BUMP TO NEXT CLASS                           
         BCT   R3,SH08                                                          
         DROP  R4                                                               
*                                                                               
SH10     MVC   P+00(39),=C'TOTAL REQUESTS ATTACHED =        NNNNNN'             
         EDIT  TOTREQS,(6,P+33),ZERO=NOBLANK                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+30(43),=C'TOTAL LOADS AVOIDED: XXXXXXX OUT OF XXXXXXX'         
         EDIT  LOADAVD,(7,P+51),ZERO=NOBLANK,ALIGN=LEFT                         
         EDIT  LOADASK,(7,P+66),ZERO=NOBLANK,ALIGN=LEFT                         
         MVC   P(10),=CL10'TERMINATE'                                           
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R7,AAPPCTAB                                                      
         LA    R2,ATBAMR1_STRUCTURE                                             
         BRAS  RE,CALLAPPC         CLEANUP TP                                   
*                                                                               
         CLOSE (FILE)              CLOSE LONG SOON JOBS RECORD FILE             
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* NEW SCHEDULER                                                       *         
* USES CLASS TABLE IN TABS DATASPACE TO DETERMINE WHICH OF THE JOBS  *          
* IS THE BEST ONE TO RUN                                              *         
*                                                                     *         
* THE CLASS ARRAY ENTRIES ARE THEN FILTERED BASED ON THE CLASS        *         
* 1. CLASS INCLUSIONS/EXCLUSIONS SET IN THE INPUT CARDS               *         
* 2. AGENCY INCLUSIONS/EXCLUSIONS SET IN THE INPUT CARDS              *         
* 3. THE MAXIMUM # OF SIMULTANEOUS TRANSACTIONS WE WILL DISPLATCH     *         
*    FOR AN AGENCY                                                    *         
***********************************************************************         
SCHEDULE NTR1  BASE=*,LABEL=*                                                   
         MVC   P(17),=CL17'STARTING SCHEDULER'                                  
         BRAS  RE,PRNT                                                          
*                                                                               
         TIME  BIN                                                              
         ST    R0,PQSRCHTM                                                      
*                                                                               
         SAM31 ,                   ROUTINE ALL IN XA                            
*                                                                               
         LHI   R1,MAXJOBQ          MAX # JOBS                                   
         MHI   R1,SRTRECL          X JOB REC LEN                                
         L     R0,ASRTBLK                                                       
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,LOCKCLS          LOCK CLASS TABLE                             
*                                                                               
         L     R8,ASRTBLK          R8 = LIST OF QUALIFYING CLASS SLOTS          
         USING SRTD,R8                                                          
         MVC   SRTD(SRTRECL),EFFS                                               
         AHI   R8,SRTRECL          SET 1ST NTRY FFS - SORTS HI FOR EOT          
*                                                                               
         ICM   R2,15,C.DSPTFRST                                                 
*                                                                               
SCH02    BRAS  RE,ARSOFF           MAKE SURE START LOOP CLEAN                   
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING JCLASSD,R2          R2 = A(CLASS TABLE)                          
         OC    JCLASSD(JCKEYL),JCLASSD                                          
         BZ    SCH34                                                            
         OC    JCFSUB,JCFSUB       ANY SUBMITTED ENTRIES?                       
         BZ    SCH32               NO                                           
         TM    JCFLAG,JCFLHOLD     HOLD THESE TRANSACTIONS?                     
         BO    SCH32               YES                                          
*                                                                               
         CLI   QTYPE,C' '      *** QUEUE TYPE FILTER?                           
         BNH   SCH03                                                            
         CLC   JCTYPE,QTYPE        INCLUDE QUEUE TYPE                           
         BE    SCH04               YES                                          
         B     SCH32               NO SKIP                                      
*                                                                               
SCH03    CLI   XQTYPE,C' '         EXCLUDE QUEUE TYPE                           
         BNH   SCH04                                                            
         CLC   JCTYPE,XQTYPE       MATCH QUEUE TYPE                             
         BE    SCH32               YES                                          
*                                                                               
SCH04    MVC   SVJCTYPE,JCTYPE                                                  
                                                                                
         LA    RF,CLASS        *** CLASS FILTER?                                
         USING CLASSD,RF                                                        
         XC    CLASSD(CLSTABLQ),CLASSD                                          
*&&CL*&& MVC   HALF,JCCLASS                                                     
*&&AG*&& MVC   HALF,JCAGY                                                       
         MVC   CLSNAME,HALF                                                     
         BRAS  RE,ARSOFF                                                        
*                                                                               
         CLI   ALLCLASS,YES        CLASS/AGYID=ALL?                             
         BNE   SCH05               NO                                           
*                                                                               
         MVI   BSP4,X'00'                                                       
         GOTO1 VBINSRCH,BSP,CLASS                                               
         TM    BSPNF,X'80'         CLASS FOUND IN TABLE?                        
         BZ    SCH06               YES - USE WHAT'S THERE                       
*                                                                               
         LA    RF,CLASS                                                         
         XC    CLASSD(CLSTABLQ),CLASSD                                          
         MVC   CLSNAME,HALF                                                     
         MVC   CLSMAX,EFFS         SET CLSMAX AS HIGH VALUE AND ADD             
         DROP  RF                                                               
*                                                                               
         MVI   BSP4,X'01'          SET TO ADD IF NOT THERE                      
         GOTO1 VBINSRCH,BSP,CLASS                                               
         OC    BSPAREC,BSPAREC                                                  
         BNZ   SCH06                                                            
         DC    H'0'                CLASS TABLE IS NOT BIG ENOUGH                
*                                                                               
SCH05    MVI   BSP4,X'00'                                                       
         GOTO1 VBINSRCH,BSP,CLASS                                               
         TM    BSPNF,X'80'         CLASS FOUND IN TABLE?                        
         BO    SCH32               NO - DO NOT RUN THIS REQUEST                 
*                                                                               
SCH06    BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET       BACK TO DATASPACE TABLE                      
         SAC   512                                                              
*                                                                               
         ICM   R4,15,JCFSUB        POINT R4 TO FIRST SUBMITTED JOB              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CPYA  AR4,AR2                                                          
         USING TBJOBTAB,R4                                                      
*                              *** AGENCY INCLUDE FILTERS                       
SCH08    OC    AGYFLTS(2),AGYFLTS                                               
         BZ    SCH08B              NO AGENCY INCLUDE FILTERS                    
*                                                                               
         LA    RF,AGYFLTS          ARRAY OF INCLUDED AGENCIES                   
SCH08A   OC    0(2,RF),0(RF)                                                    
         BZ    SCH30                                                            
         CLC   TBJAGY,0(RF)        IS THIS AGENCY IN THE ARRAY?                 
         BE    *+12                YES - PROCESS IT                             
         AHI   RF,2                                                             
         B     SCH08A                                                           
*                              *** AGENCY EXCLUDE FILTERS                       
SCH08B   OC    EXAGYS(2),EXAGYS                                                 
         BZ    SCH09               NO AGENCY EXCLUDE FILTERS                    
*                                                                               
         LA    RF,EXAGYS           ARRAY OF EXCLUDED AGENCIES                   
SCH08C   OC    0(2,RF),0(RF)                                                    
         BZ    SCH09                                                            
         CLC   TBJAGY,0(RF)        IS THIS AGENCY IN THE ARRAY?                 
         BE    SCH30               YES - SKIP IT                                
         AHI   RF,2                                                             
         B     SCH08C                                                           
*                              *** USERID INCLUDE FILTERS                       
SCH09    OC    USERFLTS(2),USERFLTS                                             
         BZ    SCH12               NO USERID INCLUDE FILTERS                    
*                                                                               
         LA    RF,USERFLTS         ARRAY OF INCLUDED USER IDS                   
SCH10    OC    0(2,RF),0(RF)                                                    
         BZ    SCH30                                                            
         CLC   TBJPQUSR,0(RF)      IS THIS USERID IN THE ARRAY?                 
         BE    SCH12               YES - PROCESS IT                             
         AHI   RF,2                                                             
         B     SCH10                                                            
*                              *** USERID EXCLUDE FILTERS                       
SCH12    OC    EXUSRIDS(2),EXUSRIDS                                             
         BZ    SCH16               NO USERID EXCLUDE FILTERS                    
*                                                                               
         LA    RF,EXUSRIDS         ARRAY OF EXCLUDED USERIDS                    
SCH14    OC    0(2,RF),0(RF)                                                    
         BZ    SCH16                                                            
         CLC   TBJPQUSR,0(RF)      IS THIS USERID IN THE ARRAY?                 
         BE    SCH30               YES - SKIP IT                                
         AHI   RF,2                                                             
         B     SCH14                                                            
*                                                                               
SCH16    CLI   TYPUPD,YES          ATTACH ONLY UPDATIVE SOONS?                  
         BNE   *+12                                                             
         TM    TBJSTAT,TBJUPDT     UPDATIVE?                                    
         BZ    SCH30               NO - IGNORE                                  
*                                                                               
         CLI   TYPNOUPD,YES        ATTACH ONLY NON-UPDATIVE SOONS?              
         BNE   *+12                                                             
         TM    TBJSTAT,TBJUPDT     UPDATIVE?                                    
         BO    SCH30               YES - IGNORE                                 
*                                                                               
         TM    TBJSTAT,TBJHOLD     IGNORE HOLD JOBS                             
         BO    SCH30                                                            
         TM    TBJSTAT,TBJKILL     IGNORE KILL JOBS                             
         BO    SCH30                                                            
*                                                                               
         CLC   SUBFILT,SPACES      SPECIAL SUBID FILTER (FOR DEBUGGING)         
         BNH   *+14                NO                                           
         CLC   SUBFILT,TBJPQSUB    MATCH SUBID?                                 
         BNE   SCH30               NO - IGNORE                                  
*                                                                               
         MVC   BYTE,TBJADV         GET FACPAK TABLE ENTRY FROM NUMBER           
         NI    BYTE,X'0F'          MASK OUT FACPAK NUMBER                       
         XR    RF,RF                                                            
         IC    RF,BYTE                                                          
         MHI   RF,L'FACITAB                                                     
         A     RF,AFACID           SUPPORTS FACIDTAB AND FACIDTABL              
*                                                                               
         USING FACITABD,RF                                                      
         CLI   FACISN4,C' '        ANY FACPAK NAME?                             
         BNH   SCH30               NO - SOMETHING NOT CORRECT                   
*                                                                               
         TM    FACIFL,FACITST      TEST TYPE SYSTEM?                            
         BZ    SCH18               NO, MUST BE ADV OR REP                       
         CLI   DSPACE,C'Q'         FQA?                                         
         BNE   SCH17A                                                           
         TM    FACIFL,FACIFQA      FQA?                                         
         BO    SCH19                                                            
         B     SCH30               SKIP THIS CHECK                              
*                                                                               
SCH17A   CLI   DSPACE,C'C'         CSC?                                         
         BNE   SCH17B                                                           
         TM    FACIFL,FACICSC      CSC?                                         
         BO    SCH19                                                            
         B     SCH30               SKIP THIS CHECK                              
*                                                                               
SCH17B   DS    0H                                                               
*&&UK                                                                           
         CLI   DSPACE,C'B'         BAR?                                         
         BNE   SCH17C                                                           
         CLI   FACIID,FAC#BAR      BAR?                                         
         BE    SCH19                                                            
         B     SCH30                                                            
*&&                                                                             
SCH17C   CLI   DSPACE,C'T'                                                      
         BE    SCH19                                                            
         DC    H'00'               SOMETHING IS WRONG                           
*                                                                               
SCH18    TM    FACIFL,FACIREP      CAME FROM A REP?                             
         BZ    SCH18A              NO                                           
         CLI   DSPACE,C'R'         YES                                          
         BE    SCH19                                                            
         B     SCH30                                                            
*                                                                               
SCH18A   CLI   DSPACE,C'A'         MUST BE ADV                                  
         BNE   SCH30                                                            
*                                                                               
SCH19    CLI   FACPAK,C' '         FACPAK FILTER?                               
         BNH   SCH20               NO                                           
         CLC   FACISN1,FACPAK      MATCH FACPAK ID                              
         BNE   *+16                NO                                           
         CLI   FACPAKFL,C'P'       WANT TO RUN IF IT MATCHES?                   
         BE    SCH20               YES, KEEP THIS ONE                           
         B     SCH30                                                            
*                                                                               
         CLI   FACPAKFL,C'P'       WANT TO RUN IF IT MATCHES?                   
         BE    SCH30               NO, IGNORE THIS ONE                          
         DROP  RF                                                               
*                                                                               
SCH20    DS    0H                                                               
*&&UK                                                                           
         CLC   TBJAGY,SPACES       USERMAXR BECOMES AGY LIMIT                   
         BNH   SCH26                                                            
         CLC   JCNRUN,USERMAXR     CAN'T BE TOO MANY RUNNING                    
         BL    SCH26                                                            
         ICM   R5,15,JCFRUN                                                     
         BZ    SCH26               NOTHING RUNNING                              
*                                                                               
         LH    R0,USERMAXR         MAKE SURE NOT TOO MANY RUNNING               
         LH    R1,SUBMAXR                                                       
         CPYA  AR5,AR2                                                          
RUN      USING TBJOBTAB,R5                                                      
*                                                                               
SCH22    CLC   TBJAGY,RUN.TBJAGY   MATCH AGYID IN RUNNING LIST                  
         BNE   SCH24               NO                                           
         CLC   TBJPQSUB,RUN.TBJPQSUB                                            
         BNE   *+12                                                             
         AHI   R1,-1                                                            
         BZ    *+8                                                              
         BCT   R0,SCH24            MAKE SURE <= MAX FOR THIS AGYID              
*                                                                               
SCH23    LAM   AR5,AR5,ARZERO                                                   
         B     SCH30               TOO MANY - IGNORE THIS ONE                   
*                                                                               
SCH24    ICM   R5,15,RUN.TBJNXT    ANOTHER RUNNING?                             
         BNZ   SCH22               YES                                          
         LAM   AR5,AR5,ARZERO                                                   
         DROP  RUN                                                              
*&&                                                                             
SCH26    DS    0H                                                               
*&&US*&& B     SCH27US                                                          
*                                                                               
*&&CL*&& MVC   SRTCLS,JCCLASS      ADD THIS JOB TO SORT LIST                    
*&&AG*&& MVC   SRTAGY,JCAGY                                                     
         B     SCH27UK                                                          
*&&US                                                                           
SCH27US  MVC   SRTCLS,JCCLASS      ADD THIS JOB TO SORT LIST                    
         MVC   SRTAGY,TBJAGY                                                    
         MVC   SRTSUBID,TBJPQSUB                                                
         MVC   SRTREPT,TBJMVSID+5                                               
*&&                                                                             
SCH27UK  MVC   SRTPRI,TBJPRTY                                                   
         CLI   SRTPRI,C'0'         ENSURE PRIORITY SET OK - X'F0'-'FF'          
         BH    *+8                                                              
         MVI   SRTPRI,C'1'                                                      
         XI    SRTPRI,X'FF'        REMEMBER C'1-9' WITH 9 HIGHEST               
*                                                                               
*&&US*&& TM    TBJSTAT,TBJRUNOW                                                 
*&&US*&& BNO   *+8                                                              
*&&US*&& MVI   SRTPRI,X'00'        IGNORE MAX CHECK LATER                       
*                                                                               
         MVC   SRTTIME,TBJSTIME    SET START TIME                               
         CLI   TBJSTIME,X'FF'      DISPLAY TIME REBUILT BY RERUN                
         BNE   SCH28               NO                                           
*                                                                               
         XR    RE,RE               CONVERT BACK TO REAL TIME                    
         ICM   RF,7,TBJSTIME                                                    
         NILF  GRF,X'0000FFFF'                                                  
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
         STCM  RF,7,SRTTIME        AND SAVE IT                                  
*                                                                               
SCH28    STCM  R4,15,SRTAJOB                                                    
         STCM  R2,15,SRTACLS                                                    
         AHI   R8,SRTRECL                                                       
*                                                                               
SCH30    ICM   R4,15,TBJNXT        ANOTHER SUBMITTED FOR THIS CLASS/AGY         
         BNZ   SCH08               YES - KEEP ADDING                            
         DROP  R4                                                               
*                                                                               
SCH32    AHI   R2,JCLASSL          NEXT CLASS TABLE ENTRY                       
         B     SCH02                                                            
*                                                                               
SCH34    BRAS  RE,ARSOFF                                                        
         LR    RE,R8               GET COUNT OF ENTRIES IN JOBSTAB              
         L     RF,ASRTBLK                                                       
         SR    RE,RF                                                            
         SRDL  RE,32                                                            
         LHI   R0,SRTRECL                                                       
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R0,RF                                                            
         CHI   R0,1                SINCE WE ADDED FIRST ENTRY                   
         BE    SCH36               THIS MEANS NOTHING TO RUN RIGHT NOW          
*                                                                               
         GOTO1 VQSORT,DMCB,ASRTBLK,(R0),SRTRECL,SRTKEYL,0,0                     
         ORG   *-2                 OR IMEDIATE LOWER FULL WORD                  
         OILF  GRF,X'80000000'     TELL QSORT 31 WERE IN 31 BIT MODE            
         BASR  RE,RF                                                            
*                                                                               
SCH36    BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREECLS          NOW UNLOCK TABLE                             
         BRAS  RE,ARSOFF                                                        
*                                                                               
         BCTR  R0,0                                                             
         STH   R0,NUMINQ                                                        
*&&US                                                                           
***********************************************************************         
* MARK ONLY N REPORT TYPE/AGENCY TO RUN IF APPLIED.                             
***********************************************************************         
         OC    REPTFLTS,REPTFLTS                                                
         BZ    SCH100              NO REPORT TYPE FILTERS                       
*                      FIRST COUNT THE REPEATED AGY/REPTYPE                     
         SAM31                                                                  
         L     R8,ASRTBLK          GET A(CURRENT SUBMIT LIST)                   
         USING SRTD,R8                                                          
THAT     USING SRTD,R9                                                          
SCH80    CLC   SRTD(SRTKEYL),EFFS  END OF SUBMITTED JOBS LIST                   
         BE    SCH89                                                            
         CLI   SRTREPTN,X'00'      ALREADY COUNTED                              
         BNE   SCH87                                                            
*                                                                               
         MVI   SRTREPTN,X'01'      COUNT THIS 1ST REPORT TYPE/AGY               
         LHI   RF,1                CURRENT COUNTER                              
         LA    R9,SRTRECL(,R8)     PT TO NEXT ENTRY                             
SCH82    CLC   THAT.SRTD(SRTKEYL),EFFS   THEN, CHECK THE REST LIST              
         BE    SCH87                                                            
         CLC   SRTAGY,THAT.SRTAGY                                               
         BNE   SCH85                                                            
         CLC   SRTREPT,THAT.SRTREPT                                             
         BNE   SCH85                                                            
         AHI   RF,1                                                             
         STC   RF,THAT.SRTREPTN    SAME AGY/REP TYPE, +1                        
SCH85    AHI   R9,SRTRECL                                                       
         B     SCH82                                                            
         DROP  THAT                                                             
*                                                                               
SCH87    AHI   R8,SRTRECL          NEXT IN LIST OF SCHEDULED JOBS               
         B     SCH80                                                            
                                                                                
**********************************************************************          
* CHECK IF EXCEED REPTYPE/AGY LIMIT                                             
**********************************************************************          
SCH89    DS    0H                                                               
         L     R8,ASRTBLK          GET A(CURRENT SUBMIT LIST)                   
         USING SRTD,R8                                                          
SCH90    CLC   SRTD(SRTKEYL),EFFS  END OF SUBMITTED JOBS LIST                   
         BE    SCH100                                                           
*                                                                               
         LA    RF,REPTFLTS         ARRAY OF REPORT TYPE                         
SCH92    OC    0(L'REPTFLTS,RF),0(RF)                                           
         BZ    SCH97               END OF ARRAY                                 
         CLC   SRTREPT,0(RF)       MATCH REPORT TYPE                            
         BE    SCH94                                                            
         AHI   RF,L'REPTFLTS                                                    
         B     SCH92                                                            
SCH94    CLC   SRTREPTN,2(RF)      EXCEED REPORT TYPE LIMIT                     
         BNH   SCH97               NO - NEXT JOB ENTRY                          
         MVI   SRTREPTN,X'FF'      YES - MARK TO BE SKIPPED                     
*                                                                               
SCH97    AHI   R8,SRTRECL          NEXT IN LIST OF SCHEDULED JOBS               
         B     SCH90                                                            
*&&                                                                             
**********************************************************************          
* TRACE OUT SORT TABLE                                                          
**********************************************************************          
SCH100   DS    0H                                                               
         CLI   TRACEFLG,YES        EXIT IF NOT TRACING                          
         BNE   EXITOK                                                           
         SAM24                                                                  
*&&DO                                                                           
         L     R8,ASRTBLK          TRACE CODE TO PRINT TABLE ENTRY              
SCHX11   DS    0H                                                               
         SAM31                                                                  
         CLC   0(SRTKEYL,R8),EFFS                                               
         BE    SCHX19                                                           
         MVC   P(SRTRECL),0(R8)                                                 
         AHI   R8,SRTRECL                                                       
         SAM24                                                                  
         GOTO1 VPRINTER                                                         
         B     SCHX11                                                           
                                                                                
SCHX19   DS    0H                                                               
         SAM24                                                                  
         GOTO1 VPRINTER                                                         
         MVC   P(100),REPTFLTS                                                  
         GOTO1 VPRINTER                                                         
*&&                                                                             
         LH    R0,NUMINQ           SHOW NUMBER OF JOBS WAITING                  
         BRAS  RE,CVDR0                                                         
         UNPK  P(4),DUB                                                         
         MVC   P+4(13),=CL13' JOBS IN RUNQ'                                     
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
         DROP  R2,R8                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* READ PARAMETER CARDS                                                *         
*                                                                     *         
* *......            "*" IN COLUMN ONE IS A COMMENT CARD              *         
* CLASS=ALL,#MAX_CONV,TPNAME     ATTACH REQ OF ANY OTHER CLASSES      *         
*  EX:   CLASS=ALL,7,TP_FOR_OTHERS                                    *         
* CLASS=CLASS,#MAX_REQ,#MAX_CONV,TPNAME    ATTACH REQ FOR THIS CLASS  *         
*  EX:   CLASS=S1,4,2,TP_FOR_S1              (1 CARD FOR EACH CLASS)  *         
*        CLASS=C1,9,3,TP_FOR_C1              (CAN HAVE MORE THAN 1)   *         
* WAITSECS=N       WAIT N SECONDS BETWEEN PQ SEARCHES (DEFAULT = 120) *         
* REQTIMER=N       WARN OPER OF LONG REQUEST EVERY N MINS (DEFAULT=5) *         
* REQTIMER=NONE    DO NOT GENERATE OPERATOR WARNINGS ON LONG REQUESTS *         
* USERMAXREQ=N     MAX REQS PER USERID (DEFAULT=5)                    *         
* SUBMAXREQ=N      MAX REQS PER SUBID (DEFAULT=5)                     *         
* JESMSG=YES       GIVE CONSOLE MSGS AT REQ START/END (DEFAULT NO)    *         
* TYPE=UPDATE      ATTACH ONLY UPDATIVE SOONS                         *         
* TYPE=NOUPDATE    ATTACH ONLY NON-UPDATIVE SOONS                     *         
* MINTIME=N        ATTACH ONLY SOONS OF AGE N MINUTES OR OLDER        *         
* CLASSPRTY=YES    CLASS LIST IS PRIORITIZED (DEFAULT NO)             *         
* REFRESH=N        REFRESH REQUEST LIST AFTER EVERY N ATTACHES        *         
* REFRESH=PRTY<N   REFRESH REQUEST LIST UPON FINDING PRTY<N           *         
* USERID=CCCCCC    ATTACH REQUESTS FOR THIS USERID ONLY               *         
* SUBID=CCC        ATTACH REQUESTS FOR THIS SUB-ID ONLY               *         
* REFNUM=N         ATTACH REQUESTS FOR THIS REFERENCE NUMBER ONLY     *         
* DATE=YESTERDAY   ATTACH REQUESTS FROM PREVIOUS BUSINESS DAY ONLY    *         
* MINPRTY=N        ATTACH REQUESTS WITH PRIORITY >= N (DEFAULT = 0)   *         
* MAXPRTY=N        ATTACH REQUESTS WITH PRIORITY <= N (DEFAULT = 9)   *         
* FACPAK=X         ATTACH REQUESTS FROM FACPAKX ONLY (SEE FASPOON)    *         
* FACPAK=-X        EXCLUDE REQUESTS FROM FACPAKX (SEE FASPOON)        *         
* EXUSERID=CCCCCC  EXCLUDE REQUESTS FOR THIS USERID                   *         
*                  (CAN HAVE > 1 OF THESE CARDS)                      *         
* REPORTTYPEMAX=CCN RUN UPTO N OF THIS REPORT TYPE PER EACH AGENCY    *         
*                   (CAN HAVE > 1 OF THESE CARDS)                     *         
* EXREPORT=SPP     EXCLUDE REQUESTS FOR THIS SYSTEM/PROGRAM           *         
*                  (E.G.: SI2 (DON'T RUN SPOT I2 REQUESTS))           *         
* MONSOONID=CCCCCCCC  ENQ THIS NAME GLOBALLY                          *         
* ENQJOBNAME=YES   ONLY ALLOW ONE "JOBNAME" TO RUN AT ONCE (VIA ENQ)  *         
* DSPACE=X         PASS A DSPACE CONTROL CARD TO MASTER               *         
* PARTNERLUID=CCCCCCCC  LUID OF MONSOON TRANSACTION SCHEDULER         *         
* SPARECONVS=N     NUMBER OF SPARE CONVERSATIONS (DEFAULT = 0)        *         
*                                                                     *         
* DUMPCRT=N        NUMBER OF MINUTES BEFORE CLEAR THE DUMP COUNTER    *         
* DUMPLIMIT=N      DUMP COUNTER LIMIT BEFORE WTOR WARNING MESSAGE     *         
*                  (DEFAULT = 10 DUMPS IN 10 MIN)                     *         
*                                                                     *         
* UNLIMITEDNIGHT=HHMM   RELAX RESTRICTIONS AFTER HHMM (DDS TIME)      *         
*                       UNTIL THE 6 AM NEXT MORNING.                  *         
*                       (DEFAULT = X'FFFF', NO RELAZATION)            *         
* UNLIMITEDWEEKEND=YES  RELAX RESTRICTIONS FOR WEEKEND/HOLIDAY        *         
*                       (DEFAULT = NO)                                *         
*                                                                     *         
* POSTTIME=YES     POST THE EARLIEST JOB'S SUBMISSION TIME IN JOBTAB  *         
*                  IN THE DATASPACE                                   *         
*                  (ONLY ONE MONSTER SHOULD DO THIS!!!!)              *         
*                                                                     *         
* THE FOLLOWING PARAMETERS ARE REALLY FOR TESTING/DEBUGGING ONLY      *         
*                                                                     *         
* ATTACH=NO        DON'T ATTACH REQUESTS; JUST FIND THEM (DEF. = YES) *         
* DDSIO=DDSIOX     ALTERNATE DDSIO (FOR MONSTER AND ATTACHED TASKS)   *         
* DUPDUMPWARN=NO   DON'T WARN CONSOLE ABOUT DUPLICATE DUMPS           *         
* DATE=ANY         ATTACH REQUESTS FROM ANY DATE                      *         
* TCBTIME=N        JOB TCB TIME > N SEC, RECORD JOB INFO IN DS        *         
* ETTIME=N         JOB ET  TIME > N SEC, RECORD JOB INFO IN DS        *         
***********************************************************************         
READCRDS NTR1  BASE=*,LABEL=*                                                   
         OPEN  (MONSYSIN)          OPEN PARAMETER CARD FILE                     
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
RC02     GET   MONSYSIN,CARD       READ A MONSTER PARAMETER CARD                
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 VPRINTER                                                         
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC02                YES                                          
*                                                                               
RC03     CLI   DSPACE,C' '                                                      
         BH    RC05                THIS WAS SET ALREADY                         
         CLC   =C'DSPACE=',CARD                                                 
         JNE   *+2                 FIRST CARD MUST BE DSPACE=                   
         MVC   DSPACE,CARD+7                                                    
         L     RF,ASSB                                                          
         MVC   SSODSPAC-SSOOFF(1,RF),DSPACE                                     
*                                                                               
         USING FACIDD,R1                                                        
         LAY   R1,FACIDTAB                                                      
         CLC   =C'????',0(R1)      OLD TABLE?                                   
         BNE   RC03D                                                            
         ST    R1,AFACID                                                        
         B     RC02                                                             
*                                                                               
RC03D    CLI   FACIDSPC,X'FF'      END OF TABLE                                 
         JE    *+2                 DEATH                                        
         CLC   FACIDSPC,DSPACE                                                  
         JE    RC03E               FOUND THEM                                   
         AHI   R1,FACIDLNQ                                                      
         J     RC03D                                                            
*                                                                               
RC03E    MVC   AFACID,FACAID                                                    
         DROP  R1                                                               
         B     RC02                                                             
*                                                                               
RC05     CLC   =C'TRACE=',CARD     TRACE CARD?                                  
         BNE   RC06                                                             
         CLC   =C'NO',CARD+6                                                    
         BE    RC02                TRACE=NO IS THE DEFAULT                      
         CLC   =C'YES',CARD+6                                                   
         BNE   INVCARD                                                          
         MVI   TRACEFLG,YES        TRACE=YES                                    
         B     RC02                                                             
*                                                                               
RC06     DS    0H                                                               
*&&CL*&& CLC   =C'CLASS=',CARD     CLASS=                                       
*&&AG*&& CLC   =C'AGYID=',CARD     AGYID=                                       
         BNE   RC07                                                             
         BRAS  RE,CHKCLASS                                                      
         B     RC02                                                             
*                                                                               
RC07     CLC   =C'MINPRTY=',CARD   MINPRTY=N                                    
         BNE   RC08                                                             
         GOTO1 VNUMVAL,DMCB,CARD+8,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   MINPRTY,CARD+8      SAVE MINIMUM PRIORITY                        
         B     RC02                                                             
*                                                                               
RC08     CLC   =C'MAXPRTY=',CARD   MAXPRTY=N                                    
         BNE   RC10                                                             
         GOTO1 VNUMVAL,DMCB,CARD+8,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   MAXPRTY,CARD+8      SAVE MAXIMUM PRIORITY                        
         B     RC02                                                             
*                                                                               
RC10     CLC   =C'DATE=YESTERDAY',CARD DATE=YESTERDAY                           
         BNE   *+12                                                             
         MVI   YESTERDY,YES        ATTACH YESTERDAY'S REQUESTS ONLY             
         B     RC02                                                             
*                                                                               
         CLC   =C'DATE=ANY',CARD   DATE=ANY                                     
         BNE   *+12                                                             
         MVI   ANYDATE,YES         ATTACH REQUESTS FROM ANY DATE                
         B     RC02                                                             
*                                                                               
RC12     CLC   =C'DDSIO=',CARD     DDSIO=                                       
         BNE   RC14                                                             
         L     RF,=V(DDSIO)                                                     
         MVC   0(8,RF),CARD+6      THIS ONE IS FOR MONSTER ONLY                 
         MVC   DDSIOMOD,CARD+6     THIS ONE IS FOR ATTACHED TASKS               
         B     RC02                                                             
*                                                                               
RC14     CLC   =C'WAITSECS=',CARD  WAITSECS=N                                   
         BNE   RC16                                                             
         GOTO1 VNUMVAL,DMCB,CARD+9,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         MHI   R1,100              SCALE THE TIME INTERVAL FOR STIMER           
         ST    R1,WAITSECS                                                      
         B     RC02                                                             
*                                                                               
RC16     CLC   =C'USERMAXREQ=',CARD  USERMAXREQ=N                               
         BNE   RC17                                                             
         CLC   CARD+11(2),=C'NO'                                                
         BNE   *+14                                                             
         MVC   USERMAXR,=X'FFFF'   SET TO FFS FOR NONE                          
         B     RC02                                                             
         GOTO1 VNUMVAL,DMCB,CARD+11,(2,0)                                       
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   USERMAXR,DMCB+6                                                  
         B     RC02                                                             
*                                                                               
RC17     CLC   =C'SUBMAXREQ=',CARD  SUBMAXREQ=N                                 
         BNE   RC18                                                             
         CLC   CARD+11(2),=C'NO'                                                
         BNE   *+14                                                             
         MVC   SUBMAXR,=X'FFFF'    SET TO FFS FOR NONE                          
         B     RC02                                                             
         GOTO1 VNUMVAL,DMCB,CARD+11,(2,0)                                       
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   SUBMAXR,DMCB+6                                                   
         B     RC02                                                             
*                                                                               
RC18     CLC   =C'USERID=',CARD    USERID=XXX                                   
         BNE   RC26                                                             
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
*                                                                               
         LA    R2,KEY              BUILD ID RECORD KEY                          
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CARD+7       USER-ID                                      
         GOTO1 VDMGR,DMCB,(0,DMREAD),CTFILE,KEY,AIO,0                           
         CLI   DMCB+8,0                                                         
         BNE   INVCARD                                                          
*                                                                               
         L     R2,AIO                                                           
         LA    R2,CTIDATA                                                       
         USING CTDSCD,R2                                                        
         XR    RF,RF                                                            
RC20     CLI   CTDSCEL,0           FIND DESCRIPTION ELEMENT (NUM)               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    RC22                                                             
         ICM   RF,1,CTDSCLEN                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BXH   R2,RF,RC20                                                       
*                                                                               
RC22     LA    RF,USERFLTS         ARRAY OF USERID FILTERS                      
         LHI   R0,USERFMXQ         MAXIMUM NUMBER OF USERID FILTERS             
RC24     OC    0(2,RF),0(RF)       IS THIS ARRAY SLOT FREE?                     
         BZ    *+14                                                             
         LA    RF,2(RF)                                                         
         BCT   R0,RC24                                                          
         DC    H'0'                TOO MANY USERID FILTERS                      
         MVC   0(2,RF),CTDSC       HEX USERID FILTER                            
         B     RC02                                                             
         DROP  R2                                                               
*                                                                               
RC26     CLC   =C'JESMSG=',CARD    JESMSG=                                      
         BNE   RC28                                                             
         CLC   =C'NO',CARD+7                                                    
         BE    RC02                JESMSG=NO IS THE DEFAULT                     
         CLC   =C'YES',CARD+7                                                   
         BNE   INVCARD                                                          
         MVI   JESMSG,YES          JESMSG=YES                                   
         B     RC02                                                             
*                                                                               
RC28     CLC   =C'SUBID=',CARD     SUBID=                                       
         BNE   *+14                                                             
         MVC   SUBFILT,CARD+6                                                   
         B     RC02                                                             
*                                                                               
         CLC   =C'EXREPORT=',CARD  EXREPORT=                                    
         BNE   *+14                                                             
         MVC   EXREPORT,CARD+9                                                  
         B     RC02                                                             
*                                                                               
         CLC   =C'ATTACH=',CARD    ATTACH=                                      
         BNE   RC30                                                             
         CLC   =C'YES',CARD+7                                                   
         BE    RC02                ATTACH=YES IS THE DEFAULT                    
         CLC   =C'NO',CARD+7                                                    
         BNE   INVCARD                                                          
         MVI   ATTCHFLG,NO         ATTACH=NO                                    
         B     RC02                                                             
*                                                                               
RC30     CLC   =C'REFNUM=',CARD    REFNUM=                                      
         BNE   RC32                                                             
         GOTO1 VNUMVAL,DMCB,CARD+7,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   REFFILT,DMCB+6      REFERENCE NUMBER                             
         B     RC02                                                             
*                                                                               
RC32     CLC   =C'REQTIMER=',CARD  REQTIMER=N                                   
         BNE   RC34                                                             
         CLC   =C'NONE',CARD+9                                                  
         BNE   *+14                                                             
         MVC   REQTIMER,=C'NONE'                                                
         B     RC02                                                             
*                                                                               
         GOTO1 VNUMVAL,DMCB,CARD+9,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   REQTIMER,DMCB+4     TIME IN MINUTES                              
         B     RC02                                                             
*                                                                               
RC34     CLC   =C'TYPE=',CARD      TYPE=                                        
         BNE   RC36                                                             
         CLC   =C'UPDATE',CARD+5                                                
         BNE   *+12                                                             
         MVI   TYPUPD,YES                                                       
         B     RC02                                                             
         CLC   =C'NOUPDATE',CARD+5                                              
         BNE   INVCARD                                                          
         MVI   TYPNOUPD,YES                                                     
         B     RC02                                                             
*                                                                               
RC36     CLC   =C'MINTIME=',CARD   MINTIME=N                                    
         BNE   RC38                                                             
         GOTO1 VNUMVAL,DMCB,CARD+8,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         L     R1,DMCB+4           TIME IN MINUTES                              
         MHI   R1,6000                                                          
         ST    R1,MINTIME                                                       
         B     RC02                                                             
*                                                                               
RC38     CLC   =C'CLASSPRTY=',CARD CLASSPRTY=                                   
         BNE   RC40                                                             
         CLC   =C'NO',CARD+10                                                   
         BE    RC02                CLASSPRTY=NO IS THE DEFAULT                  
         CLC   =C'YES',CARD+10                                                  
         BNE   INVCARD                                                          
         MVI   CLSPRTYF,YES        CLASSPRTY=YES                                
         B     RC02                                                             
*                                                                               
RC40     CLC   =C'FACPAK=',CARD    FACPAK=                                      
         BNE   RC42                                                             
         MVI   FACPAKFL,C'P'       ASSUME POSITIVE FILTER                       
         MVC   FACPAK,CARD+7                                                    
         CLI   CARD+7,C'-'                                                      
         BNE   RC02                                                             
         MVI   FACPAKFL,C'N'       IT'S A NEGATIVE FILTER                       
         MVC   FACPAK,CARD+8                                                    
         B     RC02                                                             
*                                                                               
RC42     CLC   =C'REFRESH=',CARD   REFRESH=                                     
         BNE   RC46                                                             
         CLC   =C'PRTY<',CARD+8                                                 
         BE    RC44                                                             
         GOTO1 VNUMVAL,DMCB,CARD+8,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   REFRESH,DMCB+6      REFRESH VALUE                                
         B     RC02                                                             
*                                                                               
RC44     GOTO1 VNUMVAL,DMCB,CARD+13,(2,0)                                       
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   REFPRTY,CARD+13     REFRESH PRIORITY VALUE                       
         B     RC02                                                             
*                                                                               
RC46     CLC   =C'LOAD=',CARD      LOAD=   GENERATE AND LOAD DATASPACE          
         BNE   RC48                                                             
         MVC   LOADCARD,CARD+5     SAVE MODULE NAME                             
         LOAD  EPLOC=CARD+5,ERRET=RCERR01                                       
         GOTO1 (R0),LOADPARM                                                    
         BNE   RCERR02                                                          
         DELETE EPLOC=CARD+5                                                    
         WTO   TEXT=REMS1H                                                      
         B     RC02                                                             
*                                                                               
RC48     CLC   =C'MONSOONID=',CARD   MONSOONID=                                 
         BNE   RC50                                                             
         MVC   MINORNAM,CARD+10                                                 
         ENQ   (MAJORNAM,MINORNAM,E,8,SYSTEMS),RET=USE                          
         LTR   RF,RF                                                            
         BZ    RC02                                                             
         MVC   REMS2+10(8),MINORNAM                                             
         WTO   TEXT=REMS2H                                                      
         DC    H'0'                THIS MONSTER IS ALREADY RUNNING              
*                                                                               
RC50     CLC   =C'EXUSERID=',CARD  EXUSERID=CCCCCC                              
         BNE   RC58                                                             
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
*                                                                               
         LA    R2,KEY              BUILD ID RECORD KEY                          
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,CARD+9       USER-ID                                      
         GOTO1 VDMGR,DMCB,(0,DMREAD),CTFILE,KEY,AIO,0                           
         CLI   DMCB+8,0                                                         
         BNE   INVCARD                                                          
*                                                                               
         L     R2,AIO                                                           
         LA    R2,CTIDATA                                                       
         USING CTDSCD,R2                                                        
         XR    RF,RF                                                            
RC52     CLI   CTDSCEL,0           FIND DESCRIPTION ELEMENT (NUM)               
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    RC54                                                             
         ICM   RF,1,CTDSCLEN                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BXH   R2,RF,RC52                                                       
*                                                                               
RC54     LA    RF,EXUSRIDS         ARRAY OF EXCLUDED USERIDS                    
         LHI   R0,EXUSRMXQ         MAXIMUM NUMBER OF EXUSERID CARDS             
RC56     OC    0(2,RF),0(RF)       IS THIS ARRAY SLOT FREE?                     
         BZ    *+14                                                             
         LA    RF,2(RF)                                                         
         BCT   R0,RC56                                                          
         DC    H'0'                TOO MANY EXUSERID CARDS                      
         MVC   0(2,RF),CTDSC       HEX USERID NEGATIVE FILTER                   
         B     RC02                                                             
         DROP  R2                                                               
*                                                                               
RC58     CLC   =C'ENQJOBNAME=',CARD   ENQJOBNAME=                               
         BNE   RC60                                                             
         CLC   =C'NO',CARD+11                                                   
         BE    RC02                ENQJOBNAME=NO IS THE DEFAULT                 
         CLC   =C'YES',CARD+11                                                  
         BNE   INVCARD                                                          
         MVI   ENQJOBNM,YES        ENQJOBNAME=YES                               
         B     RC02                                                             
*                                                                               
RC60     CLC   =C'DUPDUMPWARN=',CARD   WARN CONSOLE ABOUT DUP. DUMPS?           
         BNE   RC62                                                             
         CLC   =C'YES',CARD+12                                                  
         BE    RC02                YES BY DEFAULT                               
         CLC   =C'NO',CARD+12                                                   
         BNE   INVCARD                                                          
         MVI   DUMPWARN,NO         NO                                           
         B     RC02                                                             
*                                                                               
RC62     CLC   =C'PARTNERLUID=',CARD                                            
         BNE   RC64                                                             
         MVC   PARTLUID,CARD+12                                                 
         B     RC02                                                             
*                                                                               
RC64     CLC   =C'SPARECONVS=',CARD    SPARECONVS=N                             
         BNE   RC66                                                             
         GOTO1 VNUMVAL,DMCB,CARD+11,(2,0)                                       
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         MVC   SPRCONV,DMCB+4                                                   
         B     RC02                                                             
*                                                                               
RC66     CLC   =C'TCBTIME=',CARD   MAX TCB TIME ALLOWED                         
         BNE   RC68                                                             
         GOTO1 VNUMVAL,DMCB,CARD+8,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   MAXTCBTM,DMCB+4                                                  
         B     RC02                                                             
*                                                                               
RC68     CLC   =C'ETTIME=',CARD    MAX ET TIME ALLOWED                          
         BNE   RC70                                                             
         GOTO1 VNUMVAL,DMCB,CARD+7,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   MAXETTM,DMCB+4                                                   
         B     RC02                                                             
*                                                                               
RC70     CLC   =C'DUMPCRT=',CARD   DUMPCRT=N                                    
         BNE   RC72                                                             
         GOTO1 VNUMVAL,DMCB,CARD+8,(2,0)                                        
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   DCRT,DMCB+4                                                      
         B     RC02                                                             
*                                                                               
RC72     CLC   =C'DUMPLIMIT=',CARD DUMPLIMIT=N                                  
         BNE   RC74                                                             
         GOTO1 VNUMVAL,DMCB,CARD+10,(2,0)                                       
         CLI   DMCB,0                                                           
         BNE   INVCARD             NOT NUMERIC                                  
         MVC   DLMT,DMCB+4                                                      
         B     RC02                                                             
*                                                                               
RC74     CLC   =C'UNLIMITEDNIGHT=',CARD    UNLIMITEDNIGHT=HHMM                  
         BNE   RC76                                                             
         MVC   UNLMNGHT,CARD+15                                                 
         B     RC02                                                             
*                                                                               
RC76     CLC   =C'UNLIMITEDWEEKEND=',CARD  UNLIMITEDWEEKEND=NO                  
         BNE   RC78                                                             
         CLC   =C'NO',CARD+17                                                   
         BE    RC02                UNLMWKND=NO IS THE DEFAULT                   
         CLC   =C'YES',CARD+17                                                  
         BNE   INVCARD                                                          
         MVI   UNLMWKND,YES        UNLMNWKND=YES                                
         B     RC02                                                             
*                                                                               
RC78     CLC   =C'POSTTIME=',CARD  POSTTIME=                                    
         BNE   RC80                                                             
         CLC   =C'NO',CARD+9                                                    
         BE    RC02                POSTTIME=NO IS THE DEFAULT                   
         CLC   =C'YES',CARD+9                                                   
         BNE   INVCARD                                                          
         MVI   POSTTIME,YES        POSTTIME=YES                                 
         B     RC02                                                             
*                                                                               
RC80     CLC   =C'QTYPE=',CARD     QUEUE TYPE                                   
         BNE   RC90                                                             
         MVC   QTYPE,CARD+6        CAN ONLY BE INCLUDE OR EXCLUDE               
         MVI   XQTYPE,C' '                                                      
         MVC   BYTE,QTYPE                                                       
         CLI   CARD+6,C'*'         EXCLUDE INDICATION                           
         BNE   RC82                NO                                           
         MVI   QTYPE,C' '                                                       
         MVC   XQTYPE,CARD+7       CAN ONLY BE INCLUDE OR EXCLUDE               
         MVC   BYTE,XQTYPE                                                      
*                                                                               
RC82     DS    0H                                                               
*        CLI   BYTE,C'S'           SHORT                                        
*        BE    RC02                                                             
         CLI   BYTE,C'M'           MEDIUM                                       
         BE    RC02                                                             
         CLI   BYTE,C'L'           LONG                                         
         BE    RC02                                                             
         CLI   BYTE,C'C'           COMSCORE                                     
         BE    RC02                                                             
         B     INVCARD                                                          
*                                                                               
RC90     CLC   =C'AGENCY=',CARD    AGECNY=CC                                    
         BNE   RC95                                                             
*                                                                               
         LA    RF,AGYFLTS          ARRAY OF AGENCY FILTERS                      
         LHI   R0,AGYFMXQ          MAXIMUM NUMBER OF AGENCY FILTERS             
RC92     OC    0(2,RF),0(RF)       IS THIS ARRAY SLOT FREE?                     
         BZ    *+14                                                             
         LA    RF,2(RF)                                                         
         BCT   R0,RC92                                                          
         DC    H'0'                TOO MANY AGENCY FILTERS                      
         MVC   0(2,RF),CARD+7      AGENCY FILTER                                
         B     RC02                                                             
*                                                                               
RC95     CLC   =C'EXAGENCY=',CARD  EXAGECNY=CC                                  
         BNE   RC100                                                            
*                                                                               
         LA    RF,EXAGYS           ARRAY OF EXCLUDED AGENCIES                   
         LHI   R0,EXAGYMXQ         MAXIMUM NUMBER OF EXAGENCY CARDS             
RC98     OC    0(2,RF),0(RF)       IS THIS ARRAY SLOT FREE?                     
         BZ    *+14                                                             
         LA    RF,2(RF)                                                         
         BCT   R0,RC98                                                          
         DC    H'0'                TOO MANY EXAGENCY CARDS                      
         MVC   0(2,RF),CARD+9      AGENCY NEGATIVE FILTER                       
         B     RC02                                                             
*                                                                               
RC100    CLC   =C'REPORTTYPEMAX=',CARD    REPORTTYPEMAX=CCN                     
         BNE   RC200                                                            
*                                                                               
         LA    RF,REPTFLTS         ARRAY OF REPORT TYPE FILTERS                 
         LHI   R0,REPTFMXQ         MAXIMUM NUMBER OF FILTERS                    
RC104    OC    0(L'REPTFLTS,RF),0(RF)    IS THIS ARRAY SLOT FREE?               
         BZ    *+14                                                             
         LA    RF,L'REPTFLTS(,RF)                                               
         BCT   R0,RC104                                                         
         DC    H'0'                TOO MANY REPORT TYPE FILTERS                 
         MVC   0(L'REPTFLTS,RF),CARD+14                                         
         NI    2(RF),X'0F'         CHG TO HEX VALUE,(C'1' TO X'01')             
         B     RC02                                                             
*                                                                               
RC200    EQU   *                                                                
*                                                                               
INVCARD  MVC   P+20(22),=C'- INVALID CONTROL CARD'                              
         GOTO1 VPRINTER                                                         
         DC    H'0'                                                             
*                                                                               
RCX      LA    R1,TPTAB                                                         
         USING TPTABD,R1                                                        
         LH    RF,TPTMCN           1ST # MAX CONV                               
         XR    R0,R0                                                            
         ICM   R0,3,NUMTP                                                       
         BZ    RCX20                                                            
*                                                                               
RCX10    AHI   R1,TPTABLQ                                                       
         AH    RF,TPTMCN                                                        
         BCT   R0,RCX10                                                         
         DROP  R1                                                               
*                                                                               
RCX20    ST    RF,TOTCONV                                                       
         A     RF,SPRCONV                                                       
         ST    RF,MAXCONV                                                       
*                                                                               
         CLOSE (MONSYSIN)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VPRINTER            SKIP A LINE                                  
         B     EXITOK                                                           
*                                                                               
RCERR01  DC    H'0'                CANT LOAD PHASE                              
RCERR02  DC    H'0'                CANT LOAD DATASPACE                          
*                                                                               
REMS1H   DC    AL2(L'REMS1)                                                     
REMS1    DC    C'DATASPACE ACQUIRED AND LOADED'                                 
REMS2H   DC    AL2(L'REMS2)                                                     
REMS2    DC    C'MONSOONID XXXXXXXX ALREADY RUNNING'                            
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK CLASS/AGYID CARD AND ADD ENTRY TO TABLE            *         
***********************************************************************         
CHKCLASS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'ALL,',CARD+6     CLASS=ALL?                                   
         BNE   CC10                                                             
         MVI   ALLCLASS,YES        YES - SET FLAG                               
         LA    R2,CARD+10                                                       
         LA    R3,TPTMCN-TPTABD+TPTAB                                           
         BRAS  RE,CC100            GET # CONV                                   
         CLC   =X'FFFF',0(R3)                                                   
         BNE   *+6                                                              
         DC    H'0'                NO # CONV IS GIVEN                           
*                                                                               
         LA    R2,1(R1)            TP NAME FIELD                                
         LA    R3,TPTLEN-TPTABD+TPTAB                                           
         BRAS  RE,CC200            GET TP NAME & LENGTH                         
         B     CCX                                                              
*                                                                               
CC10     CLI   CARD+8,C','                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID CLASS                                
*                                                                               
         XC    CLASS,CLASS                                                      
         LA    R4,CLASS                                                         
         USING CLASSD,R4                                                        
         MVC   CLSNAME,CARD+6      PUT CLASS CODE INTO 'RECORD'                 
*                                                                               
         MVI   BSP4,X'01'                                                       
         GOTO1 VBINSRCH,BSP,CLASS                                               
         ICM   R4,15,BSPAREC                                                    
         BNZ   *+6                                                              
         DC    H'0'                CLASS TABLE IS NOT BIG ENOUGH                
*                                                                               
         TM    BSPNF,X'80'                                                      
         BO    *+6                                                              
         DC    H'0'                DUPLICATE CLASS SPECIFIED                    
*                                                                               
         LH    RF,SVCLSPRT         LAST CLASS PRIORITY VALUE                    
         LA    RF,1(RF)                                                         
         STH   RF,SVCLSPRT                                                      
         STH   RF,CLSPRTY          SAVE CLASS PRIORITY                          
*                                                                               
         LA    R2,CARD+9                                                        
         LA    R3,CLSMAX                                                        
         BRAS  RE,CC100            GET # MAX REQ                                
*                                                                               
         LA    R2,1(R1)            # CONV FIELD                                 
         LH    RF,NUMTP                                                         
         AHI   RF,1                                                             
         STH   RF,NUMTP                                                         
         CHI   RF,MAXTPQ                                                        
         BNH   *+6                                                              
         DC    H'0'                EXCESS THE TP TABLE SIZE LIMIT               
*                                                                               
         LR    R5,RF                                                            
         MHI   R5,TPTABLQ                                                       
         LA    R5,TPTAB(R5)                                                     
         ST    R5,CLSTP                                                         
         DROP  R4                                                               
*                                                                               
         LA    R3,TPTMCN-TPTABD(R5)                                             
         BRAS  RE,CC100            GET # CONV                                   
         CLC   =X'FFFF',0(R3)                                                   
         BNE   *+6                                                              
         DC    H'0'                NO # CONV IS GIVEN                           
*                                                                               
         LA    R2,1(R1)            TP NAME FIELD                                
         LA    R3,TPTLEN-TPTABD(R5)                                             
         BRAS  RE,CC200            GET TP NAME & LENGTH                         
         B     CCX                                                              
*                                                                               
CC100    SR    RF,RF                                                            
CC110    LA    R1,0(RF,R2)                                                      
         CLI   0(R1),C','                                                       
         BE    CC120                                                            
         AHI   RF,1                                                             
         B     CC110                                                            
*                                                                               
CC120    LTR   RF,RF                                                            
         BZ    CC130                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,CCPK                                                          
         CVB   RF,DUB                                                           
         STH   RF,0(R3)                                                         
         BR    RE                                                               
*                                                                               
CCPK     PACK  DUB,0(0,R2)                                                      
*                                                                               
CC130    MVC   0(2,R3),=X'FFFF'                                                 
         BR    RE                                                               
*                                                                               
CC200    MVC   2(L'TPTNAM,R3),0(R2)                                             
         SR    R1,R1               COUNT CHARACTERS                             
         CLI   0(R2),C' '          SCAN FORWARD TO FIRST BLANK                  
         BE    *+16                                                             
         AHI   R1,1                                                             
         AHI   R2,1                                                             
         B     *-16                                                             
         STH   R1,0(R3)            TP NAME LENGTH                               
         BR    RE                                                               
*                                                                               
CCX      B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SET A TIMER - WAIT PERIOD IS PARAMETER DRIVEN (WAITSECS)            *         
*                                                                     *         
* IF THERE IS STILL WORK TO DO, THEN IT WILL WAIT FOR WAITSECS BEFORE *         
* FORCING A RESCAN OF THE INPUT QUEUES                                *         
*                                                                     *         
* IF THE MONSTER IS NOT ACTIVE (REALLY WAITING IN OTHER WORDS) THEN   *         
* IT WILL WAIT FOR WAITSECS/2 BEFORE STARTING TO SCAN AGAIN           *         
*                                                                     *         
* IF WAITSECS IS <10S THEN THE WAITSECS/2 CODE IS NOT ACTIVE          *         
***********************************************************************         
SETWAIT  NTR1  BASE=*,LABEL=*                                                   
         CLI   TRACEFLG,YES                                                     
         BNE   *+14                                                             
         MVC   P(17),=CL17'SETTING WAIT TIMER'                                  
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    STIMERID,STIMERID   STIMER SET UP?                               
         BZ    SWT02               NO                                           
*                                                                               
         STIMERM TEST,ID=STIMERID,TU=FULL                                       
         ICM   R0,15,FULL                                                       
         BZ    SWT02               NO CURRENT TIMER ACTIVE                      
*                                                                               
         STIMERM CANCEL,ID=STIMERID                                             
*                                                                               
SWT02    XC    TIMERECB,TIMERECB   CLEAR ECB                                    
         ICM   R0,15,WAITSECS                                                   
         CHI   R0,(10*100)                                                      
         BNH   SWT04               IGNORE IF LESS THAN 10 SECONDS               
*                                                                               
         OC    NUMINQ,NUMINQ       ANY JOBS WAITING TO RUN?                     
         BNZ   SWT04               YES - WAIT FOR FULL PERIOD                   
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   *+14                                                             
         MVC   P(17),=CL17'NOTHING WAITING'                                     
         BRAS  RE,PRNT                                                          
*                                                                               
         SRL   R0,1                NO - WAIT FOR 1/2 PERIOD                     
*                                                                               
SWT04    STCM  R0,15,FULL          SET WAIT PERIOD                              
         STIMERM SET,ID=STIMERID,BINTVL=FULL,EXIT=TIMERXIT                      
         LTR   RF,RF                                                            
         BZ    EXITOK                                                           
         DC    H'0'                WHY CAN'T WE SET THE TIMER?                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN CONTROL FILE                                                   *         
***********************************************************************         
OPENCTFL NTR1  BASE=*,LABEL=*                                                   
         BC    0,EXITOK            ONLY OPEN CTFILE ONCE                        
         MVI   *-3,X'F0'                                                        
*                                                                               
         GOTO1 VDMGR,DMCB,(0,DMOPEN),CONTROL,CTFLIST,AIO,0                      
         B     EXITOK                                                           
*                                                                               
CTFLIST  DC    CL8'NCTFILE '                                                    
         DC    CL8'X       '                                                    
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' OR 'MODIFY'       *         
* COMMAND.  EXAMINE THE COMMAND AND TAKE THE APPROPRIATE ACTION.      *         
***********************************************************************         
CHKOPER  NTR1  BASE=*,LABEL=*                                                   
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CH02                                                             
         MVI   OPERSTOP,YES        YES -- SET STOP FLAG                         
         WTO   'STOP COMMAND ACCEPTED'                                          
         B     CHX                                                              
*                                                                               
CH02     CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT DID THE OPERATOR DO?                    
*                                                                               
         LA    R4,COMMTAB                                                       
         USING COMMTABD,R4                                                      
         XR    RF,RF                                                            
CH04     CLI   0(R4),X'FF'         EOT                                          
         BE    CHBAD               BAD COMMAND                                  
*                                                                               
         IC    RF,COMMLEN          GET MINIMUM LENGTH                           
         CH    RF,CIBDATLN         CHECK STRING LENGTH                          
         BH    CH06                                                             
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8              MATCH COMMAND IN TABLE                       
         BE    CH08                PASS                                         
         CLC   COMMCMD(0),CIBDATA                                               
*                                                                               
CH06     AHI   R4,COMMTABL         NEXT ENTRY                                   
         B     CH04                                                             
*                                                                               
CH08     ICM   RF,15,COMMRTN       GET PROCESSING ROUTINE                       
         BASR  RE,RF                                                            
         B     CHX                                                              
*                                                                               
CHBAD    WTO   '*INVALID MONSTER COMMAND*'                                      
*                                                                               
CHX      L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
OPJES    NTR1  ,               *** TOGGLE JESMSG COMMAND                        
         CLI   JESMSG,YES                                                       
         BNE   OPJES02                                                          
         MVI   JESMSG,NO                                                        
         MVC   COMMS1+33(3),=C'OFF'                                             
         B     OPJES04                                                          
*                                                                               
OPJES02  MVI   JESMSG,YES                                                       
         MVC   COMMS1+33(3),=C'ON '                                             
*                                                                               
OPJES04  L     R1,AASCB            A(ASCB)                                      
         MVC   HALF,ASCBASID-ASCB(R1)                                           
         GOTO1 VHEXOUT,DMCB,HALF,COMMS1+7,2,0                                   
         WTO   TEXT=COMMS1H                                                     
         B     EXITOK                                                           
*                                                                               
OPCLASS  NTR1  ,               *** UPDATE CLASS INFO TABLE                      
         LA    RE,CIBDATA                                                       
         ST    RE,FULL                                                          
         BRAS  RE,UPDCLST                                                       
         B     EXITOK                                                           
*                                                                               
OPSTA    NTR1  ,               *** DISPLAY STATUS                               
         MVI   BYTE,C'A'           PARAMETER TO STATUS SUBROUTINE               
         MVI   BYTE2,C'C'          FROM OPERATOR INTERRUPT                      
         BRAS  RE,STATUS           DISPLAY STATUS OF ALL CLASSES                
*                                                                               
         CLI   BACKMSG,YES                                                      
         BNE   OPSTA02                                                          
         MVI   BACKMSG,NO                                                       
         MVC   COMMS2+21(3),=C'OFF'                                             
         B     OPSTA04                                                          
*                                                                               
OPSTA02  MVI   BACKMSG,YES                                                      
         MVC   COMMS2+21(3),=C'ON '                                             
*                                                                               
OPSTA04  WTO   TEXT=COMMS2H                                                     
*                                                                               
         L     R7,AAPPCTAB         APPC/MVS CONTROL TABLE                       
OPSTA06  WTO   TEXT=OPMS1H                                                      
         AHI   R7,APPCDLEN         BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R7)       END OF TABLE?                                
         BNE   OPSTA06                                                          
         B     EXITOK                                                           
*                                                                               
OPWAIT   NTR1  ,               *** STOP ATTACHING REQUESTS                      
         MVI   ATTCHFLG,C'X'       SET FOR 1ST LOOP TO INFORM OPERATOR          
         WTO   'WAIT COMMAND ACCEPTED'                                          
         B     EXITOK                                                           
*                                                                               
OPGO     NTR1  ,               *** START ATTACHING REQUESTS                     
         MVI   ATTCHFLG,C'R'       RESUME ATTACHING REQUESTS                    
         WTO   'GO COMMAND ACCEPTED'                                            
         CLI   LOADCARD,0          NEED TO REFRESH DATASPACE IF ACTIVE          
         BE    EXITOK                                                           
         MVI   LDACTION,LDREFQ                                                  
         B     EXITOK                                                           
*                                                                               
OPTRC    NTR1  ,               *** TOGGLE TRACE FLAG?                           
         CLI   TRACEFLG,YES        YES                                          
         BNE   OPTRC02                                                          
         MVI   TRACEFLG,NO                                                      
         MVC   COMMS3+15(3),=C'OFF'                                             
         B     OPTRC04                                                          
*                                                                               
OPTRC02  MVI   TRACEFLG,YES                                                     
         MVC   COMMS3+15(3),=C'ON '                                             
*                                                                               
OPTRC04  WTO   TEXT=COMMS3H                                                     
         B     EXITOK                                                           
                                                                                
**********************************************************************          
* TABLE OF KNOWN OPERATOR COMMANDS WITH PROCESSING ROUTINES                     
**********************************************************************          
COMMTAB  DC    CL8'JESMSG  ',AL1(6,0,0,0),AL4(OPJES)                            
         DC    CL8'STATUS  ',AL1(6,0,0,0),AL4(OPSTA)                            
         DC    CL8'CLASS=  ',AL1(6,0,0,0),AL4(OPCLASS)                          
         DC    CL8'AGYID=  ',AL1(6,0,0,0),AL4(OPCLASS)                          
         DC    CL8'TRACE   ',AL1(5,0,0,0),AL4(OPTRC)                            
         DC    CL8'CLASS   ',AL1(5,0,0,0),AL4(PRTCLST)                          
         DC    CL8'WAIT    ',AL1(4,0,0,0),AL4(OPWAIT)                           
         DC    CL8'GO      ',AL1(2,0,0,0),AL4(OPGO)                             
         DC    X'FF'                                                            
*                                                                               
COMMTABD DSECT                     COVERS COMMTAB ABOVE                         
COMMCMD  DS    CL8                 INPUT COMMAND                                
COMMLEN  DS    X                   MINIMUM LENGTH                               
         DS    XL3                                                              
COMMRTN  DS    AL4                 A(PROCESSING ROUTINE)                        
COMMTABL EQU   *-COMMTABD                                                       
*                                                                               
MONSTER  CSECT                                                                  
COMMS1H  DC    AL2(L'COMMS1)                                                    
COMMS1   DC    C'ASID = XXXX:  START/END MESSAGES XXX'                          
COMMS2H  DC    AL2(L'COMMS2)                                                    
COMMS2   DC    C'BACK LOG MESSAGES IS XXX'                                      
COMMS3H  DC    AL2(L'COMMS3)                                                    
COMMS3   DC    C'DETAILED TRACE XXX'                                            
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE CLASS TABLE STATUS ON THE CONSOLE.  IF BYTE=C'B', ONLY *         
* DISPLAY BACKED UP CLASSES.  IF BYTE=C'A', DISPLAY ALL CLASSES.      *         
***********************************************************************         
STATUS   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,ARSOFF                                                        
         CLI   BYTE2,C'C'          FROM OPERATOR INTERRUPT?                     
         BNE   ST02                NO                                           
*                                                                               
         MVC   STMS3+14(8),=CL8'NORMAL'                                         
         CLI   MONMODE,NO          LIMIT=N, OFF PEAK MODE?                      
         BNE   *+10                                                             
         MVC   STMS3+14(8),=CL8'OFF PEAK'                                       
         WTO   TEXT=STMS3H                                                      
*                                                                               
ST02     ICM   R3,15,BSPNOR        ANY CLASSES IN CLASS TABLE?                  
         BZ    EXITOK              NO                                           
*                                                                               
         XC    FULL,FULL                                                        
         SAM31                                                                  
         L     R2,BSPSTRT          A(CLASS TABLE)                               
         USING CLASSD,R2                                                        
*                                                                               
ST04     TIME  BIN                 GET THE CURRENT TIME                         
         SRDL  R0,32                                                            
         D     R0,=F'6000'                                                      
         ST    R1,FULL2            CURRENT TIME IN MINUTES                      
*                                                                               
ST06     OC    CLSREQW,CLSREQW     ANY REQUESTS AWAITING EXECUTION?             
         BZ    ST20                                                             
*                                                                               
         MVC   FULL+2(2),CLSREQW   YES                                          
         L     RF,CLSTIME          TOTAL SUBMIT TIME FOR CLASS (MINS)           
         SR    RE,RE                                                            
         D     RE,FULL             RF = AVG WAIT TIME FOR CLASS (MINS)          
         L     RE,FULL2            CURRENT TIME IN MINUTES                      
         SR    RE,RF               AVERAGE WAIT TIME                            
         ST    RE,FULL                                                          
         CLI   BYTE,C'A'           DISPLAY ALL CLASSES?                         
         BE    ST08                YES                                          
         CHI   RE,CLSWAIT/2        HALF MAX AVERAGE WAIT TIME EXCEEDED?         
         BNL   *+14                YES                                          
         XC    CLSRDTM,CLSRDTM                                                  
         B     ST20                                                             
*                                                                               
         OC    CLSRDTM,CLSRDTM                                                  
         BNZ   *+26                                                             
         MVC   CLSRDTM,FULL2       STORE THE CURRENT TIME                       
         MVC   CLSPAWT,FULL        STORE THIS AVG WAITING TIME                  
         XC    CLSPRATE,CLSPRATE   SET RATE = 0                                 
         B     ST20                                                             
*                                                                               
         L     R1,FULL2                                                         
         S     R1,CLSRDTM          R1 = T'0 - T'-1                              
         CHI   R1,READTIM          PASS TIME INTERVAL TO READ AGAIN?            
         BNH   ST20                                                             
*                                                                               
         L     RF,FULL                                                          
         S     RF,CLSPAWT          RF = AWT'0 - AWT'-1                          
         MHI   RF,100              100 X (AWT'0 - AWT'-1)                       
         SR    RE,RE                                                            
         TMH   RF,X'1000'          PASS ALONG THE SIGN BIT IF -                 
         BZ    *+8                                                              
         LHI   RE,-1                                                            
         DR    RE,R1               RF = CHANGE RATE OF AWT                      
*                                                                               
         MVC   CLSRDTM,FULL2                                                    
         MVC   CLSPAWT,FULL                                                     
*                                                                               
         MVC   DUB(4),CLSPRATE                                                  
         ST    RF,CLSPRATE                                                      
*                                                                               
         LTR   RF,RF               IS CURRENT POSITIVE?                         
         BNP   ST20                NO - SKIP THE WARNING MESSAGE                
         C     RF,DUB              IS RATE INCREASING?                          
         BL    ST20                NO - SKIP THE WARNING MESSAGE                
*                                                                               
ST08     MVC   STMS2+16(2),CLSNAME CLASS NAME                                   
         EDIT  CLSREQW,(3,STMS2+23)                                             
         SR    R0,R0                                                            
         L     R1,FULL             AVERAGE WAIT TIME IN MINUTES                 
         D     R0,=F'60'                                                        
         CVD   R1,DUB              WAIT TIME IN HOURS                           
         OI    DUB+7,X'0F'                                                      
         UNPK  STMS2+56(2),DUB                                                  
         BRAS  RE,CVDR0            WAIT TIME IN MINUTES                         
         UNPK  STMS2+59(2),DUB                                                  
         WTO   TEXT=STMS2H                                                      
*                                                                               
         XC    PRTYS(40),PRTYS     CLEAR PRIORITY ACCUMLATORS                   
*                                                                               
         L     R8,ASRTBLK          A(REQUEST TABLE)                             
         USING SRTD,R8                                                          
ST10     CLC   SRTD(SRTRECL),EFFS                                               
         BE    ST14                                                             
         OC    SRTD(SRTKEYL),SRTD                                               
         BZ    ST12                                                             
*                                                                               
*&&CL*&& CLC   SRTCLS,CLSNAME      LOOK FOR REQUESTS FOR THIS CLASS             
*&&CL*&& BNE   ST12                                                             
*&&AG*&& CLC   SRTAGY,CLSNAME      LOOK FOR REQUESTS FOR THIS CLASS             
*&&AG*&& BNE   ST12                                                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,SRTPRI           R0 = PRIORITY IN EBCDIC                      
         NILF  GRE,X'0000000F'     R0 = PRIORITY IN BINARY                      
         SLL   RE,2                RE = DISPLACEMENT TO ACCUMULATOR             
         L     RF,PRTYS(RE)        RF = ACCUMULATOR VALUE                       
         LA    RF,1(RF)                                                         
         ST    RF,PRTYS(RE)        UPDATE ACCUMULATOR                           
*                                                                               
ST12     AHI   R8,SRTRECL          BUMP TO NEXT ENTRY                           
         B     ST10                                                             
*                                                                               
ST14     LA    R5,PRTYS                                                         
         SR    R4,R4               MAXIMUM OF 10 PRIORITY VALUES                
ST16     OC    0(4,R5),0(R5)       IF ACCUMULATOR IS ZERO, IGNORE IT            
         BZ    ST18                                                             
         EDIT  (B4,0(R5)),(5,STMS1+10)                                          
         EDIT  (R4),(1,STMS1+33)                                                
         WTO   TEXT=STMS1H                                                      
*                                                                               
ST18     LA    R5,4(R5)            BUMP TO NEXT ACCUMLATOR                      
         LA    R4,1(R4)                                                         
         CHI   R4,9                HIGHEST PRIORITY IS 9                        
         BNH   ST16                                                             
*                                                                               
ST20     LA    R2,CLSTABLQ(R2)     BUMP TO NEXT CLASS                           
         BCT   R3,ST06                                                          
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
*&&US                                                                           
CLSWAIT  EQU   15                  MAX WAIT BEFORE CONSOLE MSG (MINS)           
*&&                                                                             
*&&UK                                                                           
CLSWAIT  EQU   60                  MAX WAIT BEFORE CONSOLE MSG (MINS)           
*&&                                                                             
READTIM  EQU   1        MIN        TIME INTERVAL TO READ AVG WTNG TIME          
*                                                                               
STMS1H   DC    AL2(L'STMS1)                                                     
STMS1    DC    C'MONSTER:  NNNNN JOBS OF PRIORITY N'                            
*                                                                               
STMS2H   DC    AL2(L'STMS2)                                                     
STMS2    DC    C'MONSTER:  CLASS CC HAS NNN JOBS WAITING, AVERAGE WAIT +        
               = HH.MM'                                                         
*                                                                               
STMS3H   DC    AL2(L'STMS3)                                                     
STMS3    DC    C'MONSTER:  ****???????? MODE***'                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT THE CLASS INFO TABLE                                          *         
***********************************************************************         
PRTCLST  NTR1  BASE=*,LABEL=*                                                   
         WTO   'CLASS INFO LIST BELOW:'                                         
*                                                                               
         L     R2,ACLASSTB                                                      
         USING CLASSD,R2                                                        
         LHI   R3,MAXCLASS                                                      
*                                                                               
PCT10    ICM   RF,15,CLSTP         A(TP NAME TABLE ENTRY)                       
         BZ    PCT30                                                            
         LA    RE,P                                                             
         MVC   0(2,RE),CLSNAME                                                  
         MVI   2(RE),C','                                                       
         AHI   RE,3                                                             
*                                                                               
         CLC   =X'FFFF',CLSMAX                                                  
         BE    PCT20                                                            
         EDIT  CLSMAX,(5,(RE)),ALIGN=LEFT                                       
         AR    RE,R0                                                            
*                                                                               
PCT20    MVI   0(RE),C','                                                       
         AHI   RE,1                                                             
         USING TPTABD,RF                                                        
         EDIT  TPTMCN,(5,(RE)),ALIGN=LEFT                                       
         AR    RE,R0                                                            
         MVI   0(RE),C','                                                       
         MVC   1(64,RE),TPTNAM                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 VLOGIO,DMCB,X'FF000001',(80,P)                                   
*                                                                               
PCT30    AHI   R2,CLSTABLQ                                                      
         BCT   R3,PCT10                                                         
         DROP  R2                                                               
*                                                                               
         LA    RF,TPTAB            DEFAULT TP FOR ALL OTHER CLASSES             
         USING TPTABD,RF                                                        
         OC    TPTNAM,TPTNAM       ANY DEFAULT TP?                              
         BZ    PCT40                                                            
*                                                                               
         LA    RE,P                                                             
         MVC   0(4,RE),=C'ALL,'                                                 
         AHI   RE,4                                                             
*                                                                               
         EDIT  TPTMCN,(5,(RE)),ALIGN=LEFT                                       
         AR    RE,R0                                                            
         MVI   0(RE),C','                                                       
         MVC   1(64,RE),TPTNAM                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 VLOGIO,DMCB,X'FF000001',(80,P)                                   
*                                                                               
PCT40    MVC   P+00(02),=AL2(30)                                                
         MVC   P+02(30),=CL30'TOTAL # CONVERSATIONS:      '                     
         EDIT  TOTCONV,(5,P+25),ZERO=NOBLANK                                    
         MVC   P(2),=AL2(30)                                                    
         WTO   TEXT=P                                                           
*                                                                               
         MVC   P+00(02),=AL2(30)                                                
         MVC   P+02(30),=CL30'MAX.  # CONVERSATIONS:      '                     
         EDIT  MAXCONV,(5,P+25),ZERO=NOBLANK                                    
         WTO   TEXT=P                                                           
*                                                                               
         MVC   P+00(02),=AL2(30)                                                
         MVC   P+02(30),=CL30'SPARE # CONVERSATIONS:      '                     
         EDIT  SPRCONV,(5,P+25),ZERO=NOBLANK                                    
         WTO   TEXT=P                                                           
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE THE CLASS INFO TABLE & TP NAME TABLE                         *         
* NTRY:  FULL = A(INPUT BUFFER)                                       *         
***********************************************************************         
         SPACE 1                                                                
UPDCLST  NTR1  BASE=*,LABEL=*                                                   
         L     R2,FULL                                                          
*&&CL*&& CLC   =C'CLASS=ALL,',0(R2)      CLASS=ALL?                             
*&&AG*&& CLC   =C'AGYID=ALL,',0(R2)      AGYID=ALL?                             
         BNE   UCT10                                                            
*                                                                               
         AHI   R2,10               POINT TO THE UPDATE PARM                     
         LA    R4,TPTAB                                                         
         USING TPTABD,R4                                                        
         OC    TPTNAM,TPTNAM       ANY DEFAULT TP NAME?                         
         BNZ   UCT50                                                            
         WTO   TEXT=UCMS2H                                                      
         B     EXITOK                                                           
*                                                                               
UCT10    CLI   8(R2),C','                                                       
         BE    UCT20                                                            
         WTO   TEXT=UCMS1H                                                      
         B     EXITOK                                                           
*                                                                               
UCT20    XC    CLASS,CLASS                                                      
         MVC   CLSNAME-CLASSD+CLASS,6(R2)                                       
*                                                                               
         MVI   BSP4,X'00'                                                       
         GOTO1 VBINSRCH,BSP,CLASS                                               
         TM    BSPNF,X'80'                                                      
         BZ    UCT30                                                            
         WTO   TEXT=UCMS2H                                                      
         B     EXITOK                                                           
*                                                                               
UCT30    ICM   RF,15,BSPAREC                                                    
         L     R4,CLSTP-CLASSD(RF)                                              
         AHI   R2,9                POINT TO THE UPDATE PARM                     
*                                                                               
         CLC   =C'R=',0(R2)        UPDATE CONVERSATION #?                       
         BNE   UCT50                                                            
*                                                                               
         LA    R3,CLSMAX-CLASSD(RF)                                             
         B     UCT100                                                           
*                                                                               
UCT50    CLC   =C'C=',0(R2)        UPDATE CONVERSATION #?                       
         BNE   UCT60                                                            
*                                                                               
         LA    R3,TPTMCN                                                        
         B     UCT100                                                           
*                                                                               
UCT60    CLC   =C'TP=',0(R2)       UPDATE TP NAME?                              
         BNE   UCT70                                                            
*                                                                               
         LR    R3,R4                                                            
         B     UCT200                                                           
*                                                                               
UCT70    WTO   TEXT=UCMS1H                                                      
         B     EXITOK                                                           
         DROP  R4                                                               
*                                                                               
UCT100   SR    RF,RF                                                            
UCT110   LA    R1,2(RF,R2)                                                      
         CLI   0(R1),C'0'                                                       
         BL    UCT120                                                           
         CLI   0(R1),C'9'                                                       
         BH    UCT120                                                           
         AHI   RF,1                                                             
         B     UCT110                                                           
*                                                                               
UCT120   LTR   RF,RF                                                            
         BZ    UCT130                                                           
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,2(0,R2)                                                      
         CVB   RF,DUB                                                           
*                                                                               
         LH    RE,0(R3)            OLD VALUE                                    
         STH   RF,0(R3)            NEW VALUE                                    
         CLC   =C'C=',0(R2)        UPDATE CONVERSATION #?                       
         BNE   UCTOK                                                            
         SR    RF,RE               NEW - OLD VALUE                              
         A     RF,TOTCONV                                                       
         ST    RF,TOTCONV                                                       
         CLC   TOTCONV,MAXCONV                                                  
         BNH   *+14                                                             
         XC    SPRCONV,SPRCONV                                                  
         B     UCTOK                                                            
*                                                                               
         L     RE,MAXCONV                                                       
         SR    RE,RF                                                            
         ST    RE,SPRCONV                                                       
         B     UCTOK                                                            
*                                                                               
UCT130   CLC   =C'R=',0(R2)        UPDATE REQUEST #?                            
         BNE   *+14                                                             
         MVC   0(2,R3),=X'FFFF'                                                 
         B     UCTOK                                                            
*                                                                               
         WTO   TEXT=UCMS3H                                                      
         B     EXITOK                                                           
*                                                                               
UCT200   SR    RF,RF                                                            
UCT210   LA    R1,3(RF,R2)                                                      
         CLI   0(R1),X'40'                                                      
         BNH   UCT220                                                           
         AHI   RF,1                                                             
         B     UCT210                                                           
*                                                                               
UCT220   LTR   RF,RF                                                            
         BZ    UCT230                                                           
*                                                                               
         MVI   TPTNAM-TPTABD(R3),C' '                                           
         MVC   TPTNAM-TPTABD+1(L'TPTNAM-1,R3),TPTNAM-TPTABD(R3)                 
         STH   RF,TPTLEN-TPTABD(R3)                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TPTNAM-TPTABD(0,R3),3(R2)                                        
         B     UCTOK                                                            
*                                                                               
UCT230   WTO   TEXT=UCMS3H                                                      
         B     EXITOK                                                           
*                                                                               
UCTOK    WTO   TEXT=UCMS3H                                                      
         BRAS  RE,PRTCLST          PRINT OUT THE UPDATED CLASS INFO             
         B     EXITOK                                                           
*                                                                               
UCMS1H   DC    AL2(L'UCMS1)                                                     
UCMS1    DC    C'INVALID PARAMETER'                                             
*                                                                               
UCMS2H   DC    AL2(L'UCMS2)                                                     
UCMS2    DC    C'NO TP NAME DEFINED FOR THIS CLASS'                             
*                                                                               
UCMS3H   DC    AL2(L'UCMS3)                                                     
UCMS3    DC    C'THIS CLASS AND ITS TP INFO IS UPDATED'                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK THE SCHEDULE LIST FOR THE FIRST REQUEST TO ATTACH.            *         
* MAKE SURE IT HAS NOT YET BEEN ATTACHED BY ANOTHER MONSOON.          *         
* IF NOT, TURN ON THE JOB OUTPUT FLAG AND ATTACH IT.                  *         
***********************************************************************         
ATTACH   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*&&DO*&& BRAS  RE,PRTTAB           PRINT TRACE TABLES                           
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   *+14                                                             
         MVC   P(15),=CL15'STARTING ATTACH'                                     
         BRAS  RE,PRNT                                                          
*                                                                               
         CLI   ATTCHFLG,YES        ARE WE ATTACHING JCL?                        
         BNE   EXITOK              NO                                           
         MVI   JOBSLFTP,NO                                                      
*                                                                               
         L     R8,ASRTBLK          GET A(CURRENT SUBMIT LIST)                   
         SAM31                                                                  
         USING SRTD,R8                                                          
*                                                                               
ATT02    CLC   SRTD(SRTKEYL),EFFS  END OF SUBMITTED JOBS LIST                   
         BE    ATT32               YES                                          
         OC    SRTD(SRTKEYL),SRTD                                               
         BZ    ATT30               THIS JOB WAS ALREADY PROCESSED               
*&&US                                                                           
         CLI   SRTREPTN,X'FF'                                                   
         BE    ATT30               SKIP IT, EXCEED REPORT TYPE MAX              
         BRAS  RE,CHKMAX           CHECK IF EXCEED AGY/SUB MAX                  
         BNE   ATT30               SKIP THIS ONE                                
*&&                                                                             
         L     R7,AAPPCTAB         APPC/MVS CONTROL TABLE                       
ATT04    ICM   R0,15,APPCANXT                                                   
         BZ    ATT08               HERE'S A FREE CONVERSATION                   
         AHI   R7,APPCDLEN         BUMP TO NEXT CONVERSATION                    
         CLC   EOTMARK,0(R7)       END OF TABLE?                                
         BNE   ATT04               NO -- CHECK NEXT APPC TABLE ENTRY            
*                                                                               
ATT06    MVI   FREECONV,NO         NO FREE CONVERSATION AVAILABLE               
         MVI   JOBSLFTP,YES                                                     
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   EXITOK                                                           
         MVC   P(15),=CL15'NO CONVERSATIONS'                                    
         BRAS  RE,PRNT                                                          
         B     EXITOK              WE'LL HAVE TO WAIT                           
                                                                                
***********************************************************************         
* MAKE SURE THE CLASS CONVERSATION LIMIT IS NOT EXCEEDED                        
***********************************************************************         
ATT08    XC    CLASS,CLASS         GET CLASS TABLE ENTRY                        
X        USING CLASSD,CLASS                                                     
*&&CL*&& MVC   X.CLSNAME,SRTCLS                                                 
*&&AG*&& MVC   X.CLSNAME,SRTAGY                                                 
         GOTO1 VBINSRCH,BSP,CLASS                                               
         TM    BSPNF,X'80'         CLASS BETTER BE FOUND - SEE SCHEDULE         
         JNZ   *+2                                                              
         DROP  X                                                                
*                                                                               
         L     RF,BSP1             A(CLASS TABLE ENTRY)                         
         USING CLASSD,RF                                                        
         ICM   R1,15,CLSTP         A(TP NAME TABLE ENTRY)                       
         BNZ   *+8                                                              
         LA    R1,TPTAB            USE DEFAULT TP NAME                          
         DROP  RF                                                               
*                                                                               
         USING TPTABD,R1                                                        
         CLC   TPTMCN,TPTCON       ANY FREE CONVERSATIONS REMAINING?            
         BH    ATT10               YES                                          
         MVI   JOBSLFTP,YES        SOMETHING IS LEFT                            
         B     ATT30                                                            
         DROP  R1                                                               
*                                                                               
ATT10    TIME  BIN                                                              
         ST    R0,STRTTMRL         SET STARTING EXECUTION TIME                  
         BRAS  RE,LOCKBOTH         LOCK CLASS AND JOB TABLE                     
*                                                                               
         LAM   AR3,AR3,TBLET                                                    
         ICM   R3,15,SRTAJOB                                                    
         SAC   512                                                              
         USING TBJOBTAB,R3                                                      
         LHI   R0,1                                                             
         OC    TBJRTIME,TBJRTIME   ANOTHER MONSOON GRABBED JOB?                 
         BNZ   ATT16               YES - IGNORE IT                              
*                                                                               
         LHI   R0,2                                                             
         CLI   TBJSTAT,0           FLAGGED FOR DELETE?                          
         BE    ATT16               YES - IGNORE IT                              
*&&US*&& B     ATT10A                                                           
                                                                                
         CLI   ENQJOBNM,YES        ENQJOBNAME=YES                               
         BNE   ATT10A                                                           
*                                                                               
         TM    TBJSTAT,TBJIGNOR    IGNORE ENQJOBNAME                            
         BO    ATT10A                                                           
*                                                                               
         CPYA  AR2,AR3                                                          
         L     R2,=AL4(DTJOB)      LOCATE JOB TABLE                             
         NILF  GR2,X'000000FF'                                                  
         SLL   R2,6                X 64                                         
*                                                                               
         USING DMSPACED,R2                                                      
         L     R2,DSPTFRST                                                      
*                                                                               
         USING TABJOBS,R2                                                       
         L     R1,TBJNUM           NUMBER OF JOBS                               
         SR    R0,R0                                                            
         AHI   R2,L'TBJOBHDR       FIRST JOB                                    
***      AHI   R2,X'04C0'      ??? FOURTH JOB                                   
*                                                                               
OTHR     USING TBJOBTAB,R2                                                      
                                                                                
ENQJOB01 OC    0(4,R2),0(R2)       IS THIS A LIVE ENTRY                         
         BZ    ENQJOB03                                                         
         OC    OTHR.TBJETIME,OTHR.TBJETIME                                      
         BNZ   ENQJOB02                                                         
         OC    OTHR.TBJRTIME,OTHR.TBJRTIME                                      
         BZ    ENQJOB02                                                         
         CLC   TBJMVSID,OTHR.TBJMVSID                                           
         BNE   ENQJOB02                                                         
         BAS   RE,FREEBOTH         IGNORE FOR NOW                               
         B     ATT30                                                            
         DROP  OTHR                                                             
*                                                                               
ENQJOB02 LA    R2,L'TBJNTRY(,R2)                                                
         BCT   R1,ENQJOB01         THIS JOB IS CHECKED                          
         B     ATT10A                                                           
*                                                                               
ENQJOB03 LA    R2,L'TBJNTRY(,R2)                                                
         AHI   R0,1                SAFETY CHECK JOBCOUNT                        
         CHI   R0,5000                                                          
         BL    ENQJOB01                                                         
*                                                                               
ATT10A   LHI   R0,3                                                             
*&&CL*&& CLC   TBJCLASS,SRTCLS     REBUILT TABLE ON ME?                         
*&&CL*&& BNE   ATT16               YES - IGNORE IT                              
*                                                                               
*&&AG*&& CLC   TBJAGY,SRTAGY                                                    
*&&AG*&& BNE   ATT16                                                            
*                                                                               
         MVC   FULL(3),TBJSTIME    CHECK SUBMIT TIME MATCHES                    
         CLI   TBJSTIME,X'FF'      DISPLAY TIME REBUILT BY RERUN                
         BNE   ATT11               NO                                           
*                                                                               
         XR    RE,RE               CONVERT BACK TO REAL TIME                    
         ICM   RF,7,TBJSTIME                                                    
         NILF  GRF,X'0000FFFF'                                                  
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
         STCM  RF,7,FULL           AND SAVE IT                                  
*                                                                               
ATT11    CLC   SRTTIME,FULL        TIME MATCHES?                                
         BNE   ATT16               NO                                           
                                                                                
*---------------------------------------------------------------------*         
* HOOK HERE FOR COUNTING # RUNNING FOR THIS SET OF INITIALS                     
* OR THIS USERID OR ANYTHING ELSE YOU LIKE...                                   
*                                                                               
         B     ATT18               <== JUST REMOVE THIS                         
*---------------------------------------------------------------------*         
*                                                                               
ATT12    CPYA  AR2,AR3                                                          
         ICM   R2,15,SRTACLS       GET CLASS TABLE ENTRY                        
         USING JCLASSD,R2                                                       
         ICM   R4,15,JCFRUN        ANYTHING RUNNING FOR THIS CLASS?             
         BZ    ATT18               NO                                           
         CPYA  AR4,AR3                                                          
RUN      USING TBJOBTAB,R4                                                      
*                                                                               
ATT14    CLM   R4,15,JCLRUN        A(LAST RUNNING JOB)                          
         BE    ATT18                                                            
         ICM   R4,15,RUN.TBJNXT                                                 
         BNZ   ATT14                                                            
         DC    H'0'                THIS TABLE IS A MESS                         
         DROP  RUN                                                              
*                                                                               
ATT16    BRAS  RE,FREEBOTH     *** THIS JOB IS TO BE IGNORED                    
         MVI   JOBSLFTP,YES                                                     
         XC    SRTD(SRTRECL),SRTD  SO CLEAR ENTRY                               
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   ATT30                                                            
         MVC   P(17),=CL17'JOB BEING IGNORED'                                   
*                                                                               
         CHI   R0,1                R0 = 1 ANOTHER MONSOON GRABBED JOB           
         BNE   *+10                                                             
         MVC   P+30(28),=CL28'ANOTHER MONSOON GRABBED JOB'                      
         CHI   R0,2                R0 = 2 JOB FLAGGED FOR DELETE                
         BNE   *+10                                                             
         MVC   P+30(23),=CL23'JOB FLAGGED FOR DELETE'                           
         CHI   R0,3                R0 = 3 TABLE REBUILT                         
         BNE   *+10                                                             
         MVC   P+30(20),=CL20'TABLE WAS REBUILT'                                
*                                                                               
         BRAS  RE,PRNT                                                          
         B     ATT30               NEXT ENTRY IN SCHEDULE LIST                  
*                                                                               
ATT18    MVC   TBJRTIME,STRTTMRL+1 SET STARTING EXECUTION TIME                  
         BRAS  RE,ARSOFF                                                        
*                                                                               
         LAM   AR2,AR2,TBLET       NOW FIX JOBTABS START TIME                   
         ICM   R2,15,SRTAJOB                                                    
*NOP     SAC   512                                                              
*NOP     MVC   TBJRTIME-TBJNTRY(3,R2),TBJRTIME                                  
         DROP  R2,R3                                                            
*                                                                               
         ICM   R2,15,SRTACLS       NOW FIX UP (LOCKED) CLASS TABLE              
         SAC   512                                                              
         USING JCLASSD,R2                                                       
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,JCNSUB         ADJUST COUNTERS IN CLASS TABLE               
         BZ    *+10                                                             
         BCTR  R0,0                                                             
         STH   R0,JCNSUB                                                        
*                                                                               
         LH    R0,JCNRUN                                                        
         AHI   R0,1                                                             
         STH   R0,JCNRUN                                                        
         LH    R0,JCNTRUN                                                       
         AHI   R0,1                                                             
         STH   R0,JCNTRUN                                                       
*                                                                               
         CPYA  AR3,AR2                                                          
         ICM   R3,15,SRTAJOB       R3 = A(THIS JOB TABLE ENTRY)                 
ME       USING TBJOBTAB,R3                                                      
         CLM   R3,15,JCFSUB        1ST SUBMITTED JOB IN SUB QUEUE?              
         BNE   ATT20               NO - PITY                                    
*                                                                               
         MVC   JCFSUB,ME.TBJNXT    SET A(NEXT SUB OR 0) AS NEW 1ST SUB          
         XC    ME.TBJNXT,ME.TBJNXT                                              
         CLM   R3,15,JCLSUB                                                     
         BNE   *+10                                                             
         XC    JCLSUB,JCLSUB       CLEAR ADDRESS IF I WAS ONLY JOB              
         B     ATT24                                                            
*                                                                               
ATT20    CPYA  AR4,AR3                                                          
         ICM   R4,15,JCFSUB        FIRST SUBMITTED JOB                          
PS       USING TBJOBTAB,R4         PS = PRIOR SUBMITTED JOB                     
*                                                                               
ATT22    CLM   R3,15,PS.TBJNXT     GET JOB THAT POINTS TO ME                    
         BE    ATT23                                                            
         ICM   R4,15,PS.TBJNXT                                                  
         BNZ   ATT22                                                            
         B     ATT16                                                            
*                                                                               
ATT23    MVC   PS.TBJNXT,ME.TBJNXT FIX UP LINKS (TAKE ME OUT)                   
         XC    ME.TBJNXT,ME.TBJNXT                                              
*                                                                               
         CLM   R3,15,JCLSUB        WAS I THE LAST SUBMITTED?                    
         BNE   *+8                 NO                                           
         STCM  R4,15,JCLSUB        FIX LIST TAIL                                
         LAM   AR4,AR4,ARZERO                                                   
         DROP  PS,ME                                                            
*                              *** ADD TO END OF LIST OF RUNNING JOBS           
ATT24    OC    JCFRUN,JCFRUN       ANY OTHER JOBS RUNNING?                      
         BNZ   ATT26               YES                                          
         STCM  R3,15,JCFRUN        SET THIS AS 1ST RUNNING                      
         STCM  R3,15,JCLRUN        SET THIS AS LAST RUNNING                     
         B     ATT28                                                            
*                                                                               
ATT26    ICM   R4,15,JCLRUN        GET A(LAST RUNNING)                          
         CPYA  AR4,AR3                                                          
LR       USING TBJOBTAB,R4         LR = LAST RUNNING JOB                        
         STCM  R3,15,LR.TBJNXT     SET POINTER IN (OLD) LAST RUNNING            
         STCM  R3,15,JCLRUN        SET ME AS NEW LAST RUNNING JOB               
         DROP  R2,LR                                                            
*                                                                               
ATT28    BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEBOTH         UNLOCK CLEAN TABLES                          
         BRAS  RE,ARSOFF                                                        
*                                                                               
* CLASS LIST IS FIXED UP AND JOB IS FLAGGED AS STARTED SO NO-ONE ELSE           
* WILL TRY TO RUN IT                                                            
* NOW DO THE APPC VOODOO TO GET IT ACTUALLY RUNNING                             
*                                                                               
         MVC   OPMS1H,=AL2(L'OPMS1)                                             
         MVC   OPMS1,SPACES        FORMAT THE OPERATOR MESSAGE                  
         MVC   OPMS1(7),=C'RUNNING'                                             
*&&CL*&& MVC   OPMS1+30(2),SRTCLS JOB CLASS                                     
*&&AG*&& MVC   OPMS1+30(2),SRTAGY AGENCY ALPHA                                  
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,SRTAJOB                                                    
         SAC   512                                                              
         USING TBJOBTAB,R2                                                      
         MVC   HALF,TBJPQUSR       FOR GUSERNAM CALL BELOW                      
*                                                                               
         MVI   OPMS1+16,C','       INITIALS                                     
         MVC   OPMS1+17(3),TBJPQSUB                                             
         MVI   OPMS1+20,C','                                                    
*                                                                               
         MVC   APPCPQ,TBJPQID      SAVE PQ # IN APPCTAB                         
         XR    R0,R0                                                            
         ICM   R0,3,TBJPQSEQ       REPORT NUMBER                                
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
         EDIT  (R0),(5,OPMS1+21)                                                
*                                                                               
         BRAS  RE,GUSERNAM         USES HALF FROM ABOVE                         
         MVC   OPMS1+8(8),USERID   SIGNON ID                                    
*                                                                               
         MVI   OPMS1+36,C'('                                                    
         EDIT  CONVNUM,(3,OPMS1+37),ALIGN=LEFT                                  
         LR    RE,R0                                                            
         LA    RE,OPMS1+37(RE)                                                  
         MVI   0(RE),C')'                                                       
*                                                                               
         L     RF,REQUEST_TABLE_ENTRY                                           
         MVC   0(SRTRECL,RF),SRTD                                               
         XC    SRTD(SRTRECL),SRTD  SAVE ENTRY AND CLEAR FROM LIST               
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   *+20                                                             
         MVC   P+00(14),=CL14'TRY TO LAUNCH'                                    
         MVC   P+30(L'OPMS1),OPMS1                                              
         BRAS  RE,PRNT                                                          
*                                                                               
         BRAS  RE,LAUNCH           LAUNCH A MONSOON TO EXECUTE REQUEST          
*                                                                               
ATT30    AHI   R8,SRTRECL          NEXT IN LIST OF SCHEDULED JOBS               
         OC    REFFILT,REFFILT     WAS MONSTER FOR ONE REPORT ONLY?             
         BZ    ATT02               ATTACH NEXT REQUEST                          
         MVI   OPERSTOP,YES        STOP FURTHER ATTACHMENT                      
         B     EXITOK                                                           
*                                                                               
ATT32    CLC   SRTD(SRTRECL),EFFS  REACHED END OF TABLE?                        
         BE    *+8                 YES                                          
         MVI   JOBSLFTP,YES        SET STILL WORK LEFT TO DO                    
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   EXITOK                                                           
         MVC   P(17),=CL17'ATTCH OK MORE -  '                                   
         MVC   P+16(1),JOBSLFTP                                                 
         BRAS  RE,PRNT                                                          
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* LAUNCH JOB                                                          *         
***********************************************************************         
LAUNCH   NTR1  BASE=*,LABEL=*                                                   
         CLI   JESMSG,YES          ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   LCH02               NO                                           
         WTO   TEXT=OPMS1H                                                      
*                                                                               
LCH02    SAM31                                                                  
*                                                                               
*&&UK*&& STIMERM SET,ID=STIMER1,BINTVL=ONESEC,WAIT=YES                          
*                                                                               
         L     R8,REQUEST_TABLE_ENTRY                                           
         USING SRTD,R8                                                          
*                                                                               
         XC    CLASS,CLASS                                                      
*&&CL*&& MVC   CLASS(2),SRTCLS     PUT CLASS CODE INTO 'RECORD'                 
*&&AG*&& MVC   CLASS(2),SRTAGY     PUT AGY ALPHA INTO 'RECORD'                  
         GOTO1 VBINSRCH,BSP,CLASS                                               
         TM    BSPNF,X'80'         CLASS FOUND?                                 
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         L     RF,BSP1             A(CLASS TABLE ENTRY)                         
         USING CLASSD,RF                                                        
         ICM   R1,15,CLSTP         A(TP NAME TABLE ENTRY)                       
         BNZ   *+8                                                              
         LA    R1,TPTAB            USE DEFAULT TP NAME                          
         DROP  RF                                                               
*                                                                               
         USING TPTABD,R1                                                        
         LH    RE,TPTLEN                                                        
         ST    RE,TP_NAME_LENGTH                                                
         MVC   TP_NAME,TPTNAM                                                   
         LH    RE,TPTCON                                                        
         AHI   RE,1                                                             
         STH   RE,TPTCON                                                        
         DROP  R1                                                               
*                                                                               
         MVC   APPCANXT,=A(CONFALLC)                                            
         LA    R2,ATBALC2_STRUCTURE                                             
         SAM24                                                                  
         BRAS  RE,CALLAPPC         ALLOCATE THE CONVERSATION                    
         B     EXITOK                                                           
         DROP  R8                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* CONFIRM ALLOCATION                                                  *         
***********************************************************************         
CONFALLC NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBALC2_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         ICM   R0,15,RETURN_CODE                                                
         EDIT  (R0),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         BRAS  RE,PRNT                                                          
*                                                                               
         ICM   R0,15,RETURN_CODE                                                
         BZ    *+12                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
         MVC   APPCANXT,=A(PRNTATTB)                                            
         LA    R2,ATBCFM_STRUCTURE                                              
         BRAS  RE,CALLAPPC         REQUEST CONFIRMATION                         
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT OUT REPORT DETAILS TO PASS THROUGH                            *         
***********************************************************************         
PRNTATTB NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBCFM_STRUCTURE                                              
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         ICM   R0,15,RETURN_CODE                                                
         EDIT  (R0),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         BRAS  RE,PRNT                                                          
*                                                                               
         ICM   R0,15,RETURN_CODE   RETURN CODE WAS OK                           
         BZ    PABB02                                                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
PABB02   EQU   *                                                                
         CLI   REPORT_STATUS,REPORT_TRANSMIT_BAD                                
         BNE   PABB04                                                           
*                                                                               
         MVI   REPORT_STATUS,REPORT_TRANSMIT_OK                                 
         EDIT  CONVNUM,(3,PRMS1NO),ALIGN=LEFT                                   
         LA    RE,PRMS1NO                                                       
         AR    RE,R0                                                            
         MVI   0(RE),C')'                                                       
         WTO   TEXT=PRMS1H         OUTPUT MESSAGE                               
*                                                                               
PABB04   LA    R2,ATBGTA2_STRUCTURE                                             
         BRAS  RE,CALLAPPC                                                      
*                                                                               
         MVC   P(16),4(R2)                                                      
         MVC   P+30(14),=C'RETURN CODE = '                                      
         ICM   R0,15,RETURN_CODE                                                
         EDIT  (R0),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         BRAS  RE,PRNT                                                          
*                                                                               
         ICM   R0,15,RETURN_CODE                                                
         BZ    *+12                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION     *         
***********************************************************************         
PABB06   CLI   TRACEFLG,YES                                                     
         BNE   PABB14                                                           
*                                                                               
         MVC   P(17),STARS                                                      
         BRAS  RE,PRNT                                                          
         MVC   P(17),=CL17'GET ATTRIBUTES   '                                   
         BRAS  RE,PRNT                                                          
         MVC   P(17),STARS                                                      
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P(17),=CL17'CONVERSATION ID  '                                   
         GOTO1 VHEXOUT,DMCB,CONVERSATION_ID,P+30,4,0                            
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+30(17),PARTNER_LU_NAME                                         
         MVC   P+00(17),=CL17'PARTNER_LU_NAME'                                  
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+00(17),=CL17'MODE_NAME'                                        
         MVC   P+30(8),MODE_NAME                                                
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    PABB08                                                           
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    PABB08              UNKNOWN SYNC LEVEL, FREE CONVERSN            
         BRAS  RE,FREECON                                                       
         B     EXITOK                                                           
*                                                                               
PABB08   MVC   P+00(17),=CL17'SYNC_LEVEL'                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+00(17),=CL17'TP_NAME'                                          
         MVC   P+30(64),TP_NAME                                                 
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+00(17),=CL17'LOCAL_LU_NAME'                                    
         MVC   P+30(8),LOCAL_LU_NAME                                            
         BRAS  RE,PRNT                                                          
*                                                                               
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    PABB10                                                           
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    PABB10              UNKNOWN CONV TYPE, FREE CONVERSN             
         BRAS  RE,FREECON                                                       
         B     EXITOK                                                           
*                                                                               
PABB10   MVC   P+00(17),=CL17'CONVERSATION_TYPE'                                
         BRAS  RE,PRNT                                                          
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    PABB12                                                           
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    PABB12                                                           
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    PABB12                                                           
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    PABB12                                                           
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    PABB12                                                           
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    PABB12                                                           
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    PABB12                                                           
         BRAS  RE,FREECON          UNKNOWN CONV STATE, FREE CONVERSN            
         B     EXITOK                                                           
*                                                                               
PABB12   MVC   P+00(17),=CL17'CONVERSATION_STATE'                               
         BRAS  RE,PRNT                                                          
         MVC   P(17),STARS                                                      
         BRAS  RE,PRNT                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD A CONTROL BUFFER AND SEND IT TO THE ASCH                      *         
***********************************************************************         
         PUSH  USING                                                            
PABB14   XC    BUFFER,BUFFER                                                    
         USING MONSD,BUFFER                                                     
         MVC   MNSCMD,=CL8'*HEADER*'                                            
         SAM31                                                                  
*                                                                               
         L     R8,REQUEST_TABLE_ENTRY                                           
         USING SRTD,R8                                                          
         MVC   FULL,SRTAJOB                                                     
         GOTO1 VHEXOUT,DMCB,FULL,MNSAJOB,4,0                                    
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,SRTAJOB                                                    
         SAC   512                                                              
         USING TBJOBTAB,R2                                                      
         MVC   HALF,TBJPQUSR                                                    
         BRAS  RE,ARSOFF                                                        
*                                                                               
         BRAS  RE,GUSERNAM                                                      
         MVC   MNSUSER,USERID                                                   
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,SRTAJOB                                                    
         SAC   512                                                              
         USING TBJOBTAB,R2                                                      
         MVC   MNSPRTQ,TBJPQID                                                  
*                                                                               
         MVC   PQTIME,TBJSTIME     SAVE SUBMIT TIME                             
         CLI   TBJSTIME,X'FF'      CONVERT TIME REBUILT BY RERUN                
         BNE   PABB16              NO                                           
*                                                                               
         XR    RE,RE               CONVERT BACK TO REAL TIME                    
         ICM   RF,7,TBJSTIME                                                    
         NILF  GRF,X'0000FFFF'                                                  
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
         STCM  RF,7,PQTIME         AND SAVE IT                                  
*                                                                               
PABB16   MVC   BYTE,TBJADV                                                      
         NI    BYTE,X'0F'                                                       
         XR    RF,RF                                                            
         IC    RF,BYTE                                                          
         MHI   RF,L'FACITAB                                                     
         A     RF,AFACID           SUPPORTS FACIDTAB AND FACIDTABL              
         USING FACITABD,RF                                                      
         MVC   MNSFAC,FACISN1                                                   
         DROP  RF                                                               
*                                                                               
         XR    R0,R0               EDIT OUT USER ID                             
         ICM   R0,3,TBJPQUSR                                                    
         BRAS  RE,CVDR0                                                         
         UNPK  MNSPQ#,DUB                                                       
*                                                                               
         MVC   MNSPQSUB,TBJPQSUB                                                
         MVC   MNSCLS,TBJCLASS                                                  
*                                                                               
         XR    R0,R0               EDIT OUT REPORT NUMBER                       
         ICM   R0,3,TBJPQSEQ                                                    
         BRAS  RE,CVDR0                                                         
         UNPK  MNSPQSEQ,DUB                                                     
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
         GOTO1 VHEXOUT,DMCB,PQTIME,MNSRTIME,3,0                                 
*                                                                               
         MVC   MNSPRI,SRTPRI                                                    
         XI    MNSPRI,X'FF'                                                     
         MVC   MNSTRC,TRACEFLG                                                  
         MVC   MNSJES,JESMSG                                                    
*        MVC   MNSQTYPE,QTYPE                                                   
*&&US*&& MVC   MNSQTYPE,SVJCTYPE                                                
         MVC   MNSDDSIO,DDSIOMOD                                                
*                                                                               
         L     R0,REQTIMER                                                      
         BRAS  RE,CVDR0                                                         
         UNPK  MNSREQT,DUB                                                      
*                                                                               
         MVC   MNSDSPC,DSPACE                                                   
         MVC   MNSDMPW,DUMPWARN                                                 
         MVC   MNSMONS,MINORNAM                                                 
         GOTO1 VHEXOUT,DMCB,MAXETTM,MNSET,4,0                                   
         GOTO1 (RF),(R1),MAXTCBTM,MNSTCBT,4,0                                   
         MVI   MNSLIM,YES          LIMIT = Y                                    
         SAM24                                                                  
         DROP  R8                                                               
*                                                                               
         CLI   WKNFLAG,YES         IS TODAY WEEKEND/HOLIDAY?                    
         BNE   PABB18              NO                                           
         CLI   UNLMWKND,YES        UNLIMITED WEEKEND/HOLIDAY?                   
         BNE   PABB20              NO                                           
         MVI   MNSLIM,NO           SET LIMIT = N                                
         B     PABB20                                                           
*                                                                               
PABB18   CLC   UNLMNGHT,PRNTTIME   TIME TO RELAX RESTRICTION?                   
         BH    PABB20              NOT YET                                      
         MVI   MNSLIM,NO           SET LIMIT = N                                
*                                                                               
PABB20   MVC   MONMODE,MNSLIM                                                   
         LHI   R4,MONSDLQ                                                       
         ST    R4,SEND_LENGTH                                                   
         MVC   SEND_TYPE,ATB_SEND_AND_PREP_TO_RECEIVE                           
*                                                                               
*&&UK*&& CLI   TRACEFLG,YES                                                     
*&&UK*&& BNE   PABB22                                                           
         MVC   P+00(30),=CL30'SEND AND PREPARE TO RECEIVE'                      
         GOTO1 VPRINTER                                                         
         MVC   P+02(MONSDLQ),MONSD                                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
PABB22   MVC   APPCANXT,=A(RPTSNT)                                              
         LA    R2,ATBSEND_STRUCTURE                                             
         BRAS  RE,CALLAPPC         SEND CONTROL INFO                            
         B     EXITOK                                                           
         POP   USING                                                            
*                                                                               
PRMS1H   DC    AL2(PRMS1LQ)                                                     
PRMS1    DC    C'APPC COMMUNICATION RECOVERED ('                                
PRMS1NO  DC    CL4'    '                                                        
PRMS1LQ  EQU   *-PRMS1                                                          
         EJECT                                                                  
***********************************************************************         
* RESPONSE FROM MONSOON AFTER REPORT HAS BEEN SENT ACROSS             *         
***********************************************************************         
RPTSNT   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBSEND_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         ICM   R0,15,RETURN_CODE                                                
         EDIT  (R0),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         BRAS  RE,PRNT                                                          
*                                                                               
         ICM   R0,15,RETURN_CODE                                                
         BZ    *+12                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         MVC   APPCANXT,=A(GETJOBID)                                            
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         WAIT TO RECEIVE ASCH JOB ID                  
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT JOB ID FROM ASSOCIATED MONSOON                    *         
***********************************************************************         
GETJOBID NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBRCVW_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    GJID02              RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
GJID02   CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BNE   *+12                                                             
         BRAS  RE,FREECON                                                       
         B     EXITOK                                                           
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    *+12                                                             
         BRAS  RE,FREECON                                                       
         B     EXITOK                                                           
*                                  ALWAYS PRINT THIS INFO                       
*&&UK*&& CLI   TRACEFLG,C'Y'                                                    
*&&UK*&& BNE   GJID04                                                           
         MVC   P(25),=CL25'RECEIVED ASCH JOB ID'                                
         GOTO1 VPRINTER                                                         
         L     R2,RECEIVE_LENGTH                                                
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   P(0),BUFFER                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
GJID04   MVC   APPCANXT,=A(CONF2)                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BRAS  RE,CALLAPPC         ISSUE CONFIRM                                
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ISSUE CONFIRM                                                       *         
***********************************************************************         
CONF2    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBCFMD_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    CONF210             RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
CONF210  MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         MVC   APPCANXT,=A(GETANSR)                                             
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         WAIT FOR RESPONSE FROM MONSOON               
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET RESPONSE FROM MONSOON ON COMPLETION OF SCHEDULED JOB            *         
***********************************************************************         
GETANSR  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBRCVW_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
*                                                                               
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LA    RE,P+51                                                          
         AR    RE,R0                                                            
         MVI   0(RE),C')'                                                       
*                                                                               
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE  TEST GOOD (ZERO) RETURN CODE            
         BZ    *+12                     YES                                     
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
         CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BE    GANSR02                                                          
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_SEND_RECEIVED                        
         BE    GANSR04                                                          
         CLC   STATUS_RECEIVED,ATB_CONFIRM_RECEIVED                             
         BE    GANSR04                                                          
*                                                                               
GANSR02  BRAS  RE,FREECON                                                       
         B     EXITOK                                                           
*                                                                               
GANSR04  CLI   TRACEFLG,YES        DUMP BUFFER ONLY IF TRACING                  
         BNE   GANSR06                                                          
*                                                                               
         L     R0,RECEIVE_LENGTH                                                
         GOTO1 VPRNTBL,DMCB,=C'RECEIVED',BUFFER,C'DUMP',(R0),=C'1D'             
*                                                                               
GANSR06  L     R2,RECEIVE_LENGTH                                                
         AHI   R2,-12                                                           
         L     RE,BUFFER(R2)       AVOIDLD FROM MONSOONM                        
         A     RE,LOADAVD                                                       
         ST    RE,LOADAVD                                                       
         L     RE,BUFFER+4(R2)     TOTALLD FROM MONSOONM                        
         A     RE,LOADASK                                                       
         ST    RE,LOADASK                                                       
         LA    RE,BUFFER+8(R2)     TOTALLD FROM MONSOONM                        
         MVC   ABENDFLG,0(RE)      ABENDFLG FROM MONSOONM                       
         MVC   BYTE2,1(RE)                                                      
         CLC   1(1,RE),2(RE)       TCBTMFLG = ETTMFLG                           
         BE    *+8                                                              
         MVI   BYTE2,YES           EITHER ONE MUST BE 'Y'                       
*                                                                               
         CLI   ABENDFLG,YES        JOB ABENDED?                                 
         BNE   GANSR08                                                          
         GOTO1 VLOGIO,DMCB,X'FF000001',(60,BUFFER+32)                           
*                                                                               
         BRAS  RE,CNTABEND         CHECK IF TOO MANY ABEND                      
*                                                                               
GANSR08  LA    R2,ATBCFMD_STRUCTURE                                             
         BRAS  RE,CALLAPPC         ISSUE CONFIRM                                
*                                                                               
         MVC   APPCANXT,=A(DEALLOC)                                             
         CLI   BYTE2,YES           NEXT BUFFER IS LONG SOON JCLFILE             
         BNE   *+10                                                             
         MVC   APPCANXT,=A(LONGSOON)                                            
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REPORT ON LONG RUNNING SOONS                             *         
***********************************************************************         
LONGSOON NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBCFMD_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+12                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(BIGBUFLQ)                                      
*                                                SYNC APPC CALL                 
ATBP     USING CALLAPPC_BLOCK,ATBRCVW_STRUCTURE                                 
         MVC   ATBP.ATBRCVW_BUFFER,ABIGBUF       CHANGE BUFFER ADR              
         MVC   ATBP.ATBRCVW_NOTIFY_TYPE,=A(ATB_NOTIFY_TYPE_NONE)                
         LA    R2,ATBRCVW_STRUCTURE                                             
         BRAS  RE,CALLAPPC         WAIT FOR RESPONSE FROM MONSOON               
*                                                                               
         LA    R1,BUFFER                RESTORE OLD BUFFER ADR                  
         ST    R1,ATBP.ATBRCVW_BUFFER   CHANGE BUFFER ADR                       
         LA    R1,NOTIFY_TYPE                    CHANGE BACK TO ASYNC           
         ST    R1,ATBP.ATBRCVW_NOTIFY_TYPE                                      
         DROP  ATBP                                                             
*                                  RECEIVE THE BIG BUFFER                       
         LA    R2,ATBRCVW_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    LNGS02              RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
LNGS02   CLC   DATA_RECEIVED,ATB_NO_DATA_RECEIVED                               
         BE    LNGS04                                                           
*                                                                               
         CLC   STATUS_RECEIVED,ATB_CONFIRM_SEND_RECEIVED                        
         BE    LNGS06                                                           
*                                                                               
LNGS04   XC    APPCANXT,APPCANXT                                                
         XC    REQUEST_TABLE_ENTRY,REQUEST_TABLE_ENTRY                          
         MVC   OPMS1H,=AL2(L'OPMS1)                                             
         MVC   OPMS1,=CL42'NO REQUEST RUNNING NOW'                              
         B     EXITOK                                                           
*                                                                               
LNGS06   CLI   TRACEFLG,YES                                                     
         BNE   LNGS08                                                           
         GOTO1 VPRINTER            PRINT SPACE LINE                             
         MVC   P+00(80),STARS                                                   
         MVC   P+02(L'LSMSG),LSMSG                                              
         GOTO1 VPRINTER            LONG SOON MESSAGE                            
         GOTO1 VPRINTER            PRINT SPACE LINE                             
*                                                                               
LNGS08   L     R8,REQUEST_TABLE_ENTRY                                           
         SAM31                                                                  
         USING SRTD,R8                                                          
*                                                                               
         L     R3,ABIGBUF                                                       
*??      MVC   FJBSYPGM-FKEYD(L'FJBSYPGM,R3),REQSYPGM                           
         MVC   FKEYLQ+1(34,R3),DSN+10    PUT OUT LONG SOON DSN                  
*                                                                               
         LA    R4,MAXRECQ                                                       
LNGS10   OC    0(L'BIGBUF,R3),0(R3)      ANY MORE RECORDS?                      
         BZ    LNGS14                    NO                                     
         PUT   FILE,0(R3)                                                       
*                                                                               
         CLI   TRACEFLG,YES                                                     
         BNE   LNGS12                                                           
         MVI   P,C'*'                                                           
         MVC   P+2(L'BIGBUF),0(R3)                                              
         GOTO1 VPRINTER                                                         
*                                                                               
LNGS12   AHI   R3,L'BIGBUF                                                      
         BCT   R4,LNGS10                                                        
*                                                                               
LNGS14   CLI   TRACEFLG,YES                                                     
         BNE   LNGS16                                                           
         MVC   P+00(80),STARS                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
LNGS16   GOTO1 VPRINTER            PRINT SPACE LINE UNDERNEATH                  
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BRAS  RE,CALLAPPC         ISSUE CONFIRM                                
         MVC   APPCANXT,=A(DEALLOC)                                             
         B     EXITOK                                                           
*                                                                               
LSMSG    DC    C' LONG SOON JOB - DETAILS FOLLOW '                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* DEALLOCATE THE CONVERSATION                                         *         
***********************************************************************         
DEALLOC  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBCFMD_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+12                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
DALLC10  MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
*                                                                               
         MVC   APPCANXT,=A(CLEANUP)                                             
         LA    R2,ATBDEAL_STRUCTURE                                             
         BRAS  RE,CALLAPPC         DEALLOCATE THE CONVERSATION                  
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAN UP AFTER JOB HAS COMPLETED                         *         
***********************************************************************         
CLEANUP  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ATBDEAL_STRUCTURE                                             
         MVC   P(16),4(R2)                                                      
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LR    RE,R0                                                            
         LA    RE,P+51(RE)                                                      
         MVI   0(RE),C')'                                                       
         MVC   P+30(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                     
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE,RETURN_CODE                                          
         BZ    *+12                RETURN CODE WAS OK                           
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
         MVC   P+00(14),=CL14'DEALLOCATED OK'                                   
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R8,REQUEST_TABLE_ENTRY                                           
         SAM31                                                                  
         USING SRTD,R8                                                          
*                                                                               
         TIME  BIN                                                              
         ST    R0,FULL1                                                         
*                                                                               
         LA    RF,CLASS                                                         
         USING CLASSD,RF                                                        
         XC    CLASS,CLASS                                                      
*&&CL*&& MVC   CLSNAME,SRTCLS      FIND CLASS TABLE ENTRY                       
*&&AG*&& MVC   CLSNAME,SRTAGY                                                   
         MVI   BSP4,X'00'                                                       
         GOTO1 VBINSRCH,BSP,CLASS                                               
         TM    BSPNF,X'80'         CLASS FOUND?                                 
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          A(CLASS TABLE ENTRY)                         
         USING CLASSD,RF                                                        
         LH    R1,CLSTOTAL                                                      
         AHI   R1,1                                                             
         STH   R1,CLSTOTAL         INCREMENT TOTAL # FOR THIS CLASS             
         DROP  RF                                                               
*                                                                               
         L     R1,TOTREQS          INCREMENT TOTAL # REQUESTS PROCESSED         
         AHI   R1,1                                                             
         ST    R1,TOTREQS                                                       
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,LOCKBOTH         LOCK CLASS TABLE                             
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET       NOW FIX UP (LOCKED) CLASS TABLE              
         ICM   R2,15,SRTACLS                                                    
         SAC   512                                                              
         USING JCLASSD,R2                                                       
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,JCNRUN         ADJUST # OF RUNNING JOBS                     
         BNP   *+10                                                             
         BCTR  R0,0                                                             
         STCM  R0,3,JCNRUN                                                      
*                                                                               
         LH    R0,JCNRDY           ADJUST READY QUEUE                           
         AHI   R0,1                                                             
         STH   R0,JCNRDY                                                        
         LH    R0,JCNTRDY          ADJUST NOTIFY QUEUE                          
         AHI   R0,1                                                             
         STH   R0,JCNTRDY                                                       
*                                                                               
         CPYA  AR3,AR2                                                          
         ICM   R3,15,SRTAJOB       R3 = A(THIS JOB TABLE ENTRY)                 
         USING TBJOBTAB,R3                                                      
*&&CL*&& CLC   JCCLASS,TBJCLASS    MATCH CLASS                                  
*&&AG*&& CLC   JCAGY,TBJAGY        MATCH AGY ALPHA                              
         BNE   CLN10               NO - TABLES REBUILT                          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(3),TBJSTIME    CHECK SUBMIT TIME MATCHES                    
         CLI   TBJSTIME,X'FF'      DISPLAY TIME REBUILT BY RERUN                
         BNE   CLN02               NO                                           
*                                                                               
         XR    RE,RE               CONVERT BACK TO REAL TIME                    
         ICM   RF,7,TBJSTIME                                                    
         NILF  GRF,X'0000FFFF'                                                  
         SLL   RF,2                *4                                           
         D     RE,=F'3'            /3                                           
         MHI   RF,100                                                           
         STCM  RF,7,FULL           AND SAVE IT                                  
*                                                                               
CLN02    CLC   SRTTIME,FULL        TIME MATCHES?                                
         BNE   CLN10               NO - TABLES WERE REBUILT                     
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,7,FULL1+1        R0 = TIME NOW                                
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),TBJRTIME                                               
*                                                                               
         S     R0,FULL             R0 = ELAPSED REAL TIME IN 0.01 SEC           
         BNM   *+8                                                              
         A     R0,=A(24*3600*100)  ADD 24HRS, JUST WENT PAST MIDNIGHT           
         STCM  R0,7,TBJETIME                                                    
*                                                                               
         CLM   R3,15,JCFRUN        AM I 1ST RUNNING JOB IN RUN QUEUE?           
         BNE   CLN04               NO                                           
         TM    TBJSTAT,TBJHOLD     WAS THIS ENTRY ON HOLD?                      
         BZ    CLN03               NO, SO FIX UP LINKS                          
         CLI   TBJCTYPE,C'C'       COMSCORE JOB                                 
         BNE   CLN03               NO, SO NOT WHAT WE THOUGHT                   
         XC    TBJETIME,TBJETIME   CLEAR JOB RUN TIME FOR 2ND RUN               
         B     CLN14               SKIP THE CLEAN UP FOR NOW                    
*                                                                               
CLN03    MVC   JCFRUN,TBJNXT       SET A(NEXT RUN) AS NEW FIRST RUN             
*                                                                               
         CLM   R3,15,JCLRUN        WAS I THE ONLY JOB?                          
         BNE   *+10                NO                                           
         XC    JCLRUN,JCLRUN       CLEAR LIST TAIL                              
         B     CLN10                                                            
***********************************************************************         
* WE ARE LOOKING TO TAKE (ME) OUT OF LIST OF JOBS RUNNING NOW                   
* IN ORDER TO DO THIS WE LOOK FOR THE JOB JUST BEFORE (ME) IN THE LIST          
* AND MOVE THE POINTER TO THE ONE AFTER (ME) INTO IT NEATLY TAKING (ME)         
* OUT OF THE LIST. REMEMBER R3 POINTS TO (ME) THROUGHOUT.                       
*                                                                               
***********************************************************************         
CLN04    CPYA  AR4,AR3                                                          
         ICM   R4,15,JCFRUN        FIRST RUNNING JOB                            
         BZ    CLN10               CLASS TABLE WAS REBUILT ON YOU               
PR       USING TBJOBTAB,R4         PR = PRIOR RUNNING JOB                       
*                                                                               
CLN06    CLM   R3,15,PR.TBJNXT     GET JOB THAT POINTS TO ME                    
         BE    CLN08                                                            
         ICM   R4,15,PR.TBJNXT     CHECK TABLE IS OK                            
         BNZ   CLN06                                                            
         B     CLN10               TABLE IS BAD                                 
*                                                                               
CLN08    TM    TBJSTAT,TBJHOLD     WAS THIS ENTRY ON HOLD?                      
         BZ    CLN09               NO, SO FIX UP LINKS                          
         CLI   TBJCTYPE,C'C'       COMSCORE JOB                                 
         BNE   CLN09               NO, NOT RIGHT BUT CONTINUE FOR NOW           
         XC    TBJETIME,TBJETIME   CLEAR JOB RUN TIME FOR 2ND RUN               
         LH    R0,JCNRUN           ADJUST # OF RUNNING JOBS                     
         AHI   R0,1                ADD IT BACK                                  
         STH   R0,JCNRUN                                                        
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,JCNRDY         ADJUST READY QUEUE                           
         BNP   *+10                THIS SHOULDN'T HAPPEN                        
         BCTR  R0,0                REMOVE ONE                                   
         STH   R0,JCNRDY                                                        
         ICM   R0,3,JCNTRDY        ADJUST NOTIFY QUEUE                          
         BNP   *+10                THIS SHOULDN'T HAPPEN                        
         BCTR  R0,0                                                             
         STH   R0,JCNTRDY                                                       
         B     CLN14               LEAVE ENTRY AS IS (FOR PASS 2)               
*                                                                               
CLN09    MVC   PR.TBJNXT,TBJNXT    FIX UP LINKS (TAKE ME OUT)                   
         CLM   R3,15,JCLRUN        WAS I THE LAST RUNNING?                      
         BNE   *+8                 NO                                           
         STCM  R4,15,JCLRUN        FIX LIST TAIL                                
         LAM   AR4,AR4,ARZERO                                                   
         DROP  PR                                                               
*                                                                               
CLN10    XC    TBJNXT,TBJNXT                                                    
         TM    TBJSTAT,TBJKILL                                                  
         BO    CLN11                                                            
         TM    TBJSTAT,TBJERROR    JOB WAS FLAGGED AS IN ERROR?                 
         BZ    *+8                 NO                                           
         MVI   TBJSTAT,TBJERROR    FORCE ERROR CLEANUP                          
         B     CLN14       <<<< DEBUG - THE READY JOBS NEED FIXING              
*                                                                               
CLN11    XC    TBJETIME,TBJETIME   CLEAR ETIME IF KILL                          
         XC    TBJMONS,TBJMONS     CLEAR MONS IF KILL                           
         B     CLN14                                                            
*&&DO                                                                           
*                              *** ADD TO END OF LIST OF READY JOBS             
CLN10    XC    TBJNXT,TBJNXT                                                    
         OC    JCFRDY,JCFRDY       ANY OTHER JOBS READY?                        
         BNZ   CLN12               YES                                          
         STCM  R3,15,JCFRDY        SET THIS AS 1ST READY                        
         STCM  R3,15,JCLRDY        SET THIS AS LAST READY                       
         B     CLN14                                                            
*                                                                               
CLN12    ICM   R4,15,JCLRDY         GET A(LAST READY)                           
         CPYA  AR4,AR3                                                          
LR       USING TBJOBTAB,R4         LR = LAST READY JOB                          
         STCM  R3,15,LR.TBJNXT     SET POINTER IN (OLD) LAST READY              
         STCM  R3,15,JCLRDY        SET ME AS NEW LAST READY JOB                 
         DROP  LR                                                               
*&&                                                                             
CLN14    BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREEBOTH         UNLOCK TABLES                                
         BRAS  RE,ARSOFF                                                        
*                                                                               
         CLI   JESMSG,YES          ARE WE DISPLAYING CONSOLE MESSAGES?          
         BNE   CLN16               NO                                           
         MVC   OPMS1H,=AL2(L'OPMS1)                                             
         MVC   OPMS1(7),=CL7'ENDED'                                             
         WTO   TEXT=OPMS1H                                                      
*                                                                               
CLN16    BRAS  RE,FREECON                                                       
         XC    SRTD(SRTRECL),SRTD REMOVE FROM REQUEST TABLE                     
         B     EXITOK                                                           
         DROP  R8                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* FREE CONVERSATION                                                   *         
* NTRY: R7      = APPC TABLE ENTRY                                    *         
***********************************************************************         
         USING APPCD,R7                                                         
FREECON  NTR1  BASE=*,LABEL=*                                                   
         L     R8,REQUEST_TABLE_ENTRY                                           
         SAM31                                                                  
         USING SRTD,R8                                                          
         XC    CLASS,CLASS                                                      
*&&CL*&& MVC   CLASS(2),SRTCLS     PUT CLASS CODE INTO 'RECORD'                 
*&&AG*&& MVC   CLASS(2),SRTAGY                                                  
*                                                                               
         MVI   BSP4,X'00'                                                       
         GOTO1 VBINSRCH,BSP,CLASS                                               
         TM    BSPNF,X'80'         CLASS FOUND?                                 
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          A(CLASS TABLE ENTRY)                         
         USING CLASSD,RF                                                        
         ICM   R1,15,CLSTP         A(TP NAME TABLE ENTRY)                       
         BNZ   *+8                                                              
         LA    R1,TPTAB            USE DEFAULT TP NAME                          
         DROP  RF                                                               
*                                                                               
         USING TPTABD,R1                                                        
         LH    RE,TPTCON                                                        
         BCTR  RE,0                                                             
         STH   RE,TPTCON                                                        
         DROP  R1,R8                                                            
*                                                                               
         MVC   JOBNAME,SPACES      CLEAR OUT "JOBNAME"                          
*                                                                               
         XC    APPCANXT,APPCANXT                                                
*                                                                               
         MVC   OPMS1H,=AL2(L'OPMS1)                                             
         MVC   OPMS1,=CL42'NO REQUEST RUNNING NOW'                              
         MVI   OPMS1+36,C'('                                                    
         EDIT  CONVNUM,(3,OPMS1+37),ALIGN=LEFT                                  
         LR    RE,R0                                                            
         LA    RE,OPMS1+36(RE)                                                  
         MVI   0(RE),C')'                                                       
         MVI   FREECONV,YES                                                     
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*IF # ABENDS EXCEEDED THE LIMIT WITHIN A GIVEN # MIN, WTOR WARNING MSG          
***********************************************************************         
CNTABEND NTR1  BASE=*,LABEL=*                                                   
         TIME  DEC                                                              
         SRL   R0,16               R0 IN NOW 0000HHMM                           
         ST    R0,TNOW                                                          
         S     R0,TLAST            TIME DIFF IN MIN FROM LAST ABEND             
         C     R0,DCRT             COMPARE TIME ALLOWED                         
         BNH   CTAD20                                                           
*                                  LAST ABEND IS TOO LONG AGO                   
         MVC   DCNT,=F'1'          RESET DUMP COUNTER                           
         MVC   TLAST,TNOW          STORE THE TIME NOW                           
         B     EXITOK                                                           
*                                                                               
CTAD20   L     R1,DCNT             +1 DUMP COUNTER                              
         AHI   R1,1                                                             
         ST    R1,DCNT                                                          
*                                                                               
         CLC   DCNT,DLMT           EXCEED THE COUNTER LIMIT                     
         BNH   EXITOK              NO                                           
*                                                                               
         MVC   DCNT,=F'1'          RESET DUMP COUNTER                           
         MVC   TLAST,TNOW          STORE THE TIME NOW                           
         WTO   TEXT=CNTA1H         JUST MESSAGE AND EXIT IN UK                  
         J     EXITOK                                                           
                                                                                
***********************************************************************         
* DECATIVATE THIS CODE PER GRAHAM REQUEST. AUG2018                              
***********************************************************************         
         GOTO1 VDATCON,DMCB,(5,0),(12,CNTA1+53)                                 
         GOTO1 VLOGIO,DMCB,1,(L'CNTA1,CNTA1) WTOR OPERATOR                      
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)             READ BUT IGNORE REPLY         
         B     EXITOK                                                           
                                                                                
CNTA1H   DC    AL2(L'CNTA1)                                                     
CNTA1    DC    C'TOO MANY SOON DUMPS!  USE =3.4 TO FIND DATASETS SOON.M+        
               MMDD"             '                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* NTRY:  R2          = A(PARAMETER STRUCTURE)                         *         
*        R7          = APPC TABLE ENTRY                               *         
* EXIT:  RETURN_CODE = APPC/MVS RETURN CODE                           *         
***********************************************************************         
CALLAPPC NTR1  BASE=*,LABEL=*                                                   
         MVC   P(16),=C'ABOUT TO PERFORM'                                       
         MVC   P+30(16),4(R2)      PRINT ROUTINE NAME                           
         MVI   P+50,C'('                                                        
         EDIT  CONVNUM,(3,P+51),ALIGN=LEFT                                      
         LA    RE,P+51                                                          
         AR    RE,R0                                                            
         MVI   0(RE),C')'                                                       
         BRAS  RE,PRNT                                                          
*                                                                               
         SAM31                                                                  
         L     RF,0(R2)            RF = A(APPC/MVS ROUTINE)                     
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                                                                  
         CLC   RETURN_CODE,ATB_OK                                               
         BE    EXITOK              APPC/MVS CALL WAS ACCEPTED                   
*                                                                               
         MVC   P+00(17),=CL17'** BAD APPC CALL ** '                             
         MVC   P+30(16),4(R2)      PRINT NAME OF APPC/MVS ROUTINE               
         MVC   P+50(14),=C'RETURN CODE = '                                      
         EDIT  RETURN_CODE,(5,P+64),ALIGN=LEFT                                  
         BRAS  RE,PRNT                                                          
         BRAS  RE,APPCERR                                                       
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PERFORM APPC EXTRACT_ERROR FUNCTION.                                *         
* NTRY: R2     = PARAMETER STRUCTURE                                  *         
*       R7     = APPC TABLE ENTRY                                     *         
***********************************************************************         
         USING CALLAPPC_BLOCK,R2                                                
APPCERR  NTR1  BASE=*,LABEL=*                                                   
         EDIT  CONVNUM,(3,APEMS1+30),ALIGN=LEFT                                 
         LR    RE,R0                                                            
         LA    RE,APEMS1+30(RE)                                                 
         MVI   0(RE),C')'                                                       
         WTO   TEXT=APEMS1H                                                     
*                                                                               
         MVI   REPORT_STATUS,REPORT_TRANSMIT_BAD                                
*                                                                               
         L     R0,RETURN_CODE                                                   
         CHI   R0,26               SHUT DOWN MONSTER IF THIS ERROR              
         BE    *+14                                                             
         MVC   ERR26CNT,MAXCONV    RESET ERROR26 COUNT                          
         B     APPCE02A                                                         
*                                                                               
         CLI   ABENDFLG,YES                                                     
         BE    APPCE01                                                          
*                                                                               
         L     RE,ERR26TM          LOAD LAST TIME OF ERROR 26                   
         SR    R0,R0                                                            
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R0,ERR26TM          SAVE THIS TIME                               
         SR    R0,RE                                                            
         CHI   R0,5*60*100         > 5 MIN ?                                    
         BNH   *+14                NO                                           
         MVC   ERR26CNT,MAXCONV    RESET ERROR26 COUNT                          
         B     APPCE05                                                          
*                                                                               
         ICM   RE,15,ERR26CNT                                                   
         BZ    APPCE04             EXCESS LIMIT OF CONSECTIVE ERR26             
*                                                                               
         BCTR  RE,0                                                             
         ST    RE,ERR26CNT                                                      
         B     APPCE05                                                          
*                                                                               
APPCE01  MVC   ERR26CNT,MAXCONV    RESET ERROR26 COUNT                          
         MVI   ABENDFLG,NO                                                      
         MVC   P(44),=C'ERROR: ALLOCATE A TERMINATED MONSOON PARTNER'           
         GOTO1 VPRINTER                                                         
         MVI   JOBSLFTP,YES                                                     
         B     APPCE05                                                          
*                                                                               
APPCE02A CLC   AATBALC2,CALLAPPC_ROUTINE_ADDRESS ALLOCATE CALL?                 
         BNE   APPCE02B                                                         
         L     R0,RETURN_CODE                                                   
         CHI   R0,2                LU NOT ACTIVE PROBABLY?                      
         BNE   APPCE02B                                                         
         MVC   APEMS2+13(L'PARTLUID),PARTLUID                                   
         WTO   TEXT=APEMS2H                                                     
         B     APPCE04                                                          
*                                                                               
APPCE02B CLC   AATBCFM,CALLAPPC_ROUTINE_ADDRESS  CONFIRM CALL?                  
         BNE   APPCE05                                                          
         L     R0,RETURN_CODE                                                   
         CHI   R0,9                UNKNOWN TP NAME?                             
         BNE   APPCE02C                                                         
         MVC   APEMS3+16(L'TP_NAME),TP_NAME                                     
         WTO   TEXT=APEMS3H                                                     
         B     APPCE04                                                          
*                                                                               
APPCE02C L     R0,RETURN_CODE                                                   
         CHI   R0,10               SCHEDULER CLASS UNDEFINED?                   
         BNE   APPCE05                                                          
*                                                                               
         MVC   P+00(02),=AL2(L'APEMS4+L'TP_NAME)                                
         MVC   P+02(L'APEMS4),APEMS4                                            
         MVC   P+L'APEMS4+02(L'TP_NAME),TP_NAME                                 
         WTO   TEXT=P                                                           
         MVC   P,SPACES                                                         
*                                                                               
APPCE04  CLI   RECYLTPS,YES        RECYCLING ALL ASCH TP'S                      
         BE    APPCE06                                                          
         MVI   ESHTDOWN,YES                                                     
         STIMERM CANCEL,ID=STIMERID   CANCEL THE TIMER                          
         XC    TIMERECB,TIMERECB                                                
*                                                                               
         WTO   TEXT=APEMS5H        IGNORE THIS ERROR FOR NOW                    
*                                                                               
*        GOTO1 VLOGIO,DMCB,1,(L'OPMS19,OPMS19)                                  
*        GOTO1 VLOGIO,DMCB,0,(1,BYTE)                                           
*        ABEND 799,DUMP                                                         
*                                                                               
APPCE05  CLI   RECYLTPS,YES        RECYCLING ALL ASCH TP'S                      
         BE    *+8                                                              
         BRAS  RE,FREECON          FREE THIS CONVERSATION                       
*                                                                               
APPCE06  SAM31                                                                  
         LA    R2,ATBEES3_STRUCTURE                                             
         L     RF,CALLAPPC_ROUTINE_ADDRESS    RF = A(APPC/MVS ROUTINE)          
         LA    R3,CALLAPPC_ROUTINE_PARAMETERS R3 = A(PARAMETER LIST)            
         CALL  (15),MF=(E,(R3))                                                 
*                                                                               
         SAM24                                                                  
*                                                                               
         MVI   P,C'*'              '*' MEANS IT'S AN APPC/MVS CALL              
         MVC   P+1(16),CALLAPPC_ROUTINE_NAME                                    
         MVC   P+30(14),=C'RETURN CODE = '                                      
         ICM   R0,15,RETURN_CODE_ERROR_EXTRACT                                  
         EDIT  (R0),(5,P+44),ALIGN=LEFT,ZERO=NOBLANK                            
         BRAS  RE,PRNT                                                          
*                                                                               
         OC    RETURN_CODE_ERROR_EXTRACT,RETURN_CODE_ERROR_EXTRACT              
         BNZ   EXITOK              BAD RETURN CODE, SO NO ERROR INFO            
*                                                                               
         MVC   P+30(37),=C'APPC SERVICE XXXXXXXX, REASON_CODE = '               
         MVC   P+43(8),SERVICE_NAME                                             
         EDIT  SERVICE_REASON_CODE,(5,P+67),ALIGN=LEFT,ZERO=NOBLANK             
         MVC   P+00(17),=CL17'ERROR_EXTRACT       '                             
         BRAS  RE,PRNT                                                          
*                                                                               
         BRAS  RE,RERUN            RESUBMIT JOB IF RUNNING                      
*                                                                               
         ICM   R3,15,MESSAGE_TEXT_LENGTH                                        
         BZ    APPCE20             NOTHING IN FIELD                             
         LA    R2,MESSAGE_TEXT                                                  
*                                                                               
APPCE08  LR    RF,R3               PRINT OUT MESSAGE AS 132 CHAR LINES          
         CHI   RF,132              PRINT OUT MESSAGE AS 132 CHAR LINES          
         BNH   *+8                                                              
         LHI   RF,132                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R2)                                                       
         GOTO1 VPRINTER                                                         
         AHI   R2,132                                                           
         AHI   RF,1                                                             
         SR    R3,RF                                                            
         BP    APPCE08                                                          
*                                                                               
APPCE20  ICM   R3,15,ERROR_LOG_INFORMATION_LENGTH                               
         BZ    EXITOK              NOTHING IN FIELD                             
         LA    R2,ERROR_LOG_INFORMATION                                         
*                                                                               
APPCE22  LR    RF,R3               PRINT OUT ERROR LOG                          
         CHI   RF,132                                                           
         BL    *+8                                                              
         LHI   RF,132                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R2)                                                       
         GOTO1 VPRINTER                                                         
         AHI   R2,132                                                           
         AHI   RF,1                                                             
         SR    R3,RF                                                            
         BP    APPCE22                                                          
         B     EXITOK                                                           
*                                                                               
APEMS1H  DC    AL2(L'APEMS1)                                                    
APEMS1   DC    C'WARNING, APPC ERROR OCCURRED (     '                           
*                                                                               
APEMS2H  DC    AL2(L'APEMS2)                                                    
APEMS2   DC    C'INACTIVE LU, XXXXXXXXXXXXXXXXX'                                
*                                                                               
APEMS3H  DC    AL2(L'APEMS3)                                                    
APEMS3   DC    C'UNKNOWN TPNAME, XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX+        
               XXXXXXXXXXXXXXXXXXXXXXXXXX'                                      
*                                                                               
APEMS4   DC    C'UNDEFINED CLASS SCHEDULER FOR TP, '                            
*                                                                               
APEMS5H  DC    AL2(L'APEMS5)                                                    
*&&US                                                                           
APEMS5   DC    C'AUTONOTE*US-MF_FAC_NOTIFY:MONSTER: APPC ERROR!'                
*&&                                                                             
*&&UK                                                                           
APEMS5   DC    C'AUTONOTE*TCLE,RMOR:MONSTER: APPC ERROR!'                       
*&&                                                                             
*                                                                               
OPMS19   DC    C'APPC/TP ERROR!  JUST REPLY (Y), THEN RECYCLE THIS MONS+        
               TER'                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEUE RUNNING TRANSACTION ON ABEND                                *         
***********************************************************************         
RERUN    NTR1  BASE=*,LABEL=*                                                   
         MVC   P(17),=CL17'***RERUN***'                                         
         BRAS  RE,PRNT                                                          
*                                                                               
         L     R8,REQUEST_TABLE_ENTRY                                           
         SAM31                                                                  
         USING SRTD,R8                                                          
         OC    SRTD(SRTRECL),SRTD                                               
         BZ    EXITOK                                                           
*&&CL*&& OC    SRTCLS,SRTCLS                                                    
*&&AG*&& OC    SRTAGY,SRTAGY                                                    
         BZ    EXITOK                                                           
*                                                                               
         BRAS  RE,ARSOFF           LOCK TABLES                                  
         BRAS  RE,LOCKBOTH                                                      
         BRAS  RE,ARSOFF                                                        
*                                                                               
         ICM   R2,15,SRTACLS       POINT R2 TO CLASS TABLE IN TABS              
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING JCLASSD,R2                                                       
*&&CL*&& CLC   JCCLASS,SRTCLS      MATCH CLASS                                  
*&&AG*&& CLC   JCAGY,SRTAGY        MATCH AGY ALPHA                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    JCFRUN,JCFRUN       ANYTHING RUNNING?                            
         BZ    RER16               NO - TABLE REBUILT SO WE DON'T CARE          
*                                                                               
         CLC   JCFRUN,SRTAJOB      AM I THE FIRST ENTRY IN RUN QUEUE            
         BNE   RER04               NO                                           
         CLC   JCFRUN,JCLRUN       AM I THE ONLY ENTRY?                         
         BNE   RER02               NO                                           
         XC    JCFRUN,JCFRUN       CLEAR RUN QUEUE IN 1 FELL SWOOP              
         XC    JCLRUN,JCLRUN                                                    
         XC    JCNRUN,JCNRUN                                                    
         B     RER10                                                            
*                                                                               
RER02    CPYA  AR3,AR2             SET NEXT TO FIRST IN RUN QUEUE               
         ICM   R3,15,SRTAJOB                                                    
         USING TBJNTRY,R3                                                       
         MVC   JCFRUN,TBJNXT                                                    
         XC    TBJNXT,TBJNXT       CLEAR A(NEXT) OUT FROM ME                    
*                                                                               
         XR    R0,R0               REDUCE RUNNING JOB COUNT                     
         ICM   R0,3,JCNRUN                                                      
         BZ    RER10                                                            
         BCTR  R0,0                                                             
         STCM  R0,3,JCNRUN                                                      
         B     RER10                                                            
         DROP  R3                                                               
*                                                                               
RER04    CPYA  AR3,AR2             REMOVE ME FROM RUN QUEUE (IF THERE)          
         ICM   R3,15,JCFRUN                                                     
RUN      USING TBJNTRY,R3                                                       
*                                                                               
RER06    CLC   RUN.TBJNXT,SRTAJOB  DOES THIS ENTRY POINT TO ME?                 
         BE    RER08               YES                                          
         ICM   R3,15,RUN.TBJNXT                                                 
         BNZ   RER06                                                            
         B     RER10               I AM NOT IN THE RUN QUEUE                    
*                                                                               
RER08    CPYA  AR4,AR2                                                          
         ICM   R4,15,SRTAJOB       POINT R4 TO ME                               
         USING TBJNTRY,R4                                                       
         MVC   RUN.TBJNXT,TBJNXT   TAKE ME OUT OF RUN QUEUE                     
*                                                                               
         OC    TBJNXT,TBJNXT       WAS I THE LAST ENTRY?                        
         BNZ   *+8                 NO                                           
         STCM  R3,15,JCLRUN        SET NEW LAST ENTRY                           
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,JCNRUN         REDUCE # OF RUNNING JOBS                     
         BZ    *+10                                                             
         BCTR  R0,0                                                             
         STH   R0,JCNRUN                                                        
         DROP  RUN,R4                                                           
                                                                                
**********************************************************************          
* I AM NOT RUNNING ANY MORE - ADD ME BACK TO FRONT OF SUBMITTED QUEUE           
**********************************************************************          
RER10    LAM   AR3,AR4,ARZERO      CLEAR THESE                                  
         CPYA  AR3,AR2                                                          
         ICM   R3,15,SRTAJOB       R3 = A(JOB TABLE ENTRY)                      
         USING TBJNTRY,R3                                                       
*                                                                               
*&&CL*&& CLC   JCCLASS,TBJCLASS    MAKE SURE NOTHING WAS REBUILT ON YOU         
*&&AG*&& CLC   JCAGY,TBJAGY                                                     
         BE    RER12                                                            
*                                                                               
         XC    TBJMONS,TBJMONS                                                  
         XC    TBJRTIME,TBJRTIME                                                
         XC    TBJETIME,TBJETIME                                                
         B     RER16               LET RCVR DEAL WITH THIS ONE                  
*                                                                               
RER12    OC    TBJETIME,TBJETIME   JOB FINISHED BEFORE PROBLEM?                 
         BNZ   RER16               YES                                          
         OC    TBJMONS,TBJMONS     MONSTER FLAGGED ME ALREADY?                  
         BZ    *+12                NO                                           
         MVI   TBJSTAT,TBJERROR                                                 
         B     RER16                                                            
*                                                                               
         XC    TBJRTIME,TBJRTIME   SET NOT RUNNING (AGAIN)                      
*                                                                               
         LH    R0,JCNSUB                                                        
         AHI   R0,1                                                             
         STH   R0,JCNSUB           UP COUNT OF JOBS RUNNING                     
*                                                                               
         OC    JCFSUB,JCFSUB       QUEUE ALREADY EXISTS?                        
         BNZ   RER14               YES                                          
         STCM  R3,15,JCFSUB        SET ME AS ONLY ENTRY IN SUB QUEUE            
         STCM  R3,15,JCLSUB        SET ME AS ONLY ENTRY IN SUB QUEUE            
         LHI   R0,1                                                             
         STH   R0,JCNSUB                                                        
         B     RER16                                                            
*                                                                               
RER14    CPYA  AR4,AR3                                                          
         ICM   R4,15,JCLSUB                                                     
         BZ    RER16               LET RCVR DEAL WITH THIS                      
LS       USING TBJNTRY,R4                                                       
         STCM  R3,15,LS.TBJNXT                                                  
         STCM  R3,15,JCLSUB                                                     
         DROP  R2,R3,LS                                                         
*                                                                               
RER16    BRAS  RE,ARSOFF           FREE CLASS TABLE AND CLEAR ENTRY             
         BRAS  RE,FREEBOTH                                                      
         BRAS  RE,ARSOFF                                                        
         XC    SRTD(SRTRECL),SRTD                                               
         WTO   TEXT=RERMS1H                                                     
         B     EXITOK                                                           
*                                                                               
RERMS1H  DC    AL2(L'RERMS1)                                                    
RERMS1   DC    C'MONSOON JOB HAS BEEN REQUEUED'                                 
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE                                                          *         
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVC   TITLE(7),=CL7'MONSTER'                                           
         MVC   P(10),=CL10'INITIALIZE'                                          
         BRAS  RE,PRNT                                                          
*                                                                               
         LA    R2,FULL             GET JOB NAME INTO MVSNAME                    
         EXTRACT (2),FIELDS=TIOT                                                
         L     R2,FULL                                                          
         MVC   MVSNAME,0(R2)                                                    
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS MONSTER BROUGHT UP WITH 'START'?         
         BNE   INIT02                                                           
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
         DROP  R2                                                               
*                                                                               
INIT02   QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
*                                                                               
         EXTRACT FULL,'S',FIELDS=(ASID)                                         
         L     R3,FULL             MVS JOB ACCOUNTING INFORMATION               
         LOCASCB ASID=(R3)         PUTS A(ASCB) INTO R1                         
         ST    R1,AASCB            SAVE A(ASCB)                                 
*                                                                               
         XC    BSP,BSP             INITIALISE BINSRCH PARAMETERS                
         MVC   BSPSTRT,ACLASSTB    SET A(TABLE)                                 
         LHI   R0,CLSTABLQ                                                      
         ST    R0,BSPLENR                                                       
         LHI   R0,CLSKEYLQ                                                      
         ST    R0,BSPLENK                                                       
         LHI   R0,MAXCLASS                                                      
         ST    R0,BSPEND                                                        
         BRAS  RE,READCRDS         READ PARAMETER CARDS                         
         MVC   ERR26CNT,MAXCONV    RESET ERROR26 COUNT                          
*                                                                               
         BRAS  RE,GSSRT            GET STORAGE FOR SORT BLOCK                   
         BRAS  RE,OPENCTFL         OPEN CTFILE (IF NOT OPENED ALREADY)          
*                                                                               
         L     R1,X'10'(,R0)       POINT TO CPU ID                              
         L     R1,X'C4'(R1)                                                     
         LA    R1,X'10'(R1)        A(FOUR CHARACTER CPU ID)                     
         ST    R1,ACPUID                                                        
         MVC   P(8),=C'CPU ID: '                                                
         MVC   P+8(4),0(R1)                                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+00(6),=CL6'ASID: '                                             
         L     R1,AASCB            A(ASCB)                                      
         MVC   HALF,ASCBASID-ASCB(R1)                                           
         GOTO1 VHEXOUT,DMCB,HALF,P+6,2,0                                        
         GOTO1 VPRINTER                                                         
*                                                                               
         ZAP   LINE,=P'75'         EJECT PAGE NEXT TIME                         
*                                                                               
         BLDL  0,ENTRYPTS          BUILD LIST OF APPC/MVS ENTRY PTS             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                BAD RETURN FROM BLDL MACRO                   
*                                                                               
         LOAD  DE=ATBALC2                                                       
         ST    R0,AATBALC2                                                      
         LOAD  DE=ATBCFM                                                        
         ST    R0,AATBCFM                                                       
         LOAD  DE=ATBCFMD                                                       
         ST    R0,AATBCFMD                                                      
         LOAD  DE=ATBDEAL                                                       
         ST    R0,AATBDEAL                                                      
         LOAD  DE=ATBSEND                                                       
         ST    R0,AATBSEND                                                      
         LOAD  DE=ATBGTA2                                                       
         ST    R0,AATBGTA2                                                      
         LOAD  DE=ATBEES3                                                       
         ST    R0,AATBEES3                                                      
         LOAD  DE=ATBRCVW                                                       
         ST    R0,AATBRCVW                                                      
         LOAD  DE=ATBAMR1                                                       
         ST    R0,ATBAMR1_STRUCTURE                                             
*                                                                               
         L     R2,MAXCONV          MAXIMUM NUMBER OF CONVERSATIONS              
         MHI   R2,APPCDLEN         R2 = AMOUNT OF SPACE NEEDED IN TABLE         
         AHI   R2,12               ADD ROOM FOR EYE-CATCHER, EOT MARKER         
         STORAGE OBTAIN,LENGTH=(2)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
*                                                                               
         MVC   0(8,R1),=C'*APPCTAB'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AAPPCTAB         A(START OF APPC CONTROL TABLE)               
*                                                                               
         L     R2,MAXCONV          MAXIMUM NUMBER OF CONVERSATIONS              
         MHI   R2,4                R2 = SPACE NEEDED FOR ECBS                   
         LA    R2,16(R2)           EYE-CATCHER, PLUS TWO MORE ECBS              
         STORAGE OBTAIN,LENGTH=(2)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
*                                                                               
         MVC   0(8,R1),=C'*ECBLIST'                                             
         LA    R1,8(R1)            BUMP PAST LABEL                              
         ST    R1,AECBLIST         A(START OF ECBLIST)                          
*                                                                               
         L     R2,MAXCONV          MAXIMUM NUMBER OF CONVERSATIONS              
         MHI   R2,SRTRECL          R2 = SPACE NEEDED FOR ECBS                   
         LA    R2,16(R2)           EYE-CATCHER, PLUS TWO MORE ECBS              
         STORAGE OBTAIN,LENGTH=(2)                                              
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL STORAGE OBTAIN                  
         MVC   0(8,R1),=C'*SRTLIST'                                             
         MVC   8(8,R1),0(R1)                                                    
         AHI   R1,16               BUMP PAST LABEL                              
         ST    R1,ASRTLIST         A(START OF SRTLIST)                          
*                                                                               
         L     R3,AECBLIST         BUILD ECBLIST                                
         MVC   1(R3,3),AOPERECB+1  1ST ECB: OPERATOR                            
         MVI   0(R3),0                                                          
         LA    R1,TIMERECB                                                      
         STCM  R1,7,5(R3)          2ND ECB: TIMER                               
         MVI   4(R3),0                                                          
         LA    R3,8(R3)            PUMP TO 3RD ECB                              
         ST    R3,FIRSTECB         SAVE 1ST APPC CONVERSATION ECB               
*                                                                               
         L     R7,AAPPCTAB                                                      
         LHI   R2,1                                                             
*                                                                               
INIT20   ST    R7,0(R3)            FIRST FIELD IN TABLE IS ECB                  
         STC   R2,CONVNUM                                                       
*                                                                               
         LR    R0,R2                                                            
         BCTR  R0,0                                                             
         MHI   R0,SRTRECL                                                       
         A     R0,ASRTLIST                                                      
         STCM  R0,15,REQUEST_TABLE_ENTRY                                        
*                                                                               
         XC    APPC_ECB,APPC_ECB                                                
         LA    R1,APPC_ECB                                                      
         ST    R1,NOTIFY_TYPE+4                                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         MVC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         MVC   SYM_DEST_NAME,=CL8' '                                            
         MVC   PARTNER_LU_NAME,PARTLUID                                         
         MVC   LOCAL_LU_NAME,=CL8' '                                            
         MVC   MODE_NAME,=CL8'DDSLU62'                                          
         MVC   RETURN_CONTROL,ATB_WHEN_SESSION_ALLOCATED                        
         MVC   SYNC_LEVEL,ATB_CONFIRM                                           
         MVC   SECURITY_TYPE,ATB_SECURITY_PROGRAM                               
         MVC   USER_ID,=CL10'DDSSOON'                                           
         MVC   PASSWORD,=CL10'DDSSOON'                                          
         MVC   PROFILE,=CL10' '                                                 
         XC    USER_TOKEN,USER_TOKEN                                            
         XC    TPID,TPID                                                        
         XC    RECEIVE_ACCESS_TOKEN,RECEIVE_ACCESS_TOKEN                        
         XC    SEND_ACCESS_TOKEN,SEND_ACCESS_TOKEN                              
         MVC   OPMS1H,=AL2(L'OPMS1)                                             
         MVC   OPMS1,=CL42'NO REQUEST RUNNING NOW'                              
*                                                                               
         MVI   OPMS1+36,C'('                                                    
         EDIT  CONVNUM,(3,OPMS1+37),ALIGN=LEFT                                  
         LR    RE,R0                                                            
         LA    RE,OPMS1+37(RE)                                                  
         MVI   0(RE),C')'                                                       
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBALC2_STRUCTURE                                 
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBALC2                           
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'ALLOCATE'                       
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,CALLAPPC_ERROR_EXTRACT_OKAY          
         LA    R1,CONVERSATION_TYPE                                             
         ST    R1,ATBP.ATBALC2_CONVERSATION_TYPE                                
         LA    R1,SYM_DEST_NAME                                                 
         ST    R1,ATBP.ATBALC2_SYM_DEST_NAME                                    
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBP.ATBALC2_PARTNER_LU_NAME                                  
         LA    R1,MODE_NAME                                                     
         ST    R1,ATBP.ATBALC2_MODE_NAME                                        
         LA    R1,TP_NAME_LENGTH                                                
         ST    R1,ATBP.ATBALC2_TP_NAME_LENGTH                                   
         LA    R1,TP_NAME                                                       
         ST    R1,ATBP.ATBALC2_TP_NAME                                          
         LA    R1,RETURN_CONTROL                                                
         ST    R1,ATBP.ATBALC2_RETURN_CONTROL                                   
         LA    R1,SYNC_LEVEL                                                    
         ST    R1,ATBP.ATBALC2_SYNC_LEVEL                                       
         LA    R1,SECURITY_TYPE                                                 
         ST    R1,ATBP.ATBALC2_SECURITY_TYPE                                    
         LA    R1,USER_ID                                                       
         ST    R1,ATBP.ATBALC2_USER_ID                                          
         LA    R1,PASSWORD                                                      
         ST    R1,ATBP.ATBALC2_PASSWORD                                         
         LA    R1,PROFILE                                                       
         ST    R1,ATBP.ATBALC2_PROFILE                                          
         LA    R1,USER_TOKEN                                                    
         ST    R1,ATBP.ATBALC2_USER_TOKEN                                       
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBALC2_CONVERSATION_ID                                  
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBP.ATBALC2_NOTIFY_TYPE                                      
         LA    R1,TPID                                                          
         ST    R1,ATBP.ATBALC2_TP_ID                                            
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBP.ATBALC2_LOCAL_LU_NAME                                    
         LA    R1,RETURN_CODE                                                   
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBALC2_RETURN_CODE                                      
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBCFM_STRUCTURE                                  
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBCFM                            
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'REQUEST_CONFIRM'                
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,CALLAPPC_ERROR_EXTRACT_OKAY          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBCFM_CONVERSATION_ID                                   
         LA    R1,REQUEST_TO_SEND_RECEIVED                                      
         ST    R1,ATBP.ATBCFM_REQUEST_TO_SEND_RECEIVED                          
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBP.ATBCFM_NOTIFY_TYPE                                       
         LA    R1,RETURN_CODE                                                   
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBCFM_RETURN_CODE                                       
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBCFMD_STRUCTURE                                 
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBCFMD                           
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'ISSUE_CONFIRM'                  
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,CALLAPPC_ERROR_EXTRACT_OKAY          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBCFMD_CONVERSATION_ID                                  
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBP.ATBCFMD_NOTIFY_TYPE                                      
         LA    R1,RETURN_CODE                                                   
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBCFMD_RETURN_CODE                                      
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBDEAL_STRUCTURE                                 
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBDEAL                           
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'DEALLOCATE'                     
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,CALLAPPC_ERROR_EXTRACT_OKAY          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBDEAL_CONVERSATION_ID                                  
         LA    R1,DEALLOCATE_TYPE                                               
         ST    R1,ATBP.ATBDEAL_DEALLOCATE_TYPE                                  
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBP.ATBDEAL_NOTIFY_TYPE                                      
         LA    R1,RETURN_CODE                                                   
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBDEAL_RETURN_CODE                                      
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBSEND_STRUCTURE                                 
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBSEND                           
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'SEND_DATA'                      
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,CALLAPPC_ERROR_EXTRACT_OKAY          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBSEND_CONVERSATION_ID                                  
         LA    R1,SEND_TYPE                                                     
         ST    R1,ATBP.ATBSEND_SEND_TYPE                                        
         LA    R1,SEND_LENGTH                                                   
         ST    R1,ATBP.ATBSEND_SEND_LENGTH                                      
         LA    R1,SEND_ACCESS_TOKEN                                             
         ST    R1,ATBP.ATBSEND_ACCESS_TOKEN                                     
         LA    R1,BUFFER                                                        
         ST    R1,ATBP.ATBSEND_BUFFER                                           
*********LA    R1,REQUEST_TO_SEND_VALUE   **DEIS: THIS LOOKS LIKE A BUG         
         LA    R1,REQUEST_TO_SEND_RECEIVED                                      
         ST    R1,ATBP.ATBSEND_REQUEST_TO_SEND_RECEIVED                         
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBP.ATBSEND_NOTIFY_TYPE                                      
         LA    R1,RETURN_CODE                                                   
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBSEND_RETURN_CODE                                      
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBGTA2_STRUCTURE                                 
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBGTA2                           
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'GET_ATTRIBUTES'                 
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,CALLAPPC_ERROR_EXTRACT_OKAY          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBGTA2_CONVERSATION_ID                                  
         LA    R1,PARTNER_LU_NAME                                               
         ST    R1,ATBP.ATBGTA2_PARTNER_LU_NAME                                  
         LA    R1,MODE_NAME                                                     
         ST    R1,ATBP.ATBGTA2_MODE_NAME                                        
         LA    R1,SYNC_LEVEL                                                    
         ST    R1,ATBP.ATBGTA2_SYNC_LEVEL                                       
         LA    R1,CONVERSATION_CORRELATOR                                       
         ST    R1,ATBP.ATBGTA2_CONVERSATION_CORRELATOR                          
         LA    R1,LUW_ID                                                        
         ST    R1,ATBP.ATBGTA2_LUW_ID                                           
         LA    R1,WORK                                                          
         ST    R1,ATBP.ATBGTA2_TP_NAME_LENGTH                                   
         LA    R1,TP_NAME                                                       
         ST    R1,ATBP.ATBGTA2_TP_NAME                                          
         LA    R1,LOCAL_LU_NAME                                                 
         ST    R1,ATBP.ATBGTA2_LOCAL_LU_NAME                                    
         LA    R1,CONVERSATION_TYPE                                             
         ST    R1,ATBP.ATBGTA2_CONVERSATION_TYPE                                
         LA    R1,WORK                                                          
         ST    R1,ATBP.ATBGTA2_USER_ID                                          
         LA    R1,WORK                                                          
         ST    R1,ATBP.ATBGTA2_PROFILE                                          
         LA    R1,WORK                                                          
         ST    R1,ATBP.ATBGTA2_USER_TOKEN                                       
         LA    R1,CONVERSATION_STATE                                            
         ST    R1,ATBP.ATBGTA2_CONVERSATION_STATE                               
         LA    R1,RETURN_CODE                                                   
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBGTA2_RETURN_CODE                                      
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBEES3_STRUCTURE                                 
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBEES3                           
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'ERROR_EXTRACT'                  
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,0                                    
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBEES3_CONVERSATION_ID                                  
         LA    R1,SERVICE_NAME                                                  
         ST    R1,ATBP.ATBEES3_SERVICE_NAME                                     
         LA    R1,SERVICE_REASON_CODE                                           
         ST    R1,ATBP.ATBEES3_SERVICE_REASON_CODE                              
         LA    R1,MESSAGE_TEXT_LENGTH                                           
         ST    R1,ATBP.ATBEES3_MESSAGE_TEXT_LENGTH                              
         LA    R1,MESSAGE_TEXT                                                  
         ST    R1,ATBP.ATBEES3_MESSAGE_TEXT                                     
         LA    R1,ERROR_LOG_PRODUCT_SET_ID_LENGTH                               
         ST    R1,ATBP.ATBEES3_ERROR_LOG_PRODUCT_SET_ID_LENGTH                  
         LA    R1,ERROR_LOG_PRODUCT_SET_ID                                      
         ST    R1,ATBP.ATBEES3_ERROR_LOG_PRODUCT_SET_ID                         
         LA    R1,ERROR_LOG_INFORMATION_LENGTH                                  
         ST    R1,ATBP.ATBEES3_ERROR_LOG_INFORMATION_LENGTH                     
         LA    R1,ERROR_LOG_INFORMATION                                         
         ST    R1,ATBP.ATBEES3_ERROR_LOG_INFORMATION                            
         LA    R1,REASON_CODE_ERROR_EXTRACT                                     
         ST    R1,ATBP.ATBEES3_REASON_CODE                                      
         LA    R1,RETURN_CODE_ERROR_EXTRACT                                     
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBEES3_RETURN_CODE                                      
*                                                                               
ATBP     USING CALLAPPC_BLOCK,ATBRCVW_STRUCTURE                                 
         MVC   ATBP.CALLAPPC_ROUTINE_ADDRESS,AATBRCVW                           
         MVC   ATBP.CALLAPPC_ROUTINE_NAME,=CL16'RECEIVE_AND_WAIT'               
         MVI   ATBP.CALLAPPC_ROUTINE_FLAGS,CALLAPPC_ERROR_EXTRACT_OKAY          
         LA    R1,CONVERSATION_ID                                               
         ST    R1,ATBP.ATBRCVW_CONVERSATION_ID                                  
         LA    R1,FILL                                                          
         ST    R1,ATBP.ATBRCVW_FILL                                             
         LA    R1,RECEIVE_LENGTH                                                
         ST    R1,ATBP.ATBRCVW_RECEIVE_LENGTH                                   
         LA    R1,RECEIVE_ACCESS_TOKEN                                          
         ST    R1,ATBP.ATBRCVW_ACCESS_TOKEN                                     
         LA    R1,BUFFER                                                        
         ST    R1,ATBP.ATBRCVW_BUFFER                                           
         LA    R1,STATUS_RECEIVED                                               
         ST    R1,ATBP.ATBRCVW_STATUS_RECEIVED                                  
         LA    R1,DATA_RECEIVED                                                 
         ST    R1,ATBP.ATBRCVW_DATA_RECEIVED                                    
         LA    R1,REQUEST_TO_SEND_RECEIVED                                      
         ST    R1,ATBP.ATBRCVW_REQUEST_TO_SEND_RECEIVED                         
         LA    R1,NOTIFY_TYPE                                                   
         ST    R1,ATBP.ATBRCVW_NOTIFY_TYPE                                      
         LA    R1,RETURN_CODE                                                   
         OILF  GR1,X'80000000'                                                  
         ST    R1,ATBP.ATBRCVW_RETURN_CODE                                      
         DROP  ATBP                                                             
*                                                                               
         AHI   R7,APPCDLEN         BUMP TO NEXT CONVERSATION                    
         LA    R3,4(R3)            BUMP TO NEXT POSITION IN ECBLIST             
*                                                                               
         AHI   R2,1                                                             
         C     R2,MAXCONV                                                       
         BNH   INIT20                                                           
*                                                                               
         MVC   0(4,R7),EOTMARK     MARK END OF APPC CONTROL TABLE               
         SHI   R3,4                                                             
         MVI   0(R3),X'80'         MARK END OF ECBLIST                          
         ST    R3,CHKDECB          SET LAST CHECKED ECB                         
         SHI   R7,APPCDLEN         POINT BACK TO LAST APPC TABLE ENTRY          
         ST    R7,CHKDAPPC         SET LAST CHECKED APPC TABLE ENTRY            
                                                                                
**********************************************************************          
* RECYCLE ALL MONSOONS (Y/N)                                                    
**********************************************************************          
INIT30   GOTO1 VLOGIO,DMCB,1,(L'OPMS25,OPMS25) WTOR OPERATOR                    
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)          READ THE REPLY (Y/N)             
         CLI   DMCB+4,1                                                         
         BNE   INIT40                                                           
         CLI   BYTE,YES                                                         
         BE    INIT50                                                           
         CLI   BYTE,NO                                                          
         BE    INIT60                                                           
*                                                                               
INIT40   WTO   TEXT=INMS1H         INVALID - TRY AGAIN                          
         B     INIT30                                                           
*                                                                               
INIT50   BRAS  RE,RECYCLE          RECYCLE ALL MONSOONS                         
*                                                                               
INIT60   GOTO1 VDATCON,DMCB,(5,0),(3,THREE)                                     
         LA    R3,DMCB             CHECK IF TODAY'S WEEKEND/HOLIDAY             
         USING GETRETD,R3                                                       
         XC    DMCB(24),DMCB                                                    
         MVC   GRDIDY(3),THREE     TODAY'S DATE (BINARY YMD)                    
         MVC   GRDITH(2),=X'0700'          7 AM                                 
         MVC   GRDHRS,=H'2'                                                     
         GOTO1 VGETRET,DMCB                                                     
         CLI   GRDRETC,0                                                        
*&&UK*&& JNE   *+2                                                              
*&&US*&& BNE   INIT80                                                           
         CLC   THREE,GRDODY        RETURNED DATE (BINARY YMD)                   
         BE    *+8                 TODAY IS A BUSINESS DAY                      
         MVI   WKNFLAG,YES         TODAY IS WEEKEND/HOLIDAY                     
         DROP  R3                                                               
*                                                                               
INIT80   OPEN  (FILE,OUTPUT)       OPEN DS FOR RECORD LONG SOON JOBS            
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDYNALOC,DMCB,(C'D',=CL8'FILE'),DSN                              
*                                                                               
         BRAS  RE,ENQJOB           GET JOB TABLE HEADER                         
         ICM   RF,15,DMCB+4                                                     
         MVC   DSPHD,0(RF)         SAVE HEADER                                  
         NC    DSPTFRST,=XL4'3FFFFFFF'                                          
*                                                                               
         BRAS  RE,ENQCLS           GET CLASS TABLE HEADER                       
         ICM   RF,15,DMCB+4                                                     
         MVC   CSPHD,0(RF)         SAVE HEADER                                  
         NC    C.DSPTFRST,=XL4'3FFFFFFF'                                        
*                                                                               
         L     RF,ASSB             SAVE TABS ALET                               
         MVC   TBLET,SSOTBLET-SSOOFF(RF)                                        
         B     EXITOK                                                           
*                                                                               
INMS1H   DC    AL2(L'INMS1)                                                     
INMS1    DC    C'INVALID REPLY (Y/N ONLY)!'                                     
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET STORAGE FOR INTERNAL JOB TABLE                                  *         
***********************************************************************         
GSSRT    NTR1  BASE=*,LABEL=*                                                   
         LHI   R3,MAXJOBQ          MAX # JOBS                                   
         MHI   R3,SRTRECL          X JOB REC LEN                                
         LR    R4,R3               SAVE LENGTH OF TABLE                         
         AHI   R3,16               ROOM FOR LABEL, COUNTER, 4 SPARE             
*                                                                               
         SAM31                                                                  
         STORAGE OBTAIN,LENGTH=(3),LOC=ANY,BNDRY=PAGE                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(8,R1),=C'*SRTBLK*'                                             
         MVC   8(8,R1),0(R1)                                                    
         AHI   R1,16               BUMP PAST LABEL, COUNTER, SPARE              
         ST    R1,ASRTBLK          A(START OF XMTTBL)                           
*                                                                               
         LR    R0,R1               CLEAR TABLE                                  
         LR    R1,R4                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* RECYCLE ASCH ADDRESS TPS (ON DEMAND)                                *         
***********************************************************************         
RECYCLE  NTR1  BASE=*,LABEL=*                                                   
         MVI   RECYLTPS,YES        RECYCLING ALL ASCH TP'S                      
         L     R7,AAPPCTAB         APPC/MVS CONTROL TABLE                       
         GOTO1 =A(SETSYNC),ATB_NOTIFY_TYPE_NONE    SET SYNCHRONOUS              
*                                                                               
         LA    R4,TPTAB                                                         
         USING TPTABD,R4                                                        
*                                                                               
RECY02   XR    R3,R3                                                            
         ICM   R3,3,TPTMCN         MAX CONV OF THIS TPNAME                      
         BZ    RECY14                                                           
*                                                                               
         LH    RE,TPTLEN                                                        
         ST    RE,TP_NAME_LENGTH                                                
         MVC   TP_NAME,TPTNAM                                                   
*                                                                               
RECY04   LA    R2,ATBALC2_STRUCTURE  SYNCHRONOUS APPC CALL                      
         BRAS  RE,CALLAPPC           ALLOCATE THE CONVERSATION                  
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                WTOR IF ANY ERROR                          
*                                                                               
         LA    R2,ATBCFM_STRUCTURE   SYNCHRONOUS APPC CALL                      
         BRAS  RE,CALLAPPC           REQUEST CONFIRMATION                       
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                WTOR IF ANY ERROR                          
*                                                                               
         XC    BUFFER,BUFFER                                                    
         MVC   BUFFER(7),=C'RECYCLE'                                            
         MVC   BUFFER+8(64),TP_NAME                                             
         LA    R2,7+1+64                                                        
         ST    R2,SEND_LENGTH                                                   
         MVC   SEND_TYPE,ATB_SEND_AND_PREP_TO_RECEIVE                           
*                                                                               
         LA    RE,=C'SEND AND PREPARE TO RECEIVE'                               
         ST    RE,DMCB                                                          
         MVI   DMCB,27                                                          
         GOTO1 VPRNTBL,DMCB,,BUFFER,C'DUMP',(R2),=C'1D'                         
*                                                                               
         LA    R2,ATBSEND_STRUCTURE  SYNCHRONOUS APPC CALL                      
         BRAS  RE,CALLAPPC           SEND CONTROL INFO                          
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                WTOR IF ANY ERROR                          
*                                                                               
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE  SYNCHRONOUS APPC CALL                      
         BRAS  RE,CALLAPPC           WAIT TO RECEIVE ASCH JOB ID                
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                WTOR IF ANY ERROR                          
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         LA    RE,=C'RECEIVED ASCH JOB ID'                                      
         ST    RE,DMCB                                                          
         MVI   DMCB,20                                                          
         GOTO1 VPRNTBL,DMCB,,BUFFER,C'DUMP',(R2),=C'1D'                         
*                                                                               
*&&UK*&& STIMERM SET,ID=STIMER1,BINTVL=ONESEC,WAIT=YES                          
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE   SYNCHRONOUS APPC CALL                     
         BRAS  RE,CALLAPPC            ISSUE CONFIRM                             
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                 WTOR IF ANY ERROR                         
*                                                                               
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_LENGTH,=A(L'BUFFER)                                      
*                                                                               
         LA    R2,ATBRCVW_STRUCTURE   SYNCHRONOUS APPC CALL                     
         BRAS  RE,CALLAPPC            WAIT FOR RESPONSE FROM MONSOON            
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                 WTOR IF ANY ERROR                         
*                                                                               
         MVC   WORK+0(2),=H'29'                                                 
         MVC   WORK+2(21),=C'INIT MONSOON         '                             
         MVC   WORK+15(8),TPTNAM                                                
         WTO   TEXT=WORK                                                        
*                                                                               
         L     R2,RECEIVE_LENGTH                                                
         GOTO1 VPRNTBL,DMCB,=C'RECEIVED',BUFFER,C'DUMP',(R2),=C'1D'             
*                                                                               
         LA    R2,ATBCFMD_STRUCTURE                                             
         BRAS  RE,CALLAPPC            ISSUE CONFIRM                             
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                 WTOR IF ANY ERROR                         
*                                                                               
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_SYNC_LEVEL                        
         LA    R2,ATBDEAL_STRUCTURE   SYNCHRONOUS APPC CALL                     
         BRAS  RE,CALLAPPC            DEALLOCATE THE CONVERSATION               
         CLC   RETURN_CODE,ATB_OK                                               
         BNE   RECY06                 WTOR IF ANY ERROR                         
         BCT   R3,RECY04                                                        
         B     RECY14                                                           
                                                                                
**********************************************************************          
* ASK OPERATOR ?CONTINUE BY DISABLE THIS TPNAME (CONV#=0)?                      
**********************************************************************          
RECY06   MVC   RCMS5+35(L'TP_NAME),TP_NAME                                      
*                                                                               
RECY08   GOTO1 VLOGIO,DMCB,1,(L'RCMS5,RCMS5) WTOR OPERATOR                      
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)             READ THE REPLY (Y/N)          
         CLI   DMCB+4,1                                                         
         BNE   RECY10                                                           
         CLI   BYTE,YES                                                         
         BE    RECY12                                                           
         CLI   BYTE,NO                                                          
         BNE   RECY10                                                           
         MVI   ESHTDOWN,YES        READY TO SHUT DOWN                           
         B     RECY16              EXIT                                         
*                                                                               
RECY10   WTO   TEXT=RCMS1H         INVALID, TRY AGAIN                           
         B     RECY08                                                           
*                                                                               
RECY12   XC    TPTMCN,TPTMCN       MAX CONV OF THIS TPNAME SET TO 0             
         MVC   RCMS3H+2(L'TP_NAME),TP_NAME                                      
         XR    R0,R0                                                            
         WTO   TEXT=((RCMS2H,C),(RCMS3H,L),(RCMS4H,DE))                         
*                                                                               
RECY14   AHI   R4,TPTABLQ                                                       
         OC    0(TPTABLQ,R4),0(R4)                                              
         BNZ   RECY02                                                           
         MVC   WORK+0(2),=H'21'                                                 
         MVC   WORK+2(21),=C'MONSTER? INITIALISED '                             
         MVC   WORK+2(8),MVSNAME                                                
         WTO   TEXT=WORK                                                        
         DROP  R4                                                               
*                                                                               
RECY16   GOTO1 =A(SETSYNC),NOTIFY_TYPE CHANGE BACK TO ASYNCHRONOUS              
*                                                                               
         MVI   RECYLTPS,NO         FINISH RECYCLING ALL ASCH TP'S               
         B     EXITOK                                                           
*                                                                               
RCMS1H   DC    AL2(30),CL30'INVALID REPLY (Y/N ONLY)!'                          
RCMS2H   DC    AL2(15),CL15'DISABLED TP:'                                       
RCMS3H   DC    AL2(64),CL64' '                                                  
RCMS4H   DC    AL2(45),CL45'PLEASE NOTIFY FRED/YI ABOUT THIS PROBLEM'           
*                                                                               
RCMS5    DC    CL100'(Y/N?) CONTINUE && DISABLE THIS TP:'                       
                                                                                
ONESEC   DC    F'100'                                                           
                                                                                
***********************************************************************         
* SET EITHER AS SYNCHRONOUS OR ASYNCHRONOUS                                     
***********************************************************************         
SETSYNC  DS    0F                                                               
ATBP     USING CALLAPPC_BLOCK,ATBALC2_STRUCTURE                                 
         ST    R1,ATBP.ATBALC2_NOTIFY_TYPE                                      
                                                                                
ATBP     USING CALLAPPC_BLOCK,ATBCFM_STRUCTURE                                  
         ST    R1,ATBP.ATBCFM_NOTIFY_TYPE                                       
                                                                                
ATBP     USING CALLAPPC_BLOCK,ATBSEND_STRUCTURE                                 
         ST    R1,ATBP.ATBSEND_NOTIFY_TYPE                                      
                                                                                
ATBP     USING CALLAPPC_BLOCK,ATBRCVW_STRUCTURE                                 
         ST    R1,ATBP.ATBRCVW_NOTIFY_TYPE                                      
                                                                                
ATBP     USING CALLAPPC_BLOCK,ATBCFMD_STRUCTURE                                 
         ST    R1,ATBP.ATBCFMD_NOTIFY_TYPE                                      
                                                                                
ATBP     USING CALLAPPC_BLOCK,ATBDEAL_STRUCTURE                                 
         ST    R1,ATBP.ATBDEAL_NOTIFY_TYPE                                      
         DROP  ATBP                                                             
         BR    RE                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET USER ID FROM GIVEN NUMBER                                       *         
* NTRY: HALF    = COPY OF JOBTAB ENTRY                                *         
* EXIT: USERID  = USER ID                                             *         
***********************************************************************         
GUSERNAM NTR1  BASE=*,LABEL=*                                                   
         SAM24                                                                  
*                                                                               
         LA    R2,KEY              BUILD ID RECORD KEY                          
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,HALF        USER-ID NUMBER                               
         GOTO1 VDMGR,DMCB,(0,DMREAD),CTFILE,KEY,AIO,0                           
         CLI   DMCB+8,0                                                         
         BNE   GUSBAD                                                           
*                                                                               
         L     R2,AIO                                                           
         LA    R2,CTIDATA                                                       
         USING CTDSCD,R2                                                        
         XR    RF,RF                                                            
*                                                                               
GUS02    CLI   CTDSCEL,0           FIND DESCRIPTION ELEMENT (NAME)              
         BE    GUSBAD                                                           
         CLI   CTDSCEL,CTDSCELQ                                                 
         BE    GUS04                                                            
         ICM   RF,1,CTDSCLEN                                                    
         BZ    GUSBAD                                                           
         BXH   R2,RF,GUS02                                                      
*                                                                               
GUS04    MVC   USERID,CTDSC        SAVE OFF NAME                                
         B     EXITOK                                                           
*                                                                               
GUSBAD   DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT A PRINT LINE                                      *         
***********************************************************************         
PRNT     NTR1  BASE=*,LABEL=*                                                   
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
         GOTO1 VPRINTER                                                         
         B     EXITOK                                                           
*                                                                               
PRNTDUB  DS    D                   FOR PRNT MACRO                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD ECB LIST IN STORAGE                                *         
***********************************************************************         
BLDECBS  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AECBLIST         BUILD ECBLIST                                
         MVC   0(4,R3),AOPERECB    1ST ECB: OPERATOR                            
         MVI   0(R3),0                                                          
         LA    R1,TIMERECB                                                      
         ST    R1,4(R3)            2ND ECB: TIMER                               
*                                                                               
         LA    R3,8(R3)            JUMP TO 3RD ECB                              
         ST    R3,FIRSTECB         SAVE 1ST APPC CONVERSATION ECB               
*                                                                               
         LHI   R2,1                                                             
         L     R7,AAPPCTAB                                                      
*                                                                               
BECB02   ST    R7,0(R3)            FIRST FIELD IN TABLE IS ECB                  
         AHI   R7,APPCDLEN         BUMP TO NEXT CONVERSATION                    
         LA    R3,4(R3)            BUMP TO NEXT POSITION IN ECBLIST             
*                                                                               
         AHI   R2,1                                                             
         C     R2,MAXCONV                                                       
         BNH   BECB02                                                           
         AHI   R3,-4                                                            
         MVI   0(R3),X'80'         MARK END OF ECBLIST                          
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* THE TIMER EXIT ROUTINE.  IT POSTS THE TIMERECB.                     *         
***********************************************************************         
TIMERXIT SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMERXIT,RB                                                      
*                                                                               
         L     RC,ATIMCOMM         BASE REGISTER FOR TIMERECB                   
         LR    R9,RC                                                            
         AHI   R9,4096                                                          
*                                                                               
         POST  TIMERECB                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
ATIMCOMM DC    A(COMMWORK)                                                      
         DROP  RB                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* PRINT TABLES FOR TRACING                                            *         
***********************************************************************         
PRTTAB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*TPTAB                                                                          
         LHI   R2,11                                                            
         LAY   R7,TPTAB                                                         
PT10     MVC   P(TPTABLQ),0(R7)                                                 
         GOTO1 VPRINTER                                                         
         AHI   R7,TPTABLQ                                                       
         BCT   R2,PT10                                                          
*                                                                               
*CLASSTAB                                                                       
         LHI   R2,11                                                            
         L     R7,ACLASSTB                                                      
PT20     MVC   P(CLSTABLQ),0(R7)                                                
         GOTO1 VPRINTER                                                         
         AHI   R7,CLSTABLQ                                                      
         BCT   R2,PT20                                                          
*                                                                               
*SRTLIST                                                                        
         LHI   R2,11                                                            
         L     R7,ASRTLIST                                                      
PT30     MVC   P(SRTRECL),0(R7)                                                 
         GOTO1 VPRINTER                                                         
         AHI   R7,SRTRECL                                                       
         BCT   R2,PT30                                                          
*                                                                               
*SRTBLK                                                                         
         SAM31                                                                  
         LHI   R2,11                                                            
         L     R7,ASRTBLK                                                       
PT40     MVC   P(SRTRECL),0(R7)                                                 
         GOTO1 VPRINTER                                                         
         AHI   R7,SRTRECL                                                       
         BCT   R2,PT40                                                          
         SAM24                                                                  
*                                                                               
         B     EXITOK                                                           
*                                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*&&                                                                             
*&&US                                                                           
***********************************************************************         
* CHECK AGY/SUBID MAX                                                 *         
* INPUT: R8=SCHEDULE SORT BLOCK ENTRY                                 *         
***********************************************************************         
CHKMAX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   BYTE,YES            ASSUME OKAY TO RUN THIS JOB                  
*                                                                               
         BRAS  RE,LOCKCLS          LOCK CLASS TABLE                             
*                                                                               
         BRAS  RE,ARSOFF           MAKE SURE START LOOP CLEAN                   
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,SRTACLS       GET CLASS TABLE ENTRY                        
         SAC   512                                                              
         USING JCLASSD,R2          R2 = A(CLASS TABLE)                          
         USING SRTD,R8                                                          
*                                                                               
         CLI   SRTPRI,X'00'        IGNORE MAX CHECK?                            
         BE    CKMX80              YES                                          
*                                                                               
CKMX40   EQU   *                                                                
         CLC   JCNRUN,USERMAXR     CAN'T BE TOO MANY RUNNING                    
         BL    CKMX80                                                           
         ICM   R5,15,JCFRUN                                                     
         BZ    CKMX80              NOTHING RUNNING                              
*                                                                               
         LH    R0,USERMAXR         MAKE SURE NOT TOO MANY RUNNING               
         LH    R1,SUBMAXR                                                       
         SR    RF,RF                                                            
         IC    RF,SRTREPTN                                                      
*                                                                               
         CPYA  AR5,AR2                                                          
RUN      USING TBJOBTAB,R5                                                      
*                                                                               
CKMX60   CLC   SRTAGY,RUN.TBJAGY   MATCH AGYID IN RUNNING LIST                  
         BNE   CKMX70              NO                                           
*                                  ONLY RUN N THIS REPORT TYPE/AGENCY           
         CLC   SRTREPT,RUN.TBJMVSID+5                                           
         BNE   CKMX62                                                           
         BCT   RF,CKMX62                                                        
         B     CKMX65                                                           
*                                                                               
CKMX62   CLC   SRTSUBID,RUN.TBJPQSUB                                            
         BNE   *+12                                                             
         AHI   R1,-1                                                            
         BZ    *+8                                                              
         BCT   R0,CKMX70           MAKE SURE <= MAX FOR THIS AGYID              
*                                                                               
CKMX65   LAM   AR5,AR5,ARZERO                                                   
         MVI   BYTE,NO             TOO MANY - IGNORE THIS ONE                   
         B     CKMX80                                                           
*                                                                               
CKMX70   ICM   R5,15,RUN.TBJNXT    ANOTHER RUNNING?                             
         BNZ   CKMX60              YES                                          
         LAM   AR5,AR5,ARZERO                                                   
         DROP  RUN                                                              
*                                                                               
*                                                                               
*                                                                               
CKMX80   BRAS  RE,ARSOFF                                                        
         BRAS  RE,FREECLS          NOW UNLOCK TABLE                             
         BRAS  RE,ARSOFF                                                        
*                                                                               
         CLI   BYTE,YES            RUN THIS JOB?                                
         BNE   EXITL               NO                                           
         B     EXITOK              YES                                          
         LTORG                                                                  
         DROP  RB                                                               
*&&                                                                             
         DROP  R2,R8                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES AND STORAGE                                         *         
***********************************************************************         
         DS    0D                                                               
COMMWORK DC    CL8'*COMMON*'       COMMON STORAGE AREA                          
*                                                                               
CVDR0    CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         BR    RE                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
LOCKBOTH NTR1  ,                                                                
         BRAS  RE,ARSOFF           OBSESS ABOUT ACCESS REGISTERS                
         BRAS  RE,LOCKCLS                                                       
         BRAS  RE,LOCKJOB                                                       
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
*                                                                               
FREEBOTH NTR1  ,                                                                
         BRAS  RE,ARSOFF           OBSESS ABOUT ACCESS REGISTERS                
         BRAS  RE,FREEJOB                                                       
         BRAS  RE,FREECLS                                                       
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
*                                                                               
LOCKJOB  NTR1  ,                   LOCK JOB TABLE DSPACE HEADER                 
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'80'                                                       
         B     CALLSPC                                                          
*                                                                               
FREEJOB  NTR1  ,                   FREE JOB TABLE DSPACE HEADER                 
         MVC   DMCB(4),=AL4(DTJOB)                                              
         OI    DMCB,X'10'                                                       
         B     CALLSPC                                                          
*                                                                               
ENQJOB   NTR1  ,                   ENQUIRE ON JOB TABLE DSPACE HEADER           
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'20'                                                       
         B     CALLSPC                                                          
*                                                                               
LOCKCLS  NTR1  ,                   LOCK CLASS TABLE DSPACE HEADER               
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         OI    DMCB,X'80'                                                       
         B     CALLSPC                                                          
*                                                                               
FREECLS  NTR1  ,                   FREE CLASS TABLE DSPACE HEADER               
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         OI    DMCB,X'10'                                                       
         B     CALLSPC                                                          
*                                                                               
ENQCLS   NTR1  ,                   ENQUIRE ON CLASS TABLE DSPACE HEADER         
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         OI    DMCB,X'20'                                                       
         B     CALLSPC                                                          
*                                                                               
CALLSPC  DS    0H                                                               
         XC    DMCB+4(4*5),DMCB+4  CLEAR NEXT 5FS IN DMCB(FUTURE USE)           
         GOTO1 VLOCKSPC,DMCB       LOCKSPC NOW USES 4 PARMS, 7/6/12             
         B     EXIT                                                             
*                                                                               
ARZERO   DC    16F'0'                                                           
*                                                                               
MAXJOBQ  EQU   2000                MAX NUMBER OF JOBS WE WILL LIST              
         LTORG                                                                  
*                                                                               
         DC    A(0)                                                             
VPRINTER DC    V(PRINTER)                                                       
VLOCKSPC DC    V(LOCKSPC)                                                       
VBINSRCH DC    V(BINSRCH)                                                       
VDYNALOC DC    V(DYNALLOC)                                                      
VCPRINT  DC    V(CPRINT)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VLOGIO   DC    V(LOGIO)                                                         
VDMGR    DC    V(DATAMGR)                                                       
VENQDEQ  DC    V(DMENQDEQ)                                                      
VPRNTBL  DC    V(PRNTBL)                                                        
VNUMVAL  DC    V(NUMVAL)                                                        
VDATCON  DC    V(DATCON)                                                        
VGETRET  DC    V(GETRET)                                                        
VQSORT   DC    V(QSORT)                                                         
ABIGBUF  DC    A(BIGBUF)                                                        
AFACID   DC    A(0)                                                             
ASSB     DC    A(SSB)                                                           
AUTL     DC    A(UTL)                                                           
         DC    A(0)                                                             
*                                                                               
EFFS     DC    (SRTRECL)X'FF'                                                   
STARS    DC    80C'*'                                                           
DMOPEN   DC    CL8'DMOPEN  '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
RANDOM   DC    CL8'RANDOM  '                                                    
BUFFR    DC    CL8'BUFFER  '                                                    
GLIST    DC    CL8'GLIST   '                                                    
CONTROL  DC    CL8'CONTROL '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
PRTQUE   DC    CL8'PRTQU   '       PRTQ NAME                                    
*                                                                               
DSPHD    DS    XL64                                                             
CSPHD    DS    XL64                                                             
         ATBSERV                                                                
*                                                                               
FILE     DCB   DDNAME=FILE,MACRF=PM,DSORG=PS,RECFM=FB,LRECL=80                  
*                                                                               
MONSYSIN DCB   DDNAME=MONSYSIN,MACRF=GM,DSORG=PS,RECFM=FB,LRECL=80,    +        
               EODAD=RCX                                                        
*                                                                               
OPMS25   DC    C'RECYCLE ALL ASCH ADDRESS SPACES? (Y/N)          '              
         EJECT                                                                  
*********************************************************************           
* VARIABLES AND CONSTANTS                                                       
*********************************************************************           
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
                                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
TBLET    DS    A                                                                
STRTTMRL DS    F                                                                
*                                                                               
BSP      DS    0XL32               LOCAL CLASS TABLE BINSRCH PARAMETERS         
BSP1     DS    F                                                                
BSP2     DS    F                                                                
BP3      DS    F                                                                
BSP4     DS    F                                                                
BSP5     DS    F                                                                
BSP6     DS    F                                                                
BSP7     DS    F                                                                
BSP8     DS    F                                                                
*                                                                               
EOTMARK  DC    X'FFFFFFFF'                                                      
FULL     DS    F                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
THREE    DS    XL3                                                              
DSN      DS    CL44                                                             
WORK     DS    CL80                                                             
PQTIME   DS    XL3                                                              
*                                                                               
STIMER1  DS    F                                                                
*                                                                               
AIO      DC    A(IO)               A(IO AREA)                                   
ACLASSTB DC    A(CLASSTAB)         A(CLASS TABLE)                               
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ASRTBLK  DC    A(0)                                                             
TIMERECB DC    F'0'                ECB OF ATTACHED TASK TIMER                   
FIRSTECB DS    A                   1ST APPC CONVERSATION ECB ADDRESS            
CHKDECB  DS    A                   LAST CHECKED APPC CONV ECB ADDR              
CHKDAPPC DS    A                   LAST CHECKED APPC TABLE ENTRY ADDR           
FRTSCAN  DS    A                   A(APPC TBL NTRY) 1ST SCAN FOR ATTACH         
AAPPCTAB DS    A                   A(APPC TABLE)                                
ACOMM    DS    A                   A(COMMUNICATIONS PARAMETER LIST)             
AASCB    DS    A                   A(ASCB) FROM EXTRACT                         
ACPUID   DS    A                   A(FOUR CHARACTER CPU ID)                     
*                                                                               
SVCLSPRT DC    H'0'                LAST CLASS PRIORITY VALUE                    
REFNUM   DS    H                   SELECTED REPORT REFERENCE NUMBER             
REFRESH  DC    X'7FFF'             REFRESH NUMBER (INIT TO HIGH VALUE)          
NUMTP    DS    H                   NUMBER OF TP TABLE ENTRY SO FAR              
NUMINQ   DS    H                   NUMBER OF JOBS WAITING TO RUN                
*                                                                               
PRNTTIME DS    CL9                 FOR PRNT MACRO                               
LOADCARD DC    XL8'00'             LOAD= MODULE NAME                            
*                                                                               
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
*                                                                               
DCRT     DC    F'10'               # MINS INTERVAL BEFORE CLEAR DCNT            
DLMT     DC    F'10'               DUMP COUNTER LIMIT                           
DCNT     DC    F'1'                DUMP COUNTER                                 
TLAST    DC    F'0'                SYS TIME(0000HHMM) OF LAST ABEND             
TNOW     DS    F                   SYS TIME(0000HHMM) OF THIS ABEND             
*                                                                               
MAXTCBTM DC    F'60'               MAX TCB TIME ALLOWED IN SEC                  
MAXETTM  DC    F'60'               MAX ET  TIME ALLOWED IN SEC                  
APPCECB  DC    F'0'                ECB FOR APPC/MVS CALLS                       
PQSRCHTM DS    F                   TIME WE LAST SEARCHED PRINT QUEUES           
MINTIME  DC    F'0'                ATTACH REQS ONLY >= MINTIME MINUTES          
WAITSECS DC    F'01000'            DEFAULT WAIT TIME = 10 SECONDS               
REQTIMER DC    F'5'                WAIT 5 MINUTES BETWEEN WARNINGS              
LOADAVD  DC    F'0'                TOTAL # OF LOADS AVOIDED                     
LOADASK  DC    F'0'                TOTAL # OF LOAD REQUESTS                     
TOTREQS  DC    F'0'                TOTAL NUMBER OF REQUESTS                     
USERID   DS    CL10                                                             
MONMODE  DC    AL1(YES)                                                         
*                                  (EXCLUDE THE 1ST ENTRY)                      
*&&US                                                                           
USERMAXR DC    H'12'               MAXIMUM REQS PER USER IN REQTABLE            
SUBMAXR  DC    H'5'                DEFAULT IS 5                                 
*&&                                                                             
*&&UK                                                                           
USERMAXR DC    H'5'                MAXIMUM REQS PER USER IN REQTABLE            
SUBMAXR  DC    X'FFFF'             DEFAULT IS NONE                              
*&&                                                                             
EXUSRIDS DC    (EXUSRMXQ+1)H'0'    OPTIONAL HEX USERID NEGATIVE FILTERS         
EXUSRMXQ EQU   20                   MAXIMUM NUMBER OF EXCLUDED USERIDS          
USERFLTS DC    (USERFMXQ+1)H'0'    OPTIONAL HEX USERID FILTERS                  
USERFMXQ EQU   20                   MAXIMUM NUMBER OF USERID FILTERS            
         DC    C'**EXAGYS'                                                      
EXAGYS   DC    (EXAGYMXQ+1)H'0'    OPTIONAL AGENCY NEGATIVE FILTERS             
EXAGYMXQ EQU   20                   MAXIMUM NUMBER OF EXCLUDED AGENCIES         
         DC    C'**AGYFLT'                                                      
AGYFLTS  DC    (AGYFMXQ+1)H'0'     OPTIONAL AGENCY FILTERS                      
AGYFMXQ  EQU   20                   MAXIMUM NUMBER OF AGENCY FILTERS            
         DC    C'*REPTFLT'                                                      
REPTFLTS DC    (REPTFMXQ+1)AL3(0)  OPTIONAL REPORT TYPE FILTERS                 
REPTFMXQ EQU   10                   MAX NUMBER OF REPORT TYPE FILTERS           
SUBFILT  DC    CL3'   '            OPTIONAL SUBID FILTER                        
REFFILT  DC    XL2'00'             OPTIONAL REFERENCE NUMBER FILTER             
EXREPORT DC    CL3' '              EXCLUDE THESE REPORTS (SPP)                  
CLASS    DS    XL(CLSTABLQ)        CLASS TABLE ENTRY                            
MAJORNAM DC    CL8'MONSTER '       MAJOR NAME TO GLOBALLY ENQ                   
MINORNAM DC    CL8' '              MINOR NAME TO GLOBALLY ENQ                   
DSPACE   DC    C' '                DSPACE PARAMETER FOR MASTER                  
TOTCONV  DS    F                   MAX# OF CONVERSATIONS (USER MAY +/-)         
SPRCONV  DS    F                   # OF CONVERSATIONS TO SPARE                  
MAXCONV  DS    F                   MAX# OF CONVERSATIONS AVAILABLE              
ERR26CNT DS    F                   # CONSECUTIVE APPC ERROR 26 COUNTER          
ERR26TM  DC    F'0'                LAST TIME WHEN APPC ERROR 26 HAPPEN          
PARTLUID DC    CL17'MVSLU02'       PARTNER LUID                                 
TPNAME   DC    CL64'MONSOONT'      TPNAME                                       
TPNAMELN DC    F'8'                TPNAME LENGTH                                
MVSNAME  DC    CL8'        '       MVSNAME                                      
*                                                                               
PRTYS    DS    10F                 PRIORITY ACCUMULATORS                        
*                                   (INDEXED: WORD 1  = PRIORITY 0...           
*                                             WORD 10 = PRIORITY 9)             
*                                                                               
ATTCHPGM DS    CL8                 NAME OF PROGRAM TO ATTACH                    
DIRECTID DS    CL18                DIRECT CARD ID=                              
MINPRTY  DC    C'0'                LOWEST ACCEPTABLE PRIORITY TO ATTACH         
MAXPRTY  DC    C'9'                HIGHEST PRIORITY TO ATTACH                   
REFPRTY  DC    C'0'                LOWEST PRIORITY BEFORE REFRESH               
FACPAK   DC    C' '                FACPAK FILTER                                
FACPAKFL DS    C                   'P' = FACPAK FILTER IS POSITIVE              
*                                  'N' = FACPAK FILTER IS NEGATIVE              
RECYLTPS DC    AL1(NO)             'Y' = RECYCLING ALL ASCH TP'S                
ABENDFLG DC    AL1(NO)             'Y' = MONSOONM JOB ABENDED                   
TRACEFLG DC    AL1(NO)             'Y' = PRINT DETAILED TRACE                   
ALLCLASS DC    AL1(NO)             'Y' = ACCEPT ALL SOON CLASSES                
OPERSTOP DC    AL1(NO)             'Y' = OPERATOR ENTERED 'STOP'                
ESHTDOWN DC    AL1(NO)             'Y' = SHUTDOWN NEEDED DUE TO ERROR           
ATTCHFLG DC    AL1(YES)            'Y' = ATTACH REQUESTS                        
TYPUPD   DC    AL1(NO)             'Y' = ATTACH ONLY UPDATIVE SOONS             
TYPNOUPD DC    AL1(NO)             'Y' = ATTACH ONLY NON-UPDATIVE SOONS         
QTYPE    DC    C' '                INCLUDE QUEUE TYPE - DEFAULT IS ALL          
XQTYPE   DC    C' '                EXCLUDE QUEUE TYPE                           
SVJCTYPE DS    C' '                SAVE JCTYPE FOR JOB                          
YESTERDY DC    AL1(NO)             'Y' = ATTACH ONLY YESTERDAY'S JOBS           
ANYDATE  DC    AL1(NO)             'Y' = ATTACH REQUESTS FROM ANY DATE          
UPDATIVE DS    C                   'Y' = RECOVER=W SPECIFIED (UPDATIVE)         
JOBSLFTP DC    AL1(NO)             'Y' = REQUESTS LEFT TO RUN                   
         DS    C                                                                
*                                                                               
FREECONV DC    AL1(YES)            'Y' = FREE CONVERSATION AVALIABLE            
JESMSG   DC    AL1(NO)             'Y' = DO CONSOLE MSGS FOR START/END          
BACKMSG  DC    AL1(YES)            'Y' = DO CONSOLE MSGS FOR BACK LOG           
CLSPRTYF DC    AL1(NO)             'Y' = CLASS LIST IS PRIORITIZED              
LOPRTYF1 DC    AL1(NO)             'Y' = WE'VE FOUND A LOW PRTY REQUEST         
ENQJOBNM DC    AL1(NO)             'Y' = ONLY ONE REQUEST PER JOBNAME           
DUMPWARN DC    AL1(YES)            'N' = NO WARNING FOR DUPLICATE DUMPS         
UNLMNGHT DC    XL4'FFFFFFFF'       UNLIMITED NIGHT STARTS AT HHMM               
UNLMWKND DC    AL1(NO)             'Y' = UNLIMITED WEEKEND/HOLIDAY              
WKNFLAG  DC    AL1(NO)             'Y' = TODAY IS A WEEKEND/HOLIDAY             
POSTTIME DC    AL1(NO)             'Y' = POST TIME IN DATASPACE JOBTAB          
DSSUBTIM DS    XL2                 EARLIEST JOB SUBMISSION TIME                 
STIMERID DC    XL4'00'             FOR TIMER POPS                               
CARD     DS    CL80                FOR INPUT/OUTPUT OF CONTROL CARDS            
DDSIOMOD DC    CL8'DDSIO   '       DDSIO LOAD MODULE NAME                       
KEY      DS    XL25                FOR PQ INDEX AND CTFILE READS                
SCNBLK   DS    XL256                                                            
         EJECT                                                                  
***********************************************************************         
* PARAMETER LISTS FOR APPC/MVS CALLS                                  *         
*  F    A(ROUTINE)                                                    *         
*  CL16 EBCDIC ROUTINE NAME                                           *         
*  XL1  FLAGS                                                         *         
*       X'80': ROUTINE IS SYNCHRONOUS ONLY                            *         
*       X'40': ROUTINE CAN BE FOLLOWED BY EXTRACT_ERROR CALL          *         
*  XL3  SPARE                                                         *         
*  PARAMETERS (STANDARD IBM FORMAT)                                   *         
***********************************************************************         
ATBAMR1_STRUCTURE         DS    A                                               
                          DC    CL16'ASYNC. MANAGER'                            
                          DC    X'80'                                           
                          DS    XL3                                             
                          DC    X'00',AL3(FUNCTION_AM)                          
                          DC    X'00',AL3(ASYNCHRONOUS_NUMBER_AM)               
                          DC    X'80',AL3(RETURN_CODE_AM)                       
*                                                                               
FUNCTION_AM               DC    F'2'  ASYNCHRONOUS MANAGER CLEANUP              
ASYNCHRONOUS_NUMBER_AM    DS    F                                               
RETURN_CODE_AM            DS    F                                               
*                                                                               
AATBALC2 DS    A                   A(APPC/MVS ALLOCATE ROUTINE)                 
AATBCFM  DS    A                   A(APPC/MVS CONFIRM ROUTINE)                  
AATBCFMD DS    A                   A(APPC/MVS CONFIRMED ROUTINE)                
AATBDEAL DS    A                   A(APPC/MVS DEALLOCATE ROUTINE)               
AATBSEND DS    A                   A(APPC/MVS SEND_DATA ROUTINE)                
AATBGTA2 DS    A                   A(APPC/MVS GET_ATTRIBUTES ROUTINE)           
AATBEES3 DS    A                   A(APPC/MVS ERROR_EXTRACT ROUTINE)            
AATBRCVW DS    A                   A(APPC/MVS RECEIVE_AND_WAIT ROUTINE)         
AECBLIST DS    A                   A(ECBLIST)                                   
ASRTLIST DS    A                   A(SRTLIST)                                   
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
ATBALC2  DC    CL8'ATBALC2'                                                     
         DC    XL52'00'                                                         
ATBAMR1  DC    CL8'ATBAMR1'                                                     
         DC    XL52'00'                                                         
ATBCFM   DC    CL8'ATBCFM'                                                      
         DC    XL52'00'                                                         
ATBCFMD  DC    CL8'ATBCFMD'                                                     
         DC    XL52'00'                                                         
ATBDEAL  DC    CL8'ATBDEAL'                                                     
         DC    XL52'00'                                                         
ATBEES3  DC    CL8'ATBEES3'                                                     
         DC    XL52'00'                                                         
ATBGTA2  DC    CL8'ATBGTA2'                                                     
         DC    XL52'00'                                                         
ATBRCVW  DC    CL8'ATBRCVW'                                                     
         DC    XL52'00'                                                         
ATBSEND  DC    CL8'ATBSEND'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*  1ST ENTRY IN THIS TABLE IS THE TP NAME FOR ALL OTHER CLASSES       *         
*  GIVEN IN THE "CLASS=ALL,..." CARD                                  *         
***********************************************************************         
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
         DC    CL16'TPNAMTABTPNAMTAB'                                           
MAXTPQ   EQU   200                 MAXIMUM # OF TP NAMES + 1 EMPTY ONE          
TPTAB    DC    (MAXTPQ+1)XL(TPTABLQ)'00'                                        
*                                                                               
TPTABD   DSECT                                                                  
TPTCON   DS    H                   # CONVERSATIONS USED                         
TPTMCN   DS    H                   # MAX CONVERSATIONS                          
TPTLEN   DS    H                   TP NAME LENGTH                               
TPTNAM   DS    CL64                TP NAME                                      
TPTABLQ  EQU   *-TPTABD                                                         
*                                                                               
MONSTER  CSECT                                                                  
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
         DC    CL16'*CXREC***CXREC**'                                           
CXREC    DC    14336X'00'          PRINT QUEUE BUFFER                           
         EJECT                                                                  
***********************************************************************         
* FACPAK ID TABLE                                                     *         
***********************************************************************         
*FACIDTABL (US)  FACIDTAB (UK)                                                  
*&&US                                                                           
       ++INCLUDE FACIDTABL                                                      
*&&                                                                             
*&&UK                                                                           
       ++INCLUDE FACIDTAB                                                       
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* SSB AND UTL ENTRIES                                                 *         
***********************************************************************         
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
         DC    CL16'*UTL*UTL*UTL*TUL'                                           
UTL      DC    F'0',X'0A',251X'00' FOR DATAMGR (CONTROL SYSTEM)                 
*                                                                               
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
*                                                                               
         DC    CL16'*SSB*SSB*SSB*SSB'                                           
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         EJECT                                                                  
***********************************************************************         
* OTHER USEFUL BLOCKS                                                 *         
***********************************************************************         
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
         DC    CL16'RDCHAIN*RDCHAIN*'                                           
R13CHAIN DS    5000D               WORKING STORAGE                              
         DC    CL16'RDCHAINXRDCHAINX'                                           
*                                                                               
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
         DC    CL16'I/O*I/O*I/O*I/O*'                                           
IO       DC    2048X'00'           CTFILE I/O AREA (FOR ID RECORDS)             
*                                                                               
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
         DC    CL16'*BIGBUF**BIGBUF*'                                           
MAXRECQ  EQU   50                                                               
BIGBUF   DS    (MAXRECQ)CL80       BUFFER FOR LONG SOON REC                     
BIGBUFLQ EQU   *-BIGBUF                                                         
*                                                                               
MONSTER  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CLASS TABLE DEFINITIONS                                             *         
***********************************************************************         
         ORG   MONSTER+(((*-MONSTER+15)/16)*16)                                 
         DC    CL16'CLASSTAB*CLASSTAB*'                                         
MAXCLASS EQU   256                 MAXIMUM NUMBER OF CLASSES                    
CLASSTAB DC    (MAXCLASS)XL(CLSTABLQ)'00'    LIST OF CLASSES TO ATTACH          
*                                                                               
CLASSD   DSECT                                                                  
CLSNAME  DS    CL2                 CLASS NAME (E.G. A3, S4, ETC.)               
CLSKEYLQ EQU   *-CLASSD                                                         
*                                                                               
CLSMAX   DS    H                   MAX. NO. OF REQS IN THIS GO-ROUND            
CLSPRTY  DS    H                   PRIORITY OF THIS CLASS (1..N)                
CLSTOTAL DS    H                   TOTAL REQS ATTACHED FOR THIS CLASS           
CLSTIME  DS    F                   TOTAL SUBMIT TIME FOR CLASS (MINS)           
CLSREQW  DS    H                   NO. OF REQS AWAITING EXECUTION               
CLSREQI  DS    H                   NO. OF REQS IN THE REQUEST TABLE             
CLSTP    DS    A                   ADDRESS OF TP INFO                           
CLSPAWT  DS    F                   PREVIOUS AVERAGE WAITING TIME                
CLSRDTM  DS    F                   TIME @ WHICH LAST READING WAS TAKEN          
CLSPRATE DS    F                   PREV RATE OF CHANGE(AVE WTNG TIME)           
*                                    + : AVE WAITING TIME IS INCREASING         
*                                    - : AVE WAITING TIME IS DECREASING         
CLSTABLQ EQU   *-CLASSD                                                         
         EJECT                                                                  
***********************************************************************         
* APPC CONVERSATION TABLE                                             *         
***********************************************************************         
APPCD    DSECT                                                                  
APPC_ECB                        DS    F     MUST BE FIRST FIELD                 
APPCANXT                        DS    A     A(START OF NEXT APPC/MVS)           
REQUEST_TABLE_ENTRY             DS    A     A(REQUEST TABLE ENTRY)              
APPCPQ                          DS    C     PQ NUMBER FOR THIS REPORT           
CONVNUM                         DS    X                                         
*                                                                               
OPMS1H                          DS    AL2                                       
OPMS1                           DS    CL42                                      
*                                                                               
JOBNAME                         DS    0CL11 MIMICS MVS "JOBNAME"                
JOBUSER                         DS    CL8   USERID                              
JOBSYPGM                        DS    0CL3                                      
JOBSYST                         DS    C     SYSTEM                              
JOBPROG                         DS    CL2   PROGRAM                             
*                                                                               
REPORT_STATUS                   DS    C                                         
REPORT_TRANSMIT_OK              EQU   0                                         
REPORT_TRANSMIT_BAD             EQU   1                                         
*                                                                               
CONVERSATION_TYPE               DS    F                                         
SYM_DEST_NAME                   DS    CL8                                       
LOCAL_LU_NAME                   DS    CL8                                       
MODE_NAME                       DS    CL8                                       
PARTNER_LU_NAME                 DS    CL17                                      
TP_NAME_LENGTH                  DS    F                                         
TP_NAME                         DS    CL64                                      
RETURN_CONTROL                  DS    F                                         
SYNC_LEVEL                      DS    F                                         
SECURITY_TYPE                   DS    F                                         
NOTIFY_TYPE                     DS    F                                         
                                DS    A     MUST FOLLOW NOTIFY_TYPE             
*                                                                               
USER_ID                         DS    CL10                                      
PASSWORD                        DS    CL10                                      
PROFILE                         DS    CL10                                      
USER_TOKEN                      DS    XL80                                      
CONVERSATION_ID                 DS    CL8                                       
CONVERSATION_CORRELATOR         DS    CL8                                       
TPID                            DS    XL8                                       
LUW_ID                          DS    XL26                                      
CONVERSATION_STATE              DS    F                                         
RETURN_CODE                     DS    F                                         
DEALLOCATE_TYPE                 DS    F                                         
FILL                            DS    F                                         
RECEIVE_LENGTH                  DS    F                                         
RECEIVE_ACCESS_TOKEN            DS    F                                         
STATUS_RECEIVED                 DS    F                                         
DATA_RECEIVED                   DS    F                                         
REQUEST_TO_SEND_RECEIVED        DS    F                                         
REQUEST_TO_SEND_VALUE           DS    F                                         
SEND_TYPE                       DS    F                                         
SEND_LENGTH                     DS    F                                         
SEND_ACCESS_TOKEN               DS    F                                         
BUFFER                          DS    CL256                                     
SERVICE_NAME                    DS    CL8                                       
SERVICE_REASON_CODE             DS    F                                         
MESSAGE_TEXT_LENGTH             DS    F                                         
MESSAGE_TEXT                    DS    CL256                                     
ERROR_LOG_PRODUCT_SET_ID_LENGTH DS    F                                         
ERROR_LOG_PRODUCT_SET_ID        DS    CL256                                     
ERROR_LOG_INFORMATION_LENGTH    DS    F                                         
ERROR_LOG_INFORMATION           DS    CL512                                     
REASON_CODE_ERROR_EXTRACT       DS    F                                         
RETURN_CODE_ERROR_EXTRACT       DS    F                                         
                                                                                
***********************************************************************         
* EACH STRUCTURE BELOW CONSISTS OF                                              
*   A(ROUTINE)                                                                  
*   CL16'ROUTINE NAME'                                                          
*   XL1  FLAGS                                                                  
*        X'40': ROUTINE CAN BE FOLLOWED BY ERROR_EXTRACT CALL                   
*   XL3  SPARE                                                                  
*   ROUTINE PARAMETER LIST                                                      
*                                                                               
***********************************************************************         
ATBALC2_STRUCTURE    DS    (ATBALC2_BLOCK_LIST_LENGTH/4)F                       
ATBCFM_STRUCTURE     DS    (ATBCFM_BLOCK_LIST_LENGTH/4)F                        
ATBCFMD_STRUCTURE    DS    (ATBCFMD_BLOCK_LIST_LENGTH/4)F                       
ATBDEAL_STRUCTURE    DS    (ATBDEAL_BLOCK_LIST_LENGTH/4)F                       
ATBSEND_STRUCTURE    DS    (ATBSEND_BLOCK_LIST_LENGTH/4)F                       
ATBGTA2_STRUCTURE    DS    (ATBGTA2_BLOCK_LIST_LENGTH/4)F                       
ATBEES3_STRUCTURE    DS    (ATBEES3_BLOCK_LIST_LENGTH/4)F                       
ATBRCVW_STRUCTURE    DS    (ATBRCVW_BLOCK_LIST_LENGTH/4)F                       
APPCDLEN EQU   *-APPCD                                                          
         EJECT                                                                  
       ++INCLUDE DDAPPCD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SCHEDULE SORT BLOCK FOR THE CLASS ARRAYS             *         
***********************************************************************         
*&&US                                                                           
SRTD     DSECT ,                   FIRST CUT AT SCHEDULING                      
SRTPRI   DS    XL(L'TBJPRTY)       PRIORITY                                     
SRTTIME  DS    XL(L'TBJSTIME)      SUBMIT TIME                                  
SRT1AWT  DS    XL2                 INTERNAL AGENCY WEIGHTING                    
SRTKEYL  EQU   *-SRTD                                                           
SRTCLS   DS    XL(L'JCCLASS)       CLASS                                        
SRTAGY   DS    XL(L'JCAGY)         AGENCY                                       
SRTSUBID DS    XL3                 REPORT SUB ID                                
SRTREPT  DS    XL2                 REPORT TYPE                                  
SRTREPTN DS    XL1                 REPORT TYPE COUNTER                          
SRTACLS  DS    AL4                 A(CLASS ARRAY ENTRY)                         
SRTAJOB  DS    AL4                 A(JOB TABLE ENTRY)                           
SRTRECL  EQU   *-SRTD                                                           
*&&                                                                             
*&&UK                                                                           
SRTD     DSECT ,                   FIRST CUT AT SCHEDULING                      
SRTPRI   DS    XL(L'TBJPRTY)       PRIORITY                                     
SRTTIME  DS    XL(L'TBJSTIME)      SUBMIT TIME                                  
SRT1AWT  DS    XL2                 INTERNAL AGENCY WEIGHTING                    
SRTKEYL  EQU   *-SRTD                                                           
SRTAGY   DS    XL(L'JCAGY)         AGENCY                                       
SRTCLS   DS    XL(L'JCCLASS)       CLASS                                        
SRTACLS  DS    AL4                 A(CLASS ARRAY ENTRY)                         
SRTAJOB  DS    AL4                 A(JOB TABLE ENTRY)                           
SRTRECL  EQU   *-SRTD                                                           
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
                                                                                
* DDBSPARA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBSPARA                                                       
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
* DMPRTQS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQS                                                        
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* DDGETRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGETRETD                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDMONSBUFF                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDMONSBUFF                                                     
         PRINT ON                                                               
* FATABSJOB                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSJOB                                                      
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* IHASDWA MACRO                                                                 
         IHASDWA                                                                
         PRINT ON                                                               
* IEZCOM MACRO                                                                  
         PRINT OFF                                                              
         IEZCOM                                                                 
         PRINT ON                                                               
*                                                                               
         IHAASCB                                                                
*                                                                               
         PRINT ON                                                               
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
         DSECT                                                                  
         IEZCIB                                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084DDMONSTER 09/21/18'                                      
         END                                                                    
