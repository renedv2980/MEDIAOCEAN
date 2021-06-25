*          DATA SET SPTRA2C    AT LEVEL 203 AS OF 11/09/20                      
*PHASE T2162CB                                                                  
*INCLUDE GETBROAD                                                               
*INCLUDE SORTER                                                                 
*INCLUDE OFFOUT                                                                 
*INCLUDE GRID                                                                   
*  TITLE: T2162C - TRAFFIC NETWORK LIST COM SCHEDULE/BUY ACTIVITY     *         
*                                                                     *         
*  COMMENTS: THIS PROGRAM WILL LIST COMMERCIALS ASSIGNED TO UNITS     *         
*            OR LIST THOSE UNITS NEEDING ASSIGN OR REASSIGN.          *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - POINTER TO NETBLOCKD                                  *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO UNIT TABLE                      *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*                    USED IN REDREV, CKEST WHILE READING UNITS        *         
*             AIO2 - USED IN LRR RTN IF UPDATING UNITS FOR REASSIGN   *         
*             AIO3 - NETIO                                            *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTRA00-T21600)                   *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2162C NETWORK TRAFFIC - COMML SCHEDULE/BUY ACTIVITY'           
***********************************************************************         
*                                                                     *         
*  LEV134 SMUR JUL09/01 FIX BUYACT (UNA) REPORT                       *         
*  LEV135 BGRI JUL20/01 FORCE INCOMPLETE CUTINS TO SHOW ON BUY ACT    *         
*  LEV136 SMUR AUG20/01 MORE COMMERCIAL VALIDATION FOR STARCOM        *         
*  LEV137 SMUR DEC14/01 BYPASS OLD CUT-IN ELEMENT                     *         
*  LEV141 SMUR JAN09/02 FIX PFKEY7                                    *         
*  LEV142 SMUR MAR22/02 FIX C/S PRD                                   *         
*  LEV143 BGRI MAY09/02 FIX CLT/PRD BLK IF RUNNING BY OFFICE          *         
*                       AND IN GSTAT RTN                                        
*  LEV145 SMUR JUN25/02 CLIENT STRING SECURITY (NOT BY OFFICE)        *         
*  LEV146 SMUR DEC05/02 WEATHER CHANNEL 5 SECOND TAGS                 *         
*  LEV147 SMUR APR02/03 TOTAL LEN =CML LEN + VIGNETTE LEN             *         
*  LEV149 SMUR JUN02/03 SHOW ALL UNITS                                *         
*  LEV150 SMUR JUN18/03 2 CHAR DAYPART                                *         
*  LEV151 BGRI SEP04/03 CHANGE INVPRD TO INVPRDE                      *         
*  LEV151 SMUR SEP30/03 PRINT '??' IF INVALID DAYPART CODE            *         
*  LEV152 BGRI OCT17/03 ADD 253 PRODUCTS                              *         
*  LEV153 BGRI JUN14/04 ADD COMML LEN=ALL (X'FF')                     *         
*  LEV154 SMUR JUN25/04 AD-ID                                         *         
*  LEV155 SMUR AUG02/04 PRINT AD-ID IN REPORT (PHASE II)              *         
*  LEV156 BGRI SEP24/04 FIX OPTCML & PRINT RT                         *         
*  LEV157 BGRI NOV05/04 FIX OPTCML FOR *&^%$ NELSON'S SHORT 21 EL     *         
*  LEV162 SMUR JAN25/04 PRINT CML THAT NEEDS TO BE REASSIGN           *         
*  LEV163 SMUR OCT03/05 2 CHAR OFFICE CODE                            *         
*  LEV164 SMUR APR11/07 FIX MORE BRAND BUG IN CODE TO PRINT FEEDS     *         
*  LEV165 MNAS JUL11/07 FIX PU=C BUG WHEN VERIFYING PRODUCT GROUP     *         
*                       NEED TO READ CLI REC INSTEAD OF SVNCLIST      *         
*  LEV167 MNAS JAN25/08 FIX TEMPSTR CALL THAT IS CREAMING AESTBL      *         
*                       ADDRESS                                       *         
*  LEV168 MNAS FEB25/08 GRIDS CODE LIVE                               *         
*  LEV170 SMUR MAR05/08 VIGNETTE IN NET LIST AND GRIDS                *         
*  LEV172 MNAS JUL07/08 NEW TRAFFIC TYPE ON MASTER RECORD             *         
*  LEV173 SMUR AUG07/08 ACCEPT 9-12 CHAR ADID CODES                   *         
*  LEV177 SMUR DEC18/08 FOR SJ IF NO TYPE IN MASTER REC SET IT TO 'N' *         
*  LEV182 SMUR OCT29/09 CLEAR PROD GROUP IN KEY WHEN READING REV RECS *         
*  LEV183 MNAS OCT29/09 FIX MAXIO ISSUE BY SAVING THE CLIENT RECORD   *         
*         TO PASS TO NETIO, THEREFORE REDUCING THE READS BY NETVALUE  *         
*         AND ADD PRODUCT GROUP CHANGES                               *         
*  LEV184 MNAS OCT29/09 FIX BUG IN GRID END-OF-GRID ROUTINE : DDGRID  *         
*                       WAS CHANED FOR THIS FIX AS WELL               *         
*  LEV186 MNAS JUL09/10 FIX BUG WHEN PRDS NOT IN ALPHA SEQ - COMMLS   *         
*         BEING SWAPPED BUT PRDS WERE NOT - HENCE COMMLS=REASSIGN     *         
*  LEV187 MNAS AUG05/10 GETBROAD DUMPING WITH INVALID START DATE -    *         
*         STDT BEING ALTERED IN VPROG, BUT NOT BEING RESET BEFORE XIT *         
*  LEV188 MNAS AUG16/10 BUG FIX WHEN DISPLAYING ESTIMATE PACKAGE      *         
*  LEV197 MNAS APR15/11 GRIDS BUG WHEN REPORTING 2 CHAR DAYPART CODES *         
*  LEV198 MNAS JUN11/13 PROFILE TO INCLUDE/EXCLUDE DIGITAL            *         
*  LEV199 MNAS JUL02/13 FIX DUMPING ON UNALLOCATED COPY SPLIT         *         
*  LEV200 SMUR SEP12/14 FIX BUYACTIVITY FOR MONTHLY/WKLY TRAFFICKING  *         
*  LEV202 SMUR JUN06/16 UNDO LEV 199: ALL C/S PRODS COME UP AS UNAL   *         
*                       THE BUG WAS FIXED IN NBUY WE SHOULD NOT DUMP  *         
*         SMUR NOV08/16 FIX CML VALIDATION LOGIC WHEN CML HAS EXCL NET*         
*  SPEC-42503  SMUR JAN29/20 SUPPORT CONVERTED REVISION DATES         *         
***********************************************************************         
         TITLE 'T2162C NETWORK TRAFFIC - COMML SCHEDULE/BUY ACTIVITY'           
T2162C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2162C**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR2CRR                                                      
*                                                                               
         LM    RE,RF,=V(SORTER,GETBROAD)                                        
         AR    RE,R3               RELOCATE SORTER                              
         ST    RE,VSORTER                                                       
         AR    RF,R3               RELOCATE                                     
         ST    RF,VGTBROAD                                                      
*                                                                               
         L     RF,=A(GCTBL)        GRID CONTROL TABLE                           
         AR    RF,R3                                                            
         ST    RF,AGFT             A(GRID FORMAT TABLE)                         
*                                                                               
         L     RE,=A(ESTBL)        ESTIMATE TABLE                               
         AR    RE,R3                                                            
         ST    RE,AESTBL           A(ESTIMATE TABLE)                            
         L     RE,=A(OPTPGRL)      PRODUCT GROUP TABLE                          
         AR    RE,R3                                                            
         ST    RE,AOPTPGRL         A(PRODUCT GROUP TABLE)                       
*                                                                               
         L     RE,=A(CLTREC)       PRODUCT GROUP TABLE                          
         AR    RE,R3                                                            
         ST    RE,ACLTREC          A(PRODUCT GROUP TABLE)                       
*                                                                               
         CLC   LSYSD,=A(ENDSYSD-SYSD)                                           
         BNL   *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R1,SVBCLT                                                        
         ST    R1,ASVSTOR                                                       
         LH    R3,=AL2(STNETBLK-SYSD)                                           
         AR    R3,R9                                                            
         USING NETBLOCKD,R3                                                     
                                                                                
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   MAIN03                                                           
*                                                                               
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
         TM    PCGRIDS,X'10'                                                    
         BZ    MAIN01                                                           
         NI    PCGRIDS,X'FF'-X'10'                                              
         BRAS  RE,GRIDEND                                                       
         B     EXIT                                                             
         DROP  R5                                                               
                                                                                
MAIN01   DS    0H                                                               
         LA    RE,HEAD1                                                         
         LHI   RF,2000                                                          
         XCEFL                                                                  
                                                                                
         LA    RE,DLCB                                                          
         LHI   RF,256                                                           
         XCEFL                                                                  
                                                                                
         GOTO1 =V(GRID),DMCB,(C'C',AGFT),ATWA,ASPOOLD,ACOMFACS,        +        
               RR=SPTR2CRR               CLEAR THE GRID SCREEN                  
MAIN03   EQU   *                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   MAIN10                                                           
                                                                                
         CLI   TRLSTPGM,X'2C'      WAS NET LIST LAST PROG USED                  
         BE    MAIN05               YES                                         
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    MAIN05                                                           
                                                                                
         MVI   TRLSTPGM,X'2C'      USING NET LIST                               
                                                                                
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   MAIN04                                                           
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
         NI    PCGRIDS,X'FF'-PCGBEGQ                                            
         NI    PCGRIDS,X'FF'-PCGFINQ                                            
         DROP  R5                                                               
                                                                                
MAIN04   OC    CONWHEN,CONWHEN     PRINT OPTION                                 
         BZ    MAIN05                                                           
         TM    CONWHENH+4,X'80'    INPUT THIS TIME                              
         BO    MAIN05                                                           
         XC    CONWHEN,CONWHEN     NO, CLEAR PRINT FIELD                        
         MVI   CONWHENH+5,0         LENGTH                                      
         MVI   WHEN,X'80'          RESET WHEN TO IMMEDIATE                      
         NI    CONWHENH+4,X'FF'-X'40'  TURN OFF INPUT PREVIOUSLY                
         OI    CONWHENH+6,X'80'    TRANSMIT                                     
         L     RF,EFHWHEN          USE FOR EXTENDED HEADERS                     
         MVI   5(RF),0                                                          
                                                                                
MAIN05   BRAS  RE,VK                                                            
         B     EXIT                                                             
*                                                                               
MAIN10   CLI   ACTNUM,29           ACTION GRID                                  
         BNE   MAIN11                                                           
*                                                                               
         BRAS  RE,GRIDINIT         INITIALIZE GRID                              
         BNE   GRIDXIT                                                          
*                                                                               
         BRAS  RE,LRL              LIST RECORDS WITH GRIDS                      
*                                                                               
         TM    SVFLAG,GRIDNOUN                                                  
         BZ    GRIDXIT                                                          
         GOTO1 =V(GRID),DMCB,(C'C',AGFT),ATWA,ASPOOLD,ACOMFACS,        +        
               RR=SPTR2CRR               CLEAR THE GRID SCREEN                  
         NI    SVFLAG,X'FF'-GRIDNOUN                                            
                                                                                
         OI    CONHEADH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'IE#0500 NO UNITS FOUND'                           
                                                                                
         NI    TRACLTH+4,X'FF'-X'20'                                            
         NI    TRACLTH+4,X'FF'-X'40'  TURN OFF INPUT PREVIOUSLY                 
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
         NI    PCGRIDS,X'FF'-PCGBEGQ                                            
         NI    PCGRIDS,X'FF'-PCGFINQ                                            
         DROP  R5                                                               
                                                                                
         GOTO1 ERREX2                                                           
*                                                                               
         B     GRIDXIT                                                          
*                                                                               
MAIN11   CLI   OFFLINE,C'Y'                                                     
         BE    MAIN20                                                           
                                                                                
         CLI   TRLSTPGM,X'2C'      WAS NET LIST LAST PROG USED                  
         BE    MAIN20               YES                                         
         NI    TRACLTH+4,X'FF'-X'20'                                            
                                                                                
         OC    CONWHEN,CONWHEN     PRINT OPTION                                 
         BZ    MAIN05                                                           
         TM    CONWHENH+4,X'80'    INPUT THIS TIME                              
         BO    MAIN05                                                           
         XC    CONWHEN,CONWHEN     NO, CLEAR PRINT OPTION                       
         NI    CONWHENH+4,X'FF'-X'40'  TURN OFF INPUT PREVIOUSLY                
         OI    CONWHENH+6,X'80'    TRANSMIT                                     
         L     RF,EFHWHEN          USE FOR EXTENDED HEADERS                     
         MVI   5(RF),0                                                          
         B     MAIN05                                                           
                                                                                
MAIN20   CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BNE   MAIN30                                                           
         BRAS  RE,LRL                                                           
         B     EXIT                                                             
MAIN30   CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                                                              
         B     EXIT                                                             
*                                                                               
GRIDXIT  MVC   CONHEAD,GMSG                                                     
         OI    CONHEAD+6,X'80'                                                  
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     XIT1                                                                   
                                                                                
NEQEXIT  LTR   RB,RB                                                            
         B     EXIT                                                             
EQEXIT   CR    RB,RB                                                            
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE/OFFLINE REPORT ROUTINE *                                               
                                                                                
LRR      BRAS  RE,INIT                                                          
                                                                                
* FORMAT OFFLINE REPORT                                                         
                                                                                
LRR02    BRAS  RE,GTEQV                                                         
                                                                                
         BRAS  RE,NETI                                                          
         MVI   NBSEQ,C'N'          NETWORK SEQ                                  
                                                                                
         TM    UNITSW,UNITSWMI     FILTER BY NEGATIVE UNITS                     
         BO    *+12                 YES                                         
                                                                                
         TM    UNITSW,UNITSWAC     BUY ACTIVITY REPORT                          
         BZ    LRR04               NO                                           
                                                                                
         MVI   NBSELUOP,C'B'       GET EST AND ACTUAL                           
         OI    NBDMGOPT,X'08'       AND DELETED UNITS                           
LRR04    DS    0H                                                               
         TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BZ    LRR04C                                                           
         BAS   RE,GETSRT           GET KEY INTO NBKEY                           
         BZ    LRRX100                                                          
         MVI   NBFUNCT,NBFGET                                                   
         B     LRR04F                                                           
                                                                                
LRR04C   DS    0H                                                               
         OC    NBKEY,NBKEY         1ST TIME                                     
         BZ    LRR04F               YES                                         
                                                                                
         CLI   NBMODE,NBPROCUN     PROCESS UNIT                                 
         BNE   LRR04F               NO                                          
                                                                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',NBKEY,KEY             
         CLC   KEY(20),NBKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
LRR04F   OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
*        THIS INSTRUCTION WAS NOT NECESSARY IN PRE MORE BRANDS CODE             
*        BUT NOW WILL NOT WORK WITHOUT IT - NETIO FAILS WHEN YOU ASK            
*        IT TO READ THE RECORD FOR YOU                                          
         MVC   NBACTAM,BAGYMD                                                   
*                                                                               
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
*                                                                               
         TM    OPTNSW1,OPTDIGI     OPTION TO SHOW DIGITAL                       
         BO    *+20                                                             
         CLI   SVTN2PRO+14,C'Y'    PROFILE TO BYPASS DIGITAL                    
         BNE   *+12                                                             
         CLI   NBKEY+(NUKSTAT-NUKEY),X'04'   DIGITAL VIDEO MEDIA                
         BE    LRR10                                                            
*                                                                               
         TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BZ    *+12                                                             
         TM    NBPACKST,X'20'      LOCKED?                                      
         BNZ   LRR04                                                            
*                                                                               
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    LRR06                                                            
         DC    H'0'                                                             
LRR06    CLI   NBMODE,NBREQLST                                                  
         BE    LRRX                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRR04                                                            
         B     LRR20                                                            
                                                                                
LRR10    DS    0H                                                               
                                                                                
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    LRR12                NO                                          
         LA    R2,CONWHENH         FOR CURSOR POSN                              
* *%$#**  SPOOL IGNORES CODE!                                                   
*         MVI   SPMODE,X'FE'        DELETE PARTIALLY GENERATED REPORT           
*         GOTO1 SPOOL,DMCB,(R8)                                                 
         CLI   SPOOLKEY,0          TEST REPORT GENERATED                        
         BE    TRAPERR                                                          
         GOTO1 DATAMGR,DMCB,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,SPOOLBUF          
         CLI   8(R1),0             CHECK FOR ERROR                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TRAPERR                                                          
*                                                                               
LRR12    CLI   PRNTNOW,C'Y'        USING SORTER, PRINT UNITS NOW                
         BE    LRRX                                                             
                                                                                
         TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BZ    LRR14                                                            
         BAS   RE,GETSRT           GET KEY INTO NBKEY                           
         BZ    LRRX100                                                          
         MVI   NBFUNCT,NBFGET                                                   
         B     LRR14F                                                           
                                                                                
LRR14    DS    0H                                                               
         OC    NBKEY,NBKEY         1ST TIME                                     
         BZ    LRR14F               YES                                         
                                                                                
         CLI   NBMODE,NBPROCUN     PROCESS UNIT                                 
         BNE   LRR14F               NO                                          
                                                                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',NBKEY,KEY             
         CLC   KEY(20),NBKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
LRR14F   OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
*                                                                               
         TM    OPTNSW1,OPTDIGI     OPTION TO SHOW DIGITAL                       
         BO    *+20                                                             
         CLI   SVTN2PRO+14,C'Y'    PROFILE TO SUPPRESS DIGITAL                  
         BNE   *+12                                                             
         CLI   NBKEY+(NUKSTAT-NUKEY),X'04'   DIGITAL VIDEO MEDIA                
         BE    LRR10                                                            
*                                                                               
                                                                                
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    OPTN2SW,OPTSRTMD    SORTING BY MEDIA?                            
         BNZ   *+12                SKIP NEXT TEST                               
         TM    NBSUBMSK,NBSBMCLI                                                
         BO    LRRX                                                             
         TM    NBSUBMSK,NBSBMNET                                                
         BZ    LRR16                                                            
                                                                                
         TM    UNITSW,UNITSWAC    THIS BUY ACTIVITY                             
         BZ    *+12                NO                                           
         CLI   SVTNPR8,C'Y'       SHOW ALL NETS ON SAME PAGE                    
         BE    LRR16                                                            
         MVI   FORCEHED,C'Y'                                                    
                                                                                
LRR16    TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BZ    *+12                                                             
         TM    NBPACKST,X'20'      LOCKED?                                      
         BNZ   LRR10                                                            
                                                                                
         OC    NBSELPRG,NBSELPRG                                                
         BZ    LRR18                                                            
         TM    NBSUBMSK,NBSBMPRG                                                
         BO    LRRX                                                             
                                                                                
LRR18    CLI   NBMODE,NBREQLST                                                  
         BE    LRRX                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRR10                                                            
                                                                                
LRR20    DS   0H                                                                
         XC    SVLEN1(3),SVLEN1    INIT CMLS LENGTH                             
         XC    SVNUSLN1(3),SVNUSLN1  AND UNIT LENGTH                            
         XC    SVTYPE,SVTYPE             CML TYPE                               
                                                                                
         OC    NBACTNET,NBACTNET   IF NULL NETWORK                              
         BZ    LRR10                BYPASS                                      
                                                                                
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   LRR22                                                            
         CLC   SVBCLT,NBACTCLI                                                  
         BE    LRR22                                                            
         MVC   SVBCLT,NBACTCLI                                                  
         BAS   RE,INITSPT          SET FROM NET TO SPOT                         
                                                                                
* FCLT SET COND CODE EQUAL IF CLT OK *NE IF REQEND BYPASSING CLIENT *           
                                                                                
         BRAS  RE,FCLT                                                          
         BNE   LRRX                  TREAT AS NBREQLST                          
                                                                                
         MVI   FORCEHED,C'Y'                                                    
                                                                                
LRR22    TM    UNITSW,UNITSWAC    THIS BUY ACTIVITY                             
         BZ    LRR22D                                                           
         TM    UNITSW,UNITSWUN    THIS BUY ACTIVITY - UNASSIGNED                
         BO    LRR22D                                                           
                                                                                
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    LRR22B               NO                                          
         LA    R2,CONWHENH         FOR CURSOR POSN                              
         CLI   SPOOLKEY,0          TEST REPORT GENERATED                        
         BE    TRAPERR                                                          
         GOTO1 DATAMGR,DMCB,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,SPOOLBUF          
         CLI   8(R1),0             CHECK FOR ERROR                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TRAPERR                                                          
                                                                                
LRR22B   DS    0H                                                               
         CLC   NBACTNET,SVNET                                                   
         BE    LRR22C                                                           
                                                                                
         BRAS  RE,RDREV                                                         
                                                                                
LRR22D   CLC   NBACTNET,SVNET                                                   
         BE    LRR22C                                                           
         MVC   SVNET,NBACTNET                                                   
         OI    SVFLAG,NETCHGSW     TURN ON NETWORK CHANGED                      
                                                                                
* MAKE SURE NOT EXCLUDING THIS MEDIA                                            
                                                                                
LRR22C   OC    NETWORK,NETWORK     FILTERING ON NET?                            
         BNZ   LRR23A               YES - DON'T EXCLUDE MEDIA                   
         TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BNZ   LRR23A               YES - ALREADY FILTERED (HOPEFULLY)          
*                                                                               
         TM    NBKEY+(NUKSTAT-NUKEY),X'03'   OTHER MEDIA?                       
         BZ    *+16                MUST BE NET                                  
         BM    *+20                MUST BE CABLE OR SYND                        
         LA    RF,SVTAPROF+10      EXCLUDE OTHER                                
         B     LRR23                                                            
         LA    RF,SVTAPROF+11      EXCLUDE NET                                  
         B     LRR23                                                            
         TM    NBKEY+(NUKSTAT-NUKEY),X'01'   CABLE MEDIA?                       
         BZ    *+12                MUST BE SYND                                 
         LA    RF,SVTAPROF+8       EXCLUDE CABLE                                
         B     LRR23                                                            
         LA    RF,SVTAPROF+9       EXCLUDE SYND                                 
LRR23    CLI   0(RF),C'Y'                                                       
         BE    LRR10                                                            
         EJECT                                                                  
* PROCESS RECORD *                                                              
                                                                                
LRR23A   L     R6,NBAIO                                                         
                                                                                
         BAS   RE,INITNET          SET FROM SPOT TO NET                         
                                                                                
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
LRR23F   BRAS  RE,NEXTEL                                                        
         BNE   LRR23X                                                           
         CLI   2(R6),C'Q'          IS THIS A TAG                                
         BNE   LRR23F                                                           
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   LRR23F                                                           
                                                                                
         PACK  DUB,4(1,R6)                                                      
         CVB   R0,DUB              TAGS IN BINARY                               
         MHI   R0,5                TIMES 5 (5 SECONDS PER TAG)                  
         ZIC   R1,NBLEN            TOTAL UNIT LEN                               
         SR    R1,R0               MINUS TAG LEN                                
         STC   R1,NBLEN            = ACTUAL UNIT LENGTH                         
                                                                                
LRR23X   L     R6,NBAIO                                                         
                                                                                
* CONVERT EQUIVALENT PROGRAM CODES TO BASE AND SAVE COPY SPLITS *               
                                                                                
         BRAS  RE,CNVRTP                                                        
                                                                                
         OC    PROGRAM,PROGRAM                                                  
         BZ    *+14                                                             
         CLC   NBACTPRG,PROGRAM    IS THIS THE PROGRAM                          
         BNE   LRR10                NO                                          
                                                                                
         BRAS  RE,LFTR                                                          
         BNE   LRR10                                                            
                                                                                
LRR24    TM    UNITSW,UNITSWAC     THIS BUY ACTIVITY                            
         BO    LRR25                YES                                         
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRR25                                                            
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRR10                                                            
                                                                                
LRR25    OI    UNITSW,UNITSWAL     SET ON SOME ALLOCATED                        
                                                                                
         TM    UNITSW,UNITSWAC    THIS BUY ACTIVITY                             
         BZ    LRR30               NO                                           
                                                                                
* IF CHANGE OF NETWORK, NEED TO READ REVISION RECS (CABLE GEN) *                
                                                                                
         TM    SVFLAG,NETCHGSW                                                  
         BZ    LRR25B                                                           
                                                                                
         TM    UNITSW,UNITSWUN    THIS BUY ACTIVITY - UNASSIGNED                
         BO    LRR25B               YES, NO NEED FOR REVISIONS                  
                                                                                
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    LRR25B               NO                                          
         LA    R2,CONWHENH         FOR CURSOR POSN                              
         CLI   SPOOLKEY,0          TEST REPORT GENERATED                        
         BE    TRAPERR                                                          
         GOTO1 DATAMGR,DMCB,=C'CLO/PUR',=C'PRTQUE',0,SPOOLKEY,SPOOLBUF          
         CLI   8(R1),0             CHECK FOR ERROR                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     TRAPERR                                                          
                                                                                
*        GO FILTER FOR ACTIVITY - ONLY PRINT IF ATTENTION NEEDED                
                                                                                
LRR25B   DS    0H                                                               
         BRAS  RE,FACT               FILTER FOR ACTIVITY REPORT                 
         BNE   LRR10                NOT NEEDED FOR ACTIVITY REPORT              
                                                                                
         TM    SVFLAG,NETCHGSW     NETWORK CHANGED                              
         BZ    LRR30                NO                                          
         NI    SVFLAG,X'FF'-NETCHGSW                                            
                                                                                
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),NBACTNET                                             
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET                                                          
* PER MTG 7/16/08 DECIDED TO DUMP IF NEW TRFTYPE IS NOT SET                     
         CLI   STRTYPE,C' '                                                     
         BH    LRR25D                                                           
*                                                                               
         CLC   =C'SJ',AGENCY       IS AGENCY SJR                                
         BNE   *+12                                                             
         MVI   SVMEDIA,C'N'        NET IS DEFAULT                               
         B     LRR25F                                                           
*                                                                               
LRR25D   MVC   SVMEDIA,STRTYPE     (N,C,S,O,H,D)                                
         DROP  R4                                                               
                                                                                
* MUST READ TN2 PROFILE AFTER NET CHANGE USING MEDIA FROM NETWORK!              
                                                                                
LRR25F   XC    WORK,WORK                                                        
         MVI   WORK,X'A2'                                                       
         MVC   WORK+1(3),=C'TN2'   READ TN2                                     
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),SVMEDIA   USE MEDIA FROM NETWORK REC                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
         MVC   LCTN2PR6,SVPROF+1   CAL/BROAD/WEEKLY PROFILE                     
         CLI   SVTN2PRO+5,C'0'                                                  
         BE    LRR27                                                            
         CLI   SVTN2PRO+5,0                                                     
         BE    LRR27                                                            
         MVC   LCTN2PR6,SVTN2PRO+5 SAVE CAL/BROAD/WEEKLY PROFILE                
*                                                                               
LRR27    MVC   LCTN2PR8,SVTN2PRO+7 SHOW UNIT COST                               
         MVC   LCTN2PRC,WORK+31    SHOW ESTIMATE NAME AND NUMBER                
                                                                                
         MVC   SVNET,NBACTNET      SAVE FOR HEADHOOK RTN                        
                                                                                
         CLI   SVTNPR8,C'Y'        SHOW ALL NETS ON SAME PAGE                   
         BNE   LRR30                                                            
                                                                                
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR30                                                            
                                                                                
* SPECIAL PROFILE ON FOR BUY ACTIVITY NO PAGE BREAK FOR NETWORK *               
                                                                                
         MVC   P1+2(9),=C'NETWORK ='                                            
         MVC   P1+12(4),SVNET                                                   
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
LRR30    CLI   NBACTSUB,C'A'       IF SKED UNIT, NO EST CK                      
         BNL   LRR32                                                            
         BRAS  RE,CKEST            CK THIS EST-BYPASS IF CPY CD=N               
         BE    LRR10                                                            
                                                                                
LRR32    TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR36                                                            
                                                                                
         LA    R4,P2               NEXT AVAILABLE PRINT LINE                    
         TM    OPTNSW,OPTDSKAD     SHOW DISKADDR                                
         BZ    LRR33                NO                                          
         GOTO1 HEXOUT,DMCB,NBKEY+21,PDAY+9,4                                    
                                                                                
LRR33    MVC   SVNET,NBACTNET      SAVE FOR HEADHOOK RTN                        
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,PDATE)                               
         MVI   PDATE+5,C'-'                                                     
         CLI   NBACTSUB,C'A'                                                    
         BL    *+14                                                             
         MVC   PSUB(1),NBACTSUB                                                 
         B     LRR34                                                            
         EDIT  (B1,NBACTSUB),(3,PSUB),ALIGN=LEFT                                
                                                                                
LRR34    CLI   NBDAY,0             BAD DAY                                      
         BNE   *+14                                                             
         MVC   PDAY(3),=C'VAR'                                                  
         B     LRR34AA                                                          
         GOTO1 UNDAY,DMCB,NBDAY,PDAY                                            
LRR34AA  DS    0H                                                               
         GOTO1 UNTIME,(R1),NBTIME,PTIME                                         
                                                                                
         CLI   NBACTDP,0                                                        
         BE    LRR34AH                                                          
                                                                                
         MVC   BBCLT,BCLT          SAVE BCLT IF ANY                             
         MVC   BCLT,SVBCLT         PROCESSING CLIENT                            
         XC    GERROR,GERROR                                                    
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
                                                                                
         GOTO1 VALIDPT,DMCB,(X'01',NBACTDP)  GET 2 CHAR DAYPART                 
                                                                                
         MVC   BCLT,BBCLT          RESTORE BCLT                                 
         MVI   ERROPT,0                                                         
         OC    GERROR,GERROR                                                    
         BZ    LRR34AF                                                          
                                                                                
         MVC   PDP,=C'??'          BAD DAYPART CODE PRINT '??'                  
         B     *+10                                                             
*NOP     MVC   PDP,NBACTDP                                                      
LRR34AF  MVC   PDP,QDPT2           MOVE IN 2 CAHR DAYPART CODE                  
                                                                                
LRR34AH  MVC   PPROG,NBACTPRG                                                   
                                                                                
         MVC   PFEED,FEEDFLG                                                    
         MVC   PCUTIN,CUTNFLG                                                   
                                                                                
         CLI   EQVPRGSW,C'X'       THIS AN UNCOVERED EQUIV PROG CODE            
         BNE   LRR34A                                                           
         MVC   PPROG+132(14),=C'(PROGRAM EQUIV'                                 
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,SVPSTDT),(5,PPROG+132+15)                         
         MVC   PPROG+132+24(2),=C'TO'                                           
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,SVPENDT),(5,PPROG+132+27)                         
         MVI   PPROG+132+35,C')'                                                
         B     LRR35                                                            
                                                                                
LRR34A   CLI   EQVPRGSW,C'Y'       THIS AN EQUIVALENT PROG CODE                 
         BNE   LRR35                                                            
         MVI   PPROG+132-1,C'('                                                 
         MVC   PPROG+132(6),SVEQVPRG                                            
         LA    RF,PPROG+132+5                                                   
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
*                                                                               
         OC    SVPENDT,SVPENDT     ERROR?                                       
         BZ    LRR35                                                            
         MVC   PCMMLT+132(27),=C'ERROR - ONLY COVERED UP TO '                   
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,SVPENDT),(5,PCMMLT+132+27)                        
*                                                                               
LRR35    MVC   PPROGNM,NBPROGNM                                                 
                                                                                
         TM    NURSTAT-NURECD(R6),X'80' THIS UNIT DELETED                       
         BZ    LRR36                                                            
         MVC   PPROD(28),DELMSG                                                 
         CLI   NUKSUB-NURECD(R6),X'C1' THIS SKED (TRAFFIC) UNIT                 
         BL    *+8                      NO                                      
         MVI   PPROD+3,C'T'       TRAFFIC DELETE                                
         MVC   P2,SPACES                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRR10                                                            
                                                                                
LRR36    MVC   BPRD1C,NBPR1CL3                                                  
                                                                                
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    LRR36C               YES                                         
         OC    BPRD3C,BPRD3C       MAYBE ITS A T/B                              
         BNZ   LRR38                YES                                         
         B     LRR37                                                            
                                                                                
LRR36C   DS    0H                                                               
         SR    R2,R2                                                            
         IC    R2,NBLEN                                                         
         CLC   NBPR1CL3,SPACES     PRODUCT?                                     
         BNH   LRR37J               NO                                          
         B     LRR38                                                            
                                                                                
LRR37    CLC   NBPR2CL3,SPACES       PARTNER                                    
         BNH   LRR38                                                            
                                                                                
* CALCULATE PRODUCT UNIT LENGTHS *                                              
                                                                                
         LLC   R1,NBLEN1           1ST PRD LEN                                  
         CLI   NBLEN1,0            IF ZERO                                      
         BNE   LRR37C                                                           
*                                                                               
         SR    R0,R0                                                            
         LLC   R1,NBLEN            TOTAL LEN                                    
         D     R0,=F'2'                                                         
*        STC   R1,BSLN2                                                         
         LR    R2,R1               PRD2 LEN                                     
         AR    R1,R0               ADD REMAINDER 1 IF ANY      ANY              
         STC   R1,BSLN                                                          
         LLC   R0,NBLEN                                                         
*                                                                               
         BRAS  RE,FIXLEN           FIX PROD LEN = CML LEN                       
         B     LRR37F                                                           
                                                                                
LRR37C   STC   R1,BSLN                                                          
         LLC   R2,NBLEN            TOTAL LEN                                    
         LR    R0,R2                                                            
         SR    R2,R1                                                            
         BRAS  RE,FIXLEN           FIX PROD LEN = CML LEN                       
*                                                                               
LRR37F   STC   R1,SVNUSLN1                                                      
         STC   R2,SVNUSLN2                                                      
         B     LRR40                                                            
                                                                                
LRR37J   DS    0H                                                               
         LA    R5,SVCSPRDC         LIST OF T/B PRDS                             
         LA    R0,L'SVCSPRDC/3     MAX T/B                                      
         MVC   BPRD1C,0(R5)                                                     
                                                                                
LRR38    MVC   BSLN,NBLEN                                                       
         MVC   SVNUSLN1,NBLEN                                                   
         B     LRR40                                                            
                                                                                
LRR38J   LA    R5,3(R5)            BUMP IN SVCSPRD                              
         OC    0(3,R5),0(R5)             ARE WE DONE ?                          
         BZ    LRR42                YES                                         
         MVC   BPRD1C,0(R5)                                                     
         MVI   BSLN,0                                                           
                                                                                
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR40B                                                           
                                                                                
         LA    R6,132(R6)                                                       
         LA    R4,132(R4)                                                       
         LA    R1,P4+132           CHK IF PASSED LINE 4                         
         CR    R4,R1                                                            
         BNL   LRR42               ALL 4 PRINT LINES ARE FILLED                 
         B     LRR40B                                                           
                                                                                
LRR40    DS    0H                                                               
         OC    BPRD3C,BPRD3C       IS IT T/B                                    
         BZ    LRR40A               NO                                          
                                                                                
         L     R6,NBAIO                                                         
         SR    R2,R2                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   LRR40A                                                           
                                                                                
         LR    R2,R6                                                            
         USING NUCMLEL,R2                                                       
                                                                                
         OC    NUCML1(16),NUCML1                                                
*NOP     BNZ   *+14                                                             
****     OC    NUCML3,NUCML3       NO TRI-BACKS                                 
         BZ    LRR40A                                                           
                                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(16),NUCML1                                                  
*NOP     MVC   WORK+16(8),NUCML3                                                
*        CLC   WORK(8),WORK+8                                                   
*        BNE   *+10                                                             
******   XC    WORK+8(8),WORK+8                                                 
*        CLC   WORK(8),WORK+16                                                  
*        BNE   *+10                                                             
*******  XC    WORK+16(8),WORK+16                                               
                                                                                
         MVI   JUSTLEN,C'Y'                                                     
         BRAS  RE,FCMLA                                                         
         MVI   JUSTLEN,C'N'                                                     
                                                                                
         CLI   SVLEN1,X'FF'        ALL LENGTHS COMML?                           
         BNE   *+8                                                              
         MVI   SVLEN1,0                                                         
         CLI   SVLEN2,X'FF'        ALL LENGTHS COMML?                           
         BNE   *+8                                                              
         MVI   SVLEN2,0                                                         
         CLI   SVLEN3,X'FF'        ALL LENGTHS COMML?                           
         BNE   *+8                                                              
         MVI   SVLEN3,0                                                         
                                                                                
                                                                                
         ZIC   R1,SVLEN1           SET LENGTHS IF NEEDED                        
                                                                                
         ZIC   RF,SVLEN2                                                        
                                                                                
         AR    R1,RF                                                            
                                                                                
         ZIC   RF,SVLEN3                                                        
         AR    R1,RF               TOTAL CML LENGTH                             
                                                                                
         OC    NUCML1(16),NUCML1                                                
         BZ    LRRA40                                                           
         OC    NUCML1,NUCML1                                                    
         BZ    LRRA40                                                           
                                                                                
         CLC   NBPR2CL3,SPACES                                                  
         BNH   TEMP0010                                                         
         OC    NUCML2,NUCML2                                                    
         BZ    LRRA40                                                           
                                                                                
TEMP0010 CLC   =C'VIGNETTE',NUCMLBSN                                            
         BNE   LRRV40                                                           
         ZIC   RF,NUCMLBSL                                                      
         AR    R1,RF               ADD LEN OF VIGNETTE                          
                                                                                
LRRV40   ZIC   RE,SVNUSLN1                                                      
         ZIC   RF,SVNUSLN2                                                      
         AR    RE,RF               TOTAL UNIT LENGTH                            
         CR    R1,RE                                                            
         BE    LRRA40                                                           
         BL    *+6                                                              
         DC    H'0'                IF CML=ALL, SHOULD BE LESS THAN UNIT         
                                                                                
         OI    NUCMLFLG,X'80'      UNIT LEN CHANGED                             
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
         B     LRR40A                                                           
                                                                                
LRRA40   ZIC   RF,NBLEN                                                         
         CR    R1,RF                                                            
         BE    LRR40A                                                           
         BL    *+6                                                              
         DC    H'0'                CML LEN > UNIT LEN???                        
         SR    RF,R1                                                            
         CLI   SVLEN1,0                                                         
         BNE   *+12                                                             
         STC   RF,SVLEN1                                                        
         B     LRR40A                                                           
         CLI   SVLEN2,0                                                         
         BNE   *+12                                                             
         STC   RF,SVLEN2                                                        
         B     LRR40A                                                           
         STC   RF,SVLEN3                                                        
                                                                                
LRR40A   LA    R6,PPROD                                                         
                                                                                
LRR40B   MVC   PPRDPRD,BPRD1C                                                   
         OC    SVLEN1(3),SVLEN1    ANY CML LENGTH                               
         BZ    *+10                                                             
         MVC   BSLN,SVLEN1                                                      
                                                                                
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR40C                                                           
                                                                                
         BRAS  RE,PPRD                                                          
                                                                                
         BAS   RE,INITSPT          SET TO SPOT                                  
         BRAS  RE,PPRDN                                                         
         LA    R1,PPRODNM                                                       
         CR    R1,R4               IS THIS THE LINE+1                           
         BL    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R1               SAVE POTENTIAL PRINT PLACE                   
         LA    R1,132(R1)                                                       
         CR    R1,R4               IS THIS THE LINE+1                           
         BL    *-8                                                              
         MVC   0(20,RE),PRDNM                                                   
                                                                                
LRR40C   TM    NBUNST3,X'40'       IS IT C/S                                    
         BO    LRR40E                                                           
                                                                                
         OC    BPRD3C,BPRD3C       IS IT T/B                                    
         BNZ   LRR41                YES                                         
                                                                                
LRR40E   DS    0H                                                               
         XC    BPRD2C,BPRD2C                                                    
         CLC   NBPR2CL3,SPACES        PARTNER                                   
         BH    LRR40F                                                           
         OC    SVCSPRDC(3),SVCSPRDC   ARE WE DOING COPY SPLITS                  
         BZ    LRR42                   NO                                       
         BCT   R0,LRR38J              R0 SET @ LRR37J                           
                                                                                
LRR40F   STC   R2,BSLN                                                          
         MVC   PPRDPRD,NBPR2CL3                                                 
         MVC   BPRD2C,NBPR2CL3                                                  
         B     LRR41C                                                           
                                                                                
LRR41    DS    0H                                                               
         MVI   BSLN,0                                                           
         MVC   PPRDPRD,BPRD2C                                                   
                                                                                
LRR41C   DS    0H                                                               
         LA    R6,PPROD+132                                                     
                                                                                
         OC    SVLEN1(3),SVLEN1    ANY CML LENGTH                               
         BZ    *+10                                                             
         MVC   BSLN,SVLEN2                                                      
                                                                                
         BRAS  RE,PPRD                                                          
         BRAS  RE,PPRDN                                                         
         MVC   PPRODNM+132,PRDNM                                                
         MVC   PCMML+132+3(3),=C'N/A'                                           
         LA    R4,132(,R4)                                                      
                                                                                
         OC    BPRD3C,BPRD3C       IS IT T/B                                    
         BZ    LRR41E               NO                                          
                                                                                
         MVC   PPRDPRD,BPRD3C                                                   
                                                                                
         OC    SVLEN1(3),SVLEN1    ANY CML LENGTH                               
         BZ    *+10                                                             
         MVC   BSLN,SVLEN3                                                      
                                                                                
         LA    R6,PPROD+132+132                                                 
         BRAS  RE,PPRD                                                          
         BRAS  RE,PPRDN                                                         
         MVC   PPRODNM+132+132,PRDNM                                            
         MVC   PCMML+132+132+3(3),=C'N/A'                                       
         LA    R4,132(,R4)                                                      
                                                                                
LRR41E   DS    0H                                                               
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    LRR44                NO                                          
         MVC   PCMML+3(10),=C'COPY SPLIT'                                       
         MVC   PCMML+132+3(3),SPACES                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,P2                                                            
         B     LRR44                                                            
                                                                                
LRR42    MVC   PCMML+3(3),=C'N/A'                                               
                                                                                
LRR44    BAS   RE,INITNET          SET TO NET                                   
         L     R6,NBAIO                                                         
         SR    R2,R2                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    LRR45                                                            
                                                                                
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR78                                                            
                                                                                
         MVI   SVCMLFLG,0                                                       
         MVI   SVCMLFL2,0                                                       
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    *+10                 NO                                          
         MVC   PPROD(28),=CL28'NATIONAL-NO PROD ASSIGNED'                       
                                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'22'       FIND ANY FEED                                 
         BAS   RE,GETEL                                                         
         BNE   LRR66                                                            
         MVC   PPROG+3-P(7,R4),=C'UNIT IS'                                      
         MVC   PPROG+11-P(5,R4),=C'FEED='                                       
         MVC   PPROG+16-P(4,R4),2(R6)                                           
         LA    R4,132(,R4)                                                      
         B     LRR66                                                            
                                                                                
         USING NUCMLEL,R2                                                       
LRR45    LR    R2,R6               FLAG THAT 21 ELEM WAS FOUND                  
                                                                                
         XC    WORK+16(8),WORK+16                                               
                                                                                
         MVC   WORK(16),NUCML1                                                  
*NOP     CLC   WORK(8),WORK+8      *NO TRI-BACKS                                
*        BNE   *+10                                                             
*        XC    WORK+8(8),WORK+8                                                 
*                                                                               
*        OC    BPRD3C,BPRD3C       IS THIS A TRI-BACK                           
*        BZ    LRR45C               NO                                          
*        OC    NUCML3,NUCML3                                                    
*        BZ    LRR45C                                                           
*                                                                               
******   MVC   WORK+16(8),NUCML3                                                
                                                                                
LRR45C   DS    0H                                                               
         MVI   JUSTLEN,C'Y'                                                     
         BRAS  RE,FCMLA                                                         
         MVI   JUSTLEN,C'N'                                                     
                                                                                
         OC    NUCML1(16),NUCML1                                                
         BZ    LRR45M                                                           
         OC    NUCML1,NUCML1                                                    
         BZ    LRR45M                                                           
                                                                                
         CLC   NBPR2CL3,SPACES                                                  
         BNH   *+14                                                             
         OC    NUCML2,NUCML2                                                    
         BZ    LRR45M                                                           
                                                                                
         ZIC   RE,SVNUSLN1                                                      
         ZIC   RF,SVNUSLN2                                                      
         AR    RE,RF               TOTAL UNIT LENGTH                            
                                                                                
         CLI   SVLEN1,X'FF'        ALL LENGTHS COMML?                           
         BNE   *+12                                                             
         MVI   SVLEN1,0                                                         
         MVI   CMLALLSW,X'80'                                                   
         CLI   SVLEN2,X'FF'        ALL LENGTHS COMML?                           
         BNE   *+12                                                             
         MVI   SVLEN2,0                                                         
         MVI   CMLALLSW,X'40'                                                   
                                                                                
         CLI   CMLALLSW,0          ANY ALL                                      
         BE    *+16                                                             
         MVC   SVLEN1,SVNUSLN1                                                  
         MVC   SVLEN2,SVNUSLN2                                                  
                                                                                
         ZIC   RF,SVLEN1                                                        
         ZIC   R1,SVLEN2                                                        
         AR    R1,RF               TOTAL CML LENGTH                             
                                                                                
         CLC   =C'VIGNETTE',NUCMLBSN                                            
         BNE   LRR45E                                                           
         ZIC   RF,NUCMLBSL                                                      
         AR    R1,RF               ADD LEN OF VIGNETTE                          
                                                                                
LRR45E   DS    0H                                                               
         OC    BPRD3C,BPRD3C       IS THIS A TRI-BACK                           
         BZ    LRR45J               NO                                          
                                                                                
         CLI   SVLEN3,X'FF'        ALL LENGTHS COMML?                           
         BNE   *+8                                                              
         MVI   SVLEN3,0                                                         
                                                                                
         ZIC   RF,SVLEN3                                                        
         AR    R1,RF               TOTAL CML LENGTH                             
                                                                                
LRR45J   CR    R1,RE                                                            
         BE    LRR45M                                                           
         OI    NUCMLFLG,X'80'      UNIT LEN CHANGED                             
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
                                                                                
LRR45M   TM    NUCMLFL2,NUCMLFFD   FEED W/O NATIONAL ?                          
         BZ    *+18                                                             
         MVI   SVCMLFL2,X'FF'       YES                                         
         MVC   PCMML+3(13),=C'*NO NATIONAL*'                                    
         B     LRR66                                                            
                                                                                
         MVC   SVCMLFLG,NUCMLFLG                                                
                                                                                
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    LRR48                NO                                          
         OC    NUCMPROD,NUCMPROD                                                
         BNZ   LRR45N                                                           
         CLI   NUCMLPRD,0                                                       
         BE    LRR46                                                            
         TM    SECFLAG,NECONPRD                                                 
         BO    LRR46                                                            
                                                                                
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
LRR45MA  CLC   NUCMLPRD,3(RE)                                                   
         BE    LRR45MB                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,LRR45MA                                                       
         DC    H'0'                                                             
LRR45MB  MVC   NUCMPROD,0(RE)                                                   
                                                                                
LRR45N   DS    0H                                                               
         CLC   NUCMPROD,NBPR1CL3   THIS ONE OF 2 AVAIL                          
         BE    LRR48                                                            
         CLC   NUCMPROD,NBPR2CL3   THIS OTHER OF 2 AVAIL                        
         BE    LRR48                                                            
                                                                                
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRR46                                                            
         LA    RE,L'SVCSPRDC/3                                                  
         LA    RF,SVCSPRDC                                                      
LRR45P   OC    0(3,RF),0(RF)                                                    
         BZ    LRR46                                                            
         CLC   NUCMPROD,0(RF)                                                   
         BE    LRR48                                                            
         LA    RF,3(,RF)                                                        
         BCT   RE,LRR45P                                                        
                                                                                
LRR46    MVC   PPROD(28),=CL28'NATIONAL-NO PROD ASSIGNED'                       
                                                                                
LRR48    TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR49                                                            
                                                                                
         CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   LRR50                                                            
         TM    NUCMLFLG,X'08'     TRAFFIC DELETE                                
         BZ    LRR50                                                            
         MVC   PCMML(28),DELMSG                                                 
         MVI   PCMML+3,C'T'       TRAFFIC DELETE                                
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
LRR49    TM    WHEN,X'20'          SOON                                         
*NOP     BO    LRR10               YES,CAN'T MARK FILE                          
         B     LRR10               DISABLE UNIT RECORD UPDATE                   
                                                                                
         TM    SVFLAG,RECCHGSW     RECORD CHANGED ?                             
         BZ    LRR10                NO                                          
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         CLI   TWAWRITE,C'Y'       WRITE OK                                     
         BNE   LRR10                NO UPDATE                                   
                                                                                
* DO DUMMY GETREC FOR PUTREC                                                    
                                                                                
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'UNTFIL',NBKEY+21,    C        
               (R6),DMWORK                                                      
                                                                                
         L     R6,NBAIO                                                         
         GOTO1 DATAMGR,(R1),=C'PUTREC',=C'UNTFIL',NBKEY+21,(R6),DMWORK          
                                                                                
         NI    SVFLAG,X'FF'-RECCHGSW RESET RECORD CHANGED                       
         B     LRR10                                                            
                                                                                
LRR50    MVI   ELCODE,X'22'       FIND ANY FEED                                 
         L     R6,NBAIO                                                         
         BAS   RE,GETEL                                                         
         BNE   LRR52                                                            
                                                                                
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR52                                                            
                                                                                
         MVC   PPROG+3-P(7,R4),=C'UNIT IS'                                      
         MVC   PPROG+11-P(5,R4),=C'FEED='                                       
         MVC   PPROG+16-P(4,R4),2(R6)                                           
                                                                                
LRR52    LR    R6,R2               RESTORE 21 ELEM PTR                          
                                                                                
         TM    NUCMLFLG,X'E0'      ANY CHANGES-ELEM MUST BE REASSIGNED          
         BZ    LRRDESC                                                          
         OC    NUCML1(16),NUCML1   IF NO CMLS                                   
         BZ    LRR59               THEN NO NEED TO REASSIGN                     
         MVC   PCMML(9),REASSIGN   NEEDED                                       
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR52B                                                           
                                                                                
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),NUCML1                                                  
         TM    NUCMADFL,NUCMADF1    AD-ID CML 1                                 
         BZ    LRR52B                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML1                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR52B   TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    LRR52C               YES                                         
         CLC   NBPR2CL3,SPACES     PARTNER                                      
         BNH   LRR52C                                                           
         MVC   PCMML+132(9),REASSIGN                                            
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR52C                                                           
                                                                                
         MVI   PCMML-1+132,C'*'                                                 
         MVC   PCMML+132(8),NUCML2                                              
                                                                                
         TM    NUCMADFL,NUCMADF2    AD-ID CML 2                                 
         BZ    LRR52C                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML2                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML+132(8),WORK                                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132,SVCMLADI                                               
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR52C   LA    R5,PCMML+13                                                      
         LR    R0,R5                                                            
         TM    NUCMLFLG,X'80'      LENGTH                                       
         BZ    *+14                                                             
         MVC   0(4,R5),=C'LEN/'                                                 
         LA    R5,3(,R5)                                                        
         TM    NUCMLFLG,X'20'      DATE                                         
         BZ    *+14                                                             
         MVC   1(5,R5),=C'DATE/'                                                
         LA    R5,5(,R5)                                                        
         TM    NUCMLFLG,X'40'      PRODUCT                                      
         BNZ   *+20                                                             
         CLI   0(R5),C'/'                                                       
         BNE   LRR59                                                            
         MVI   0(R5),C' '                                                       
         B     LRR59                                                            
                                                                                
LRRDESC  DS    0H                                                               
         BRAS  RE,RSDESC                                                        
                                                                                
LRR59    DS    0H                  ENTRY BACK FROM ROUTINE                      
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR78                                                            
                                                                                
         BAS   RE,INITSPT          SET FROM NET TO SPOT                         
                                                                                
         OC    NUCMLPOS,NUCMLPOS   ANY POSITION                                 
         BZ    LRR60                                                            
         MVC   PCMML-P(4,R4),=C'POS='                                           
         MVC   PCMML+4-P(4,R4),NUCMLPOS                                         
         LA    R4,132(,R4)                                                      
                                                                                
LRR60    TM    NUCMLFLG,X'04'      BILLBOARD REQUIRED                           
         BZ    LRR66                                                            
         CLI   NUCMLBSL,0          BILLBOARD ASSIGNED                           
         BNE   LRR66             YES                                            
         MVC   PCMML-P(18,R4),=CL18'BILLBOARD REQUIRED'                         
         LA    R4,132(,R4)         NEXT PRINT LINE                              
         LA    R0,P4               CK IF PAST P4                                
         CR    R4,R0                                                            
         BNH   LRR66                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,P1                                                            
                                                                                
LRR66    TM    NBUNITST,X'40'      PREEMPT                                      
         BZ    LRR68                                                            
         MVC   PPROG-P(10,R4),=C'PRE-EMPTED'                                    
                                                                                
LRR68    TM    NBUNITST,X'80'      MINUS                                        
         BZ    LRR70                                                            
         MVC   PPROG-P(10,R4),=C'MINUS UNIT'                                    
                                                                                
LRR70    TM    NBUNITST,X'01'      MAKE-GOOD                                    
         BZ    LRR72                                                            
         BAS   RE,INITNET          SET TO NET                                   
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMGD,R6                                                         
LRR71    MVC   PCMML-P(13,R4),=C'MAKE-GOOD FOR'                                 
         MVC   PCMML+14-P(16,R4),NUMGPNM                                        
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(4,PCMML+31-P(R4))                      
         LA    R4,132(,R4)         NEXT PRINT LINE                              
         LA    R0,P4               CK IF PAST P4                                
         CR    R4,R0                                                            
         BNH   LRR71A                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,P1                                                            
LRR71A   BAS   RE,NEXTEL                                                        
         BNE   LRR72                                                            
         B     LRR71                                                            
                                                                                
* CHECK MISSED LAST, AS A MAKE-GOOD COULD BE MISSED *                           
                                                                                
LRR72    TM    NBUNITST,X'02'      MISSED                                       
         BZ    LRR74                                                            
         BAS   RE,INITNET          SET TO NET                                   
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
LRR73    MVC   P-P(,R4),SPACES                                                  
         MVC   PCMML-P(12,R4),=C'MADE-GOOD BY'                                  
         MVC   PCMML+13-P(16,R4),NUMGPNM                                        
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NUMGDATE),(4,PCMML+30-P(R4))                      
                                                                                
         LA    R4,132(,R4)         NEXT PRINT LINE                              
         LA    R0,P4               CK IF PAST P4                                
         CR    R4,R0                                                            
         BNH   LRR73A                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,P1                                                            
LRR73A   BAS   RE,NEXTEL                                                        
         BE    LRR73                                                            
         DROP  R6                                                               
                                                                                
LRR74    TM    NBUNITST,X'04'      IF PFB, BONUS                                
         BZ    LRR75                                                            
         TM    NBUNITST,X'01'+X'02'                                             
         BNZ   LRR75                                                            
         MVC   PCMML-P(10,R4),=C'BONUS UNIT'                                    
         B     LRR76                                                            
                                                                                
LRR75    L     R6,NBAIO                                                         
         LA    R6,27(,R6)                                                       
         MVI   ELCODE,X'02'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   LRR76                                                            
                                                                                
         USING NUSDRD,R6                                                        
         TM    NUSDST3,X'02'       SEE IF ADU UNIT (COST NOT ALLOWED)           
         BZ    LRR76                                                            
         MVC   PCMML-P(8,R4),=C'ADU UNIT'                                       
         DROP  R6                                                               
                                                                                
LRR76    LTR   R2,R2               WAS 21 ELEM FOUND                            
         BZ    LRR78               NO                                           
         CLI   NUCMLBSL,0          BILLBOARD                                    
         BE    LRR78               NO                                           
         MVC   SVBBSLN,NUCMLBSL                                                 
         MVC   SVBBPOS,NUCMLBPS                                                 
                                                                                
         MVC   SVBBFLG,NUCMADFL    SAVE BILLBOARD FLAG                          
                                                                                
         MVC   WORKPROD,BPRD1C     MOVE PROD                                    
         BRAS  RE,PBB              GO PRINT BILLBOARD                           
         DROP  R2                                                               
                                                                                
LRR78    BRAS  RE,PRTFEED          PRINT FEED IF ANY                            
                                                                                
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR220                                                           
                                                                                
         CLC   P,SPACES                                                         
         BNH   LRR90                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,P1                                                            
LRR90    L     R6,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LRR90C   BAS   RE,NEXTEL                                                        
         BNE   LRR95                                                            
         CLI   2(R6),C'Q'          IS THIS A TAG                                
         BNE   LRRTAGX                                                          
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   LRRTAGX                                                          
         BAS   R1,LRR90P           FIND WHERE TO PRINT                          
         MVC   2(2,RE),3(R6)       MOVE TAG TO PRINT LINE                       
         B     LRR90C                                                           
                                                                                
LRRTAGX  CLI   2(R6),C'F'          THIS A BUY TYPE CODE                         
         BE    LRR93                                                            
         CLI   2(R6),C'H'          TRAFFIC SUPPLIER?                            
         BE    LRR92                                                            
         CLI   2(R6),C'L'          MULTI RUN UNIT                               
         BNE   LRR90C                                                           
         BAS   R1,LRR90P           FIND WHERE TO PRINT                          
         B     LRR91C                                                           
                                                                                
LRR90P   LA    RE,PTIME-P(R4)                                                   
         LA    RF,PPROG-P(R4)                                                   
LRR91    CR    RE,RF               START OF PRINT LINE?                         
         BER   R1                                                               
         CLI   0(RE),C' '                                                       
         BNER  R1                                                               
         BCT   RE,LRR91                                                         
         BR    R1                                                               
                                                                                
LRR91C   MVC   3(13,RE),=C'**MULTI RUN**'                                       
         B     LRR90C                                                           
                                                                                
LRR92    BAS   R1,LRR90P           FIND WHERE TO PRINT                          
         MVC   3(3,RE),=C'TS='                                                  
         ZIC   R1,1(R6)            GET LEN (MAX 5)                              
         SH    R1,=H'4'            MINUS OVERHEAD                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   6(0,RE),3(R6)       PRINT TRAFFIC SUPPLIER                       
         B     LRR90C                                                           
                                                                                
LRR93    BAS   R1,LRR90P           FIND WHERE TO PRINT                          
         CLI   3(R6),C'O'          OPPORTUNISTIC                                
         BNE   *+10                                                             
         MVC   3(11,RE),=C'**OPPORTU**'                                         
                                                                                
         CLI   3(R6),C'S'          SCATTER                                      
         BNE   *+10                                                             
         MVC   3(11,RE),=C'**SCATTER**'                                         
                                                                                
         CLI   3(R6),C'U'          UPFRONT                                      
         BNE   LRR90C                                                           
         MVC   3(11,RE),=C'**UPFRONT**'                                         
         B     LRR90C                                                           
                                                                                
LRR95    CLC   PPROG(50),SPACES                                                 
         BE    LRR100                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
LRR100   CLI   LCTN2PRC,C'Y'       SHOW ESTIMATE NAME AND NUMBER                
         BNE   LRR200               NO                                          
                                                                                
         CLI   NBACTSUB,C'A'       IF SKED UNIT, NO EST CK                      
         BNL   LRR200                                                           
                                                                                
         LA    R0,L'ESTBL/25                                                    
*        LH    R1,=AL2(ESTBL-SYSD)                                              
*        AR    R1,R9                                                            
         L     R1,AESTBL                                                        
LRR110   CLI   0(R1),0             EMPTY ENTRY                                  
         BE    LRR130                                                           
         CLC   NBACTEST,0(R1)      SAME EST                                     
         BNE   LRR120                                                           
         CLC   NBPR1CL3,1(R1)      SAME PRODUCT                                 
         BE    LRR150                                                           
LRR120   LA    R1,25(,R1)                                                       
         BCT   R0,LRR110                                                        
                                                                                
* ESTIMATE TABLE IS FULL, REPLACE OLD ENTRIES (PUT FF IN COPY CODE)             
                                                                                
*        LH    R1,=AL2(ESTBL-SYSD)                                              
*        AR    R1,R9                                                            
         L     R1,AESTBL                                                        
         LA    R0,L'ESTBL/25                                                    
LRR122   CLI   4(R1),X'FF'                                                      
         BE    LRR125                                                           
         MVI   4(R1),X'FF'         THIS IS A REPLACED ENTRY                     
         MVC   0(1,R1),NBACTEST    MOVE IN NEW EST NUMBER                       
         MVC   1(3,R1),NBPR1CL3     PRODUCT                                     
         B     LRR140                                                           
                                                                                
LRR125   LA    R1,25(,R1)                                                       
         BCT   R0,LRR122                                                        
         AHI   R1,-25            REUSE LAST ENTRY (UNLIKELY TO HAPPEN)          
                                                                                
LRR130   MVC   0(1,R1),NBACTEST    MOVE IN NEW EST NUMBER                       
         MVC   1(3,R1),NBPR1CL3     PRODUCT                                     
                                                                                
LRR140   BRAS  RE,RDEST               READ ESTIMATE RECORD                      
                                                                                
LRR150   LA    R4,P1+28                                                         
         MVC   0(9,R4),=C'ESTIMATE='                                            
         EDIT  (B1,NBACTEST),(3,9(R4)),ALIGN=LEFT                               
         LA    R4,15(R4)                                                        
         MVC   0(14,R4),=C'ESTIMATE NAME='                                      
         MVC   14(20,R4),3(R1)                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
LRR200   BAS   RE,INITSPT          SET FROM NET TO SPOT                         
                                                                                
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
LRR220   TM    WHEN,X'20'          SOON                                         
*NOP     BO    LRR10               YES,CAN'T MARK FILE                          
         B     LRR10               DISABLE UNIT UPDATE                          
                                                                                
         TM    SVFLAG,RECCHGSW     RECORD CHANGED ?                             
         BZ    LRR10                NO                                          
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         CLI   TWAWRITE,C'Y'       WRITE OK                                     
         BNE   LRR10                NO UPDATE                                   
                                                                                
* DO DUMMY GETREC FOR PUTREC                                                    
                                                                                
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'UNTFIL',NBKEY+21,    C        
               (R6),DMWORK                                                      
                                                                                
         L     R6,NBAIO                                                         
         GOTO1 DATAMGR,(R1),=C'PUTREC',=C'UNTFIL',NBKEY+21,(R6),DMWORK          
                                                                                
         NI    SVFLAG,X'FF'-RECCHGSW RESET RECORD CHANGED                       
         BAS   RE,INITSPT          SET FROM NET TO SPOT                         
         B     LRR10                                                            
         EJECT                                                                  
* CHECK IF SORTING, AND PASS BACK SORTED RECS *                                 
                                                                                
LRRX     CLI   OPTPOSF,0           TEST POS OPTION GIVEN                        
         BE    LRRX100              NO                                          
*                                                                               
         MVI   PRNTNOW,C'Y'        SET PRINT SWITCH TO YES                      
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R2,4(R1)            R2=A(RECORD)                                 
         LTR   R2,R2                                                            
         BZ    LRRX100                                                          
         MVC   WORK(SRTLN),0(R2)                                                
         LA    R2,WORK                                                          
         USING SORTD,R2                                                         
         MVC   NBACTCLI,SRTCLT                                                  
         MVC   NBACTEST,SRTEST                                                  
         MVC   NBACTNET,SRTNET                                                  
         MVC   NBACTPRG,SRTPROG                                                 
         MVC   NBACTDP,SRTDP                                                    
         MVC   NBACTSUB,SRTSUB                                                  
         MVC   NBACTDAT,SRTDATE                                                 
*                                                                               
         L     R6,NBAIO                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'UNTFIL',SRTDSKAD,(R6),DMWORK          
*                                                                               
         CLI   DMCB+8,0            RECORD NOT FOUND OR ANY ERROR                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   27(R6),01                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NBMAINEL(80),27(R6)                                              
         CLI   FIRSTIME,C'N'                                                    
         BE    LRRX20                                                           
         MVC   SAVCLT(L'SAVCLT),SRTCLT                                          
         MVI   FIRSTIME,C'N'                                                    
*                                                                               
LRRX20   CLC   SAVCLT,SRTCLT                                                    
*         BNE   CLTERR              CLIENT MUST BE SAME                         
         BE    *+14                                                             
         MVC   GERROR,=Y(NOPOSF)                                                
         B     TRAPERR2                                                         
*                                                                               
         CLC   SAVNET,SRTNET        TEST FOR NETWORK BREAK                      
         BE    LRRX24                                                           
                                                                                
         MVC   SAVNET(L'SVNET),SRTNET                                           
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
LRRX24   CLC   OPTPOSFL,SRTPOSL     TEST FOR POS BREAK                          
         BE    LRRX30                                                           
         MVC   OPTPOSFL,SRTPOSL                                                 
         MVI   FORCEHED,C'Y'                                                    
LRRX30   B     LRR24                                                            
         DROP  R2                                                               
*                                                                               
LRRX100  NI    TRACLTH+4,X'FF'-X'20'                                            
         GOTO1 VSORTER,DMCB,=C'END'                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO GET KEYS FROM SORTER - RETURNS CC = 0 FOR NO MORE RECS             
                                                                                
GETSRT   LR    R0,RE                                                            
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     RF,4(R1)            RF=A(RECORD)                                 
         MVC   NBKEY(L'NUKPKEY+5),1(RF)     KEY + STAT & DA                     
         CLC   LMEDIA,0(RF)        SAME MEDIA?                                  
         BE    *+14                                                             
         MVC   LMEDIA,0(RF)                                                     
         MVI   FORCEHED,C'Y'       PAGE BREAK ON NEW MEDIA                      
*                                                                               
         LR    RE,R0                                                            
         LTR   RF,RF               SET CC FOR RETURN                            
         BR    RE                                                               
         EJECT                                                                  
* RESET FILES TO SPOT *                                                         
                                                                                
*                                                                               
INITNET  DS    0H                                                               
         MVI   LKEY+1,20                                                        
         MVI   DATADISP+1,27                                                    
         MVI   LSTATUS+1,1                                                      
         MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVI   SYSFIL,C'U'                                                      
         MVI   SYSFIL+1,C'N'                                                    
         MVI   SYSFIL+2,C'T'                                                    
         BR    RE                                                               
INITSPT  MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVI   LSTATUS+1,1                                                      
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVI   SYSFIL,C'S'                                                      
         MVI   SYSFIL+1,C'P'                                                    
         MVI   SYSFIL+2,C'T'                                                    
         BR    RE                                                               
*                                                                               
* RESET FILES TO XFILE *                                                        
*                                                                               
INITXSP  MVI   DATADISP+1,42       SET FROM SPOT TO XSP                         
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVI   SYSDIR,C'X'                                                      
         MVI   SYSDIR+1,C'S'                                                    
         MVI   SYSDIR+2,C'P'                                                    
         MVI   SYSFIL,C'X'                                                      
         MVI   SYSFIL+1,C'S'                                                    
         MVI   SYSFIL+2,C'P'                                                    
         BR    RE                                                               
                                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
                                                                                
TRAPERR  GOTO1 ERREX                                                            
                                                                                
TRAPERR2 GOTO1 VTRAERR             USING GETTXT CALL                            
         EJECT                                                                  
         LTORG                                                                  
REASSIGN DC    CL9'REASSIGN '                                                   
DELMSG   DC    CL28'** M D E L E T E **'                                        
         EJECT                                                                  
*                                                                               
                                                                                
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,34,C'COMMERCIAL SCHEDULE LIST'                                
         SSPEC H2,34,C'------------------------'                                
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H5,3,C'CLIENT'                                                   
         SSPEC H5,85,RUN                                                        
         SSPEC H5,73,REPORT                                                     
         SSPEC H6,3,C'NETWORK'                                                  
         SSPEC H6,73,REQUESTOR                                                  
         SSPEC H6,103,PAGE                                                      
         SSPEC H9,3,C'DATE'                                                     
         SSPEC H10,3,C'--------'                                                
         SSPEC H9,13,C'PROGRAM - TITLE'                                         
         SSPEC H10,13,C'----------------------'                                 
         SSPEC H9,37,C'PRD-LEN'                                                 
         SSPEC H10,37,C'---------'                                              
         SSPEC H9,45,C'NAME'                                                    
         SSPEC H10,45,C'-------------------'                                    
         SSPEC H9,66,C'COMMERCIAL-TITLE'                                        
         SSPEC H10,66,C'------------------------'                               
         SSPEC H9,95,C'TIME'                                                    
         SSPEC H10,95,C'-----'                                                  
         SSPEC H9,107,C'DP'                                                     
         SSPEC H10,107,C'--'                                                    
         SSPEC H9,110,C'DAY'                                                    
         SSPEC H10,110,C'---'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
*----------------------------------------------------------------------         
* GET 8 CHAR PID FROM XL2 CODE                                                  
*----------------------------------------------------------------------         
GETPID   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    SVPID,SVPID                                                      
         L     R2,ASECBLK                                                       
         USING SECD,R2                                                          
                                                                                
         USING SA0REC,R4                                                        
         LA    R4,KEY                                                           
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,SECAGY                                                   
         MVC   SA0KNUM,SVPIDHEX                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO2,0                    
         L     R4,AIO2                                                          
         CLC   KEY(25),0(R4)                                                    
         BNE   GPIDXIT                                                          
                                                                                
GPID10   EQU   *                                                                
         LA    R5,SA0DATA                                                       
GPID20   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),SAPALELQ                                                   
         BE    GPID30                                                           
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GPID20                                                           
GPID30   EQU   *                                                                
         USING SAPALD,R5                                                        
         MVC   SVPID,SAPALPID                                                   
                                                                                
GPIDXIT  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2,R4,R5                                                         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
**NGRID                                                                         
***********************************************************************         
* GRIDS PROCESSING                                                              
***********************************************************************         
GRIDINIT NTR1  BASE=*,LABEL=*                                                   
         OI    CONSERVH+6,X'81'          AVOID NOTHING ENTERED MESSAGE          
*                                                                               
         NI    GSTAT,X'FF'-X'80'                                                
         CLI   1(RA),C'*'                IS THIS A DDS TERMINAL?                
         BNE   *+8                       . NO                                   
         OI    GSTAT,X'80'                                                      
                                                                                
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
         TM    PCGRIDS,PCGCOPQ           COLUMNS STARTED?                       
         BNO   GIOKX                                                            
         TM    PCGRIDS,PCGFINQ                                                  
         BNO   GIOKX                                                            
         BRAS  RE,GRIDEND                                                       
         B     GIERX                                                            
         DROP  R5                                                               
*                                                                               
GIOKX    CR    RB,RB                                                            
         B     GIX                                                              
GIERX    LTR   RB,RB                                                            
GIX      J     EXIT                                                             
         LTORG                                                                  
                                                                                
*----------------------------------------------------------------------         
* GRIDS END                                                                     
*----------------------------------------------------------------------         
GRIDEND  NTR1  BASE=*                                                           
         GOTO1 =V(GRID),DMCB,(C'E',AGFT),ATWA,ASPOOLD,ACOMFACS,        +        
               RR=SPTR2CRR                                                      
         JNL   EXIT                                                             
         MVI   ERROR,BADPIG              NO GRID LINES GENERATED                
         J     ERREXIT                                                          
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*----------------------------------------------------------------------         
* GRID COLUMN TABLE                                                             
*----------------------------------------------------------------------         
GCTBL    DC    CL3'2C0'                       GRID FORMAT ID                    
         DC    AL1(NUDATA-NURECD)             DEFAULT DISP TO 1ST ELEM          
         DC    AL1(5)                         DOR ADJUSTMENT                    
         DC    AL1(0)                         INDICATOR                         
         DC    AL2(GRDLIN1-T216FFD)           DISP TO FIRST LINE                
         DC    AL2(GRDLINL-T216FFD)           DISP TO LAST LINE                 
         DC    AL1(0)                         GENERAL INDICATOR                 
         DC    AL1(0)                         N/D                               
         DC    AL1(L'GRDLIN1)                 LENGTH OF LINE                    
         DC    AL1(GRDLIN2-GRDLIN1)           DISP BETWEEN LINES                
         DC    CL1' '                         NUMBER OF FIXED COLS-C''          
         DC    AL1(0)                         N/D                               
         DC    AL2(PCDRIVE-T216FFD)           TWA-PCDRIVEN                      
         DC    AL2((AIO1-GEND)+TOGENDQ)       W/S-A(IO AREA)                    
         DC    AL2((DLCB-SYSD)+TOSYSDQ)       W/S-DLCB                          
         DC    AL2(HEAD1-SPOOLD)              W/S-GSB                           
         DC    AL2((GCOSEL-SYSD)+TOSYSDQ)     W/S-COLUMN SELECTOR               
         DC    AL2((GMSG-SYSD)+TOSYSDQ)       W/S-MESSAGE OUTPUT AREA           
         DC    AL2((GSTAT-SYSD)+TOSYSDQ)      W/S-GRID STATUS BYTE              
         DC    AL4(0)                         N/D                               
*                                                                               
TOGENDQ  EQU   SPOOLEND-SPOOLD                                                  
TOSYSDQ  EQU   LIOALBLQ+(IO-GEND)+TOGENDQ                                       
TONETBQ  EQU   (STNETBLK-SYSD)+TOSYSDQ                                          
*---------------------------------------------                                  
* GRID DESCRIPTION DATA                                                         
*---------------------------------------------                                  
*        DC    AL1(GRLDQ)                     GRID DECRIPTION DATA              
*        DC    AL1(0)                         GENERAL INDICATOR                 
*        DC    AL1(L'GRDCLTN)                 DESCR. NAME FIELD LENGTH          
*        DC    AL1(L'GRDCLT)                  DESCR. NAME DATA LENGTH           
*        DC    AL2(            )              DISPL. TO NAME (IN TWA)           
*        DC    AL2(           )               DISPL. TO DATA (IN TWA)           
*        DC    AL4(0)                         N/D                               
*                                                                               
*---------------------------------------------                                  
* GRID COLUMNS                                                                  
*---------------------------------------------                                  
         DC    CL3'NT'    * NETWORK *         COLUMN ID           +00           
         DC    AL1(GRCTTXT)                   DATA TYPE           +03           
         DC    AL1(0)                         FORMAT OPTIONS      +04           
         DC    AL1(0)                         GENERAL INDICATOR   +05           
         DCDDL NE#NTWK,7                      COLUMN NAME         +06           
         DC    AL2((NBACTNET-NETBLOCKD)+TONETBQ) DISP TO DATA     +10           
         DC    AL1(L'NBACTNET)                LENGTH OF DATA      +12           
         DC    AL1(GRCDWS)                    DATA INDICATOR      +13           
         DC    AL1(0)                         ELEMENT CODE        +14           
         DC    AL1(0)                         ELEMENT SUB-CODE    +15           
         DC    AL1(0)                         OVERRIDE KEY LENGTH +16           
         DC    AL1(0)                         SPLIT POSITION      +17           
         DC    AL1(0)                         COLUMN SELECTOR     +18           
         DC    AL1(0)                         COLUMN INDICATOR    +19           
         DC    AL2(0)                         N/D                 +20           
*                                                                               
         DC    CL3'PG'                             * PROGRAM *                  
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL NE#PRGMS,7                                                       
         DC    AL2((NBACTPRG-NETBLOCKD)+TONETBQ)                                
         DC    AL1(L'NBACTPRG)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'PN'                             * PROGRAM NAME               
         DC    AL1(GRCTTXT,0,0)                                                 
         DCDDL NE#PNAM,12                                                       
         DC    AL2((NBPROGNM-NETBLOCKD)+TONETBQ)                                
         DC    AL1(L'NBPROGNM)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'DT',AL1(GRCTDAT,0,0)            * UNIT DATE                  
         DCDDL NE#UNDTE,9                                                       
         DC    AL2((NBACTDAT-NETBLOCKD)+TONETBQ)                                
         DC    AL1(2)                                                           
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'UN',AL1(GRCTTXT,0,0)            * UNIT LINE #                
         DCDDL NE#UNTLN,7                                                       
         DC    AL2((GRLIN#-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRLIN#)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'DY',AL1(GRCTTXT,0,0)            * DAY                        
         DCDDL NE#DAY,4                                                         
         DC    AL2((GRDAY-SYSD)+TOSYSDQ)                                        
         DC    AL1(L'GRDAY)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'RV',AL1(GRCTTXT,32,0)           * REVISION                   
         DCDDL NE#REV,4                                                         
         DC    AL2((GRREV-SYSD)+TOSYSDQ)                                        
         DC    AL1(L'GRREV)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'TM',AL1(GRCTTXT,32,0)           * TIME                       
         DCDDL NE#TIMES,4                                                       
         DC    AL2((GRTIME-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRTIME-1)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'DP',AL1(GRCTTXT,32,0)           * DAYPART                    
         DCDDL NE#DPT2,5                                                        
         DC    AL2((GRDPT2-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRDPT2)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'EP',AL1(GRCTTXT,32,0)           * ESTIMATE/PACKAGE           
         DCDDL NE#ESPAK,7                                                       
         DC    AL2((GRESPK-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRESPK)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'RO',AL1(GRCTTXT,0,0)            * ROTATION                   
         DCDDL NE#ROTTN,8                                                       
         DC    AL2((GROT-SYSD)+TOSYSDQ)                                         
         DC    AL1(L'GROT)                                                      
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'OC',AL1(GRCTTXT,0,0)            * OLD COMMERCIAL             
         DCDDL NE#CMMLO,9                                                       
         DC    AL2((GRCMLO-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRCMLO)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'O2',AL1(GRCTTXT,0,0)            * OLD COMML 2 *              
         DCDDL NE#CMLO2,11                                                      
         DC    AL2((GRCMLO2-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCMLO2)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'C1',AL1(GRCTTXT,0,0)            * COMML 1                    
         DCDDL NE#CMML,5                                                        
         DC    AL2((GRCML1-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRCML1)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'T1',AL1(GRCTTXT,32,0)            * CML 1 TITLE 1 *           
         DCDDL NE#CM1T1,24                                                      
         DC    AL2((GRCM1T1-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCM1T1)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'T2',AL1(GRCTTXT,32,0)            * CML 1 TITLE 2 *           
*        DCDDL NE#CM1T2,24                                                      
*        DC    AL2((GRCM1T2-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM1T2)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'T3',AL1(GRCTTXT,32,0)            * CML 1 TITLE 3 *           
*        DCDDL NE#CM1T3,24                                                      
*        DC    AL2((GRCM1T3-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM1T3)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'C2',AL1(GRCTTXT,0,0)            * COMML 2                    
         DCDDL NE#CMML2,6                                                       
         DC    AL2((GRCML2-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRCML2)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'T4',AL1(GRCTTXT,32,0)            * CML 2 TITLE 1 *           
         DCDDL NE#CM2T1,24                                                      
         DC    AL2((GRCM2T1-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCM2T1)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'T5',AL1(GRCTTXT,32,0)            * CML 2 TITLE 2 *           
*        DCDDL NE#CM2T2,24                                                      
*        DC    AL2((GRCM2T2-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM2T2)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'T6',AL1(GRCTTXT,32,0)            * CML 2 TITLE 3 *           
*        DCDDL NE#CM2T3,24                                                      
*        DC    AL2((GRCM2T3-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM2T3)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'C3',AL1(GRCTTXT,32,0)           * COMML 3                    
         DCDDL NE#CMML3,6                                                       
         DC    AL2((GRCML3-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRCML3)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'T7',AL1(GRCTTXT,32,0)            * CML 3 TITLE 1 *           
         DCDDL NE#CM3T1,24                                                      
         DC    AL2((GRCM3T1-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCM3T1)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'T8',AL1(GRCTTXT,32,0)            * CML 3 TITLE 2 *           
*        DCDDL NE#CM3T2,24                                                      
*        DC    AL2((GRCM3T2-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM3T2)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'T9',AL1(GRCTTXT,32,0)            * CML 3 TITLE 3 *           
*        DCDDL NE#CM3T3,24                                                      
*        DC    AL2((GRCM3T3-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM3T3)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'C4',AL1(GRCTTXT,32,0)           * COMML 4                    
         DCDDL NE#CMML4,6                                                       
         DC    AL2((GRCML4-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRCML4)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'TA',AL1(GRCTTXT,32,0)            * CML 4 TITLE 1 *           
         DCDDL NE#CM4T1,24                                                      
         DC    AL2((GRCM4T1-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCM4T1)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'TB',AL1(GRCTTXT,32,0)            * CML 4 TITLE 2 *           
*        DCDDL NE#CM4T2,24                                                      
*        DC    AL2((GRCM4T2-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM4T2)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'TC',AL1(GRCTTXT,32,0)            * CML 4 TITLE 3 *           
*        DCDDL NE#CM4T3,24                                                      
*        DC    AL2((GRCM4T3-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM4T3)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'C5',AL1(GRCTTXT,32,0)           * COMML 5                    
         DCDDL NE#CMML5,6                                                       
         DC    AL2((GRCML5-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRCML5)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'TD',AL1(GRCTTXT,32,0)            * CML 5 TITLE 1 *           
         DCDDL NE#CM5T1,24                                                      
         DC    AL2((GRCM5T1-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCM5T1)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'TE',AL1(GRCTTXT,32,0)            * CML 5 TITLE 2 *           
*        DCDDL NE#CM5T2,24                                                      
*        DC    AL2((GRCM5T2-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM5T2)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'TF',AL1(GRCTTXT,32,0)            * CML 5 TITLE 3 *           
*        DCDDL NE#CM5T3,24                                                      
*        DC    AL2((GRCM5T3-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM5T3)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'C6',AL1(GRCTTXT,32,0)           * COMML 6                    
         DCDDL NE#CMML6,6                                                       
         DC    AL2((GRCML6-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRCML6)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'TG',AL1(GRCTTXT,32,0)            * CML 6 TITLE 1 *           
         DCDDL NE#CM6T1,24                                                      
         DC    AL2((GRCM6T1-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCM6T1)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'TH',AL1(GRCTTXT,32,0)            * CML 6 TITLE 2 *           
*        DCDDL NE#CM6T2,24                                                      
*        DC    AL2((GRCM6T2-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM6T2)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'TI',AL1(GRCTTXT,32,0)            * CML 6 TITLE 3 *           
*        DCDDL NE#CM6T3,24                                                      
*        DC    AL2((GRCM6T3-SYSD)+TOSYSDQ)                                      
*        DC    AL1(L'GRCM6T3)                                                   
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'P1',AL1(GRCTTXT,0,0)            * PRD-LN                     
         DCDDL NE#PRDL,6                                                        
         DC    AL2((GRPRD1-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRPRD1)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'P2',AL1(GRCTTXT,0,0)            * PRT-LN                     
         DCDDL NE#PRTL,6                                                        
         DC    AL2((GRPRD2-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRPRD2)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'P3',AL1(GRCTTXT,32,0)           * PRD-LN 3                   
         DCDDL NE#PRDL3,7                                                       
         DC    AL2((GRPRDT3-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRPRDT3)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'P4',AL1(GRCTTXT,32,0)           * PRD-LN 4                   
         DCDDL NE#PRDL4,7                                                       
         DC    AL2((GRPRDT4-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRPRDT4)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'P5',AL1(GRCTTXT,32,0)           * PRD-LN 5                   
         DCDDL NE#PRDL5,7                                                       
         DC    AL2((GRPRDT5-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRPRDT5)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'P6',AL1(GRCTTXT,32,0)           * PRD-LN 6                   
         DCDDL NE#PRDL6,7                                                       
         DC    AL2((GRPRDT6-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRPRDT6)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'AD',AL1(GRCTTXT,0,0)            * ADID                       
         DCDDL NE#ADID,5                                                        
         DC    AL2((GRADID1-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRADID1)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'A2',AL1(GRCTTXT,0,0)            * ADID 2                     
         DCDDL NE#ADID2,7                                                       
         DC    AL2((GRADID2-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRADID2)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'A3',AL1(GRCTTXT,32,0)           * ADID 3                     
         DCDDL NE#ADID3,7                                                       
         DC    AL2((GRADID3-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRADID3)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'A4',AL1(GRCTTXT,32,0)           * ADID 4                     
         DCDDL NE#ADID4,7                                                       
         DC    AL2((GRADID4-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRADID4)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'A5',AL1(GRCTTXT,32,0)           * ADID 5                     
         DCDDL NE#ADID5,7                                                       
         DC    AL2((GRADID5-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRADID5)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'A6',AL1(GRCTTXT,32,0)           * ADID 6                     
         DCDDL NE#ADID6,7                                                       
         DC    AL2((GRADID6-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRADID6)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'F1',AL1(GRCTTXT,32,0)           * FEED 1                     
         DCDDL NE#FEED,4                                                        
         DC    AL2((GFEED1-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GFEED1)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'F2',AL1(GRCTTXT,32,0)           * FEED 2                     
         DCDDL NE#FEED2,6                                                       
         DC    AL2((GFEED2-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GFEED2)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'F3',AL1(GRCTTXT,32,0)           * FEED 3                     
         DCDDL NE#FEED3,6                                                       
         DC    AL2((GFEED3-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GFEED3)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'F4',AL1(GRCTTXT,32,0)           * FEED 4                     
         DCDDL NE#FEED4,6                                                       
         DC    AL2((GFEED4-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GFEED4)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'F5',AL1(GRCTTXT,32,0)           * FEED 5                     
         DCDDL NE#FEED5,6                                                       
         DC    AL2((GFEED5-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GFEED5)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'F6',AL1(GRCTTXT,32,0)           * FEED 6                     
         DCDDL NE#FEED6,6                                                       
         DC    AL2((GFEED6-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GFEED6)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'TY',AL1(GRCTTXT,32,0)           * NETWORK TYPE *             
         DCDDL NE#NTYPE,6                                                       
         DC    AL2((GRNTYPE-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRNTYPE)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
****     DC    CL3'BT',AL1(GRCTTXT,32,0)           * BUY TYPE *                 
****     DCDDL NE#BTYPE,6                                                       
****     DC    AL2((GRBTYPE-SYSD)+TOSYSDQ)                                      
****     DC    AL1(L'GRBTYPE)                                                   
****     DC    AL1(GRCDWS)                                                      
****     DC    AL1(0,0,0,0)                                                     
****     DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'BB',AL1(GRCTTXT,32,0)           * BB REQ  *                  
         DCDDL NE#BBREQ,6                                                       
         DC    AL2((GRBB-SYSD)+TOSYSDQ)                                         
         DC    AL1(L'GRBB)                                                      
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'BC',AL1(GRCTTXT,32,0)           * BB COML *                  
         DCDDL NE#BBCML,6                                                       
         DC    AL2((GRBBCML-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRBBCML)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'ST',AL1(GRCTTXT,32,0)           * STATUS *                   
         DCDDL NE#STAT,6                                                        
         DC    AL2((GRSTAT-SYSD)+TOSYSDQ)                                       
         DC    AL1(L'GRSTAT)                                                    
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'CI',AL1(GRCTTXT,32,0)           * CUT-IN *                   
         DCDDL NE#CUTIN,6                                                       
         DC    AL2((GRCUTIN-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRCUTIN)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'RD',AL1(GRCTTXT,32,0)           * REASSIGN DESC *            
         DCDDL NE#REASS,28                                                      
         DC    AL2((GRREASS-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRREASS)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'R2',AL1(GRCTTXT,32,0)           * REASSIGN DESC 2 *          
         DCDDL NE#R2ASS,28                                                      
         DC    AL2((GRR2ASS-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRR2ASS)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
***      DC    CL3'PR',AL1(GRCTTXT,32,0)           * PATTERN REF *              
***      DCDDL NE#PTREF,7                                                       
***      DC    AL2((GRPTREF-SYSD)+TOSYSDQ)                                      
***      DC    AL1(L'GRPTREF)                                                   
***      DC    AL1(GRCDWS)                                                      
***      DC    AL1(0,0,0,0)                                                     
***      DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'DA',AL1(GRCTTXT,32,0)           * DATE ADDED *               
         DCDDL NE#DAADD,14                                                      
         DC    AL2((GRDTADD-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRDTADD)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'D1',AL1(GRCTTXT,32,0)          * DATE CHANGED TR *           
         DCDDL NE#DACHA,17                                                      
         DC    AL2((GRDTCHAT-SYSD)+TOSYSDQ)                                     
         DC    AL1(L'GRDTCHAT)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'D2',AL1(GRCTTXT,32,0)          * DATE CHANGED NB *           
         DCDDL NE#DTCHN,13                                                      
         DC    AL2((GRDTCHAN-SYSD)+TOSYSDQ)                                     
         DC    AL1(L'GRDTCHAN)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
****     DC    CL3'T?',AL1(GRCTTXT,32,0)           * TIME ADDED *               
****     DCDDL NE#TMADD,8                                                       
****     DC    AL2((GRTMADD-SYSD)+TOSYSDQ)                                      
****     DC    AL1(L'GRTMADD)                                                   
****     DC    AL1(GRCDWS)                                                      
****     DC    AL1(0,0,0,0)                                                     
****     DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'PT',AL1(GRCTTXT,32,0)           * PID ON F1  *               
         DCDDL NE#GRPID,16                         * TRAFFIC CHANGE *           
         DC    AL2((GRPID-SYSD)+TOSYSDQ)                                        
         DC    AL1(L'GRPID)                                                     
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'PA',AL1(GRCTTXT,32,0)          * ADD PID ON 99 *             
         DCDDL NE#ADPID,13                        * BUY ADD *                   
         DC    AL2((GRPIDNA-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRPIDNA)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'PC',AL1(GRCTTXT,32,0)          * CHG PID ON 99 *             
         DCDDL NE#CHPID,13                        * BUY CHANGE *                
         DC    AL2((GRPIDNC-SYSD)+TOSYSDQ)                                      
         DC    AL1(L'GRPIDNC)                                                   
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
         DC    CL3'AC',AL1(GRCTTXT,32,0)           * ACTUAL COST                
         DCDDL NE#ACTCO,8                                                       
         DC    AL2((GRACTUAL-SYSD)+TOSYSDQ)                                     
         DC    AL1(L'GRACTUAL)                                                  
         DC    AL1(GRCDWS)                                                      
         DC    AL1(0,0,0,0)                                                     
         DC    AL1(0,0),AL2(0)                                                  
*                                                                               
*        DC    CL3'AS',AL1(GRCTTXT,32,0)           * ASSIGNED COST              
*        DCDDL NE#ASSCO,8                                                       
*        DC    AL2((GRASSIGN-SYSD)+TOSYSDQ)                                     
*        DC    AL1(L'GRASSIGN)                                                  
*        DC    AL1(GRCDWS)                                                      
*        DC    AL1(0,0,0,0)                                                     
*        DC    AL1(0,0),AL2(0)                                                  
         DC    X'FFFF'                                                          
         DROP  R7,RB,RC                                                         
***********************************************************************         
PRTFEED  NMOD1 0,*PRTFEE*,R7                                                    
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'23'        FEED ELEM                                    
         BRAS  RE,GETEL                                                         
         BNE   PFEEDX                                                           
         USING NUFDCEL,R6                                                       
LRR80    TM    NUFDCFL2,X'80'      WAS FEED DELETED                             
         BZ    *+12                 NO                                          
         TM    NUFDCFL2,X'40'      WAS FEED PRINTED ON INSTR                    
         BZ    LRR88                NO                                          
                                                                                
         TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    LRR83A                                                           
                                                                                
         CLC   P2+PPROD-PSTART(3),SPACES                                        
         BNH   LRR80B                                                           
         CLC   P2+PPROD-PSTART+3(4),SPACES                                      
         BH    LRR80B                                                           
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
LRR80B   DS    0H                                                               
                                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   PPROG+11(5),=C'FEED='                                            
         MVC   PPROG+16(4),NUFDCFED                                             
                                                                                
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    LRR82                NO                                          
         OC    NUFDPROD,NUFDPROD                                                
         BNZ   LRR80H                                                           
         CLI   NUFDCPRD,0                                                       
         BE    LRR80P                                                           
         TM    SECFLAG,NECONPRD                                                 
         BO    LRR80P                                                           
                                                                                
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
LRR80E   CLC   NUFDCPRD,3(RE)                                                   
         BE    LRR80F                                                           
         LA    RE,4(,RE)                                                        
         BCT   RF,LRR80E                                                        
         DC    H'0'                                                             
LRR80F   MVC   NUFDPROD,0(RE)                                                   
                                                                                
LRR80H   CLC   NUFDPROD,NBPR1CL3   THIS ONE OF PRODS AVAIL                      
         BE    LRR81                                                            
         CLC   NUFDPROD,NBPR2CL3   THIS OTHER OF PRODS AVAIL                    
         BE    LRR81                                                            
                                                                                
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRR80P                                                           
         LA    RE,L'SVCSPRDC/3                                                  
         LA    RF,SVCSPRDC                                                      
LRR80M   OC    0(3,RF),0(RF)                                                    
         BZ    LRR80P                                                           
         CLC   NUFDPROD,0(RF)                                                   
         BE    LRR81                                                            
         LA    RF,3(,RF)                                                        
         BCT   RE,LRR80M                                                        
                                                                                
LRR80P   MVC   PPROD+132(21),=C'PROD MUST BE ASSIGNED'                          
         B     LRR82                                                            
                                                                                
LRR81    MVC   BPRD1C,NUFDPROD                                                  
         MVC   PPRDPRD,NUFDPROD                                                 
         LR    R2,R6                                                            
         MVC   BSLN,SVNUSLN1                                                    
         LA    R6,PPROD                                                         
         BRAS  RE,PPRD                                                          
         LR    R6,R2                                                            
                                                                                
         BRAS  RE,INITSPT          SET TO SPOT                                  
         BRAS  RE,PPRDN                                                         
         MVC   PPRODNM,PRDNM                                                    
         BRAS  RE,INITNET          SET TO NET                                   
                                                                                
         MVC   BSLN,SVNUSLN2                                                    
LRR82    TM    NUFDCFL2,X'80'      WAS FEED DELETED                             
         BZ    *+14                 NO                                          
         MVC   PCMML(7),=C'DELETED'                                             
         B     LRR88                NO                                          
         MVC   PCMML+3(3),=C'N/A'                                               
         BRAS  RE,INITSPT          SET FROM NET TO SPOT                         
         OC    NUFDCML1,NUFDCML1                                                
         BZ    LRR84                                                            
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRR82J                                                           
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BE    LRR84                                                            
                                                                                
LRR82J   DS    0H                                                               
         MVC   PCMML(8),NUFDCML1                                                
                                                                                
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
                                                                                
LRR83A   MVC   WORK(8),NUFDCML1                                                 
         MVC   WORKPROD,BPRD1C                                                  
         XC    SVTYPE,SVTYPE                                                    
         XC    SVNUCML1(24),SVNUCML1 CLEAR FOR SVNUCML1/2/3                     
                                                                                
         CLC   NUFDPROD,SPACES                                                  
         BH    LRR83F                                                           
         CLI   NUFDCPRD,0                                                       
         BE    LRR83G                                                           
         TM    SECFLAG,NECONPRD                                                 
         BO    LRR83G                                                           
                                                                                
*  IT'S POSSIBLE THIS LOOP IS REDUNDANT - RESEARCH LATER                        
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
LRR83D   CLC   NUFDCPRD,3(RE)                                                   
         BE    LRR83E                                                           
         LA    RE,4(,RE)                                                        
         BCT   RF,LRR83D                                                        
         DC    H'0'                                                             
LRR83E   MVC   NUFDPROD,0(RE)                                                   
                                                                                
LRR83F   MVC   WORKPROD,NUFDPROD                                                
LRR83G   MVC   BYTE1,NUFDCFLG                                                   
         BRAS  RE,FCMLA                                                         
         BNE   LRR82F                                                           
                                                                                
         MVC   PCMML(8),WORK          8 CHAR CML                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    LRR83K                                                           
         MVC   PCMML(9),REASIGN                                                 
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR83H                                                           
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),NUFDCML1                                                
                                                                                
         TM    NUFDADFL,NUFDADF1   AD-ID FEED CML 1                             
         BZ    LRR83H                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUFDCML1                                                    
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR83H   MVC   PCMML+9(37),SPACES                                               
         MVC   PCMML+9(15),=C'MAX DATE ERROR '                                  
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   PCMML+9(20),=C'NOT APPROVED TO AIR '                             
         B     LRR84                                                            
                                                                                
LRR83K   XC    WORK+8(16),WORK+8                                                
                                                                                
         MVC   WORK(16),NUFDCML1                                                
*NOP     CLC   WORK(8),WORK+8                                                   
*        BNE   *+10                                                             
*******  XC    WORK+8(8),WORK+8                                                 
                                                                                
         MVI   JUSTLEN,C'Y'                                                     
         BRAS  RE,FCMLA                                                         
         MVI   JUSTLEN,C'N'                                                     
                                                                                
         TM    NBUNST3,X'40'       IS IT C/S                                    
         BO    LRR82D                                                           
                                                                                
         CLC   NBPR2CL3,SPACES                                                  
         BNH   LRR82D                                                           
         OC    NUFDCML2,NUFDCML2                                                
         BZ    LRR83                                                            
                                                                                
LRR82D   ZIC   RE,SVNUSLN1                                                      
         ZIC   RF,SVNUSLN2                                                      
         AR    RE,RF               TOTAL UNIT LENGTH                            
                                                                                
         ZIC   RF,SVLEN1                                                        
         ZIC   R1,SVLEN2                                                        
         AR    R1,RF               TOTAL CML LENGTH                             
                                                                                
         CLC   =C'VIGNETTE',NUFDCBSN                                            
         BNE   LRRV82                                                           
         ZIC   RF,NUFDCBSL                                                      
         AR    R1,RF               ADD LEN OF VIGNETTE                          
                                                                                
LRRV82   CR    R1,RE                                                            
         BE    LRR83                                                            
         OI    BYTE1,X'80'         UNIT LEN CHANGED                             
         MVC   ELEM+1(11),=C'LEN CHANGED'                                       
                                                                                
LRR82F   MVC   PCMML(9),REASIGN                                                 
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR82K                                                           
                                                                                
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),NUFDCML1                                                
                                                                                
         TM    NUFDADFL,NUFDADF1   AD-ID FEED CML 1                             
         BZ    LRR82K                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUFDCML1                                                    
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR82K   MVC   PCMML+9(37),ELEM                                                 
         CLC   BYTE1,NUFDCFLG                                                   
         BE    LRR84                                                            
         MVC   NUFDCFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
         B     LRR84                                                            
LRR83    MVC   PCMMLT,SVCMTITL                                                  
                                                                                
LRR84    OC    NUFDCML2,NUFDCML2                                                
         BZ    LRR86                                                            
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRR84C                                                           
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRR86                                                            
LRR84C   DS    0H                                                               
                                                                                
         CLC   NUFDCML1,NUFDCML2       THIS A PIGGYBACK                         
         BNE   *+14                                                             
         MVC   PCMML+132+3(3),=C'P/B'                                           
         B     LRR86                                                            
         MVC   PCMML+132(8),NUFDCML2                                            
                                                                                
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
                                                                                
         MVC   WORK(8),NUFDCML2                                                 
         MVC   WORKPROD,BPRD2C                                                  
         MVC   BYTE1,NUFDCFLG                                                   
         BRAS  RE,FCMLA                                                         
         BNE   LRR85                                                            
                                                                                
         MVC   PCMML+132(8),WORK                                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132,SVCMLADI                                               
                                                                                
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    LRR85S                                                           
         MVC   PCMML+132(9),REASIGN                                             
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR84F                                                           
                                                                                
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),NUFDCML2                                                
                                                                                
         TM    NUFDADFL,NUFDADF2   AD-ID FEED CML 2                             
         BZ    LRR84F                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUFDCML2                                                    
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR84F   MVC   PCMML+132+9(37),SPACES                                           
         MVC   PCMML+132+9(15),=C'MAX DATE ERROR '                              
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   PCMML+132+9(20),=C'NOT APPROVED TO AIR '                         
         B     LRR86                                                            
                                                                                
LRR85    MVC   PCMML+132(8),REASIGN                                             
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR85F                                                           
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),NUFDCML2                                                
                                                                                
         TM    NUFDADFL,NUFDADF2   AD-ID FEED CML 2                             
         BZ    LRR85F                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUFDCML2                                                    
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR85F   MVC   PCMML+132+9(37),ELEM                                             
         CLC   NUFDCFLG,BYTE1                                                   
         BE    LRR85S                                                           
         MVC   NUFDCFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
         B     LRR86                                                            
LRR85S   MVC   PCMMLT+132,SVCMTITL                                              
                                                                                
LRR86    TM    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         BO    PFEEDX                                                           
                                                                                
         CLI   NUFDCBSL,0          BILLBOARD                                    
         BE    LRR88               NO                                           
         MVC   SVBBSLN,NUFDCBSL                                                 
         MVC   SVBBPOS,NUFDCBPS                                                 
                                                                                
         MVC   SVBBFLG,NUFDADFL    SAVE BILLBOARD FLAG                          
                                                                                
         MVC   WORKPROD,BPRD1C     MOVE PROD                                    
         BRAS  RE,PBB              GO PRINT BILLBOARD                           
                                                                                
LRR88    BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         MVI   ELCODE,X'23'        FEED ELEM                                    
         BRAS  RE,NEXTEL                                                        
         BE    LRR80                                                            
                                                                                
PFEEDX   XIT1                                                                   
                                                                                
REASIGN  DC    CL9'REASSIGN '                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
* FORMAT ONLINE LIST                                                            
                                                                                
LRL      NMOD1 0,**+LRL**,R7                                                    
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         LA    R1,=C'DMREAD'                                                    
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)                                                 
         MVI   DMCB+8,1                                                         
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(OPTPLEN)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,AOPTPGRL,,(RF)                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
LRL00B   DS    0H                                                               
                                                                                
         OC    TOTUNITS,TOTUNITS   IN MIDDLE OF LIST                            
         BZ    LRL00G              YES                                          
         LA    R1,=C'DMREAD'                                                    
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)                                                 
         MVI   DMCB+8,4                                                         
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(ESTBLEN)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,AESTBL,,(RF)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
LRL00G   DS    0H                                                               
         MVI   NLISTS,14           MAX NUMBER OF LINES TO LIST                  
         OI    GLSTSTAT,RETEXTRA   EXTRA CALL AFTER LISTMON                     
                                                                                
         XC    SVNET,SVNET         RESTART FOR RDREV IF ACT OPT                 
         XC    TRATSP,TRATSP                                                    
         OI    TRATSUPH+1,X'0C'    CHANGE FLDS TO LOW INTENSITY                 
         OI    TRATSUPH+6,X'80'    TRANSMIT                                     
         OI    TRATSPH+1,X'0C'                                                  
         OI    TRATSPH+6,X'80'                                                  
         XC    BLOCK(200),BLOCK                                                 
         OC    TOTUNITS,TOTUNITS   IN MIDDLE OF LIST                            
         BNZ   LRL04               YES                                          
                                                                                
LRL01    BRAS  RE,NETI                                                          
                                                                                
         TM    OPTNSW1,OPTALLU     SHOW ALL UNITS                               
         BO    LRL01F                                                           
                                                                                
         TM    UNITSW,UNITSWMI     FILTER BY NEGATIVE UNITS                     
         BO    *+12                 YES                                         
         TM    UNITSW,UNITSWAC    THIS BUY ACTIVITY                             
         BZ    LRL02               NO                                           
                                                                                
LRL01F   MVI   NBSELUOP,C'B'       GET EST AND ACTUAL                           
         OI    NBDMGOPT,X'08'     READ DELETED UNITS                            
                                                                                
         OC    NBKEY,NBKEY         1ST TIME                                     
         BZ    LRL02                YES                                         
         CLI   NBMODE,NBPROCUN     PROCESS UNIT                                 
         BNE   LRL02                NO                                          
                                                                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',NBKEY,KEY             
         CLC   KEY(20),NBKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
LRL02    OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
         MVC   NBACLI,ACLTREC                                                   
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
*                                                                               
         TM    OPTNSW1,OPTDIGI     OPTION TO SHOW DIGITAL                       
         BO    *+20                                                             
         CLI   SVTN2PRO+14,C'Y'    PROFILE TO SUPPRESS DIGITAL                  
         BNE   *+12                                                             
         CLI   NBSTATYP,C'V'       MEDIA TYPE FROM STATION RECORD               
         BE    LRL10                                                            
*                                                                               
         CLI   ACTNUM,29                                                        
         BNE   LRL02D                                                           
         XC    GRDFLDS1(GRFLDLN1),GRDFLDS1                                      
         XC    GRDFLDS2(GRFLDLN2),GRDFLDS2                                      
         XC    GRDFLDS3(GRFLDLN3),GRDFLDS3                                      
         XC    GRDFLDS4(GRFLDLN4),GRDFLDS4                                      
                                                                                
         CLI   LCTN2PR8,C'Y'                                                    
         BNE   LRL02A                                                           
         EDIT  (B4,NBACTUAL),(13,GRACTUAL),2,ZERO=NOBLANK,ALIGN=LEFT            
         EDIT  (B4,NBASSIGN),(13,GRASSIGN),2,ZERO=NOBLANK,ALIGN=LEFT            
                                                                                
LRL02A   MVC   GRLIN#(1),NBACTSUB                                               
         CLI   NBACTSUB,C'A'                                                    
         BNL   LRL02C                                                           
         MVC   GRLIN#B,NBACTSUB                                                 
         EDIT  (B1,GRLIN#B),(3,GRLIN#),0,ZERO=NOBLANK,ALIGN=LEFT                
                                                                                
LRL02C   DS    0H                                                               
         CLI   NBACTDP,0                                                        
         BE    LRL02D                                                           
         MVC   HALF,QDPT2                                                       
         MVC   BYTE,QDPT                                                        
         GOTO1 VALIDPT,DMCB,(X'01',NBACTDP)  GET 2 CHAR DAYPART                 
         MVC   GRDPT2,QDPT2                                                     
         MVC   QDPT2,HALF                                                       
         MVC   QDPT,BYTE                                                        
                                                                                
LRL02D   DS    0H                                                               
                                                                                
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LRL98                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRL02                                                            
         B     LRL20                                                            
                                                                                
LRL04    BAS   RE,RDTWA                                                         
         TM    ADIDSW,PRNTSCND                                                  
         BZ    LRL04A                                                           
         MVC   LISTAR,SPACES                                                    
         MVC   LCML+2(L'SVADIDLS),SVADIDLS                                      
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
         NI    ADIDSW,X'FF'-PRNTSCND                                            
         XC    SVLISTAR,SVLISTAR                                                
         XC    SVADIDLS,SVADIDLS                                                
         B     LRL04C                                                           
                                                                                
LRL04A   DS    0H                                                               
         TM    ADIDSW,PRNTBOTH                                                  
         BZ    LRL04C                                                           
         MVC   LISTAR,SVLISTAR                                                  
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
         MVC   LISTAR,SPACES                                                    
         MVC   LCML+2(L'SVADIDLS),SVADIDLS                                      
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
         XC    SVLISTAR,SVLISTAR                                                
         XC    SVADIDLS,SVADIDLS                                                
         NI    ADIDSW,X'FF'-PRNTBOTH                                            
LRL04C   DS    0H                                                               
         OI    SVFLAG,SVKEYSW      SAVE THIS KEY                                
                                                                                
         CLI   ACTNUM,29                                                        
         BE    LRL04F                                                           
                                                                                
         CLI   PFKEY,6             PF6 - TOP                                    
         BE    *+12                                                             
         CLI   PFKEY,7             PF7 - UP                                     
         BNE   LRL04F                                                           
                                                                                
         BRAS  RE,TBLKEY           GET THAT KEY                                 
         TM    SVFLAG,SVKEYSW                                                   
         BZ    *+8                                                              
         NI    SVFLAG,X'FF'-SVKEYSW                                             
                                                                                
         CLI   PFKEY,6             PF6 - TOP                                    
         BE    LRL01                                                            
                                                                                
LRL04F   TM    UNITSW,UNITSWAC     THIS BUY ACTIVITY                            
         BZ    LRL05                                                            
         TM    UNITSW,UNITSWUN     THIS BUY ACTIVITY - UNASSIGNED               
         BO    LRL05                                                            
                                                                                
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         SR    R2,R2                                                            
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   TRAPERR1             YES                                         
                                                                                
         BRAS  RE,RDREV                                                         
                                                                                
* RESTART NETIO FOR ONLINE LIST *                                               
                                                                                
LRL05    MVC   NBAIO,AIO3                                                       
         L     R1,SYSPARMS                                                      
         L     RF,16(,R1)           COMFACS ADDRESS                             
         ST    RF,NBACOM                                                        
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A27')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANETIO                                                        
                                                                                
         CLI   ACTNUM,29                                                        
         BE    LRL05X                                                           
                                                                                
         CLI   PFKEY,7                                                          
         BNE   LRL05X                                                           
                                                                                
         MVC   NBKEY,KEY                                                        
         MVI   NBFUNCT,0                                                        
         MVI   NBRESUME,NBPROCUN                                                
         B     *+8                                                              
LRL05X   MVI   NBFUNCT,NBFRDHI                                                  
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   LRL05Z                                                           
         TM    SVFLAG,GRIDCTU                                                   
         BZ    *+12                                                             
         NI    SVFLAG,X'FF'-GRIDCTU                                             
         B     LRL91                                                            
                                                                                
LRL05Z   OC    NBKEY,NBKEY         1ST TIME                                     
         BZ    LRL06                YES                                         
         CLI   NBMODE,NBPROCUN     PROCESS UNIT                                 
         BNE   LRL06                NO                                          
                                                                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',NBKEY,KEY             
         CLC   KEY(20),NBKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
LRL06    OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
         TM    ADIDSW,SKIPNTIO     THIS SWITCH WILL NEVER BE ON IN              
*        BO    LRL06A              THE GRID ENVIRONMENT                         
         BZ    *+10                                                             
         MVC   NBKEY,NBKEYLST                                                   
         MVC   NBACLI,ACLTREC                                                   
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
LRL06A   DS    0H                                                               
         NI    ADIDSW,X'FF'-SKIPNTIO                                            
                                                                                
         CLI   ACTNUM,29                                                        
         BNE   LRL06M                                                           
         XC    GRDFLDS1(GRFLDLN1),GRDFLDS1                                      
         XC    GRDFLDS2(GRFLDLN2),GRDFLDS2                                      
         XC    GRDFLDS3(GRFLDLN3),GRDFLDS3                                      
         XC    GRDFLDS4(GRFLDLN4),GRDFLDS4                                      
                                                                                
         CLI   LCTN2PR8,C'Y'                                                    
         BNE   LRL06E                                                           
         EDIT  (B4,NBACTUAL),(13,GRACTUAL),2,ZERO=NOBLANK,ALIGN=LEFT            
         EDIT  (B4,NBASSIGN),(13,GRASSIGN),2,ZERO=NOBLANK,ALIGN=LEFT            
                                                                                
LRL06E   MVC   GRLIN#(1),NBACTSUB                                               
         CLI   NBACTSUB,C'A'                                                    
         BNL   LRL06J                                                           
         MVC   GRLIN#B,NBACTSUB                                                 
         EDIT  (B1,GRLIN#B),(3,GRLIN#),0,ZERO=NOBLANK,ALIGN=LEFT                
                                                                                
LRL06J   DS    0H                                                               
         CLI   NBACTDP,0                                                        
         BE    LRL06M                                                           
         MVC   HALF,QDPT2                                                       
         MVC   BYTE,QDPT                                                        
         GOTO1 VALIDPT,DMCB,(X'01',NBACTDP)  GET 2 CHAR DAYPART                 
         MVC   GRDPT2,QDPT2                                                     
         MVC   QDPT2,HALF                                                       
         MVC   QDPT,BYTE                                                        
LRL06M   DS    0H                                                               
*                                                                               
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         MVI   NBFUNCT,NBFNORM                                                  
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LRL98                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRL06                                                            
         B     LRL14                                                            
                                                                                
LRL10    MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         SR    R2,R2                                                            
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   TRAPERR1             YES                                         
                                                                                
         XC    SVCMLADI,SVCMLADI                                                
         XC    SVCMLAD1,SVCMLAD1                                                
         XC    SVCMLAD2,SVCMLAD2                                                
                                                                                
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   LRL10A                                                           
         TM    SVFLAG,GRIDCTU                                                   
         BZ    *+12                                                             
         NI    SVFLAG,X'FF'-GRIDCTU                                             
         B     LRL91                                                            
                                                                                
LRL10A   OC    NBKEY,NBKEY         1ST TIME                                     
         BZ    LRL14                YES                                         
         CLI   NBMODE,NBPROCUN     PROCESS UNIT                                 
         BNE   LRL14                NO                                          
                                                                                
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',NBKEY,KEY             
         CLC   KEY(20),NBKEY                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
*                                                                               
*        BAS   RE,RDTWA                                                         
*        TM    ADIDSW,SKIPNTIO     THIS SWITCH WILL NEVER BE ON IN              
*        BO    LRL10A1             THE GRID ENVIRONMENT                         
*                                                                               
         MVC   NBACLI,ACLTREC                                                   
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
LRL10A1  DS    0H                                                               
*                                                                               
         TM    OPTNSW1,OPTDIGI     OPTION TO SHOW DIGITAL                       
         BO    *+20                                                             
         CLI   SVTN2PRO+14,C'Y'    PROFILE TO SUPPRESS DIGITAL                  
         BNE   *+12                                                             
         CLI   NBSTATYP,C'V'       MEDIA TYPE FROM STATION RECORD               
         BE    LRL10                                                            
*                                                                               
         CLI   ACTNUM,29                                                        
         BNE   LRL10D                                                           
         XC    GRDFLDS1(GRFLDLN1),GRDFLDS1                                      
         XC    GRDFLDS2(GRFLDLN2),GRDFLDS2                                      
         XC    GRDFLDS3(GRFLDLN3),GRDFLDS3                                      
         XC    GRDFLDS4(GRFLDLN4),GRDFLDS4                                      
                                                                                
         CLI   LCTN2PR8,C'Y'                                                    
         BNE   LRL10B                                                           
         EDIT  (B4,NBACTUAL),(13,GRACTUAL),2,ZERO=NOBLANK,ALIGN=LEFT            
         EDIT  (B4,NBASSIGN),(13,GRASSIGN),2,ZERO=NOBLANK,ALIGN=LEFT            
                                                                                
LRL10B   MVC   GRLIN#(1),NBACTSUB                                               
         CLI   NBACTSUB,C'A'                                                    
         BNL   LRL10C                                                           
         MVC   GRLIN#B,NBACTSUB                                                 
         EDIT  (B1,GRLIN#B),(3,GRLIN#),0,ZERO=NOBLANK,ALIGN=LEFT                
                                                                                
LRL10C   DS    0H                                                               
         CLI   NBACTDP,0                                                        
         BE    LRL10D                                                           
         MVC   HALF,QDPT2                                                       
         MVC   BYTE,QDPT                                                        
         GOTO1 VALIDPT,DMCB,(X'01',NBACTDP)  GET 2 CHAR DAYPART                 
         MVC   GRDPT2,QDPT2                                                     
         MVC   QDPT2,HALF                                                       
         MVC   QDPT,BYTE                                                        
LRL10D   DS    0H                                                               
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    LRL14                                                            
         DC    H'0'                                                             
LRL14    TM    NBSUBMSK,NBSBMCLI                                                
         BO    LRL98                                                            
         OC    NETWORK,NETWORK     WAS NETWORK ENTERED                          
         BZ    LRL16                                                            
         TM    NBSUBMSK,NBSBMNET                                                
         BO    LRL98                                                            
         OC    NBSELPRG,NBSELPRG                                                
         BZ    LRL16                                                            
         TM    NBSUBMSK,NBSBMPRG                                                
         BO    LRL98                                                            
LRL16    CLI   NBMODE,NBREQLST                                                  
         BE    LRL98                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LRL10                                                            
                                                                                
LRL20    L     R6,NBAIO                                                         
                                                                                
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
                                                                                
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
LRL20F   BRAS  RE,NEXTEL                                                        
         BNE   LRL21                                                            
         CLI   2(R6),C'Q'          IS THIS A TAG                                
         BNE   LRL20F                                                           
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   LRL20F                                                           
                                                                                
         PACK  DUB,4(1,R6)                                                      
         CVB   R0,DUB              TAGS IN BINARY                               
         MHI   R0,5                TIMES 5 (5 SECONDS PER TAG)                  
         ZIC   R1,NBLEN            TOTAL UNIT LEN                               
         SR    R1,R0               MINUS TAG LEN                                
         STC   R1,NBLEN            = ACTUAL UNIT LENGTH                         
                                                                                
LRL21    L     R6,NBAIO                                                         
                                                                                
* CONVERT EQUIVALENT PROGRAM CODES TO BASE AND SAVE COPY SPLITS *               
                                                                                
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
                                                                                
         BRAS  RE,CNVRTP                                                        
                                                                                
         OC    PROGRAM,PROGRAM                                                  
         BZ    *+14                                                             
         CLC   NBACTPRG,PROGRAM    IS THIS THE PROGRAM                          
         BNE   LRL10                NO                                          
                                                                                
         BRAS  RE,LFTR                                                          
         BNE   LRL10                                                            
                                                                                
         CLI   ACTNUM,29                                                        
         BNE   LR21Z                                                            
         L     R1,NBAIO                                                         
         MVC   BYTE,22(R1)                                                      
         NI    BYTE,X'7F'                                                       
         MVI   GRNTYPE,C'N'                                                     
         CLI   BYTE,0                                                           
         BE    LR21C                                                            
         MVI   GRNTYPE,C'C'                                                     
         CLI   BYTE,1                                                           
         BE    LR21C                                                            
         MVI   GRNTYPE,C'S'                                                     
         CLI   BYTE,2                                                           
         BE    LR21C                                                            
         MVI   GRNTYPE,C'O'                                                     
         CLI   BYTE,3                                                           
         BE    LR21C                                                            
         MVI   GRNTYPE,C' '                                                     
                                                                                
LR21C    DS    0H                                                               
         ST    R6,TEMPADDR            MUST SAVE AND RESTORE THIS ADDR           
                                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR21D                                                            
         GOTO1 DATCON,DMCB,(3,2(R6)),(21,GRDTADD)                               
         GOTO1 DATCON,DMCB,(3,8(R6)),(21,GRDTCHAT)                              
         MVC   GRPID,20(R6)                                                     
                                                                                
LR21D    DS    0H                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'99'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR21E                                                            
         GOTO1 DATCON,DMCB,(3,4(R6)),(21,GRDTADD)                               
         GOTO1 DATCON,DMCB,(3,9(R6)),(21,GRDTCHAN)                              
                                                                                
         MVC   SVPIDHEX,NUACTCID-NUACTD(R6)                                     
         BRAS  RE,GETPID                                                        
         MVC   GRPIDNC,SVPID                                                    
                                                                                
         MVC   SVPIDHEX,NUACTAID-NUACTD(R6)                                     
         BRAS  RE,GETPID                                                        
         MVC   GRPIDNA,SVPID                                                    
                                                                                
LR21E    DS    0H                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LR21X                                                            
         CLI   NUSPRTYP-NUSPRD(R6),C'U'                                         
         BNE   LR21X                                                            
         MVC   GRCUTIN,=C'CUT-IN'                                               
                                                                                
LR21X    DS    0H                                                               
         L     R6,TEMPADDR                                                      
                                                                                
LR21Z    TM    OPTNSW1,OPTALLU     SHOW ALL UNITS                               
         BO    LRL22                                                            
                                                                                
         TM    UNITSW,UNITSWAC     THIS BUY ACTIVITY                            
         BO    LRL22                                                            
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRL22                                                            
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRL10                                                            
                                                                                
LRL22    OI    UNITSW,UNITSWAL     SET ON SOME ALLOCATED                        
                                                                                
         TM    UNITSW,UNITSWAC     THIS BUY ACTIVITY                            
         BZ    LRL30                                                            
                                                                                
         CLC   NBACTNET,SVNET                                                   
         BE    LRL26                                                            
                                                                                
         MVC   SVNET,NBACTNET                                                   
                                                                                
*        GO READ AND TABLE ALL REVISION RECS FOR CABLE FOR REPT PERIOD          
                                                                                
         TM    UNITSW,UNITSWUN    THIS BUY ACTIVITY - UNASSIGNED                
         BO    LRL26                                                            
                                                                                
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         SR    R2,R2                                                            
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   TRAPERR1             YES                                         
                                                                                
         BRAS  RE,RDREV                                                         
                                                                                
LRL26    BRAS  RE,FACT               FILTER FOR ACTIVITY REPORT                 
         BNE   LRL10                                                            
                                                                                
LRL30    CLI   NBACTSUB,C'A'       IF SKED UNIT, NO EST CK                      
         BNL   LRL31                                                            
                                                                                
         BRAS  RE,CKEST            CK THIS EST-BYPASS IF CPY CD=N               
         BE    LRL10                                                            
                                                                                
LRL31    MVC   LISTAR,SPACES                                                    
         MVC   LNET,NBACTNET                                                    
         MVC   LPROG,NBACTPRG                                                   
                                                                                
         CLI   FEEDFLG,C' '        WAS THERE A MISSING/ERR IN FEED CML          
         BNH   *+8                                                              
         MVI   LREASON,C'F'                                                     
                                                                                
         CLI   CUTNFLG,C' '        WAS THERE A MISSING/ERR IN CUTIN CML         
         BNH   LRL31A                                                           
         MVI   LREASON,C'C'                                                     
         MVC   GRCUTIN,=C'CUT-IN'  SET UP IF CUTIN NEEDS ATTENTION              
                                                                                
LRL31A   TM    OPTNSW,OPTPNAME     SHOW PROG NAME, NOT CODE?                    
         BZ    LRL32                                                            
         MVC   LPROG,NBPROGNM                                                   
                                                                                
LRL32    CLI   EQVPRGSW,C'X'       THIS AN UNCOVERED EQUIV PROG CODE            
         BNE   *+8                                                              
         MVI   LPROG-1,C'*'                                                     
                                                                                
         CLI   EQVPRGSW,C'Y'       THIS AN EQUIVALENT PROG CODE                 
         BNE   LRL33                                                            
         MVI   LPROG-1,C'='                                                     
                                                                                
         OC    SVPENDT,SVPENDT     ANY ERROR?                                   
         BZ    *+8                                                              
         MVI   LPROG+6,C'*'        SHOW ERROR ON SCREEN                         
                                                                                
LRL33    TM    NURSTAT-NURECD(R6),X'80' THIS UNIT DELETED                       
         BZ    LRL34                                                            
                                                                                
         MVC   LPROD(13),=CL13'** MDELETE **'                                   
         CLI   NUKSUB-NURECD(R6),X'C1' THIS SKED (TRAFFIC) UNIT                 
         BL    LRL44               NO                                           
         MVI   LPROD+3,C'T'       TRAFFIC DELETE                                
         B     LRL44                                                            
                                                                                
LRL34    MVC   BPRD1C,NBPR1CL3                                                  
         XC    BPRD2C,BPRD2C                                                    
                                                                                
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    LRL36                                                            
                                                                                
         OC    BPRD3C,BPRD3C       TRIBACK                                      
         BNZ   LRL40                                                            
                                                                                
         OC    SVCSPRDC(3),SVCSPRDC   MAYBE TRI-BACK?                           
         BNZ   LRL43                   YES                                      
         CLC   NBPR2CL3,SPACES        PARTNER                                   
         BNH   LRL40                                                            
                                                                                
* CALCULATE PRODUCT UNIT LENGTHS *                                              
                                                                                
         LLC   R1,NBLEN1           1ST PRD LEN                                  
         CLI   NBLEN1,0            IF ZERO                                      
         BNE   LRL35C                                                           
*                                                                               
         SR    R0,R0                                                            
         LLC   R1,NBLEN            TOTAL LEN                                    
         D     R0,=F'2'                                                         
*        STC   R1,BSLN2                                                         
         LR    R2,R1               PRD2 LEN                                     
         AR    R1,R0               ADD REMAINDER 1 IF ANY                       
         STC   R1,BSLN                                                          
         LLC   R0,NBLEN                                                         
*                                                                               
         BRAS  RE,FIXLEN           FIX PROD LEN = CML LEN                       
         B     LRL42                                                            
*                                                                               
*******************                                                             
*        ZIC   R2,NBLEN            THEN TOTAL LEN                               
*        SRL   R2,1                DIVIDE BY 2                                  
*        LR    R1,R2                                                            
*        STC   R1,BSLN                                                          
*******  B     LRL42                                                            
                                                                                
LRL35C   STC   R1,BSLN                                                          
         LLC   R2,NBLEN            TOTAL LEN                                    
         LR    R0,R2                                                            
         SR    R2,R1                                                            
         BRAS  RE,FIXLEN           FIX PROD LEN = CML LEN                       
         B     LRL42                                                            
                                                                                
LRL36    DS    0H                  ONLY GET HERE ON COPY/SPLIT                  
         CLC   NBPR1CL3,SPACES     PRODUCT?                                     
         BNH   LRL43                NO                                          
                                                                                
LRL40    MVC   BSLN,NBLEN                                                       
         SR    R2,R2                                                            
                                                                                
LRL42    LA    R6,LPROD                                                         
         MVC   PPRDPRD,BPRD1C                                                   
         BRAS  RE,PPRD                                                          
                                                                                
         CLC   NBPR2CL3,SPACES     PARTNER                       +              
         BNH   LRL44                                                            
         MVC   BPRD2C,NBPR2CL3                                                  
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    *+8                                                              
         STC   R2,BSLN                                                          
         LA    R6,LPROD2                                                        
         MVC   PPRDPRD,BPRD2C                                                   
         BRAS  RE,PPRD                                                          
         B     LRL44                                                            
                                                                                
LRL43    DS    0H                                                               
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   LRL43B                                                           
         ZIC   R2,NBLEN            THEN TOTAL LEN                               
                                                                                
         STC   R2,BSLN                    PRODUCT 1                             
         LA    R6,LPROD                                                         
         MVC   PPRDPRD,SVCSPRDC                                                 
         BRAS  RE,PPRD                                                          
                                                                                
         OC    SVCSPRDC+3(3),SVCSPRDC+3   PRODUCT 2                             
         BZ    LRL43A                                                           
         LA    R6,LPROD2                                                        
         MVC   PPRDPRD,SVCSPRDC+3                                               
         BRAS  RE,PPRD                                                          
                                                                                
         OC    SVCSPRDC+6(3),SVCSPRDC+6   PRODUCT 3                             
         BZ    LRL43A                                                           
         LA    R6,GRPRD3                                                        
         MVC   PPRDPRD,SVCSPRDC+6                                               
         BRAS  RE,PPRD                                                          
                                                                                
         OC    SVCSPRDC+9(3),SVCSPRDC+9   PRODUCT 4                             
         BZ    LRL43A                                                           
         LA    R6,GRPRD4                                                        
         MVC   PPRDPRD,SVCSPRDC+9                                               
         BRAS  RE,PPRD                                                          
                                                                                
         OC    SVCSPRDC+12(3),SVCSPRDC+12  PRODUCT 5                            
         BZ    LRL43A                                                           
         LA    R6,GRPRD5                                                        
         MVC   PPRDPRD,SVCSPRDC+12                                              
         BRAS  RE,PPRD                                                          
                                                                                
         OC    SVCSPRDC+15(3),SVCSPRDC+15  PRODUCT 6                            
         BZ    LRL43A                                                           
         LA    R6,GRPRD6                                                        
         MVC   PPRDPRD,SVCSPRDC+15                                              
         BRAS  RE,PPRD                                                          
LRL43A   DS    0H                                                               
         B     LRL44                                                            
                                                                                
LRL43B   DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM             BUILD PRD LIST                               
         MVI   BSLN,0              CLEAR UNIT LENGTH                            
         LA    R4,SVCSPRDC                                                      
         LA    R0,2                THERE'S ROOM FOR ONLY 2 PRDS TO DISP         
LRL43C   MVC   PPRDPRD,0(R4)                                                    
         BRAS  RE,PPRD                                                          
         LA    R6,2(,R6)                                                        
         CLI   0(R6),C' '                                                       
         BNH   *+8                                                              
         LA    R6,1(,R6)                                                        
         MVI   0(R6),C'/'                                                       
         LA    R6,1(,R6)                                                        
         LA    R4,3(R4)            GET NEXT PRD                                 
         MVC   BSLN,NBLEN          AND UNIT LENGTH                              
         BCT   R0,LRL43C                                                        
         BCTR  R6,0                GET RID OF LAST '/'                          
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    *+14                                                             
         MVC   0(3,R6),=C'*MB'                                                  
         B     *+10                                                             
         MVC   0(3,R6),=C'***'                                                  
         LA    R4,LPROD                                                         
         MVC   0(L'LPRODTB,R4),ELEM                                             
         FIXDT02                                                                
LRL44    GOTO1 DATCON,DMCB,(2,NBACTDAT),(4,LDATE)                               
                                                                                
         CLI   NBACTSUB,C'A'                                                    
         BL    *+10                                                             
         MVC   LSUBL,NBACTSUB                                                   
                                                                                
*GRID                                                                           
         MVC   GRDAY(3),=C'VAR'                                                 
*GRID                                                                           
         CLI   NBDAY,0             BAD DAY                                      
         BNE   *+14                                                             
         MVC   LDAY(3),=C'VAR'                                                  
         B     LRL44C                                                           
                                                                                
         GOTO1 UNDAY,(R1),NBDAY,LDAY                                            
*GRID                                                                           
         MVC   GRDAY,LDAY                                                       
*GRID                                                                           
                                                                                
LRL44C   DS    0H                                                               
         CLI   ACTNUM,29                                                        
         BE    *+12                                                             
         TM    OPTNSW1,OPTROT      SHOW ROTATION DAYS                           
         BZ    LRL45                NO                                          
         L     R6,NBAIO                                                         
         LA    R6,27(,R6)                                                       
         MVI   ELCODE,02                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   LRL45                                                            
         USING NUSDRD,R6                                                        
                                                                                
         TM    NUSDST3,X'02'       SEE IF ADU UNIT (COST NOT ALLOWED)           
         BZ    *+10                                                             
         MVC   GRSTAT,=C'ADU UNIT'                                              
         TM    NBUNITST,X'01'                                                   
         BZ    *+10                                                             
         MVC   GRSTAT,=C'MAKEGOOD'                                              
                                                                                
         CLI   NUSDROT,0             BAD DAY                                    
         BE    LRL45                                                            
                                                                                
         GOTO1 UNDAY,DMCB,NUSDROT,LDAY                                          
                                                                                
LRL45    DS    0H                                                               
         GOTO1 UNTIME,DMCB,NBTIME,LTIME                                         
                                                                                
         MVC   GRTIME,LTIME                                                     
         CLI   ACTNUM,29                                                        
         BE    LRL45A                                                           
                                                                                
         TM    OPTNSW,OPTEP        SHOW EST/PACKAGE                             
         BZ    LRL46                NO                                          
LRL45A   XC    ELEM,ELEM                                                        
         LA    R6,ELEM+1                                                        
         MVC   BYTE,NBACTEST                                                    
         BAS   RE,EDT                                                           
         AR    R6,R0                                                            
         MVI   0(R6),C'/'                                                       
         LA    R6,1(,R6)                                                        
         MVC   BYTE,NBPACK                                                      
         BAS   RE,EDT                                                           
         LA    R1,7                                                             
         LA    RE,LTIME+3                                                       
         LA    RF,ELEM+7                                                        
                                                                                
         CLI   ACTNUM,29                                                        
         BNE   LRL45C                                                           
         LA    RF,ELEM                                                          
         CLI   0(RF),0                                                          
         BH    *+10                                                             
         BCTR  R1,0                                                             
         LA    RF,1(RF)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GRESPK(0),0(RF)                                                  
                                                                                
*        CLI   GRESPK,0                                                         
*        BNE   LRL46                                                            
*        MVI   GRESPK,C'0'                                                      
         B     LRL46                                                            
                                                                                
LRL45C   CLI   0(RF),0                                                          
         BH    *+14                                                             
         BCTR  R1,0                                                             
         LA    RE,1(,RE)                                                        
         BCT   RF,*-14                                                          
         EX    R1,LRLMVC                                                        
                                                                                
LRL46    L     R6,NBAIO                                                         
         XC    SVLEN1(3),SVLEN1    INIT CMLS LENGTH                             
         XC    SVTYPE,SVTYPE       AND CML TYPE                                 
         TM    NURSTAT-NURECD(R6),X'80' THIS UNIT DELETED                       
         BO    LRL76                                                            
                                                                                
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRL74                                                            
         USING NUCMLEL,R6                                                       
*                                                                               
         OC    BPRD3C,BPRD3C       IS THIS A TRIBACK?                           
         BNZ   *+12                 YES                                         
         TM    UNITSW,UNITSWPT     DISPLAY PATTERN REF (& ROT DATE)             
         BZ    LRL47                                                            
         CLI   LISTNUM,12          IS THIS LINE 13 OF 14                        
         BNH   LRL47                                                            
         MVC   LISTAR,SPACES                                                    
*                                                                               
         CLI   ACTNUM,29           ACTION GRID                                  
         BE    LRL47                                                            
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
         DC    H'0'                                                             
                                                                                
LRL47    DS    0H                                                               
*GRID    XC    LDAY+5(3),LDAY+5                                                 
                                                                                
         EDIT  (B1,NUCMLREV),(3,SVREV),0,ZERO=NOBLANK,ALIGN=LEFT                
*GRID    MVC   GRDAY,LDAY                                                       
                                                                                
         TM    NUCMLFLG,X'E0'                                                   
         BZ    LRL47C                                                           
                                                                                
         LA    R5,GRREASS                                                       
         TM    NUCMLFLG,X'80'      LENGTH                                       
         BZ    *+14                                                             
         MVC   0(4,R5),=C'LEN/'                                                 
         LA    R5,3(,R5)                                                        
                                                                                
         TM    NUCMLFLG,X'20'      DATE                                         
         BZ    *+14                                                             
         MVC   1(5,R5),=C'DATE/'                                                
         LA    R5,5(,R5)                                                        
                                                                                
         TM    NUCMLFLG,X'40'      PRODUCT                                      
         BNZ   LRL47C                                                           
         MVI   0(R5),C' '                                                       
         B     LRL47F                                                           
                                                                                
LRL47C   DS    0H                                                               
         LA    R5,PCMML+9                                                       
         LR    R0,R5                                                            
         BRAS  RE,RSDESC                                                        
                                                                                
         OC    GRREASS,GRREASS                                                  
         BNZ   *+10                                                             
                                                                                
         MVC   GRREASS,PCMML+9                                                  
         MVC   GRR2ASS,PCMML+9+132                                              
                                                                                
LRL47F   DS    0H                                                               
         TM    NUCMLFL2,NUCMLFFD   FEED W/O NATIONAL ?                          
         BZ    *+12                                                             
         MVI   SVCMLFL2,X'FF'       YES                                         
         B     LRL74                                                            
                                                                                
         TM    OPTNSW,OPTRV        SHOW REVISION NUMBER                         
         BZ    LRL48                NO                                          
         XC    LDAY+5(3),LDAY+5                                                 
         ZIC   R0,NUCMLREV                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LDAY+5(2),DUB                                                    
*GRID                                                                           
*        MVC   GRDAY,LDAY                                                       
*GRID                                                                           
                                                                                
LRL48    CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   LRL50               NO                                           
         TM    NUCMLFLG,X'08'     TRAFFIC DELETE                                
         BZ    LRL50               YES                                          
         MVC   LPROD(13),=CL13'** MDELETE **'                                   
         MVI   LPROD+3,C'T'                                                     
         B     LRL80                                                            
                                                                                
LRLMVC   MVC   0(0,RE),ELEM                                                     
                                                                                
EDT      EDIT  (B1,BYTE),(3,(R6)),ALIGN=LEFT                                    
         BR    RE                                                               
                                                                                
LRL50    DS   0H                                                                
*NOP     CLI   NUCMLBSL,X'FF'      THIS A VIGNETTE                              
         CLC   =C'VIGNETTE',NUCMLBSN                                            
         BE    LRL52                YES                                         
                                                                                
         TM    NUCMLFLG,X'04'     BILLBOARD REQUIRED                            
         BO    *+12                YES                                          
         CLI   NUCMLBSL,0                                                       
         BE    LRL54                                                            
         MVI   LREASON,C'B'                                                     
         MVC   GRBB,=C'BB'                                                      
                                                                                
         MVC   GRBBCML(8),NUCMLBSN                                              
         TM    NUCMADFL,NUCMADF3    AD-ID CML 1                                 
         BZ    LRL54                                                            
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCMLBSN                                                    
         BRAS  RE,FCMLA                                                         
         MVC   GRBBCML(8),WORK                                                  
                                                                                
         CLC   SVCMLADI,SPACES                                                  
         BNH   *+10                                                             
         MVC   GRBBCML,SVCMLADI                                                 
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
         B     LRL54                                                            
                                                                                
LRL52    DS   0H                                                                
         MVI   LREASON,C'V'        FLAG AS VIGNETTE                             
         MVI   GRBB,C'V'                                                        
         MVC   GRBBCML(8),=C'VIGNETTE'                                          
                                                                                
LRL54    TM    NBUNITST,X'C2'     PREEMPT/MISSED/MINUS                          
         BNZ   LRL74                                                            
         TM    NUCMLFLG,X'E0'      ANY CHANGES-ELEM MUST BE REASSIGNED          
         BNZ   LRL60                                                            
                                                                                
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRL56                                                            
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRL64                                                            
                                                                                
LRL56    DS    0H                                                               
         OC    NUCML1,NUCML1       ANY CML ASSIGNED                             
         BZ    LRL64                                                            
                                                                                
         OC    NUCMLR3F,NUCMLR3F   IS THIS FROM PATTERN                         
         BZ    *+12                 NO                                          
         LA    R0,NUCML1                                                        
         BRAS  RE,CPRD             GO INVERT COMMLS IF NEEDED                   
                                                                                
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
                                                                                
         XC    WORK,WORK                                                        
         MVC   LCML(L'NUCML1),NUCML1                                            
         MVC   WORK(8),NUCML1                                                   
         MVC   WORKPROD,BPRD1C                                                  
         XC    SVTYPE,SVTYPE                                                    
         MVI   DATADISP+1,24                                                    
         BRAS  RE,FCMLA                                                         
         BNE   LRL60                                                            
                                                                                
         MVC   GRCML1,WORK                                                      
         USING CMLRECD,R2                                                       
         L     R2,AIO                                                           
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BZ    LRL58                                                            
         OI    ADIDSTAT,CML1AD                                                  
         XC    GRCML1,GRCML1                                                    
         MVC   GRCML1(8),=C'SEE ADID'                                           
         DROP  R2                                                               
                                                                                
LRL58    DS    0H                                                               
         XC    GRADID1,GRADID1                                                  
         OC    GRCML1,SPACES                                                    
         CLC   GRCML1,SVCMLADI                                                  
         BE    *+10                                                             
         MVC   GRADID1,SVCMLADI                                                 
                                                                                
         MVC   GRCM1T1,SVTITL1                                                  
         MVC   GRCM1T2,SVTITL2                                                  
         MVC   GRCM1T3,SVTITL3                                                  
                                                                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   LCML(L'SVCMLADI),SVCMLADI                                        
         XC    SVCMLAD2,SVCMLAD2                                                
                                                                                
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BNZ   LRL60                                                            
                                                                                
         OC    NUCML1,NUCML1                                                    
         BZ    LRL64                                                            
                                                                                
         CLC   NBPR2CL3,SPACES     P/B                                          
         BH    LRL64               YES                                          
                                                                                
         XC    WORK+8(16),WORK+8   CLEAR REST OF WORK AREA                      
         MVC   WORK(8),NUCML1                                                   
                                                                                
         BAS   RE,CHKLEN                                                        
         BE    LRL64                                                            
                                                                                
         MVC   GRREASS,SPACES                                                   
         MVC   GRREASS(3),=C'LEN'                                               
                                                                                
LRL60    DS   0H                                                                
         OC    NUCML1(16),NUCML1   ANY CML ASSIGNED                             
         BZ    LRL64                                                            
         MVC   GRCMLO(L'NUCML1),NUCML1                                          
         XC    GRADID1,GRADID1                                                  
         MVC   GRADID1(8),=C'REASSIGN'                                          
         MVC   GRCML1(8),=C'REASSIGN'                                           
         TM    NUCMADFL,NUCMADF1    AD-ID CML 1                                 
         BZ    LRL60B                                                           
*        OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML1                                                      
         MVI   DATADISP+1,24                                                    
         BRAS  RE,FCMLA                                                         
         MVC   GRCMLO,WORK                                                      
         CLC   SVCMLADI,SPACES                                                  
         BNH   *+10                                                             
         MVC   GRCMLO,SVCMLADI                                                  
         NI    SVFLAG,X'FF'-GETISCII                                            
LRL60B   DS    0H                                                               
         MVC   LCML,SPACES                                                      
         MVC   LCML(8),=C'REASSIGN'                                             
                                                                                
LRL64    TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    *+10                                                             
         MVC   LREASON-1(2),=C'CS'                                              
         OC    NUCML2,NUCML2       ANY CML ASSIGNED                             
         BZ    LRL76                                                            
                                                                                
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRL65                                                            
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRL76                                                            
                                                                                
LRL65    DS    0H                                                               
         TM    NUCMLFLG,X'E0'      ANY CHANGES-ELEM MUST BE REASSIGNED          
         BNZ   LRL70                                                            
         CLC   NUCML1,NUCML2       IF SAME, PIGGYBACK                           
         BNE   LRL66                                                            
         MVC   SVCMLAD2+3(3),=C'P/B'                                            
         OC    BPRD3C,BPRD3C       IS THIS A TRI-BACK                           
         BNZ   LRL66               YES                                          
                                                                                
         OC    NUCML1,NUCML1                                                    
         BZ    LRL76                                                            
                                                                                
         XC    WORK+8(16),WORK+8   CLEAR REST OF WORK AREA                      
         MVC   WORK(8),NUCML1                                                   
                                                                                
         BAS   RE,CHKLEN                                                        
         BE    LRL76                YES                                         
         B     LRL70                                                            
                                                                                
LRL66    DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   GRCML2(L'NUCML2),NUCML2                                          
         MVC   SVCMLAD2(L'NUCML2),NUCML2                                        
         MVC   WORK(L'NUCML2),NUCML2                                            
         MVC   WORKPROD,BPRD2C                                                  
         BRAS  RE,FCMLA                                                         
         BNE   LRL70                                                            
         MVC   GRADID2,SVCMLADI                                                 
         CLC   SVCMLADI,SPACES                                                  
         BNH   *+10                                                             
                                                                                
         MVC   SVCMLAD2,SVCMLADI                                                
         MVC   GRCML2,WORK                                                      
         USING CMLRECD,R2                                                       
         L     R2,AIO                                                           
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BZ    LRL68                                                            
         OI    ADIDSTAT,CML2AD                                                  
         XC    GRCML2,GRCML2                                                    
         MVC   GRCML2(8),=C'SEE ADID'                                           
         DROP  R2                                                               
                                                                                
LRL68    TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BNZ   LRL70                                                            
                                                                                
         OC    BPRD3C,BPRD3C       T/B                                          
         BNZ   LRL76               YES                                          
                                                                                
         OC    NUCML1,NUCML1                                                    
         BZ    LRL76                                                            
         OC    NUCML2,NUCML2                                                    
         BZ    LRL76                                                            
                                                                                
         XC    WORK+16(8),WORK+16  CLEAR REST OF WORK AREA                      
         MVC   WORK(16),NUCML1                                                  
                                                                                
         BAS   RE,CHKLEN                                                        
         BE    LRL76                YES                                         
                                                                                
LRL70    DS    0H                                                               
         MVC   GRCMLO(L'NUCML1),NUCML1                                          
         XC    GRADID1,GRADID1                                                  
         MVC   GRADID1(8),=C'REASSIGN'                                          
         MVC   GRCML1(8),=C'REASSIGN'                                           
         TM    NUCMADFL,NUCMADF1    AD-ID CML 1                                 
         BZ    LRL70B                                                           
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK(L'NUCML1),NUCML1                                            
         BRAS  RE,FCMLA                                                         
         MVC   GRCMLO,WORK                                                      
         CLC   SVCMLADI,SPACES                                                  
         BNH   *+10                                                             
         MVC   GRCMLO,SVCMLADI                                                  
         NI    SVFLAG,X'FF'-GETISCII                                            
LRL70B   DS    0H                                                               
         MVC   LCML,SPACES                                                      
         MVC   LCML(8),=C'REASSIGN'                                             
                                                                                
LRL71    OC    NUCML2,NUCML2       IF NO CML                                    
         BZ    LRL76               NO NEED TO REASSIGN                          
         MVC   GRCMLO2(L'NUCML2),NUCML2                                         
         MVC   GRADID2(8),=C'REASSIGN'                                          
         MVC   GRCML2(8),=C'REASSIGN'                                           
         TM    NUCMADFL,NUCMADF2    AD-ID CML 1                                 
         BZ    LRL71B                                                           
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK(L'NUCML2),NUCML2                                            
         BRAS  RE,FCMLA                                                         
         MVC   GRCMLO2,WORK                                                     
         CLC   SVCMLADI,SPACES                                                  
         BNH   *+10                                                             
         MVC   GRCMLO2,SVCMLADI                                                 
         NI    SVFLAG,X'FF'-GETISCII                                            
LRL71B   DS    0H                                                               
         MVC   SVCMLAD2,SPACES                                                  
         MVC   SVCMLAD2(8),=C'REASSIGN'                                         
         B     LRL76                                                            
                                                                                
LRL74    TM    NBUNITST,X'C2'     PREEMPT/MISSED/MINUS/MAKEGOOD                 
         BZ    LRL75                                                            
         MVC   LCML,SPACES                                                      
         MVC   LCML(8),=CL8'PREEMPT'                                            
         MVC   GRSTAT,LCML                                                      
         TM    NBUNITST,X'40'                                                   
         BO    LRL75                                                            
         MVC   LCML(8),=CL8'MISSED'                                             
         MVC   GRSTAT,LCML                                                      
         TM    NBUNITST,X'02'                                                   
         BO    LRL75                                                            
         MVC   LCML(8),=CL8'MINUS'                                              
         MVC   GRSTAT,LCML                                                      
         TM    NBUNITST,X'80'                                                   
         BO    LRL75                                                            
                                                                                
LRL75    CLI   ACTNUM,29                                                        
         BNE   LRL76                                                            
         MVC   GRCMLO(L'NUCML1),NUCML1                                          
         TM    NUCMADFL,NUCMADF1    AD-ID CML 1                                 
         BZ    LRL75B                                                           
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK(L'NUCML1),NUCML1                                            
         BRAS  RE,FCMLA                                                         
         MVC   GRCMLO,WORK                                                      
         CLC   SVCMLADI,SPACES                                                  
         BNH   *+10                                                             
         MVC   GRCMLO,SVCMLADI                                                  
         NI    SVFLAG,X'FF'-GETISCII                                            
LRL75B   DS    0H                                                               
                                                                                
LRL76    DS   0H                                                                
*        DONE WITH REASSIGNS - CLEAR DESC IF NOT REASSIGN                       
         CLC   LCML(8),=C'REASSIGN'                                             
         BE    *+10                                                             
         XC    GRREASS,GRREASS                                                  
         CLC   SVCMLAD2(8),=C'REASSIGN'                                         
         BE    *+10                                                             
         XC    GRR2ASS,GRR2ASS                                                  
                                                                                
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         DROP  R6                                                               
         MVI   ELCODE,X'22'       FIND ANY FEED                                 
         L     R6,NBAIO                                                         
         BRAS  RE,GETEL                                                         
         BNE   LRL80                                                            
         MVC   LREASON,2(R6)                                                    
                                                                                
         MVC   SVFEED,2(R6)                                                     
         CLI   ACTNUM,29                                                        
         BNE   *+10                                                             
         MVC   LFEED,SVFEED                                                     
                                                                                
LRL80    TM    OPTNSW,OPTDSKAD     SHOW DISKADDR                                
         BZ    LRL84                NO                                          
         GOTO1 HEXOUT,DMCB,NBKEY+21,LPROD2,4                                    
                                                                                
LRL84    DS    0H                                                               
         TM    OPTNSW1,OPTRESG                                                  
         BZ    LRL84C                                                           
         CLC   LCML(8),=CL8'REASSIGN'                                           
         BE    LRL84C                                                           
         CLC   SVCMLAD2(8),=CL8'REASSIGN'                                       
         BNE   LRL10                                                            
                                                                                
LRL84C   DS    0H                                                               
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   LRL84T                                                           
                                                                                
         L     R6,NBAIO                                                         
         USING NUFDCEL,R6                                                       
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRL84X                                                           
         MVC   GFEED2,NUFDCFED                                                  
         TM    NUFDADFL,NUFDADF1    AD-ID CML 1                                 
         BZ    *+8                                                              
         OI    GRCML2ST,NUFDADF1                                                
         MVC   GRCML2,NUFDCML1                                                  
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   LRL84M                                                           
         MVC   GFEED3,NUFDCFED                                                  
         TM    NUFDADFL,NUFDADF1    AD-ID CML 1                                 
         BZ    *+8                                                              
         OI    GRCML3ST,NUFDADF1                                                
         MVC   GRCML3,NUFDCML1                                                  
                                                                                
LRL84E   BRAS  RE,NEXTEL                                                        
         BNE   LRL84M                                                           
         MVC   GFEED4,NUFDCFED                                                  
         TM    NUFDADFL,NUFDADF1    AD-ID CML 1                                 
         BZ    *+8                                                              
         OI    GRCML4ST,NUFDADF1                                                
         MVC   GRCML4,NUFDCML1                                                  
                                                                                
LRL84G   BRAS  RE,NEXTEL                                                        
         BNE   LRL84M                                                           
         MVC   GFEED5,NUFDCFED                                                  
         TM    NUFDADFL,NUFDADF1    AD-ID CML 1                                 
         BZ    *+8                                                              
         OI    GRCML5ST,NUFDADF1                                                
         MVC   GRCML5,NUFDCML1                                                  
                                                                                
LRL84J   BRAS  RE,NEXTEL                                                        
         BNE   LRL84M                                                           
         MVC   GFEED6,NUFDCFED                                                  
         TM    NUFDADFL,NUFDADF1    AD-ID CML 1                                 
         BZ    *+8                                                              
         OI    GRCML6ST,NUFDADF1                                                
         MVC   GRCML6,NUFDCML1                                                  
                                                                                
                                                                                
LRL84M   DS    0H                                                               
         LA    R5,5                CONVERT ANY COMPRESSED CMML'S                
         LA    R4,GRADID2          FILL IN ADID FIELDS                          
         LA    R2,GRCML            TO TEXT FORMAT                               
LRL84M3  DS    0H                                                               
         OC    0(GRCMLLEN,R2),0(R2)                                             
         BZ    LRL84N                                                           
         TM    0(R2),NUFDADF1      AD-ID CML 1                                  
         BZ    LRL84M5                                                          
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK(8),1(R2)                                                    
         BRAS  RE,FCMLA                                                         
         MVC   1(L'GRCML2,R2),WORK                                              
                                                                                
         USING CMLRECD,R6                                                       
         L     R6,AIO                                                           
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BZ    LRL84M4                                                          
         OI    ADIDSTAT,CML1AD                                                  
         XC    1(L'GRCML2,R2),1(R2)                                             
         MVC   1(8,R2),=C'SEE ADID'                                             
                                                                                
LRL84M4  DS    0H                                                               
         MVI   DATADISP+1,24                                                    
         USING CMLADIEL,R6                                                      
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   0(L'GRADID2,R4),CMLADID                                          
         MVI   DATADISP+1,27                                                    
         DROP  R6                                                               
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
LRL84M5  LA    R2,GRCMLLEN(R2)                                                  
         LA    R4,L'GRADID2(R4)                                                 
         BCT   R5,LRL84M3                                                       
                                                                                
LRL84N   DS    0H                                                               
         OC    GRCML2,GRCML2                                                    
         BZ    LRL84P                                                           
         MVC   WORK(8),GRCML2                                                   
         MVC   WORKPROD,LPROD2                                                  
         XC    SVTYPE,SVTYPE                                                    
         BRAS  RE,FCMLA                                                         
         BNE   LRL84NA                                                          
         MVC   GRADID2,SVCMLADI                                                 
                                                                                
         CLC   SVCMLADI,SPACES                                                  
         BNH   *+10                                                             
         MVC   SVCMLAD2,SVCMLADI                                                
                                                                                
LRL84NA  MVC   GRCM2T1,SVTITL1                                                  
         MVC   GRCM2T2,SVTITL2                                                  
         MVC   GRCM2T3,SVTITL3                                                  
                                                                                
         OC    GRCML3,GRCML3                                                    
         BZ    LRL84P                                                           
         MVC   WORK(8),GRCML3                                                   
         MVC   WORKPROD,GRPRD3                                                  
         XC    SVTYPE,SVTYPE                                                    
         BRAS  RE,FCMLA                                                         
         MVC   GRCM3T1,SVTITL1                                                  
         MVC   GRCM3T2,SVTITL2                                                  
         MVC   GRCM3T3,SVTITL3                                                  
                                                                                
         OC    GRCML4,GRCML4                                                    
         BZ    LRL84P                                                           
         MVC   WORK(8),GRCML4                                                   
         MVC   WORKPROD,GRPRD4                                                  
         XC    SVTYPE,SVTYPE                                                    
         BRAS  RE,FCMLA                                                         
         MVC   GRCM4T1,SVTITL1                                                  
         MVC   GRCM4T2,SVTITL2                                                  
         MVC   GRCM4T3,SVTITL3                                                  
                                                                                
         OC    GRCML5,GRCML5                                                    
         BZ    LRL84P                                                           
         MVC   WORK(8),GRCML5                                                   
         MVC   WORKPROD,GRPRD5                                                  
         XC    SVTYPE,SVTYPE                                                    
         BRAS  RE,FCMLA                                                         
         MVC   GRCM5T1,SVTITL1                                                  
         MVC   GRCM5T2,SVTITL2                                                  
         MVC   GRCM5T3,SVTITL3                                                  
                                                                                
         OC    GRCML6,GRCML6                                                    
         BZ    LRL84P                                                           
         MVC   WORK(8),GRCML6                                                   
         MVC   WORKPROD,GRPRD6                                                  
         XC    SVTYPE,SVTYPE                                                    
         BRAS  RE,FCMLA                                                         
         MVC   GRCM6T1,SVTITL1                                                  
         MVC   GRCM6T2,SVTITL2                                                  
         MVC   GRCM6T3,SVTITL3                                                  
                                                                                
LRL84P   DS    0H                                                               
         OC    OPTADID,OPTADID                                                  
         BZ    LRL84T                                                           
         CLC   GRADID1,OPTADID                                                  
         BE    LRL84T                                                           
         CLC   GRADID2,OPTADID                                                  
         BE    LRL84T                                                           
         CLC   GRADID3,OPTADID                                                  
         BE    LRL84T                                                           
         CLC   GRADID4,OPTADID                                                  
         BE    LRL84T                                                           
         CLC   GRADID5,OPTADID                                                  
         BE    LRL84T                                                           
         CLC   GRADID6,OPTADID                                                  
         BE    LRL84T                                                           
         XC    GRDFLDS1(GRFLDLN1),GRDFLDS1                                      
         XC    GRDFLDS2(GRFLDLN2),GRDFLDS2                                      
         XC    GRDFLDS3(GRFLDLN3),GRDFLDS3                                      
         XC    GRDFLDS4(GRFLDLN4),GRDFLDS4                                      
         B     LRL10                                                            
                                                                                
LRL84T   DS    0H                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRL84X                                                           
                                                                                
         CLI   SVCMLFL2,X'FF'       FEED W/O NATIONAL                           
         BE    *+14                                                             
         MVC   LFEED-1(4),=C'*NAT'  IT'S A NATIONAL UNIT                        
         B     LRL84X                                                           
                                                                                
         USING NUFDCEL,R6                                                       
                                                                                
         MVC   LFEED,NUFDCFED      SHOW FEED AND                                
         OC    NUFDCML1,NUFDCML1                                                
         BZ    *+10                                                             
         MVC   LCML(L'NUFDCML1),NUFDCML1       COMMERCIAL                       
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   LRL84X                                                           
         MVI   LFEED-1,C'*'        MORE THAN ONE FEED EXISTS                    
                                                                                
         DROP  R6                                                               
                                                                                
LRL84X   L     R6,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
LRL85    BRAS  RE,NEXTEL                                                        
         BNE   LRL85X                                                           
         CLI   2(R6),C'Q'          IS THIS A TAG                                
         BNE   LRLTAGX                                                          
         CLI   3(R6),C'T'          T=TAG                                        
         BNE   LRLTAGX                                                          
         MVC   LREASON-1(2),3(R6)  MOVE TO SCREEN TN (N=1-9)                    
         B     LRL85                                                            
                                                                                
LRLTAGX  CLI   2(R6),C'F'          THIS A BUY TYPE CODE                         
         BE    LRL85K               YES                                         
         CLI   2(R6),C'H'          TRAFFIC SUPPLIER?                            
         BNE   LRL85                                                            
                                                                                
         CLC   TRATSP,SPACES       ANY TSUPPS YET?                              
         BNH   LRL85A               NO                                          
         XC    BLOCK(200),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(30,TRATSPH),(6,BLOCK+64)                           
                                                                                
         CLI   DMCB+4,0            ANY TSUPPS                                   
         BNE   LRL85B              YES                                          
                                                                                
LRL85A   ZIC   R1,1(R6)            ELEM LENGTH                                  
         AHI   R1,-4               MINUS OVERHEAD AND 1 FOR EX                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRATSP(0),3(R6)     MOVE TSUPP TO SCREEN                         
         LA    RE,TRATSP                                                        
         AR    RE,R1                                                            
         LA    R1,1(R1)            ACTUAL LENGTH OF TSUPP                       
         STC   R1,TRATSPH+5        SAVE TSUPP LENGTH                            
         B     LRL85I                                                           
                                                                                
LRL85B   ZIC   R1,DMCB+4           GET NUMBER OF BLOCKS                         
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
         ZIC   R5,1(R6)            GET LEN FROM ELEM (MAX 5)                    
         AHI   R5,-3               MINUS OVERHEAD                               
         LA    RF,TRATSP                                                        
                                                                                
LRL85C   ZIC   RE,0(R4)            GET LENGTH OF ENTRY                          
         CR    RE,R5               SAME LENGTH?                                 
         BNE   LRL85F                                                           
                                                                                
         BCTR  R5,0                ADJUST LENGTH FOR EX-CLC                     
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),3(R6)      SAME TRAFFIC SUPPLIER                        
         BE    LRL85                YES, GET NEXT ELEM                          
         AHI   R5,1                ADJUST LENGTH                                
                                                                                
LRL85F   LA    R4,52(R4)            GET NEXT ENTRY IN BLOCK                     
         BCT   R1,LRL85C                                                        
                                                                                
LRL85G   XC    BLOCK(60),BLOCK                                                  
         LA    RF,BLOCK            SAVE LIST IN BLOCK                           
         LA    R1,TRATSP           TSUPP FIELD                                  
         LA    R4,BLOCK+64                                                      
         ZIC   RE,0(R4)            GET LENGTH OF 1ST ENTRY                      
         LA    RE,1(RE)            PLUS 1 FOR COMMA                             
                                                                                
         ZIC   R4,TRATSPH+5        GET LENGTH                                   
                                                                                
         CLI   DMCB+4,5            5 TSUPPS?                                    
         BL    *+12                NO, JUST ADD THIS ONE                        
         SR    R4,RE               FIELD LEN MINUS LEN OF 1ST TSUPP             
         STC   R4,TRATSPH+5        SAVE TSUPP FIELD LENGTH                      
         AR    R1,RE               PT TO 2ND ENTRY                              
                                                                                
         BCTR  R4,0                ADJ FOR EX-MOVE                              
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R1)       MOVE TSUPPS OVER TO BLOCK                    
                                                                                
         ZIC   RE,TRATSPH+5        GET TSUPP LIST LENGTH                        
         AR    RF,RE               BUMP TO THE END OF LIST                      
         MVI   0(RF),C','          ADD COMMA                                    
         ZIC   R5,1(R6)            GET LEN FROM ELEM (MAX 5)                    
         AHI   R5,-4               MINUS OVERHEAD                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),3(R6)       MOVE TSUPP TO THE END OF THE LIST            
         LA    R5,2(R5)            TRUE TSUPP LENGTH PLUS ONE FOR COMMA         
         AR    RE,R5               LIST LENGTH PLUS THIS TSUPP LEN              
         STC   RE,TRATSPH+5        AND SAVE IT IN HEADER                        
                                                                                
         XC    TRATSP,TRATSP                                                    
         MVC   TRATSP,BLOCK        MOVE LIST TO THE SCREEN                      
                                                                                
LRL85I   NI    TRATSUPH+1,X'FF'-X'0C' CHANGE FLDS TO NORMAL INTENSITY           
         NI    TRATSPH+1,X'FF'-X'0C'                                            
         OI    TRATSUPH+6,X'80'    TRANSMIT                                     
         OI    TRATSPH+6,X'80'                                                  
                                                                                
LRL85K   CLI   3(R6),C'O'          OPPORTUNISTIC                                
         BNE   *+8                                                              
         MVI   LFEED-1,C'O'                                                     
                                                                                
         CLI   3(R6),C'S'          SCATTER                                      
         BNE   *+8                                                              
         MVI   LFEED-1,C'S'                                                     
                                                                                
         CLI   3(R6),C'U'          UPFRONT                                      
         BNE   LRL85                                                            
         MVI   LFEED-1,C'U'                                                     
                                                                                
LRL85X   DS    0H                                                               
         MVC   GFEED1,LFEED-1                                                   
         MVC   GRPRD1,LPROD                                                     
         MVC   GRPRD2,LPROD2                                                    
         OC    TOTUNITS,TOTUNITS   FIRST TIME THROUGH                           
         BNZ   LRL87                                                            
         BRAS  RE,TBLKEY           YES, TABLE THIS KEY                          
                                                                                
LRL87    LH    R1,TOTUNITS                                                      
         LA    R1,1(,R1)                                                        
         STH   R1,TOTUNITS                                                      
         MVC   KEY,NBKEY                                                        
                                                                                
         BAS   RE,SVTWA                                                         
                                                                                
         TM    SVFLAG,SVKEYSW      SAVE THIS KEY ?                              
         BZ    LRL87B                                                           
                                                                                
         NI    SVFLAG,X'FF'-SVKEYSW                                             
         BRAS  RE,TBLKEY            YES, TABLE IT                               
                                                                                
LRL87B   L     R6,NBAIO                                                         
                                                                                
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LRL90                                                            
         USING NUCMLEL,R6                                                       
                                                                                
* IF EITHER TRI-BACK PRODUCTS OR SHOW PATTERN INFO, SECOND LINE *               
                                                                                
         CLI   ACTNUM,29           ACTION GRID                                  
         BE    LRL87Q                                                           
                                                                                
         OC    BPRD3C,BPRD3C       IS THIS A TRIBACK?                           
         BNZ   LRL87C               YES                                         
         CLI   SVCMLAD2,C' '                                                    
         BH    LRL87C                                                           
         TM    UNITSW,UNITSWPT     DISPLAY PATTERN REF (& ROT DATE)             
         BZ    LRL90                NO                                          
         B     LRL87Q                                                           
                                                                                
LRL87C   DS    0H                                                               
         CLI   LISTNUM,12                                                       
         BNE   LRL87E                                                           
         GOTO1 LISTMON             PRINT FIRST LINE                             
         MVC   LISTAR,SPACES                                                    
         MVC   LCML+2(L'SVCMLAD2),SVCMLAD2    PRINT SECOND LINE                 
         GOTO1 LISTMON                                                          
         GOTO1 LISTMON                                                          
         B     LRL87Q                                                           
                                                                                
LRL87E   CLI   LISTNUM,13                                                       
         BNE   LRL87H                                                           
         OI    ADIDSW,SKIPNTIO                                                  
         OI    ADIDSW,PRNTBOTH                                                  
*        OI    ADIDSW,PRNTSCND                                                  
         MVC   SVLISTAR,LISTAR     SAVE FOR NEXT SCREEN                         
         MVC   SVADIDLS,SVCMLAD2   SAVE FOR NEXT SCREEN                         
         MVC   LISTAR,SPACES                                                    
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
*        GOTO1 LISTMON             FORCE TO NEW SCREEN                          
         B     LRL87Q                                                           
                                                                                
LRL87H   DS    0H                                                               
         CLI   SVCMLAD2,C' '                                                    
         BH    LRL87M                                                           
         GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
         B     LRL87Q                                                           
                                                                                
LRL87M   DS    0H                                                               
LRL87N   GOTO1 LISTMON                                                          
         MVC   LISTAR,SPACES                                                    
         MVC   LCML+2(L'SVCMLAD2),SVCMLAD2                                      
         GOTO1 LISTMON                                                          
         XC    SVCMLAD2,SVCMLAD2                                                
         MVC   LCML,SPACES                                                      
         MVC   LISTAR,SPACES                                                    
LRL87Q   DS    0H                                                               
         BAS   RE,SVTWA                                                         
*                                                                               
         OC    NUCMLR3F,NUCMLR3F   IS THERE A PATTERN APPLIED                   
         BZ    LRL87R               NO                                          
         MVC   FLDH(3),NUCMLR3F                                                 
         XC    FLDH(3),=X'FFFFFF'                                               
         SR    R0,R0                                                            
         ICM   R0,7,FLDH                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  GRPTREF,DUB                                                      
                                                                                
LRL87R   TM    UNITSW,UNITSWPT     DISPLAY PATTERN REF (& ROT DATE)             
         BZ    LRL89                NO                                          
         MVC   LDATE(20),SPACES                                                 
                                                                                
         OC    NUCMLR3F,NUCMLR3F   IS THERE A PATTERN APPLIED                   
         BZ    LRL87X               NO                                          
                                                                                
         MVC   LDATE+7(2),=C'#='                                                
         EDIT  NUCMLR3F,(5,LDATE+9),ZERO=NOBLANK,ALIGN=LEFT                     
                                                                                
         LA    R2,LDATE+15                                                      
         LR    R0,R2                                                            
                                                                                
         TM    NUCMLKEY,X'80'      PROG SPECIFIC                                
         BZ    *+14                                                             
         MVC   0(4,R2),=C'PRG'                                                  
         LA    R2,3(,R2)                                                        
                                                                                
         TM    NUCMLKEY,X'40'      FEED SPECIFIC                                
         BZ    LRL87R2                                                          
         CR    R0,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C'/'                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(4,R2),=C'FEED'                                                 
         LA    R2,4(,R2)                                                        
                                                                                
LRL87R2  TM    NUCMLKEY,X'20'      DAYPART SPECIFIC                             
         BZ    LRL87R4                                                          
         CR    R0,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C'/'                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(3,R2),=C'DPT'                                                  
         LA    R2,3(,R2)                                                        
                                                                                
LRL87R4  TM    NUCMLKEY,X'10'      ALL NETWORKS                                 
         BZ    LRL87R6                                                          
         CR    R0,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C'/'                                                       
         LA    R2,1(,R2)                                                        
                                                                                
         MVC   0(7,R2),=C'ALL NET'                                              
         LA    R2,7(,R2)                                                        
                                                                                
LRL87R6  TM    NUCMLKEY,X'08'      MEDIA SPECIFIC                               
         BZ    LRL87X                                                           
         CR    R0,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C'/'                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(5,R2),=C'MEDIA'                                                
         LA    R2,5(,R2)                                                        
                                                                                
LRL87X   DS    0H                                                               
         LR    R4,R6                                                            
         MVI   SVPRGDAY,0                                                       
                                                                                
         MVC   SVPRGDAY,NBSDROT                                                 
                                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         USING NPGEL92,R6                                                       
                                                                                
         MVC   SVPRGDAY,NPGROT                                                  
                                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'E3'        TRAFFIC OVERRIDE ELEM                        
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         USING NPGELE3,R6                                                       
         MVC   SVPRGDAY,NPGTDAY                                                 
                                                                                
         CLI   SVPRGDAY,0                                                       
         BE    *+8                                                              
         BRAS  RE,GEDT             GO GET END DATE                              
                                                                                
         LR    R6,R4                                                            
                                                                                
LRL89    DS    0H                                                               
         CLC   LISTAR,SPACES       ANYTHING TO SHOW                             
         BE    LRL10                NO                                          
                                                                                
LRL90    DS    0H                                                               
         BRAS  RE,INITSPT          SET FROM NET TO SPOT                         
*                                                                               
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   LRL92                                                            
*                                                                               
         OC    NBSDROT,NBSDROT                                                  
         BZ    LRL91                                                            
         GOTO1 UNDAY,DMCB,NBSDROT,GROT                                          
LRL91    DS    0H                                                               
                                                                                
         MVC   GRREV,SVREV                                                      
                                                                                
         GOTO1 =V(GRID),DMCB,AGFT,ATWA,ASPOOLD,ACOMFACS,               +        
               RR=SPTR2CRR                                                      
         BE    LRL10                                                            
*                                                                               
         OI    SVFLAG,SVKEYSW+GRIDCTU                                           
         BAS   RE,SVTWA                                                         
         B     LRLX                                                             
*                                                                               
LRL92    GOTO1 LISTMON                                                          
         BE    LRL10               NOT END OF SCREEN                            
                                                                                
         OI    SVFLAG,SVKEYSW      SAVE NEXT KEY                                
         BAS   RE,SVTWA                                                         
         GOTO1 LISTMON             DUMMY CALL                                   
* TEMP                                                                          
         DC    H'0'                                                             
         B     LRLX                SHOULD NEVER GET HERE                        
                                                                                
* CHECK LENGTHS (TOTAL CML LEN = TOTAL UNIT LEN)                                
                                                                                
         USING NUCMLEL,R6                                                       
CHKLEN   NTR1                                                                   
         MVI   JUSTLEN,C'Y'                                                     
         BRAS  RE,FCMLA                                                         
         MVI   JUSTLEN,C'N'                                                     
                                                                                
         ZIC   R1,SVLEN3                                                        
         ZIC   RE,SVLEN1           RE=TOTAL COMMERCIAL LENGTHS                  
         ZIC   R0,SVLEN2                                                        
         CLC   WORK(8),WORK+8      IF COMS ARE THE SAME                         
         BNE   *+6                                                              
         SR    R0,R0               CONSIDER ONLY ONE LENGTH                     
         CLC   WORK(8),WORK+16                                                  
         BNE   *+6                                                              
         SR    R1,R1                                                            
         CLC   WORK+8(8),WORK+16                                                
         BNE   *+6                                                              
         SR    R0,R0                                                            
                                                                                
         CLI   SVLEN2,X'FF'        COMML GOOD FOR ALL LENGTHS                   
         BE    CHKLEN14                                                         
         CLI   SVLEN2,0            OR NONE                                      
         BNE   CHKLEN18                                                         
CHKLEN14 DS    0H                                                               
         CLI   SVLEN3,X'FF'        COMML GOOD FOR ALL LENGTHS                   
         BE    CHKLEN16                                                         
         CLI   SVLEN3,0            OR NONE                                      
         BNE   CHKLEN18                                                         
CHKLEN16 DS    0H                                                               
         CLI   SVLEN1,X'FF'        COMML GOOD FOR ALL LENGTHS                   
         BE    CHKLENX                                                          
CHKLEN18 DS    0H                                                               
         AR    RE,R0                                                            
         AR    RE,R1               TOTAL LENGTH                                 
                                                                                
         CLC   =C'VIGNETTE',NUCMLBSN                                            
         BNE   CHKLEN80                                                         
         ZIC   RF,NUCMLBSL         VIGNETTE LEN                                 
         AR    RE,RF                                                            
                                                                                
CHKLEN80 ZIC   R1,NBLEN            UNIT LENGTH                                  
                                                                                
         CR    R1,RE                                                            
                                                                                
CHKLENX  DS    0H                                                               
         DROP  R6                                                               
         XIT1                                                                   
                                                                                
* END OF LIST, CLEAN UP *                                                       
                                                                                
LRL98    CLI   ACTNUM,29           ACTION GRID                                  
         BNE   LRL99                                                            
         OC    TOTUNITS,TOTUNITS                                                
         BNZ   *+12                                                             
         OI    SVFLAG,GRIDNOUN                                                  
         B     LRL99                                                            
         BRAS  RE,GRIDEND                                                       
                                                                                
         OI    GENSTAT7,GES7BACK                                                
                                                                                
LRL99    DS    0H                                                               
         TM    ADIDSW,PRNTBOTH                                                  
         BZ    LRL99E                                                           
         BAS   RE,SVTWA                                                         
         MVC   LISTAR,SPACES                                                    
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
         GOTO1 LISTMON             FORCE TO NEW SCREEN                          
                                                                                
LRL99E   DS    0H                                                               
         XC    TOTUNITS,TOTUNITS                                                
         NI    TRACLTH+4,X'FF'-X'20'                                            
LRL99H   BAS   RE,SVTWA                                                         
                                                                                
LRLX     DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    LRLSTXZ                                                          
         LA    R1,=C'DMWRT '                                                    
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)                                                 
         MVI   DMCB+8,4                                                         
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(ESTBLEN)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,AESTBL,,(RF)                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
LRLSTXZ  DS    0H                                                               
         XIT1                                                                   
                                                                                
TRAPERR1 GOTO1 ERREX                                                            
         EJECT                                                                  
*NOP     LTORG                                                                  
                                                                                
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                            
                                                                                
SVTWA    LA    R1,=C'DMWRT'                                                     
         B     TWA                                                              
                                                                                
* RESTORE TWA *                                                                 
                                                                                
RDTWA    LA    R1,=C'DMREAD'                                                    
TWA      NTR1                                                                   
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(SVSTREQ)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVSTOR                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
                                                                                
         LTORG                                                                  
*                                                                               
*----------------------------------------------------------------               
* ADJUST IF NEEDED PROD LENGTH TO COMMERCIAL LENGTH FOR P/B PAIRS               
* ON ENTRY R0 = TOTAL, LEN R1 = PRD LEN,  R2 = PRD2 LEN                         
*----------------------------------------------------------------               
*                                                                               
FIXLEN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         STC   R1,SVLEN1                                                        
         STC   R2,SVLEN2                                                        
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FIXLENX                                                          
         USING NUCMLEL,R6                                                       
*                                                                               
         OC    NUCML1(16),NUCML1                                                
         BZ    FIXLENX                                                          
*                                                                               
         CLC   NUCML1,NUCML2       ONE CML FOR BOTH PRDS                        
         BE    FIXLENX              DONE                                        
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(16),NUCML1     CML1/CML2                                    
*                                                                               
         MVI   JUSTLEN,C'Y'                                                     
         BRAS  RE,FCMLA             GET CML LEN                                 
         MVI   JUSTLEN,C'N'                                                     
*                                                                               
         CLI   SVLEN1,0                                                         
         BE    *+10                                                             
         MVC   BSLN,SVLEN1                                                      
*                                                                               
         CLI   SVLEN2,0                                                         
         BE    FIXLENX                                                          
*                                                                               
         LLC   R1,SVLEN2                                                        
         OC    NUCML1,NUCML1                                                    
         BNZ   *+14                                                             
         SR    R0,R1               TOTAL LEN - LEN2                             
         STC   R0,BSLN             = LEN1                                       
         STC   R0,SVLEN1                                                        
*                                                                               
         LLC   R1,SVLEN1                                                        
         OC    NUCML2,NUCML2                                                    
         BNZ   *+10                                                             
         SR    R0,R1               TOTAL LEN - LEN1                             
         STC   R0,SVLEN2           = LEN2                                       
*                                                                               
         LLC   R2,SVLEN2                                                        
         XIT1  REGS=(R1,R2)                                                     
*                                                                               
FIXLENX  J     EXIT                                                             
*                                                                               
         EJECT                                                                  
*-----------------------------------------------------------------*             
* FIND END OF ROTATION DATE                                                     
*-----------------------------------------------------------------*             
GEDT     NTR1  BASE=*                                                           
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK)                                
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
         ZIC   RF,0(R1)            GET DAY OF WEEK                              
                                                                                
         XC    WORK,WORK                                                        
         ZIC   R0,SVPRGDAY                                                      
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
                                                                                
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         SLL   R0,1                ONLY COUNT DAYS AFTER NBACTDAT               
         BCT   RF,*-4                                                           
                                                                                
         LTR   R0,R0               ANY DAYS TO ADD?                             
         BZ    GEDTX                NOPE                                        
                                                                                
         LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    *+12                                                             
         SLL   R0,1                                                             
         BCT   R1,*-10                                                          
                                                                                
         LPR   R0,R1               GET # OF DAYS IN ROT AFTER NBACTDAT          
                                                                                
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK)                                
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),(0,WORK+6),(4,LDATE)                                 
                                                                                
GEDTX    DS    0H                                                               
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        NEED TO MAKE THIS A SHARED ROUTINE FOR ONLINE AND OFFLINE              
*        TO ACCOMODATE ONLINE GRIDS REPORTING                                   
*-------------------------------------------------------------------*           
RSDESC   NMOD1 0,**RSDE**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
*ADDED WHEN THIS WAS MADE A SHARED ROUTINE FOR ONLINE AND OFFLINE               
*        CLI   ACTNUM,29                                                        
*        BE    *+12                                                             
         TM    NUCMLFLG,X'E0'      ANY CHANGES-ELEM MUST BE REASSIGNED          
         BZ    LRR54                                                            
*ADDED                                                                          
                                                                                
         XC    SVNUCML1(24),SVNUCML1 CLEAR FOR SVNUCML1/2/3                     
                                                                                
         MVC   WORK(8),NUCML1                                                   
         MVC   WORKPROD,BPRD1C                                                  
         XC    SVTYPE,SVTYPE                                                    
                                                                                
         CLC   NUCMPROD,SPACES                                                  
         BH    LRR52D                                                           
         CLI   NUCMLPRD,0                                                       
         BE    LRR52E                                                           
         TM    SECFLAG,NECONPRD                                                 
         BO    LRR52E                                                           
                                                                                
*  IT'S POSSIBLE THIS LOOP IS REDUNDANT - RESEARCH LATER                        
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
LRR52CA  CLC   NUCMLPRD,3(RE)                                                   
         BE    LRR52CB                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,LRR52CA                                                       
         DC    H'0'                                                             
LRR52CB  MVC   NUCMPROD,0(RE)                                                   
                                                                                
LRR52D   MVC   WORKPROD,NUCMPROD                                                
                                                                                
LRR52E   MVC   BYTE1,NUCMLFLG                                                   
                                                                                
         BRAS  RE,FCMLA                                                         
         BNE   LRR52F                                                           
                                                                                
         MVC   SVNUCML1,WORK         MOVE IN 8 CHAR CML                         
                                                                                
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    LRR53                                                            
         MVC   1(37,R5),SPACES                                                  
         MVC   1(15,R5),=C'MAX DATE ERROR '                                     
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   1(20,R5),=C'NOT APPROVED TO AIR '                                
                                                                                
         LR    R5,R0                                                            
         LA    R5,132(R5)                                                       
         B     LRR53                                                            
                                                                                
LRR52F   CLC   BYTE1,NUCMLFLG                                                   
         BE    *+14                                                             
         MVC   NUCMLFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
         MVC   1(37,R5),ELEM                                                    
*GRID                                                                           
         MVC   GRREASS,ELEM                                                     
*GRID                                                                           
         LR    R5,R0                                                            
         LA    R5,132(R5)                                                       
                                                                                
LRR53    DS    0H                                                               
         OC    NUCML2,NUCML2                                                    
         BZ    RDSCXIT                                                          
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRR53E                                                           
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    RDSCXIT                                                          
                                                                                
LRR53E   DS    0H                                                               
         MVC   WORK(8),NUCML2                                                   
         MVC   WORKPROD,BPRD2C                                                  
         MVC   BYTE1,NUCMLFLG                                                   
         BRAS  RE,FCMLA                                                         
         BNE   LRR53L                                                           
                                                                                
         MVC   SVNUCML2,WORK         MOVE IN 8 CHAR CML                         
                                                                                
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    LRR53S                                                           
                                                                                
         MVC   1(37,R5),SPACES                                                  
         MVC   1(15,R5),=C'MAX DATE ERROR '                                     
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   1(20,R5),=C'NOT APPROVED TO AIR '                                
         B     LRR53S                                                           
                                                                                
LRR53L   MVC   0(37,R5),ELEM                                                    
         CLC   BYTE1,NUCMLFLG                                                   
         BE    *+14                                                             
         MVC   NUCMLFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
                                                                                
LRR53S   OC    BPRD3C,BPRD3C       IS THIS A TRI-BACK                           
         BZ    RDSCXIT                                                          
         OC    NUCML3,NUCML3                                                    
         BZ    RDSCXIT                                                          
                                                                                
         LA    R5,132(R5)                                                       
                                                                                
         MVC   WORK(8),NUCML3                                                   
         MVC   WORKPROD,BPRD3C                                                  
         MVC   BYTE1,NUCMLFLG                                                   
         BRAS  RE,FCMLA                                                         
         BNE   LRR53U                                                           
                                                                                
         MVC   SVNUCML3,WORK       MOVE IN 8 CHAR CML                           
                                                                                
         TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    RDSCXIT                                                          
         MVC   0(37,R5),SPACES                                                  
         MVC   0(15,R5),=C'MAX DATE ERROR '                                     
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   0(20,R5),=C'NOT APPROVED TO AIR '                                
         B     RDSCXIT                                                          
                                                                                
LRR53U   MVC   0(37,R5),ELEM                                                    
         CLC   BYTE1,NUCMLFLG                                                   
         BE    *+14                                                             
         MVC   NUCMLFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
                                                                                
         B     RDSCXIT                                                          
                                                                                
LRR54    DS    0H                                                               
         MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSFIL,C'S'                                                      
         MVI   SYSFIL+1,C'P'                                                    
                                                                                
         OC    NUCML1,NUCML1                                                    
         BZ    LRR56                                                            
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRR54A                                                           
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRR56                                                            
                                                                                
LRR54A   DS    0H                                                               
         OC    NUCMLR3F,NUCMLR3F   IS THIS FROM PATTERN                         
         BZ    *+12                 NO                                          
         LA    R0,NUCML1                                                        
         BRAS  RE,CPRD             GO INVERT COMMLS IF NEEDED                   
                                                                                
         MVC   PCMML(8),NUCML1                                                  
                                                                                
* VALIDATE CML NOT DELETED, RELEASE/RECALL DATES OK, & PRD OK                   
                                                                                
         MVC   WORK(8),NUCML1                                                   
         MVC   WORKPROD,BPRD1C                                                  
         XC    SVTYPE,SVTYPE                                                    
                                                                                
         CLC   NUCMPROD,SPACES                                                  
         BH    LRR54C                                                           
         CLI   NUCMLPRD,0                                                       
         BE    LRR54CA                                                          
         TM    SECFLAG,NECONPRD                                                 
         BO    LRR54CA                                                          
                                                                                
*  IT'S POSSIBLE THIS LOOP IS REDUNDANT - RESEARCH LATER                        
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
LRR54AA  CLC   NUCMLPRD,3(RE)                                                   
         BE    LRR54AB                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,LRR54AA                                                       
         DC    H'0'                                                             
LRR54AB  MVC   NUCMPROD,0(RE)                                                   
                                                                                
LRR54C   MVC   WORKPROD,NUCMPROD                                                
LRR54CA  MVC   BYTE1,NUCMLFLG                                                   
                                                                                
         BRAS  RE,FCMLA                                                         
         BNE   LRR54K                                                           
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         TM    OPTNSW1,OPTPADID    OPTION PRINT ADID                            
         BZ    LRR54D                                                           
                                                                                
         OC    SVCMLADI,SVCMLADI   ANY ADID                                     
         BZ    LRR54D                                                           
                                                                                
         MVC   PTIME(12),SVCMLADI                                               
         MVC   PDP,SPACES                                                       
                                                                                
LRR54D   TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    LRR54S                                                           
                                                                                
         MVC   PCMML(9),=CL9'REASSIGN '                                         
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR54G                                                           
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),NUCML1                                                  
                                                                                
         TM    NUCMADFL,NUCMADF1    AD-ID CML 1                                 
         BZ    LRR54G                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML1                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR54G   MVC   PCMML+9(37),SPACES                                               
         MVC   PCMML+9(15),=C'MAX DATE ERROR '                                  
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   PCMML+9(20),=C'NOT APPROVED TO AIR '                             
         B     LRR56                                                            
                                                                                
LRR54K   MVC   PCMML(9),=CL9'REASSIGN '                                         
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR54M                                                           
                                                                                
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),NUCML1                                                  
                                                                                
         TM    NUCMADFL,NUCMADF1    AD-ID CML 1                                 
         BZ    LRR54M                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML1                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR54M   DS    0H                                                               
         MVC   PCMML+9(38),ELEM                                                 
         CLC   BYTE1,NUCMLFLG                                                   
         BE    LRR56                                                            
         MVC   NUCMLFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
         B     LRR56                                                            
LRR54S   MVC   PCMMLT,SVCMTITL                                                  
                                                                                
LRR56    OC    NUCML2,NUCML2                                                    
         BZ    LRR58                                                            
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BH    LRR56A                                                           
         OC    SVCSPRDC(3),SVCSPRDC                                             
         BZ    LRR58                                                            
LRR56A   CLC   NUCML1,NUCML2       THIS A PIGGYBACK                             
         BNE   LRR56C                                                           
         MVC   PCMML+132+3(3),=C'P/B'                                           
                                                                                
         OC    BPRD3C,BPRD3C       THIS A TRI-BACK                              
         BZ    LRR58                NO                                          
         MVI   PCMML+132+3,C'T'    PRESET FOR T/B                               
         CLC   NUCML2,NUCML3       THIS A TRIBACK                               
         BE    LRR58                                                            
         MVC   PCMML+132+3(3),SPACES                                            
         B     LRR58                                                            
                                                                                
LRR56C   DS    0H                                                               
         MVC   PCMML+132(8),NUCML2                                              
                                                                                
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
                                                                                
         MVC   WORK(8),NUCML2                                                   
         MVC   WORKPROD,BPRD2C                                                  
         MVC   BYTE1,NUCMLFLG                                                   
         BRAS  RE,FCMLA                                                         
         BNE   LRR56K                                                           
                                                                                
         MVC   PCMML+132(8),WORK      8 CHAR CML                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132,SVCMLADI                                               
                                                                                
         TM    OPTNSW1,OPTPADID    OPTION PRINT ADID                            
         BZ    LRR56F                                                           
                                                                                
         OC    SVCMLADI,SVCMLADI   ANY ADID                                     
         BZ    LRR56F                                                           
         MVC   PTIME+132(12),SVCMLADI                                           
         MVC   PDP+132(2),SPACES                                                
                                                                                
LRR56F   TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    LRR57                                                            
         MVC   PCMML+132(8),=CL9'REASSIGN '                                     
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR56H                                                           
                                                                                
         MVI   PCMML-1+132,C'*'                                                 
         MVC   PCMML+132(8),NUCML2                                              
                                                                                
         TM    NUCMADFL,NUCMADF2    AD-ID CML 2                                 
         BZ    LRR56H                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML2                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML+132(8),WORK                                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132,SVCMLADI                                               
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR56H   MVC   PCMML+132+9(37),SPACES                                           
         MVC   PCMML+132+9(15),=C'MAX DATE ERROR '                              
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   PCMML+132+9(20),=C'NOT APPROVED TO AIR '                         
         B     LRR58                                                            
                                                                                
LRR56K   CLC   BYTE1,NUCMLFLG                                                   
         BE    *+14                                                             
         MVC   NUCMLFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
                                                                                
         MVC   PCMML+132(8),=CL9'REASSIGN '                                     
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR56M                                                           
         MVI   PCMML-1+132,C'*'                                                 
         MVC   PCMML+132(8),NUCML2                                              
                                                                                
         TM    NUCMADFL,NUCMADF2    AD-ID CML 2                                 
         BZ    LRR56M                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML2                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML+132(8),WORK                                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132,SVCMLADI                                               
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR56M   MVC   PCMML+132+9(37),ELEM                                             
         B     LRR58                                                            
                                                                                
LRR57    MVC   PCMMLT+132,SVCMTITL                                              
                                                                                
LRR58    DS   0H                                                                
         OC    BPRD3C,BPRD3C       IS THIS A TRI-BACK                           
         BZ    RDSCXIT                                                          
         OC    NUCML3,NUCML3                                                    
         BZ    *+14                                                             
         MVC   PCMML+132+132+3(3),SPACES                                        
         B     LRR58C                                                           
                                                                                
         MVC   PCMML+132+132+3(3),=C'N/A'                                       
         B     RDSCXIT                                                          
                                                                                
LRR58C   DS   0H                                                                
         CLC   NUCML2,NUCML3                                                    
         BNE   LRR58D                                                           
         CLC   NUCML1,NUCML3                                                    
         BNE   LRR58D                                                           
         MVC   PCMML+132+3(3),=C'T/B'                                           
         B     RDSCXIT                                                          
                                                                                
LRR58D   MVC   PCMML+132+132(8),NUCML3                                          
                                                                                
         MVC   WORK(8),NUCML3                                                   
         MVC   WORKPROD,BPRD3C                                                  
         MVC   BYTE1,NUCMLFLG                                                   
         BRAS  RE,FCMLA                                                         
         BNE   LRR58G                                                           
                                                                                
         MVC   PCMML+132+132(8),WORK    8 CHAR CML                              
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132+132,SVCMLADI                                           
                                                                                
         TM    OPTNSW1,OPTPADID    OPTION PRINT ADID                            
         BZ    LRR58F                                                           
                                                                                
         OC    SVCMLADI,SVCMLADI   ANY ADID                                     
         BZ    LRR58F                                                           
         MVC   PTIME+132+132(12),SVCMLADI                                       
         MVC   PDP+132+132(2),SPACES                                            
                                                                                
LRR58F   TM    CMLFLAG1,NOAIR+MAXDTE+CADTE ANY ERRORS ?                         
         BZ    LRR58K                                                           
         MVC   PCMML+132+132(8),=CL9'REASSIGN '                                 
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR58F5                                                          
         MVI   PCMML-1+132+132,C'*'                                             
         MVC   PCMML+132+132(8),NUCML3                                          
                                                                                
         TM    NUCMADFL,NUCMADF3    AD-ID CML 3                                 
         BZ    LRR58F5                                                          
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML3                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML+132+132(8),WORK                                            
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132+132,SVCMLADI                                           
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR58F5  MVC   PCMML+132+132+9(37),SPACES                                       
         MVC   PCMML+132+132+9(15),=C'MAX DATE ERROR '                          
         TM    CMLFLAG1,MAXDTE                                                  
         BO    *+10                                                             
         MVC   PCMML+132+132+9(20),=C'NOT APPROVED TO AIR '                     
         B     RDSCXIT                                                          
                                                                                
LRR58G   CLC   BYTE1,NUCMLFLG                                                   
         BE    *+14                                                             
         MVC   NUCMLFLG,BYTE1                                                   
         OI    SVFLAG,RECCHGSW     RECORD CHANGED                               
                                                                                
         MVC   PCMML+132+132(8),=CL9'REASSIGN '                                 
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    LRR58I                                                           
         MVI   PCMML-1+132+132,C'*'                                             
         MVC   PCMML+132+132(8),NUCML3                                          
                                                                                
         TM    NUCMADFL,NUCMADF3    AD-ID CML 3                                 
         BZ    LRR58I                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,NUCML3                                                      
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML+132+132(8),WORK                                            
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132+132,SVCMLADI                                           
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
LRR58I   MVC   PCMML+132+132+9(37),ELEM                                         
         B     RDSCXIT                                                          
                                                                                
LRR58K   MVC   PCMMLT+132+132,SVCMTITL                                          
                                                                                
RDSCXIT  XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------------*                   
*                                                                               
* THIS ROUTINE ADDS/DELETES 1ST KEY OF EACH SCREEN                              
* TO AND FROM THE TABLE (FOR PFKEY FUNCTIONS)                                   
*                                                                               
TBLKEY   NMOD1 0,**TBLK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         LH    R4,=AL2(LKEYTBL-SYSD)                                            
         AR    R4,R9               R4 POINTS TO TABLE                           
         LR    R1,R4                                                            
         LA    R5,L'LKEYTBL        R5 HAS LENGTH OF TABLE                       
         LR    RE,R5                                                            
                                                                                
         CLI   PFKEY,6             PF6 - TOP                                    
         BNE   TBLKEY10                                                         
                                                                                
         OC    TOTUNITS,TOTUNITS                                                
         BZ    TBLKEY20                                                         
                                                                                
         SR    RF,RF                                                            
         MVCL  R4,RE               CLEAR TABLE OF KEYS FOR LIST                 
                                                                                
         XC    TOTUNITS,TOTUNITS                                                
         XC    CURKEY,CURKEY                                                    
         B     TBLKEYX                                                          
                                                                                
TBLKEY10 CLI   PFKEY,0                                                          
         BE    TBLKEY20            NO PFKEY                                     
         CLI   PFKEY,8                                                          
         BE    TBLKEY20            PF8 - DOWN                                   
                                                                                
         CLI   PFKEY,7             PF7 - UP                                     
         BNE   TBLKEY20                                                         
                                                                                
         OC    TOTUNITS,TOTUNITS   FIRST TIME THROUGH                           
         BZ    TBLKEY20             YES                                         
                                                                                
         LH    RF,CURKEY           DISP TO CURRENT KEY IN TABLE                 
         BCTR  RF,0                ADJUST ENTRY NUMBER                          
         LTR   RF,RF               ARE WE AT THE TOP OF THE LIST                
         BNZ   TBLKEY15             YES                                         
                                                                                
         MVC   NBKEY,0(R4)         MOVE FIRST KEY                               
         MVC   KEY,NBKEY                                                        
         B     TBLKEYX                                                          
                                                                                
TBLKEY15 MH    RF,=AL2(LKEYENT)    ENTRY NO. TIMES ENTRY LENGTH                 
         AR    R4,RF               GET DISP INTO TABLE                          
         XC    0(LKEYENT,R4),0(R4) CLEAR LAST KEY ENTRY                         
         AHI   R4,-LKEYENT         POINT TO PREV ENTRY                          
                                                                                
         MVC   NBKEY,0(R4)                                                      
         MVC   KEY,NBKEY                                                        
                                                                                
         SR    R4,R1               DISPLACEMENT INTO TABLE                      
         LTR   R4,R4                                                            
         BNZ   *+16                                                             
         LA    R4,1(R4)            ADJUST ENTRY NUMBER                          
         STH   R4,CURKEY                                                        
         B     TBLKEYX                                                          
                                                                                
         LR    R5,R4                                                            
         SR    R4,R4                                                            
         D     R4,=A(LKEYENT)      DIVIDED BY ENTRY LENGTH                      
         LA    R5,1(R5)            ADJUST ENTRY NUMBER                          
         STH   R5,CURKEY                                                        
         B     TBLKEYX                                                          
                                                                                
TBLKEY20 LA    R0,LKEYTBLN         NUMBER OF ENTRIES IN TABLE                   
         LR    R5,R4                                                            
                                                                                
TBLKEY30 OC    0(LKEYENT,R4),0(R4)                                              
         BZ    TBLKEY50                                                         
                                                                                
         CLC   NBKEY,0(R4)         SAME KEY?                                    
         BE    TBLKEYX              YES                                         
                                                                                
         LA    R4,LKEYENT(R4)      BUMP TO NEXT ENTRY                           
         BCT   R0,TBLKEY30                                                      
                                                                                
* KEY TABLE IS FULL. DELETE FIRST ENTRY/MOVE UP THE REST AND                    
* ADD THIS ENTRY TO THE END OF THE TABLE                                        
                                                                                
         LA    RE,LKEYENT(R1)      POINT TO THE SECOND ENTRY                    
         LR    R0,R1                                                            
         LA    R1,L'LKEYTBL-LKEYENT    LENGTH -1 ENTRY                          
         LR    RF,R1                                                            
         MVCL  R0,RE               SHIFT ALL 1 OVER (DELETE 1ST)                
                                                                                
         AHI   R4,-LKEYENT         BACK TO LAST ENTRY                           
         LR    R1,R5               START OF TABLE                               
                                                                                
TBLKEY50 MVC   0(LKEYENT,R4),NBKEY SAVE KEY IN TABLE                            
         SR    R4,R1                                                            
         LTR   R4,R4                                                            
         BNZ   *+16                                                             
         LA    R4,1(R4)            ADJUST ENTRY NUMBER                          
         STH   R4,CURKEY                                                        
         B     TBLKEYX                                                          
                                                                                
         LR    R5,R4                                                            
         SR    R4,R4                                                            
         D     R4,=A(LKEYENT)      DIVIDED BY ENTRY LENGTH                      
         LA    R5,1(R5)            ADJUST ENTRY NUMBER                          
         STH   R5,CURKEY           SAVE DISPLACEMENT                            
                                                                                
TBLKEYX  XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* PRINT BILLBOARD INFO *                                                        
                                                                                
PBB      NMOD1 0,**PBB***                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         CLC   =C'VIGNETTE',SVBBSLN+1                                           
         BE    PBB02                                                            
                                                                                
         MVC   PPROG+22(9),=C'BILLBOARD'                                        
         B     PBB04                                                            
                                                                                
PBB02    CLI   SVBBSLN,255         BETTER ONLY BE VIGNETTE                      
         BE    PBB06                                                            
                                                                                
PBB04    MVC   PPROD+10(4),=C'LEN='                                             
         EDIT  (B1,SVBBSLN),(3,PPROD+14),ALIGN=LEFT                             
                                                                                
         B     PBB07                                                            
PBB06    XC    WORKPROD,WORKPROD   DISABLE COMML PRODUCT CHECKING               
                                                                                
PBB07    MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
                                                                                
         OC    SVBBPOS,SVBBPOS                                                  
         BZ    PBB10                                                            
         CLC   SVBBPOS,SPACES                                                   
         BE    PBB10                                                            
         MVC   PTIME(4),=C'POS='                                                
         MVC   PTIME+4(4),SVBBPOS                                               
                                                                                
PBB10    OC    SVBBSLN+1(8),SVBBSLN+1                                           
         BZ    PBB20                                                            
                                                                                
         CLC   =C'VIGNETTE',SVBBSLN+1                                           
         BE    PBB12                                                            
         MVC   PPRODNM+11(5),=C'SLIDE'                                          
                                                                                
PBB12    MVC   PCMML,SVBBSLN+1                                                  
         MVC   WORK(8),SVBBSLN+1                                                
         XC    SVTYPE,SVTYPE                                                    
         XC    SVCMTITL,SVCMTITL                                                
         CLC   =C'VIGNETTE',WORK                                                
         BE    PBB14                                                            
         BRAS  RE,FCMLA                                                         
         BNE   PBB13                                                            
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         TM    OPTNSW1,OPTPADID    OPTION PRINT ADID                            
         BZ    PBB14                                                            
                                                                                
         OC    SVCMLADI,SVCMLADI   ANY ADID                                     
         BZ    PBB14                                                            
         MVC   PTIME(12),SVCMLADI                                               
         MVC   PTIME+13(2),SPACES                                               
         XC    PDP,PDP                                                          
         B     PBB14                                                            
                                                                                
PBB13    MVC   PCMML(8),=C'REASSIGN'                                            
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    PBB13F                                                           
         MVI   PCMML-1,C'*'                                                     
         MVC   PCMML(8),SVBBSLN+1                                               
                                                                                
         TM    SVBBFLG,SVBBSADI    AD-ID BB SLIDE                               
         BZ    PBB13F                                                           
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,SVBBSLN+1                                                   
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML(8),WORK                                                    
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML,SVCMLADI                                                   
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
PBB13F   MVC   PCMML+9(37),ELEM                                                 
         B     PBB20                                                            
PBB14    MVC   PCMMLT,SVCMTITL                                                  
                                                                                
PBB20    OC    SVBBSLN+9(8),SVBBSLN+9                                           
         BZ    PBB40                                                            
         MVC   PPRODNM+11+132(4),=C'COPY'                                       
         MVC   PCMML+132,SVBBSLN+9                                              
         MVC   WORK(8),SVBBSLN+9                                                
         XC    SVTYPE,SVTYPE                                                    
         XC    SVCMTITL,SVCMTITL                                                
         BRAS  RE,FCMLA                                                         
         BNE   PBB25                                                            
                                                                                
         MVC   PCMML+132(8),WORK                                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132,SVCMLADI                                               
                                                                                
                                                                                
         TM    OPTNSW1,OPTPADID    OPTION PRINT ADID                            
         BZ    PBB30                                                            
                                                                                
         OC    SVCMLADI,SVCMLADI   ANY ADID                                     
         BZ    PBB30                                                            
         MVC   PTIME+132(12),SVCMLADI                                           
         MVC   PTIME+132+13(2),SPACES                                           
         XC    PDP+132(2),PDP+132                                               
         B     PBB30                                                            
                                                                                
PBB25    MVC   PCMML+132(8),=C'REASSIGN'                                        
                                                                                
         TM    SVFLAG,PREASGN      PRINT CML REASSIGN                           
         BZ    PBB28                                                            
         MVI   PCMML-1+132,C'*'                                                 
         MVC   PCMML+132(8),SVBBSLN+9                                           
                                                                                
         TM    SVBBFLG,SVBBCADI    AD-ID BB COPY                                
         BZ    PBB28                                                            
                                                                                
         OI    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         MVC   WORK,SVBBSLN+9                                                   
         BRAS  RE,FCMLA                                                         
                                                                                
         MVC   PCMML+132(8),WORK                                                
         CLI   SVCMLADI,C' '                                                    
         BNH   *+10                                                             
         MVC   PCMML+132,SVCMLADI                                               
                                                                                
         NI    SVFLAG,X'FF'-GETISCII                                            
                                                                                
PBB28    MVC   PCMML+132+9(37),ELEM                                             
         B     PBB40                                                            
PBB30    MVC   PCMMLT+132,SVCMTITL                                              
                                                                                
PBB40    MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
                                                                                
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
         EJECT                                                                  
* READ ESTIMATE RECORD TO GET ESTIMATE NAME                                     
                                                                                
RDEST    NMOD1 0,**RDEST*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
         XC    KEY,KEY                                                          
         XC    5(L'EDESC,R1),5(R1)  CLEAR OLD ESTIMATE NAME                     
         MVC   KEY+1(3),BAGYMD                                                  
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   RDEST32                                                          
         MVC   KEY+2(2),SVBCLT                                                  
         OC    SVBCLT,SVBCLT                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
RDEST32  CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BNH   RDEST36              YES TRY AS POL                              
         LA    RE,NCLSTSIZ                                                      
         L     RF,ASVNCLST                                                      
RDEST34  CLC   NBPR1CL3,0(RF)                                                   
         BE    RDEST38                                                          
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,RDEST34                                                       
                                                                                
* TRY UNKNOWN PRODUCT AS POL                                                    
                                                                                
RDEST36  LA    RF,=C'POL'          TRY AS PRODUCT POL                           
                                                                                
RDEST38  MVC   KEY+4(3),0(RF)                                                   
         MVC   KEY+7(1),NBACTEST                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDEST40                                                          
         CLC   NBPR1CL3,SPACES        UNALLOCATED                               
         BNH   RDEST50                                                          
         TM    NBKEY+12,X'C0'      THIS A CLOSED OUT UNIT                       
         BO    RDEST50              YES                                         
         DC    H'0'                                                             
                                                                                
         USING ESTHDRD,R6                                                       
RDEST40  MVC   5(L'EDESC,R1),EDESC  ESTIMATE NAME                               
         B     *+10                                                             
RDEST50  XC    1(3,R1),1(R1)       NO EST HDR REC FOR PRD POL                   
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
                                                                                
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
         DROP  R6                                                               
         EJECT                                                                  
                                                                                
* VALIDATE KEY ROUTINE *                                                        
                                                                                
VK       NMOD1 0,***VK***                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         BAS   RE,VMD              FAKE VALIDATE MEDIA                          
                                                                                
         MVI   SVFLAG,0                                                         
                                                                                
* READ ANY REVISION RECORD TO SEE IF THE FILE HAS BEEN CONVERTED.               
         NI    SVFLAG,X'FF'-CONVSW INIT CONVERTED RECORDS                       
         BRAS  RE,INITXSP          SET TO XSPOT                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A1D'                                                  
         GOTO1 HIGH                                                             
         CLC   =X'0A1D',KEY        ANY REV REC?                                 
         BE    VK06                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVC   0(L'NOTEMSG,R1),NOTEMSG                                          
         LA    R1,L'NOTEMSG(,R1)                                                
         MVC   0(2,R1),AGENCY                                                   
         MVC   3(3,R1),QCLT                                                     
         MVC   7(4,R1),TRANET                                                   
         MVC   12(8,R1),TRAPER                                                  
                                                                                
         OC    ELEM,SPACES                                                      
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'NOTEMSG+72,ELEM)                       
         B     VK07                NO REC FOUND, TREATE AS CONVERTED            
*                                                                               
VK06     TM    KEY+32,X'03'        CONVERTED RECORDS?                           
         BZ    *+8                                                              
VK07     OI    SVFLAG,CONVSW                                                    
*                                                                               
         TM    KEY+32,X'03'        CONVERTED RECORDS?                           
         BZ    *+8                                                              
         OI    SVFLAG,CONVSW                                                    
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
*                                                                               
         LA    R2,TRACLTH                                                       
         XC    BCLT,BCLT                                                        
         XC    SVBCLT,SVBCLT                                                    
         MVC   QCLT,SPACES                                                      
         MVI   SVOFF,0                                                          
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK10                 YES                                         
         CLC   =C'DDS,T/A',CONWHEN  IS THIS AN AUTO T/A REQUEST                 
         BNE   MISSERR                                                          
         B     VK20                                                             
                                                                                
VK10     CLI   8(R2),C'*'          REQUEST BY OFFICE                            
         BNE   VK18                                                             
         CLI   5(R2),3             MUST BE LENGTH OF 2                          
         BH    BADOFFER                                                         
                                                                                
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
                                                                                
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
                                                                                
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
                                                                                
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   ERREXIT                                                          
                                                                                
         MVI   ERROR,0                                                          
                                                                                
* VALIDATE OFFICE AND CONVERT TO 1 BYTE IF NEEDED                               
                                                                                
         XC    WORKSEC,WORKSEC                                                  
         LA    R5,WORKSEC                                                       
         USING OFFICED,R5                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,9(R2)       2 CHAR OFFICE CODE                           
         CLI   5(R2),3                                                          
         BE    *+10                                                             
         OC    OFCOFC2,SPACES      1 CHAR OFFICE PADDED W/SPACE                 
                                                                                
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VK14                JUST CONVERT, DO NOT VALIDATE                
                                                                                
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VK12                                                             
                                                                                
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VK12                VALIDATE AND CONVERT                         
                                                                                
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VK12                                                             
                                                                                
         MVI   LAOFFICE,0          INIT LIMITED ACCESS OFFICE                   
                                                                                
         BAS   RE,GOFF             GET OFFICE FOR THIS CLIENT                   
         B     VK14                                                             
                                                                                
VK12     MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
                                                                                
VK14     XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   INVOFERR                                                         
                                                                                
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    VK16                                                             
                                                                                
         MVC   SVOFF,OFCOFC        SAVE 1 BYTE OFFICE CODE                      
         B     *+10                                                             
VK16     MVC   SVOFF,9(R2)                                                      
         BRAS  RE,VOFF             VALIDATE OFFICE CODE                         
         BE    VK20                                                             
         MVC   GERROR,=Y(NOCLTS)                                                
         GOTO1 VTRAERR             USING GETTXT CALL                            
                                                                                
         DROP  R5                                                               
                                                                                
VK18     GOTO1 VALICLT                                                          
         L     RE,AIO1                                                          
         L     R0,ACLTREC                                                       
         LA    R1,1500                                                          
         LA    RF,1500                                                          
         MVCL  R0,RE               SHIFT ALL 1 OVER (DELETE 1ST)                
                                                                                
VK19     DS    0H                                                               
         MVC   SVBCLT,BCLT                                                      
                                                                                
VK20     XC    WORK,WORK           * READ TN PROFILE *                          
         MVC   WORK(4),=C'S0TN'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
                                                                                
         MVC   LCTN2PR6,SVPROF+1   CAL/BROAD/WEEKLY PROFILE                     
                                                                                
         MVC   WORK+2(2),=C'N0'                                                 
         GOTO1 (RF),(R1),,NBUSER                                                
                                                                                
         CLI   OFFLINE,C'Y'        IF ONLINE, BYPASS SORT                       
         BNE   VK25                                                             
                                                                                
         MVC   WORK+2(2),=C'TA'    READ TA ACTIVITY PROFILE                     
         GOTO1 (RF),(R1),,SVTAPROF                                              
                                                                                
         CLI   SVTAPROF+7,C'Y'     SORT BY MEDIA                                
         BNE   VK25                                                             
         OI    OPTN2SW,OPTSRTMD                                                 
                                                                                
VK25     LA    R2,TRANETH          NETWORK                                      
         XC    NETWORK,NETWORK                                                  
         BRAS  RE,VNET                                                          
                                                                                
         MVI   BYTE,0              COMES BACK 'FF' IF PGROUP REQUEST            
                                                                                
         BRAS  RE,VOPT             VALIDATE OPTIONS                             
                                                                                
* MUST READ TN2 PROFILE AFTER VNET USING MEDIA FROM NETWORK!                    
                                                                                
         XC    WORK,WORK                                                        
         MVI   WORK,X'A2'                                                       
         MVC   WORK+1(3),=C'TN2'   READ TN2                                     
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),SVMEDIA   USE MEDIA FROM NETWORK REC                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVTN2PRO,DATAMGR                               
                                                                                
         CLI   SVTN2PRO+5,C'0'                                                  
         BE    *+18                                                             
         CLI   SVTN2PRO+5,0                                                     
         BE    *+10                                                             
         MVC   LCTN2PR6,SVTN2PRO+5 SAVE CAL/BROAD/WEEKLY PROFILE                
         MVC   LCTN2PR8,SVTN2PRO+7 SHOW UNIT COST                               
         MVC   LCTN2PRC,WORK+31    SHOW ESTIMATE NAME AND NUMBER                
         MVC   SVTN2PR0,SVTN2PRO+0 BY PROD OR PGROUP                            
                                                                                
* VALIDATE PGROUP (IF ANY) AFTER TN2 PROFILE IS READ                            
                                                                                
         CLI   BYTE,X'FF'          PGROUP REQUEST?                              
         BNE   VK28                                                             
         BAS   RE,VGRP             VALIDATE PRODUCT GROUP                       
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    VK28                                                             
         LA    R1,=C'DMWRT '                                                    
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)                                                 
         MVI   DMCB+8,1                                                         
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(OPTPLEN)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,AOPTPGRL,,(RF)                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VK28     DS    0H                                                               
         LA    R2,TRAPERH          VALIDATE PERIOD                              
         BRAS  RE,VPER                                                          
                                                                                
         LA    R2,TRAPROGH         PROGRAM                                      
         XC    PROGRAM,PROGRAM                                                  
         BRAS  RE,VPROG                                                         
                                                                                
         TM    TRACLTH+4,X'20'     TEST FIELDS PREVIOUSLY VALIDATED             
         BZ    VK30                NO                                           
         TM    TRANETH+4,X'20'                                                  
         BZ    VK30                                                             
         TM    TRAPROGH+4,X'20'                                                 
         BZ    VK30                                                             
         TM    TRAPERH+4,X'20'                                                  
         BZ    VK30                                                             
         TM    TRAOPTH+4,X'20'                                                  
         BNZ   VKX                                                              
                                                                                
VK30     XC    TOTUNITS,TOTUNITS                                                
         XC    CURKEY,CURKEY                                                    
                                                                                
         CLI   ACTNUM,29           ACTION GRID                                  
         BNE   VK40                                                             
         LHI   R5,PCDRIVE-T216FFD                                               
         A     R5,ATWA                                                          
         USING PCDRIVEN,R5                                                      
         NI    PCGRIDS,X'FF'-PCGBEGQ                                            
         NI    PCGRIDS,X'FF'-PCGFINQ                                            
         DROP  R5                                                               
                                                                                
VK40     DS    0H                                                               
         LH    R0,=AL2(LKEYTBL-SYSD)                                            
         AR    R0,R9                                                            
         LA    R1,L'LKEYTBL                                                     
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR TABLE OF KEYS FOR LIST                 
                                                                                
         L     R0,AESTBL                                                        
         LA    R1,L'ESTBL                                                       
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R0,EQVPTBL                                                       
         LA    R1,L'EQVPTBL                                                     
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         BRAS  RE,GTEQV                                                         
                                                                                
         OI    TRACLTH+4,X'20'    FLAG FIELDS AS VALIDATED                      
         OI    TRANETH+4,X'20'                                                  
         OI    TRAPROGH+4,X'20'                                                 
         OI    TRAPERH+4,X'20'                                                  
         OI    TRAOPTH+4,X'20'                                                  
                                                                                
VKX      XIT1                                                                   
*                                                                               
NOTEMSG  DC    C'AUTONOTE*SMUR:NO 0A1D REC FOUND IN 2C '                        
*                                                                               
         EJECT                                                                  
* GET OFFICE FOR SINGLE CLIENT LIMITED ACCESS                                   
                                                                                
GOFF     NTR1                                                                   
                                                                                
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),T216FFD+6  LIMITED ACCESS CLT                           
                                                                                
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
         CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+6                                                              
         DC    H'0'                WHAT'S WRONG                                 
                                                                                
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
                                                                                
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
                                                                                
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   LAOFFICE,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   LAOFFICE,C'A'       IF THERE IS ONE                              
         BNL   *+10                                                             
         MVC   LAOFFICE,COFFICE    USE MEDIA OFFICE                             
                                                                                
         XC    FILENAME,FILENAME                                                
                                                                                
         XIT1                                                                   
         EJECT                                                                  
*=====================================================                          
* VALIDATE PRODUCT GROUP                                                        
*=====================================================                          
                                                                                
VGRP     NTR1                                                                   
         LA    R2,TRAOPTH                                                       
         GOTO1 SCANNER,DMCB,(20,TRAOPTH),(5,BLOCK+64)                           
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
*                                                                               
VGRP10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,PGRPCLC                                                       
         BE    VGRP15                                                           
         LA    R4,42(R4)           POINT TO NEXT BLOCK                          
         BCT   R3,VGRP10                                                        
         DC    H'0'                THIS CAN'T BE                                
                                                                                
PGRPCLC  CLC   12(0,R4),=C'PGRP '                                               
                                                                                
VGRP15   DS   0H                                                                
         GOTO1 VALIPGR                                                          
         MVC   OPTPRGRS,SVTN2PR0   MOVE SCHEME TO LOCAL STORAGE                 
         MVC   OPTPRGR,SVPGRP      MOVE GROUP CODE                              
         BRAS  RE,BLDPGRP                                                       
         J     EXIT                                                             
         EJECT                                                                  
* FAKE VALIDATE MEDIA *                                                         
                                                                                
         DS    0H                                                               
VMD      LR    R0,RE                                                            
         XC    SVBCLT(OPTIONS-SVBCLT),SVBCLT                                    
         LA    RE,OPTIONS                                                       
         LA    RF,ENDOPT-OPTIONS                                                
         XCEF                                                                   
                                                                                
         LA    R2,ELEM             FAKE VALIDATE MEDIA                          
         MVC   ELEM,=X'0A01000184010001'                                        
         MVI   ELEM+8,C'N'                                                      
         GOTO1 VALIMED                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
ERREXIT  GOTO1 ERREX                                                            
                                                                                
BADOFFER MVC   GERROR,=Y(BADOFF)                                                
         GOTO1 VTRAERR             USING GETTXT CALL                            
                                                                                
INVOFERR DS    0H                                                               
*GRID                                                                           
         B     BADOFFER                                                         
*GRID                                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVOFMSG),INVOFMSG                                     
         GOTO1 ERREX2                                                           
INVOFMSG DC    C'* ERROR * INVALID OFFICE *'                                    
                                                                                
         LTORG                                                                  
         EJECT                                                                  
BLDPGRP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RDUPDATE,C'N'       ON RETURN KEY HAS FIRST PRDGRP KEY           
         GOTO1 HIGH                                                             
*                                                                               
         LA    R4,L'OPTPGRL/3                                                   
         L     R5,AOPTPGRL                                                      
*                                                                               
BLDPGR2  LTR   R4,R4                                                            
         BZ    MAXPGER                                                          
         TM    SECFLAG,NECONPRD                                                 
         BZ    BLDPGR4                                                          
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,X'03'                                                     
         MVC   FLD(3),KEY+8                                                     
         MVI   ERROPT,C'Y'                                                      
                                                                                
         MVC   ELEM(L'KEY+L'KEYSAVE),KEY                                        
         LR    R0,R2                                                            
         LA    R2,FLDH                                                          
         GOTO1 VALIPRD                                                          
         LR    R2,R0                                                            
         MVC   KEY(L'KEY+L'KEYSAVE),ELEM                                        
         GOTO1 HIGH                                                             
         MVC   0(3,R5),FLD                                                      
                                                                                
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    BLDPGR12                                                         
                                                                                
         MVC   GERROR,=Y(INVPRDE)                                               
         XC    WORK,WORK                                                        
         MVI   WORK,4                                                           
         MVC   WORK+1(3),KEY+8                                                  
         LA    R1,WORK                                                          
         STCM  R1,7,GASUBST                                                     
         GOTO1 VTRAERR                                                          
                                                                                
BLDPGR4  LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
BLDPGR6  CLC   0(3,RE),KEY+8                                                    
         BE    BLDPGR10                                                         
         LA    RE,4(,RE)                                                        
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         BCT   RF,BLDPGR6                                                       
         DC    H'0'                                                             
*                                                                               
BLDPGR10 MVC   0(3,R5),0(RE)                                                    
*                                                                               
BLDPGR12 LA    R5,3(R5)                                                         
*                                                                               
BLDPGR14 MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BLDPGRX                                                          
         BCT   R4,BLDPGR2                                                       
         B     MAXPGER                                                          
                                                                                
BLDPGRX  XIT1                                                                   
                                                                                
MAXPGER  MVC   GERROR,=Y(MAXPGRP)                                               
         GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
* FILTER FOR BUY ACTIVITY REPORT *                                              
                                                                                
FACT     NMOD1 0,**FACT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         MVI   SVCMLFLG,0                                                       
         MVI   SVCMLFL2,0                                                       
         NI    OPTN2SW,X'FF'-OPTNACT SET OFF NEEDS TO BE PRTD SW                
         MVI   CUTNFLG,C' '        RESET CUTIN NEEDS ATTENTION                  
         MVI   FEEDFLG,C' '        RESET FEED NEEDS ATTENTION                   
                                                                                
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FACT40                                                           
         USING NUCMLEL,R6                                                       
         TM    NUCMLFL2,NUCMLFFD   FEED W/O NATIONAL                            
         BZ    *+12                                                             
         MVI   SVCMLFL2,X'FF'                                                   
         B     FACT18              GO GET FEED                                  
                                                                                
         MVC   SVCMLFLG,NUCMLFLG                                                
                                                                                
         CLI   SVTNPR5,C'Y'       TRAFFIC DELETES ALLOWED                       
         BNE   *+12                NO                                           
         TM    NUCMLFLG,X'08'     TRAFFIC DELETE                                
         BO    FACT10              YES                                          
                                                                                
         L     R1,NBAIO                                                         
         TM    NURSTAT-NURECD(R1),X'80' THIS UNIT DELETED                       
         BO    FACT10                                                           
                                                                                
         TM    NBUNITST,X'42'    PREEMPT/MISSED                                 
         BZ    FACT14                                                           
FACT10   TM    NUCMLFLG,X'10'      WAS UNIT EVER PRINTED ON INSTR               
         BZ    NEXIT                NO, BYPASS                                  
         TM    NUCMLFLG,X'02'      WAS UNIT PRINTED SINCE DEL/MISS              
         BO    NEXIT                YES, BYPASS                                 
         B     FACT54                                                           
                                                                                
FACT14   TM    UNITSW,UNITSWUN     THIS BUY ACTIVITY UNASSIGNED                 
         BO    FACT15               YES, KEEP ON CHECKING                       
                                                                                
         BAS   RE,CKREV            CK REV FOR CABLE                             
         BE    FACT15               ALL OKAY                                    
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
                                                                                
FACT15   TM    NUCMLFLG,X'E0'      REASSIGN NEEDED                              
         BNZ   FACT54               YES                                         
                                                                                
                                                                                
         TM    UNITSW,UNITSWUN    THIS BUY ACTIVITY - UNASSIGNED                
         BO    *+16                                                             
         TM    NUCMLFL2,NUCMLFLP   UNIT CHANGED SINCE PRINTED                   
         BZ    *+8                  NO                                          
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
                                                                                
         MVC   BPRD1C,NBPR1CL3                                                  
         MVC   BPRD2C,NBPR2CL3                                                  
         CLC   NBPR1CL3,SPACES           UNALLOCATED                            
         BH    FACT15A                                                          
         TM    NBUNST3,X'40'             COPY SPLIT                             
         BZ    FACT54                     NO                                    
         OC    SVCSPRDC(3),SVCSPRDC      ANY COPY SPLIT PRODS                   
         BZ    FACT54                     NO                                    
                                                                                
FACT15A  TM    NBUNST3,X'40'             COPY SPLIT                             
         BZ    FACT15E                    NO                                    
         OC    NUCMPROD,NUCMPROD                                                
         BNZ   FACT15D                                                          
         CLI   NUCMLPRD,0                PRD ASSIGNED TO COPY SPLIT             
         BE    FACT50                     NO                                    
         TM    SECFLAG,NECONPRD                                                 
         BO    FACT50                                                           
                                                                                
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
FACT15B  CLC   NUCMLPRD,3(RE)                                                   
         BE    FACT15C                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,FACT15B                                                       
         DC    H'0'                                                             
FACT15C  MVC   NUCMPROD,0(RE)                                                   
                                                                                
FACT15D  MVC   BPRD1C,NUCMPROD                                                  
         XC    BPRD2C,BPRD2C                                                    
                                                                                
FACT15E  TM    NUCMLFLG,X'04'     BILLBOARD REQUIRED                            
         BZ    *+16                NO                                           
         CLI   NUCMLBSL,0          IS THERE A BILLBOARD SPOT LEN                
         BNE   *+8                  YES                                         
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
                                                                                
         OC    NUCMLR3F,NUCMLR3F   WAS THIS FROM PATTERN                        
         BZ    *+12                 NO, LEAVE THEM ALONE                        
         LA    R0,NUCML1                                                        
         BRAS  RE,CPRD             CK PRD SEQ, INVERT CMMLS                     
                                                                                
* VALIDATE CML(S) NOT DELETED, AND RELEASE/RECALL DATES OK                      
                                                                                
         MVC   SVCMLS,NUCML1                                                    
         BAS   RE,VCMLS                                                         
         BE    *+8                                                              
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
                                                                                
* SEE IF BILLBOARD REQUIRED AND NONE ASSIGNED *                                 
                                                                                
         TM    NUCMLFLG,X'04'      MEDIA REQUIRES BB                            
         BZ    FACT16                                                           
         CLI   NUCMLBSL,0                                                       
         BNE   FACT16                                                           
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
                                                                                
* VALIDATE BILLBOARD CML(S) NOT DELETED, AND RELEASE/RECALL DATES OK            
                                                                                
FACT16   OC    NUCMLBSN,NUCMLBSN   SLIDE ASSIGNED                               
         BZ    FACT17                                                           
         CLC   =C'VIGNETTE',NUCMLBSN                                            
         BE    FACT18                                                           
         MVC   SVCMLS(8),NUCMLBSN                                               
         XC    BPRD1C,BPRD1C                                                    
         BAS   RE,VCMLS                                                         
                                                                                
         BE    FACT17              OKAY                                         
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
                                                                                
FACT17   OC    NUCMLBCN,NUCMLBCN   COPY ASSIGNED                                
         BZ    FACT18                                                           
         CLC   =C'VIGNETTE',NUCMLBSN                                            
         BE    FACT18                                                           
         MVC   SVCMLS(8),NUCMLBCN                                               
         BAS   RE,VCMLS                                                         
                                                                                
         BE    FACT18              OKAY                                         
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
                                                                                
* DO SAME FOR ANY FEEDS *                                                       
                                                                                
FACT18   BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FACT30                                                           
                                                                                
         MVI   FEEDFLG,C'F'       SET UP IF FEED NEEDS ATTENTION                
         MVI   BYTE,0                                                           
                                                                                
         USING NUFDCEL,R6                                                       
                                                                                
FACT20   DS   0H                                                                
         TM    NUFDCFL2,X'80'      FEED DELETED                                 
         BO    FACT26               BYPASS                                      
                                                                                
         TM    NBUNST3,X'40'      COPY SPLIT                                    
         BZ    FACT22              NO                                           
         OC    NUFDPROD,NUFDPROD                                                
         BNZ   FACT22                                                           
         CLI   NUFDCPRD,0         PRD ASSIGNED TO COPY SPLIT                    
         BNE   FACT22              YES                                          
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
         MVI   BYTE,1                                                           
         B     FACT30                                                           
                                                                                
FACT22   DS    0H                                                               
         TM    NUFDCFL2,NUFDCFLP   NEEDS PRINTING ON INSTR (CHANGED)            
         BZ    *+16                 YES                                         
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
         MVI   BYTE,1                                                           
         B     FACT30                                                           
                                                                                
         OC    NUFDCR3F,NUFDCR3F   WAS THIS FROM PATTERN                        
         BZ    *+12                 NO, LEAVE THEM ALONE                        
         LA    R0,NUFDCML1                                                      
         BRAS  RE,CPRD             CK PRD SEQ, INVERT CMMLS                     
                                                                                
         MVC   SVCMLS,NUFDCML1                                                  
         OC    NUFDPROD,NUFDPROD                                                
         BNZ   FACT22G                                                          
         CLI   NUFDCPRD,0         PRD ASSIGNED TO COPY SPLIT                    
         BE    FACT23              NO                                           
         TM    SECFLAG,NECONPRD                                                 
         BO    FACT23                                                           
                                                                                
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
FACT22C  CLC   NUFDCPRD,3(RE)                                                   
         BE    FACT22D                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,FACT22C                                                       
         DC    H'0'                                                             
FACT22D  MVC   NUFDPROD,0(RE)                                                   
                                                                                
FACT22G  DS    0H                                                               
         MVC   BPRD1C,NUFDPROD                                                  
         XC    BPRD2C,BPRD2C                                                    
                                                                                
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
                                                                                
FACT23   DS    0H                                                               
         BAS   RE,VCMLS                                                         
         BE    *+16                                                             
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
         MVI   BYTE,1                                                           
         B     FACT30                                                           
                                                                                
         TM    NUFDCFLG,X'04'     BILLBOARD REQUIRED                            
         BZ    *+24                NO                                           
         CLI   NUFDCBSL,0         IS THERE A BILLBOARD SPOT LEN                 
         BNE   *+16                YES                                          
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
         MVI   BYTE,1                                                           
         B     FACT30                                                           
                                                                                
* VALIDATE BILLBOARD CML(S) NOT DELETED, AND RELEASE/RECALL DATES OK            
                                                                                
         OC    NUFDCBSN,NUFDCBSN   SLIDE ASSIGNED                               
         BZ    FACT24                                                           
         MVC   SVCMLS,NUFDCBSN                                                  
         BAS   RE,VCMLS                                                         
                                                                                
         BE    FACT24              OKAY                                         
                                                                                
         OI    OPTN2SW,OPTNACT     SET ON NEEDS TO BE PRTD SW                   
         MVI   BYTE,1                                                           
         B     FACT30                                                           
                                                                                
FACT24   DS   0H                                                                
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         MVI   ELCODE,X'23'                                                     
FACT26   BRAS  RE,NEXTEL                                                        
         BE    FACT20                                                           
                                                                                
         CLI   BYTE,0              DID FEED ERROR OCCUR?                        
         BNE   FACT30               YES                                         
                                                                                
         MVI   FEEDFLG,C' '        RESET FEED NEEDS ATTENTION                   
         DROP  R6                                                               
                                                                                
* CHECK HERE FOR CUTINS                                                         
                                                                                
FACT30   DS   0H                                                                
         CLC   =C'SJ',AGENCY       IS AGENCY STARCOM                            
         BE    FACT31               NO, DONE                                    
         CLC   =C'H9',AGENCY       IS AGENCY STARCOM                            
         BNE   FACT38               NO, DONE                                    
                                                                                
* SEE IF ANY CUTIN COMML ELEM                                                   
                                                                                
FACT31   L     R6,NBAIO                                                         
         XC    BLOCK(256),BLOCK    CLEAR AREA FOR CUTIN COMMLS                  
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'17'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FACT32                                                           
                                                                                
         ZIC   RF,1(R6)                                                         
         BCTR  RF,0                LESS 1 FOR MOVE                              
         EX    RF,GCUTMVC                                                       
                                                                                
FACT32   DS   0H                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FACT38                                                           
         USING NUSPREL,R6                                                       
                                                                                
* VALIDATE CML(S) NOT DELETED, AND RELEASE/RECALL DATES OK                      
                                                                                
FACT34   DS   0H                                                                
         CLI   NUSPRTYP,C'U'       ONLY WANT CUTINS                             
         BNE   FACT36                                                           
         CLI   NUSPRLEN,NUSPRLN1   BYPASS OLD ELEMS                             
         BE    FACT36                                                           
                                                                                
         MVI   CUTNFLG,C'C'        SET UP IF CUTIN NEEDS ATTENTION              
                                                                                
         CLI   NUSPRLEN,NUSPRLN4   BYPASS OLD ELEMS                             
         BL    FACT34B                                                          
         OC    NUSPRTPC,NUSPRTPC                                                
         BNZ   FACT34H                                                          
FACT34B  CLI   NUSPRTPR,0           ANY PRODUCT?                                
         BE    FACT54               NO                                          
         TM    SECFLAG,NECONPRD     IF CLI IS CONVERTED MUST BE PROD            
         BNO   *+6                  IN NUSPRTPC THEREFORE SHOULD NOT            
         DC    H'0'                 EVER GET HERE                               
         LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
FACT34C  CLC   NUSPRTPR,3(RE)                                                   
         BE    FACT34D                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,FACT34C                                                       
         DC    H'0'                                                             
FACT34D  MVC   NUSPRTPC,0(RE)      SET NUSPRTPC                                 
                                                                                
FACT34H  DS    0H                                                               
         OC    NUSPRCIS,NUSPRCIS   ANY CUTIN STATION?                           
         BZ    FACT54               NO                                          
                                                                                
         CLI   NUSPRCMI,0          ANY COMML?                                   
         BE    FACT54               NO                                          
                                                                                
         ZIC   RF,NUSPRCMI         GET POINTER TO COMML                         
         BCTR  RF,0                                                             
         SLL   RF,3                TIMES 8                                      
         LA    RE,BLOCK+2(RF)                                                   
         XC    SVCMLS,SVCMLS                                                    
         MVC   SVCMLS(8),0(RE)                                                  
         OC    SVCMLS,SVCMLS       THERE HAD BETTER BE SOMETHING                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BPRD1C,NUSPRTPC                                                  
                                                                                
         BAS   RE,VCMLS                                                         
                                                                                
         BNE   FACT54                                                           
                                                                                
FACT36   DS   0H                                                                
         BRAS  RE,NEXTEL                                                        
         BE    FACT34                                                           
                                                                                
         MVI   CUTNFLG,C' '        RESET CUTIN NEEDS ATTENTION                  
                                                                                
FACT38   DS   0H                                                                
         TM    OPTN2SW,OPTNACT     THIS NEEDS TO BE PRTD SW ON?                 
         BO    FACT54                                                           
                                                                                
         B     NEXIT                                                            
         DROP  R6                                                               
                                                                                
* IF NO CML ELEM AND PREEMPTED OR MISSED BYPASS *                               
                                                                                
FACT40   TM    NBUNITST,X'42'     PREEMPT/MISSED                                
         BNZ   NEXIT               YES, BYPASS                                  
         L     R1,NBAIO                                                         
         TM    NURSTAT-NURECD(R1),X'80' THIS UNIT DELETED                       
         BO    NEXIT                     YES, BYPASS                            
         B     FACT54                                                           
                                                                                
FACT50   DS   0H                                                                
         TM    OPTN2SW,OPTNACT     THIS NEEDS TO BE PRTD SW ON?                 
         BZ    NEXIT                                                            
                                                                                
FACT54   DS   0H                                                                
         NI    OPTN2SW,X'FF'-OPTNACT SET OFF NEEDS TO BE PRTD SW                
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         B     EEXIT                                                            
                                                                                
GCUTMVC  MVC   BLOCK(0),0(R6)                                                   
         EJECT                                                                  
* VALIDATE CML(S) NOT DELETED, AND RELEASE/RECALL DATES OK                      
                                                                                
VCMLS    NTR1                                                                   
         OC    SVCML1,SVCML1       CML ASSIGNED                                 
         BZ    NEXIT                NO                                          
                                                                                
         XC    WORK,WORK          TEST1                                         
         MVC   WORK(8),SVCML1                                                   
         MVC   WORKPROD,BPRD1C                                                  
         XC    SVTYPE,SVTYPE                                                    
         BRAS  RE,FCMLA                                                         
         BNE   NEXIT                                                            
                                                                                
         CLC   NBPR2CL3,SPACES                                                  
         BNH   EEXIT                                                            
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    EEXIT                YES                                         
         OC    SVCML2,SVCML2       CML ASSIGNED                                 
         BZ    NEXIT               BYPASS                                       
                                                                                
         CLC   SVCML1,SVCML2       THIS P/B CML                                 
         BE    EEXIT               YES CK FEEDS                                 
                                                                                
* VALIDATE CML NOT DELETED, AND RELEASE/RECALL DATES OK                         
                                                                                
         MVC   WORK(8),SVCML2                                                   
         MVC   WORKPROD,BPRD2C                                                  
         BRAS  RE,FCMLA                                                         
         BNE   NEXIT                                                            
         B     EEXIT                                                            
                                                                                
NEXIT    LTR   RB,RB                                                            
         B     *+6                                                              
EEXIT    CR    RB,RB                                                            
EXIT1    XIT1                                                                   
         EJECT                                                                  
* CK REVSION REC TABLE FOR CABLE AND SEE IF PRINTED *                           
                                                                                
         DS    0H                                                               
         USING NUCMLEL,R6                                                       
CKREV    NTR1                                                                   
         LH    R4,=AL2(REVRTBL-SYSD)                                            
         AR    R4,R9                                                            
         LA    R5,REVRTBLN                                                      
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BNE   CKREV10                                                          
                                                                                
         L     R4,VADUMMY                                                       
         LA    R5,OFREVTBN                                                      
OFREVTBN EQU   2000                                                             
                                                                                
         USING REVRECTD,R4                                                      
CKREV10  OC    REVRENT,REVRENT     EMPTY ENTRY                                  
         BZ    CKREV24                                                          
         CLC   REVRPRG,NBACTPRG                                                 
         BNE   CKREV20                                                          
         CLC   REVRSDT,NBACTDAT                                                 
         BH    CKREV20                                                          
         CLC   REVREDT,NBACTDAT                                                 
         BL    CKREV20                                                          
         CLC   REVRVN,NUCMLREV    THIS CABLE INS                                
         BNL   CKREV30                                                          
*NOP     B     CKREV24                                                          
                                                                                
CKREV20  LA    R4,REVRNEXT                                                      
         BCT   R5,CKREV10                                                       
                                                                                
CKREV24  LTR   RB,RB               SET UNEQ CC                                  
         B     EXIT1                                                            
                                                                                
CKREV30  CR    RB,RB               SET EQ CC                                    
         B     EXIT1                                                            
         DROP  R4                                                               
         EJECT                                                                  
* SEE IF PRODS ARE IN ALPHA SEQ *                                               
                                                                                
                                                                                
CPRD     NMOD1 0,**CPRD**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         CLC   BPRD2C,SPACES       IF PTR PROD, CK SEQ                          
         BNH   CPRDX                                                            
                                                                                
         TM    NBUNST3,X'40'       IF COPY SPLIT                                
         BO    CPRDX               DO NOT SWAP CMLS                             
                                                                                
         TM    SECFLAG,NECONPRD                                                 
         BZ    CPRD05                                                           
         LA    R1,BPRD1C                                                        
         LA    RF,BPRD2C                                                        
         B     CPRD24                                                           
                                                                                
CPRD05   DS    0H                                                               
         LA    RE,NCLSTSIZ                                                      
         L     R1,ASVNCLST                                                      
         LR    RF,R1                                                            
                                                                                
CPRD10   CLC   BPRD1C,0(R1)                                                     
         BE    CPRD16                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,CPRD10                                                        
                                                                                
         LA    R1,=C'???'          DELETED PRODUCT                              
                                                                                
CPRD16   DS    0H                                                               
         LA    RE,NCLSTSIZ                                                      
                                                                                
CPRD20   CLC   BPRD2C,0(RF)                                                     
         BE    CPRD24                                                           
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,CPRD20                                                        
                                                                                
         LA    RF,=C'???'          DELETED PRODUCT                              
                                                                                
CPRD24   CLC   0(3,R1),0(RF)                                                    
         BNH   CPRDX                                                            
         LR    R1,R0                                                            
CPRDX    XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* FIND CML AND CK FOR RELEASE/RECALL DATES AND DELETED STATUS *                 
* (ON ENTRY NUCMLFLG IS IN BYTE1                                                
*  BYTE1 IS UPDATED W/ MORE ERRORS IF ANY)                                      
                                                                                
FCMLA    NMOD1 0,**FCML**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         BRAS  RE,INITSPT          SET TO SPOT                                  
                                                                                
         XC    SVTITL1(SVTITALL),SVTITL1                                        
                                                                                
         CLI   JUSTLEN,C'Y'        JUST GET CML LEN                             
         BNE   FCMLA01F                                                         
                                                                                
         LA    R2,WORK             NUCML1/CML2/CML3                             
         LA    R4,SVLEN1           SLN1/SLN2/SLN3                               
         LA    R5,3                3 CMLS TO PROCESS                            
                                                                                
FCMLA01C OC    0(8,R2),0(R2)       ANY CML                                      
         BNZ   FCMLA01F                                                         
                                                                                
         LA    R2,8(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,FCMLA01C                                                      
         B     FCMLAX                                                           
                                                                                
FCMLA01F BAS   RE,BCMLKEY                                                       
                                                                                
         TM    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         BO    FCMLA01H                                                         
                                                                                
         XC    ELEM,ELEM                                                        
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FCMLA01K                                                         
                                                                                
         MVC   KEY,KEYSAVE                                                      
                                                                                
FCMLA01H MVI   KEY+1,X'C1'         SEE IF AD-ID                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FCMLAERC                                                         
                                                                                
FCMLA01K BAS   RE,TRACE                                                         
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FCMLAERC                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,TRACE                                                         
*                                                                               
         CLI   JUSTLEN,C'Y'        JUST GET CML LEN                             
         BE    FCMLA01P                                                         
*                                                                               
         XC    SVCMLADI,SVCMLADI                                                
         L     R5,AIO1                                                          
         MVC   SVCMLADI(8),5(R5)                                                
         MVI   ELCODE,X'A0'        ADID ELEMENT                                 
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVCMLADI,2(R6)      SAVE PRINTABLE ADID                          
                                                                                
         L     R6,AIO1                                                          
                                                                                
         USING CMLRECD,R6                                                       
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BO    *+14                                                             
         MVC   WORK(8),5(R6)       MOVE IN 8 CHAR CMLS FOR PRINTING             
         B     FCMLA01M                                                         
         DROP  R6                                                               
                                                                                
         USING CMLADIEL,R6                                                      
         L     R6,AIO1                                                          
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCMLA01M                                                         
         MVC   WORK(12),CMLADID    MOVE IN 12 CHAR CMLS FOR PRINTING            
         DROP  R6                                                               
                                                                                
FCMLA01M DS    0H                                                               
         TM    SVFLAG,GETISCII     JUST GET 8 CHAR ISCII                        
         BO    FCMLAX                                                           
                                                                                
         L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCMLA01P                                                         
         USING CMLDSCEL,R6                                                      
         IC    R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVTITL1(0),CMLDSC                                                
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   FCMLA01P                                                         
         IC    R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVTITL2(0),CMLDSC                                                
                                                                                
         BRAS  RE,NEXTEL                                                        
         BNE   FCMLA01P                                                         
         IC    R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVTITL3(0),CMLDSC                                                
                                                                                
FCMLA01P DS    0H                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
                                                                                
         CLI   JUSTLEN,C'Y'        JUST GET CML LEN                             
         BE    FCML01R                                                          
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         BO    FCMLAERA                                                         
         B     FCMLA02                                                          
                                                                                
FCML01R  MVC   0(1,R4),CMLSLN      RETURN COML LEN IN SVLEN                     
         CLC   0(8,R2),8(R2)                                                    
         BE    FCML01U                                                          
         LA    R2,8(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,FCMLA01C                                                      
         B     FCMLAX                                                           
                                                                                
FCML01U  MVI   1(R4),0             P/B CML                                      
         B     FCMLAX                                                           
                                                                                
FCMLA02  XC    WORK+40(20),WORK+40                                              
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK+40)                             
         GOTO1 GETDAY,DMCB,WORK+40,WORK+46                                      
         ZIC   RF,0(R1)            GET DAY OF WEEK                              
                                                                                
*NOP     XC    WORK+40(20),WORK+40                                              
         ZIC   R0,NBSDROT                                                       
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
                                                                                
         LTR   RF,RF                                                            
         BZ    FCMLA05                                                          
         SLL   R0,1                ONLY COUNT DAYS AFTER NBACTDAT               
         BCT   RF,*-4                                                           
                                                                                
FCMLA05  LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    FCMLA10                                                          
         SLL   R0,1                                                             
         BCT   R1,FCMLA05                                                       
                                                                                
FCMLA10  LPR   R0,R1                                                            
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,WORK+40) UNIT DATE                   
         GOTO1 ADDAY,(R1),WORK+40,WORK+46,(R0)                                  
         GOTO1 DATCON,(R1),(0,WORK+46),(3,WORK+52) PLUS ROTATION                
         GOTO1 (RF),(R1),(0,WORK+40),(3,WORK+46)  UNIT DATE                     
                                                                                
         MVC   SVSTDATE,WORK+46    SAVE UNIT START DATE                         
         MVC   SVENDATE,WORK+52     AND END DATE                                
                                                                                
         CLC   CMLRLSE,WORK+52     SEE IF AIR DATE FALLS IN CML PERIOD          
         BH    FCMLAERB                                                         
         CLC   CMLRCL,WORK+46                                                   
         BL    FCMLAERB                                                         
                                                                                
         MVC   SVCMTITL,CMLTITLE                                                
         OC    SVTITL1,SVTITL1                                                  
         BNZ   *+10                                                             
         MVC   SVTITL1(L'SVCMTITL),SVCMTITL                                     
                                                                                
         CLC   NBPR2CL3,SPACES                                                  
         BH    FCMLA12                                                          
                                                                                
         CLI   CMLSOLO,0                                                        
         BE    FCMLA14                                                          
         CLI   CMLSOLO,C'S'        SOLO                                         
         BE    FCMLA14                                                          
         B     PBSERR                                                           
                                                                                
FCMLA12  TM    NBUNST3,X'40'       COPY SPLIT                                   
         BO    FCMLA14              YES                                         
         CLI   CMLSOLO,0                                                        
         BE    FCMLA14                                                          
         CLI   CMLSOLO,C'P'        P/B                                          
         BE    FCMLA14                                                          
         B     PBSERR                                                           
                                                                                
FCMLA14  OC    SVTYPE,SVTYPE                                                    
         BNZ   *+14                                                             
         MVC   SVTYPE,CMLTYPE                                                   
         B     FCMLA15                                                          
                                                                                
         CLC   SVTYPE,CMLTYPE                                                   
         BNE   TYPERR                                                           
                                                                                
FCMLA15  OC    WORKPROD,WORKPROD   ANY PRD                                      
         BZ    FCMLA30                                                          
                                                                                
         IC    R2,ELCODE                                                        
                                                                                
         MVI   ELCODE,X'29'        LOOK AT PRODUCT LIST                         
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),X'FF'         ALL PRODS                                    
         BE    FCMLA30                                                          
                                                                                
         XC    PRDS29EL,PRDS29EL    CLEAR PRDS SAVE AREA                        
                                                                                
         ZIC   R0,1(R6)            GET LEN OF ELEM                              
         BCTR  R0,0                R0 USED AS COUNTER                           
         BCTR  R0,0                                                             
         LA    R1,2(,R6)           POINT TO PRODUCT LIST                        
FCMLA20  CLC   WORKPROD,0(R1)                                                   
         BE    FCMLA30                                                          
         LA    RF,PRDS29EL                                                      
         LA    R4,84               MAX PRODS IN PRDS29EL                        
FCMLA23  OC    0(3,RF),0(RF)                                                    
         BZ    FCMLA25                                                          
         LA    RF,3(RF)                                                         
         BCT   R4,FCMLA23                                                       
         DC    H'0'                END OF WORK                                  
                                                                                
FCMLA25  MVC   0(3,RF),0(R1)       SAVE PRDS                                    
         LA    R1,3(,R1)                                                        
         SH    R0,=H'2'            PRODS ARE NOW CL3 INSTEAD OF XL1             
         BCT   R0,FCMLA20                                                       
         B     FCMLAERD            BAD PRODUCT                                  
                                                                                
FCMLA30  BAS   RE,NCML             NETWORK RESTRICTIONS                         
         BNE   NETWKERR                                                         
                                                                                
         NI    CMLFLAG1,X'FF'-(NOAIR+MAXDTE+CADTE) INIT ERR FLAGS               
         BRAS  RE,VCMLAPR                                                       
                                                                                
         STC   R2,ELCODE                                                        
                                                                                
FCMLA60  CR    RB,RB                                                            
         B     FCMLAX                                                           
NETWKERR DS   0H                                                                
         MVC   ELEM(13),=C'NETWORK ERROR'                                       
         OI    BYTE1,X'E0'         TURN ON ALL CHG/CML INVALID BITS             
         B     FCMLAER                                                          
TYPERR   DS   0H                                                                
         MVC   ELEM(13),=C'CMLS TYPE ERR'                                       
         OI    BYTE1,X'E0'         TURN ON ALL CHG/CML INVALID BITS             
         B     FCMLAER                                                          
PBSERR   DS   0H                                                                
         MVC   ELEM(16),=C'CML P/B SOLO ERR'                                    
         OI    BYTE1,X'E0'         TURN ON ALL CHG/CML INVALID BITS             
         B     FCMLAER                                                          
FCMLAERA DS   0H                                                                
         MVC   ELEM(7),DELETED                                                  
         B     FCMLAER                                                          
FCMLAERB MVC   ELEM(7),=CL7'REL/RCL'                                            
         OI    BYTE1,X'20'         DATE CHANGED                                 
         B     FCMLAER                                                          
FCMLAERC MVC   ELEM(7),=CL7'NOT FND'                                            
         B     FCMLAER                                                          
FCMLAERD DS    0H                                                               
         MVC   ELEM(7),=CL7'BAD PRD'                                            
         MVC   ELEM+8(4),=C'CML='                                               
         MVC   ELEM+12(12),WORK                                                 
         LA    R6,ELEM+20                                                       
         CLI   0(R6),C' '                                                       
         BNH   *+12                                                             
         LA    R6,1(R6)                                                         
         B     *-12                                                             
         LA    R6,1(R6)                                                         
                                                                                
         MVC   0(4,R6),=C'PRD='                                                 
         MVI   BSLN,0                                                           
         LA    R6,4(R6)            PRODS OUTPUT                                 
         LA    R5,PRDS29EL         LIST OF PRD FOR THIS CML                     
         B     *+12                                                             
FCMLAER5 MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
         MVC   PPRDPRD,0(R5)                                                    
         BRAS  RE,PPRD             GET PROD NAME                                
         LA    R6,3(R6)            BUMP IN ELEM                                 
         LA    R5,3(R5)                                                         
         CLI   0(R5),0             ANY MORE PRDS                                
         BNE   FCMLAER5              YES                                        
         MVC   GRREASS,ELEM                                                     
         OI    BYTE1,X'40'         PRODUCT CHANGED                              
         STC   R2,ELCODE                                                        
                                                                                
FCMLAER  LTR   RB,RB                                                            
                                                                                
FCMLAX   BRAS  RE,INITNET          SET TO NET                                   
                                                                                
         XIT1                                                                   
PRDS29EL DS    CL254      24CL3 PLUS 1 FOR EOT MARKET                           
         EJECT                                                                  
                                                                                
* BUILD CMML KEY *                                                              
                                                                                
BCMLKEY  NTR1                                                                   
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD  & BCLT                                         
         OC    NBACTCLI,NBACTCLI                                                
         BZ    *+10                                                             
         MVC   CMLKCLT,NBACTCLI                                                 
                                                                                
         MVC   CMLKCML,WORK                                                     
                                                                                
         CLI   JUSTLEN,C'Y'                                                     
         BNE   *+10                                                             
         MVC   CMLKCML,0(R2)                                                    
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         XIT1  REGS=(R6)                                                        
         DROP  R4                                                               
                                                                                
* SEE IF CML IS NETWORK SPECIFIC *                                              
                                                                                
         DS    0H                                                               
NCML     NTR1                                                                   
                                                                                
         MVI   ELCODE,X'22'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   NCMLX                                                            
                                                                                
         USING CMLNETEL,R6                                                      
                                                                                
NCML10   DS    0H                                                               
         CLI   CMLNETLN,6          OLD REC?                                     
         BE    NCML30               YES                                         
                                                                                
         TM    CMLFLG,CMLEXNET     EXCLUDE THIS NETWORK?                        
         BZ    NCML30                                                           
         CLC   NBACTNET,CMLNET     IS THIS IT                                   
         BNE   NCML40                                                           
         OI    BYTE1,X'E0'         INVALID CML FOR THIS NETWORK                 
         B     NCMLNE                                                           
                                                                                
NCML30   DS    0H                                                               
         CLC   NBACTNET,CMLNET     COMPARE NETWORK SPECIFIC CML                 
         BE    NCMLX                                                            
         BRAS  RE,NEXTEL           GET NEXT NETWORK                             
         BE    NCML10                                                           
         OI    BYTE1,X'E0'         INVALID CML FOR THIS NETWORK                 
         B     NCMLNE                                                           
                                                                                
NCML40   BRAS  RE,NEXTEL           GET NEXT EXCLUDED NETWORK                    
         BE    NCML10                                                           
         B     NCMLX                                                            
                                                                                
NCMLNE   LTR   RB,RB                                                            
         B     *+6                                                              
NCMLX    CR    RB,RB                                                            
         XIT1                                                                   
                                                                                
* RTN TO TRACE I/O'S                                                            
                                                                                
TRACE    TM    UNITSW,UNITSWTR     TRACE REQUESTED                              
         BZR   RE                                                               
         LR    R0,RE                                                            
         ST    RF,TRACEFW          SAVE LAST GOTO1 ADDRESS                      
                                                                                
         BRAS  RE,TRA              GO TO TRACE RTN                              
         LR    RE,R0                                                            
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
DELETED  DC    C'DELETED'                                                       
         EJECT                                                                  
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
                                                                                
VCMLAPR  NMOD1 0,**VAPR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         MVI   ELCODE,X'90'        BORADCAST BUSINESS ELEMENT                   
                                                                                
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    VAPR10                                                           
         CLI   SVTN1PRD,C'Y'       IS THIS BRAND AGENCY                         
         BNE   VAPRX                                                            
         OI    CMLFLAG1,NOAIR      YES, NOT APPROVED TO AIR                     
         B     VAPRX                                                            
                                                                                
         USING CMLBBEL,R6                                                       
VAPR10   DS    0H                                                               
         CLI   CMLBBBAG,C'Y'       IS BRAND AGY=Y                               
         BNE   VAPRX                NO                                          
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VAPR20                                                           
                                                                                
         CLC   SVENDATE,CMLBBMXD UNIT END DATE TO CML MAX USE DTE               
         BNH   VAPR20                                                           
         OI    CMLFLAG1,MAXDTE     MAX DATE ERROR                               
         B     VAPRGX                                                           
                                                                                
VAPR20   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVTN1PRD,C'Y'       IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         BNE   VAPRX                NO, DONE                                    
                                                                                
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         BNZ   *+12                                                             
         OI    CMLFLAG1,CADTE       NO, ERROR                                   
         B     VAPRX                                                            
                                                                                
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BNE   VAPR40                                                           
                                                                                
         OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         BNZ   *+12                                                             
         OI    CMLFLAG1,NOAIR      NO, ERROR                                    
         B     VAPRX                                                            
                                                                                
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
                                                                                
         LA    R2,CMLBBREF                                                      
         BAS   RE,BCMLKEY1         GET COMMERCIAL RECORD                        
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         BE    *+12                                                             
         OI    CMLFLAG1,NOAIR      NOT APPROVED TO AIR                          
         B     VAPRX                                                            
                                                                                
         USING CMLBBEL,R6                                                       
                                                                                
         CLI   CMLATAIR,C'Y'                                                    
         BE    VAPR30                                                           
         OI    CMLFLAG1,NOAIR      NOT APPROVED TO AIR                          
         B     VAPRX                                                            
                                                                                
VAPR30   DS    0H                                                               
         MVC   SVANET,CMLANET      SAVE NET APPROVAL BITS                       
         BAS   RE,GSTATION                                                      
VAPRX    XIT1                                                                   
VAPRGX   XIT1  REGS=(R6)                                                        
                                                                                
VAPR40   DS    0H                                                               
         CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         BE    VAPR30                                                           
         OI    CMLFLAG1,NOAIR      NOT APPROVED TO AIR                          
         B     VAPRX                                                            
                                                                                
* SEE THAT NET IS APPROVED                                                      
                                                                                
GSTATION NTR1                                                                   
         MVC   SVWORK,WORK                                                      
                                                                                
         BRAS  RE,INITNET          CHANGE TO UNIT FILE                          
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATRECD,R4         STATION RECORD                               
         MVC   STATKID,=X'29'      RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
                                                                                
         MVC   STATKNET,NETWORK    VALIDATE THIS NETWORK ONLY                   
         OC    STATKNET,SPACES                                                  
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(7),KEYSAVE      IS THIS IT                                   
         BNE   GSTATX                                                           
                                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'10'                                                     
                                                                                
         BRAS  RE,GETEL                                                         
         BE    GSTAT20                                                          
         DC    H'0'                                                             
                                                                                
         USING STADATEL,R6                                                      
GSTAT20  CLC   STAIDATE,=X'FFFFFF' ANY INACTIVE DATE                            
         BE    GSTAT30              NO                                          
                                                                                
         MVC   IDATE,STAIDATE                                                   
         XC    IDATE,=X'FFFFFF'    INVERT INACTIVE DATE                         
                                                                                
         CLC   IDATE,SVSTDATE      IF INACTIVE BEFORE UNIT START                
         BL    GSTATX                                                           
                                                                                
GSTAT30  DS    0H                                                               
         CLC   STAADATE,SVENDATE   ACTIVE DATE AFTER UNIT END DATE              
         BH    GSTATX               YES                                         
         DROP  R6                                                               
                                                                                
         L     R6,AIO              AIO2                                         
GSTAT40  MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING STACODEL,R6                                                      
                                                                                
         MVC   SVCODE,STACODE      SAVE STATION CODE                            
                                                                                
         MVC   WORK(8),SVANET      APPROVED NETWORKS                            
         NC    WORK(8),SVCODE      AGAINST THIS NETWORK CODE                    
         XC    SVCODE,WORK                                                      
         BZ    GSTATX                                                           
         OI    CMLFLAG1,NOAIR      NOT APPROVED TO AIR                          
                                                                                
GSTATX   DS    0H                                                               
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
         MVC   AIO,AIO1                                                         
                                                                                
         MVC   WORK,SVWORK         RESTORE WORK                                 
         XIT1                                                                   
                                                                                
* BUILD REFERENCE CML RECORD KEY                                                
                                                                                
BCMLKEY1 NTR1                                                                   
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD  & BCLT                                         
         OC    NBACTCLI,NBACTCLI                                                
         BZ    *+10                                                             
         MVC   CMLKCLT,NBACTCLI                                                 
                                                                                
         MVC   CMLKCML,0(R2)       REFERENCE CML                                
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         DROP  R4,R6                                                            
                                                                                
         EJECT                                                                  
* GET PRODUCT NAME *                                                            
                                                                                
PPRDN    NMOD1 0,**PRDN**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         CLC   QPRD,=C'---'                                                     
         BE    PPRDN10                                                          
                                                                                
         CLC   QPRD,=C'???'                                                     
         BE    PPRDN20                                                          
                                                                                
         CLC   SVPRD,QPRD          SAME AS LAST                                 
         BE    PPRDNX                                                           
                                                                                
         MVC   SVPRD,QPRD                                                       
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),NBACTCLI                                                
         MVC   KEY+4(3),QPRD                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         BAS   RE,TRACE1                                                        
                                                                                
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BAS   RE,TRACE1                                                        
                                                                                
         USING PRDHDRD,R1                                                       
         MVC   PRDNM,PNAME                                                      
         B     PPRDNX                                                           
PPRDN10  MVC   PRDNM,=CL20'TO BE ALLOCATED'                                     
         MVC   SVPRD,QPRD                                                       
         B     PPRDNX                                                           
                                                                                
PPRDN20  MVC   PRDNM,=CL20'UNKNOWN PRODUCT'                                     
         MVC   SVPRD,QPRD                                                       
PPRDNX   XIT1                                                                   
         EJECT                                                                  
* RTN TO TRACE I/O'S                                                            
                                                                                
TRACE1   TM    UNITSW,UNITSWTR     TRACE REQUESTED                              
         BZR   RE                                                               
         LR    R0,RE                                                            
         ST    RF,TRACEFW          SAVE LAST GOTO1 ADDRESS                      
                                                                                
         BRAS  RE,TRA              GO TO TRACE RTN                              
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
* PRINT PRODUCT CODE AND SPOT LEN - R6 POINTS TO OUTPUT AREA *                  
                                                                                
PPRD     NMOD1 0,**PPRD**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         LA    R1,PPRDPRD                                                       
         CLC   PPRDPRD,SPACES      ANY PRODUCT CODE                             
         BNH   PPRD12                                                           
         B     PPRD14                                                           
*        LA    R0,NCLSTSIZ                                                      
*        L     R1,ASVNCLST         ADDRESS OF SAVED C LIST (VALICLT)            
*PRD10   CLC   PPRDPRD,0(R1)                                                    
*        BE    PPRD14                                                           
*        LA    R1,4(R1)                                                         
*        CLI   0(R1),C' '                                                       
*        BH    PPRD10                                                           
*                                                                               
*        LA    R1,=C'???'          DELETED PRODUCT                              
*        B     PPRD14                                                           
                                                                                
PPRD12   LA    R1,=C'---'                                                       
                                                                                
PPRD14   MVC   0(3,R6),0(R1)                                                    
         MVC   QPRD,0(R1)                                                       
         CLI   BSLN,0             ANY SPOT LEN                                  
         BE    PPRDX               NO                                           
         LA    R6,2(,R6)                                                        
         CLI   0(R6),C' '                                                       
         BNH   PPRD16                                                           
         LA    R6,1(,R6)                                                        
PPRD16   MVI   0(R6),C'-'                                                       
         EDIT  (B1,BSLN),(3,1(R6)),ALIGN=LEFT                                   
                                                                                
PPRDX    CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    PRDOX                                                            
         XIT1  REGS=(R6)                                                        
PRDOX    XIT1                      DO NOT SV R6 WHEN RUNNING REPORT             
                                                                                
* INITIALIZE AREAS *                                                            
                                                                                
INIT     NMOD1 0,**INIT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         LA    RE,SVSTART                                                       
         AH    RE,=H'6144'        LENGTH OF SAVED AREA                          
         LR    RF,R9                                                            
         AH    RF,=AL2((EDSVSYSD-SYSD)-6144)    START OF SAVED                  
                                                                                
         CR    RE,RF              USE LOWER ADDR                                
         BH    *+6                                                              
         LR    RF,RE                                                            
         ST    RF,ASVSTOR                                                       
         AH    RF,=H'6144'        END OF SAVED AREA                             
         LR    R1,R9                                                            
         AH    R1,=AL2(EDSVSYSD-SYSD)                                           
         CR    RF,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
         LH    R1,=AL2(ENDSYSD-SYSD)                                            
         C     R1,LSYSD                                                         
         BNH   *+6                                                              
         DC    H'0'                USED MORE THAN LSYSD                         
         LM    R0,R1,=A(HEADING,HDHK)     HEADING LINE FOR REPORT               
         A     R0,SPTR2CRR                                                      
         ST    R0,SPECS           STORE FOR CONTROLLER                          
         A     R1,SPTR2CRR                                                      
         ST    R1,HEADHOOK        STORE FOR CONTROLLER                          
                                                                                
         XC    SVNET,SVNET                                                      
         NI    SVFLAG,X'FF'-NETCHGSW  INIT NETWORK CHANGED                      
*                                                                               
* SET UP SORT PARAMS AT RUN-TIME (SO THEY CAN BE CHANGED SILLY)                 
*                                                                               
         MVI   SORTCARD+13,C'1'    START SORT COL                               
         LA    R4,SKEYLEN          L'SORT KEY                                   
         LA    R5,SRTLN            L'SORT RECORD                                
*                                                                               
         TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BZ    INIT10                                                           
         LA    R4,SKEYLEN2         L'SORT KEY                                   
         LA    R5,SRTLN2           L'SORT RECORD                                
         CLI   OFFLINE,C'Y'                                                     
         BE    INIT20                                                           
         MVI   ERROR,NOTONLIN                                                   
         GOTO1 ERREX                                                            
*                                                                               
INIT10   CLI   OPTPOSF,0          SORT ON POS                                   
         BE    INITX               NO SORTING                                   
         CLI   OFFLINE,C'Y'        BETTER BE OFFLINE                            
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
INIT20   CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB                                               
                                                                                
         CVD   R5,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB                                                
                                                                                
*RR01A   LA    RF,SORTCARD+13      EDIT EXPRESSIONS CANT EXCEED 8 CHARS         
*        EDIT  (R1),(1,(RF)),      SET KEY START POSITION                       
*        LA    RF,SORTCARD+15                                                   
*        EDIT  (R4),(3,(RF)),FILL=0  SET L'SORT_KEY                             
*        LA    RF,RECCARD+21                                                    
*        EDIT  (R5),(3,(RF)),FILL=0  SET L'SORT_REC                             
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
*                                                                               
                                                                                
* SET ON RECOVER OFFLINE DISK UPDATE COPIES                                     
                                                                                
         L     RE,TWAMASTC                                                      
         L     RF,MCSSB-MCBLOCK(RE)                                             
         OI    SSBSTAT2-SSBD(RF),SSBSROLC                                       
                                                                                
         TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BZ    INITX                                                            
                                                                                
         BRAS  RE,MEDSRT                                                        
                                                                                
INITX    XIT1                                                                   
RECCARD  DC    C'RECORD TYPE=F,LENGTH=XXX  '                                    
SORTCARD DC    C'SORT FIELDS=(X,XXX,A),FORMAT=BI,WORK=1  '                      
                                                                                
* VALIDATE NETWORK                                                              
                                                                                
VNET     NMOD1 0,**VNET**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VNETX                NO                                          
         GOTO1 ANY                                                              
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),WORK                                                 
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    VNET10                                                           
         MVC   GERROR,=Y(NONET)                                                 
         GOTO1 VTRAERR             USING GETTXT CALL                            
                                                                                
VNET10   MVC   NETWORK,WORK                                                     
         L     R4,AIO                                                           
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,BNET                                                          
                                                                                
* PER MTG 7/16/08 DECIDED TO DUMP IF NEW TRFTYPE IS NOT SET                     
         CLI   STRTYPE,C' '                                                     
         BH    VNET12                                                           
*                                                                               
         CLC   =C'SJ',AGENCY       IS AGENCY SJR                                
         BNE   *+12                                                             
         MVI   SVMEDIA,C'N'        NET IS DEFAULT                               
         B     VNET12C                                                          
*                                                                               
         DC    H'0'                TELL NELSON HE'S GOT A BUG                   
*                                                                               
VNET12   MVC   SVMEDIA,STRTYPE     (N,C,S,O,H,D)                                
*                                                                               
VNET12C  CLI   STRTYPE,C'N'                                                     
         BE    VNETX                                                            
         CLI   STRTYPE,C'C'                                                     
         BE    VNETX                                                            
         CLI   STRTYPE,C'S'                                                     
         BE    VNETX                                                            
         CLI   STRTYPE,C'O'                                                     
         BE    VNETX                                                            
         MVI   SVMEDIA,C'N'        DEFAULT                                      
                                                                                
VNETX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PROGRAM *                                                            
                                                                                
VPROG    NMOD1 0,**VPROG*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VPROGX                                                           
                                                                                
         OC    NETWORK,NETWORK                                                  
         BNZ   *+12                                                             
         LA    R2,TRANETH                                                       
         B     MISSERRP                                                         
                                                                                
         GOTO1 ANY                                                              
         MVC   PROGRAM,WORK                                                     
                                                                                
         LA    R0,EQVPTBL                                                       
         LA    R1,L'EQVPTBL                                                     
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         BRAS  RE,GTEQV                                                         
                                                                                
         MVC   BYTE,STDATE                                                      
         MVI   STDATE,0            VLD PRG REGARDLESS OF DATE FIRST             
VPROG10  BRAS  RE,NETI                                                          
                                                                                
         MVI   NBSELPST,C'B'                                                    
VPROG20  OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    VPROG24                                                          
         DC    H'0'                                                             
VPROG24  CLI   NBMODE,NBREQLST                                                  
         BE    VPROG30                                                          
         CLI   NBMODE,NBPROCUN                                                  
         BNE   VPROG20                                                          
                                                                                
         CLI   STDATE,0                                                         
         BNE   *+14                                                             
         MVC   STDATE(1),BYTE                                                   
         B     VPROG10                                                          
                                                                                
         L     R6,NBAIO                                                         
         BRAS  RE,CNVRTP                                                        
                                                                                
         CLC   NBACTPRG,PROGRAM                                                 
         BNE   VPROG20                                                          
                                                                                
VPROGX   DS    0H                                                               
         CLI   STDATE,0                                                         
         BNE   *+10                                                             
         MVC   STDATE(1),BYTE                                                   
         XIT1                                                                   
                                                                                
* LOOK FOR ANY OTHER EQV PROGS *                                                
                                                                                
VPROG30  LA    R5,EQVPTBL                                                       
         LA    R6,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,R5                                                      
         OC    EQVNET,EQVNET       EQUVIVALENCY RECORDS NOT BEING USED          
         BZ    VPROGX                                                           
                                                                                
VPROG34  OC    EQVNET,EQVNET       AT END OF ENTRIES                            
         BZ    VPROG35              ERROR                                       
                                                                                
         CLI   EQVUSED,C'Y'                                                     
         BNE   VPROG36                                                          
                                                                                
         LA    R5,EQVNEXT                                                       
         BCT   R6,VPROG34                                                       
                                                                                
VPROG35  DS    0H                                                               
         CLI   NBSELSTR,0                                                       
         BE    *+18                                                             
         MVC   GERROR,=Y(NOUNTPER) NO UNIT FOR THIS PERIOD                      
         LA    R2,TRAPERH                                                       
         B     *+14                                                             
         MVC   GERROR,=Y(NOPROG)                                                
         LA    R2,TRAPROGH                                                      
         GOTO1 VTRAERR             USING GETTXT CALL                            
                                                                                
VPROG36  MVC   PROGRAM,EQVEPROG                                                 
         MVI   EQVUSED,C'Y'                                                     
         B     VPROG10                                                          
                                                                                
MISSERRP MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE PERIOD - MONTH AND YR, AND DEVELOP                                   
*                   CALENDAR OR BROADCAST START/END DATES                       
*                                  BROADCAST MONTH                              
                                                                                
VPER     NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
* FORCE TN2 PROFILE TO BE INDICATOR OF BRD/CAL/WEEKLY                           
                                                                                
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    VPER02                                                           
         CLI   LCTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    VPER02                                                           
         CLI   LCTN2PR6,C'W'       TRAFFIC OVERRIDE WEEKLY                      
         BE    VPER02                                                           
                                                                                
         MVI   LCTN2PR6,C'C'       FORCE TO CALENDAR                            
                                                                                
         CLI   NBUSER+4,C'B'       BROADCAST MONTH                              
         BNE   VPER02                                                           
         MVI   LCTN2PR6,C'B'       FORCE TO BROADCAST                           
                                                                                
VPER02   XC    DATES,DATES                                                      
         SR    R5,R5                                                            
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VPER50                                                           
                                                                                
         LA    R4,TRAPER                                                        
         CLI   TRAPER,C'?'         QUESTION MARK HELP                           
         BNE   VPER04                                                           
         LA    R4,1(,R4)                                                        
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         STC   RF,5(R2)                                                         
                                                                                
* SEE IF FROM-TO DATES, IF NOT CHECK PERIOD *                                   
                                                                                
VPER04   GOTO1 DATVAL,DMCB,(0,(R4)),STDATE                                      
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    VPER10              CK FOR PERIOD                                
                                                                                
         CLM   R6,1,5(R2)          WAS THERE ONLY 1 DATE                        
         BNE   *+14                 NO                                          
         MVC   ENDATE,STDATE       END DATE IS SAME AS START DATE               
         B     VPER06                                                           
                                                                                
         LA    R4,1(R6,R4)                                                      
         GOTO1 DATVAL,(R1),(0,(R4)),ENDATE                                      
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERRA             NO                                           
                                                                                
         FIXDT02                                                                
VPER06   GOTO1 DATCON,(R1),(0,STDATE),(2,STDATEP)                               
         FIXDT02                                                                
         GOTO1 (RF),(R1),(0,ENDATE),(2,ENDATEP)                                 
         CLC   STDATEP,ENDATEP                                                  
         BNH   VPER94                                                           
         MVC   GERROR,=Y(DATSEQER)                                              
         B     VPTRAP2                                                          
         EJECT                                                                  
* MUST ENTER DATE - THEN MAY SHOW CALENDAR/BROADCAST START/END *                
* OR WILL PROCEED WITH REQUEST IF NO QUESTION MARK ENTERED     *                
                                                                                
VPER10   GOTO1 DATVAL,DMCB,(2,(R4)),STDATE                                      
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERRA             ERROR, SEE IF FROM/TO DATES                  
                                                                                
         CLM   R6,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER12                                                           
         AR    R4,R6                                                            
         CLI   0(R4),C'-'                                                       
         BNE   DATERRA                                                          
         LA    R4,1(,R4)                                                        
         GOTO1 (RF),(R1),(0,(R4)),ENDATE  DATVAL MO/DA/YR                       
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BNZ   VPER12               YES                                         
         GOTO1 (RF),(R1),(2,(R4)),ENDATE  DATVAL PERIOD                         
         L     R6,DMCB                                                          
         LTR   R6,R6               WAS DATE VALID                               
         BZ    DATERRA             ERROR, SEE IF FROM/TO DATES                  
                                                                                
VPER12   GOTO1 DATCON,(R1),(0,STDATE),(3,PERIOD)                                
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    VPER20                                                           
         CLI   LCTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    VPER14                                                           
         CLI   LCTN2PR6,C'W'       TRAFFIC OVERRIDE WEEKLY                      
         BE    VPER14                                                           
         DC    H'0'                                                             
                                                                                
* CALC CALENDAR START/END OF MONTH DATES *                                      
                                                                                
VPER14   MVC   STDATE+4(2),=C'01'                                               
         ICM   R5,15,STDATE                                                     
                                                                                
         OC    ENDATE,ENDATE         END PERIOD ENTERED                         
         BZ    *+14                   NO                                        
         CLC   ENDATE+4(2),=C'00'    END DATE ENTERED (NOT PERIOD)              
         BNE   VPER18                 YES                                       
                                                                                
         OC    ENDATE,ENDATE         END PERIOD ENTERED                         
         BZ    *+10                   NO                                        
         MVC   STDATE(4),ENDATE                                                 
                                                                                
         GOTO1 ADDAY,(R1),STDATE,ENDATE,F'31'                                   
VPER16   GOTO1 (RF),(R1),ENDATE,ENDATE,F'-1'                                    
         CLC   STDATE(4),ENDATE                                                 
         BNE   VPER16                                                           
         STCM  R5,15,STDATE                                                     
                                                                                
VPER18   B     VPER30                                                           
         EJECT                                                                  
* GET BROADCAST MONTH                                                           
                                                                                
VPER20   MVC   STDATE+4(2),=C'15'                                               
         MVC   WORK(12),STDATE                                                  
         GOTO1 VGTBROAD,DMCB,(1,WORK),STDATE,GETDAY,ADDAY                       
                                                                                
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         OC    WORK+6(6),WORK+6    END PERIOD ENTERED                           
         BZ    VPER30               NO                                          
                                                                                
         CLC   WORK+10(2),=C'00'   END DATE ENTERED (NOT PERIOD)                
         BNE   VPER22               YES                                         
                                                                                
         CLC   WORK(4),WORK+6      START AND END PERIOD SAME?                   
         BE    VPER30                                                           
                                                                                
         MVC   WORK(6),STDATE                                                   
         GOTO1 (RF),(R1),(1,WORK+6),STDATE,GETDAY,ADDAY                         
                                                                                
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   STDATE,WORK                                                      
         B     VPER30                                                           
                                                                                
VPER22   MVC   ENDATE,WORK+6       RESTORE END DATE ENTERED                     
                                                                                
         FIXDT02                                                                
VPER30   GOTO1 DATCON,(R1),(0,STDATE),(2,STDATEP)                               
         FIXDT02                                                                
         GOTO1 (RF),(R1),(0,ENDATE),(2,ENDATEP)                                 
                                                                                
         CLC   STDATEP,ENDATEP     DATES MUST BE ENTERED IN SEQUENCE            
         BNH   VPER40                                                           
         MVC   GERROR,=Y(DATSEQER)                                              
         B     VPTRAP2                                                          
                                                                                
VPER40   CLI   TRAPER,C'?'         QUESTION MARK HELP                           
         BNE   VPER94                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=CL31'PERIOD IS CALENDAR  MONTH FROM'                
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    VPER42                                                           
         CLI   LCTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    VPER44                                                           
         CLI   LCTN2PR6,C'W'       TRAFFIC OVERRIDE WEEKLY                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CONHEAD+10(6),=C'WEEKLY'                                         
         MVC   CONHEAD+16(15),SPACES                                            
         B     VPERERX2                                                         
                                                                                
VPER42   MVC   CONHEAD+10(9),=CL9'BROADCAST'                                    
                                                                                
         FIXDT02                                                                
VPER44   GOTO1 DATCON,DMCB,(2,STDATEP),(5,CONHEAD+32)                           
         MVC   CONHEAD+41(2),=C'TO'                                             
         FIXDT02                                                                
         GOTO1 (RF),(R1),(2,ENDATEP),(5,CONHEAD+44)                             
VPERERX2 GOTO1 ERREX2                                                           
         EJECT                                                                  
* CALCULATE THREE OR N MONTHS FOR BUY ACTIVITY *                                
                                                                                
VPER50   TM    WHEN,X'C0'          IMMED/NOW                                    
         BNZ   MISSERRA            ERROR                                        
                                                                                
*NOP     TM    UNITSW,UNITSWAC     THIS BUY ACTIVITY                            
*        BZ    VPERX                                                            
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(0,STDATE)                                     
         CLI   SVTAPROF+5,0        ANY DATE ADJUSTMENT                          
         BE    VPER56               NO                                          
                                                                                
         CLI   SVTAPROF+5,1        IF ONE, DONE                                 
         BE    VPER54                                                           
         ZIC   R0,SVTAPROF+5       GET MONTHS TO GO BACK                        
         BCTR  R0,0                                                             
         MH    R0,=H'30'                                                        
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),STDATE,STDATE,(R0)                                    
                                                                                
VPER54   MVC   STDATE+4(2),=C'01'                                               
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BNE   VPER56                                                           
         MVC   STDATE+4(2),=C'15'                                               
         GOTO1 VGTBROAD,DMCB,(1,STDATE),WORK,GETDAY,ADDAY                       
         MVC   STDATE,WORK                                                      
                                                                                
         FIXDT02                                                                
VPER56   GOTO1 DATCON,(R1),STDATE,(2,STDATEP)                                   
                                                                                
         LA    R4,3                STANDARD 3 MONTHS IN FUTURE                  
         CLI   SVTAPROF+6,0                                                     
         BE    *+8                                                              
         IC    R4,SVTAPROF+6       GET NON-STD                                  
                                                                                
         GOTO1 (RF),(R1),(5,0),(0,WORK)                                         
                                                                                
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    VPER80                                                           
         CLI   LCTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    VPER60                                                           
         CLI   LCTN2PR6,C'W'       TRAFFIC OVERRIDE WEEKLY                      
         BE    VPER60                                                           
         DC    H'0'                                                             
                                                                                
VPER60   MVC   WORK+4(2),=C'01'                                                 
                                                                                
VPER62   GOTO1 ADDAY,(R1),WORK,ENDATE,F'31'                                     
                                                                                
VPER64   GOTO1 (RF),(R1),ENDATE,ENDATE,F'-1'                                    
         CLC   WORK(4),ENDATE                                                   
         BNE   VPER64                                                           
         GOTO1 (RF),(R1),ENDATE,WORK,F'1'                                       
         BCT   R4,VPER62                                                        
         B     VPER90                                                           
         EJECT                                                                  
VPER80   GOTO1 VGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
                                                                                
VPER84   GOTO1 ADDAY,(R1),WORK+6,WORK+6,F'7' FORCE TO CAL MONTH                 
         MVC   WORK(6),WORK+12                                                  
         GOTO1 ADDAY,(R1),WORK,WORK,F'1'                                        
         GOTO1 VGTBROAD,(R1),(1,WORK),WORK+6,GETDAY,ADDAY                       
         BCT   R4,VPER84                                                        
                                                                                
         MVC   ENDATE,WORK+12                                                   
                                                                                
         FIXDT02                                                                
VPER90   GOTO1 DATCON,(R1),(0,ENDATE),(2,ENDATEP)                               
                                                                                
VPER94   GOTO1 DATCON,DMCB,(0,STDATE),(3,STDATE3)                               
         GOTO1 (RF),(R1),(0,ENDATE),(3,ENDATE3)                                 
                                                                                
         GOTO1 GETDAY,(R1),(0,STDATE),WORK                                      
         MVC   WORK(6),STDATE                                                   
         CLI   0(R1),1             IF MONDAY                                    
         BE    VPER96               DONE                                        
         ZIC   R3,DMCB                                                          
         BCTR  R3,0                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,(R1),STDATE,WORK,(R3)                                      
         FIXDT02                                                                
VPER96   GOTO1 DATCON,(R1),(0,WORK),(2,STDATEPM)                                
                                                                                
VPERX    XIT1                                                                   
         EJECT                                                                  
DATERRA  MVC   GERROR,=Y(DATPERER)                                              
VPTRAP2  GOTO1 VTRAERR             USING GETTXT CALL                            
                                                                                
MISSERRA MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE OPTIONS *                                                            
                                                                                
VOPT     NMOD1 0,**VOPT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         LA    RE,OPTIONS                                                       
         LA    RF,ENDOPT-OPTIONS                                                
         XCEF                                                                   
         LA    R2,TRAOPTH                                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT94              NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPT06              YES                                          
         CLI   5(R2),4                                                          
         BNH   *+12                                                             
         LA    R1,4                                                             
         B     VOPT04                                                           
         ZIC   R1,5(R2)                                                         
VOPT04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VOPT08                                                           
VOPT06   DS    0H                                                               
         MVC   GERROR,=Y(HELPMSG)                                               
         B     VOPTTRAP                                                         
                                                                                
VOPT08   GOTO1 SCANNER,DMCB,(20,TRAOPTH),(5,BLOCK+64)                           
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRB            NO                                           
         LA    R4,BLOCK+64         ADDRESS OF FIRST BLOCK                       
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
                                                                                
         CLI   ACTNUM,29                                                        
         BNE   VOPT30                                                           
         LA    R5,OPTTABLE                                                      
         LA    R6,OPTGRENT                                                      
VOPT25   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,VOPTCLC                                                       
         BE    VOPT28                                                           
         LA    R5,L'OPTTABLE(R5)                                                
         BCT   R6,VOPT25                                                        
         B     VOPT30                                                           
VOPT28   DS    0H                                                               
         MVC   GERROR,=Y(GRIDOPT)                                               
         B     VOPTTRAP                                                         
                                                                                
                                                                                
VOPT30   LA    R5,OPTTABLE                                                      
         CLI   ACTNUM,29                                                        
         BNE   *+8                                                              
         LA    R5,OPTTGRID                                                      
         EX    R1,VOPTCLC                                                       
         BE    VOPTGO                                                           
         LA    R5,L'OPTTABLE(R5)                                                
         CLI   0(R5),X'FF'                                                      
         BNE   *-16                                                             
         CLI   ACTNUM,29                                                        
         BNE   *+8                                                              
         B     VOPTGRD             NOT A VALID GRID OPTION                      
         B     VOPTHLP             BAD OPTION                                   
VOPTCLC  CLC   12(0,R4),0(R5)                                                   
VOPTGO   L     RE,10(R5)                                                        
         A     RE,SPTR2CRR                                                      
         BR    RE                                                               
                                                                                
* SHOW MULTI-RUN UNITS                                                          
                                                                                
VOPTMR   OI    OPTNSW1,OPTMR        SET ON                                      
         B     VOPT90                                                           
                                                                                
* EP (SHOW EST AND PACKAGE)                                                     
                                                                                
VOPTEP   OI    OPTNSW,OPTEP        SET ON                                       
         B     VOPT90                                                           
                                                                                
* REV (SHOW REVISION NUMBER)                                                    
                                                                                
VOPTREV  OI    OPTNSW,OPTRV        SET ON                                       
         B     VOPT90                                                           
                                                                                
* PRINT COMMERCIAL THAT NEEDS REASSIGN                                          
                                                                                
VOPTREAS DS    0H                                                               
         TM    WHEN,X'78'          NOW,SOON,OV                                  
         BZ    VOPTHLPR             YES, ERROR                                  
                                                                                
         OI    SVFLAG,PREASGN                                                   
         B     VOPT90                                                           
                                                                                
* PRINT ADID ON REPORT (OFFLINE ONLY)                                           
                                                                                
VOPTPADI DS    0H                                                               
         TM    WHEN,X'80'          ONLINE                                       
         BO    VOPTHLPA             YES, ERROR                                  
                                                                                
         CLI   1(R4),0             LEN OF SECOND HALF                           
         BNE   VOPTHLP                                                          
                                                                                
         OI    OPTNSW1,OPTPADID                                                 
         B     VOPT90                                                           
                                                                                
* FILTER ON ADID CML                                                            
                                                                                
VOPTADID DS    0H                                                               
         CLI   1(R4),9             ADID IS 9-12 CHAR                            
         BL    BDADIDER                                                         
                                                                                
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
                                                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            ADDRESS OF TRPACK                            
                                                                                
         GOTO1 (RF),DMCB,(C'P',22(R4)),SVADIDP                                  
         BNE   INVADIER                                                         
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(8),SVADIDP                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVADIER                                                         
                                                                                
         MVC   OPTADID,22(R4)      SAVE 12 CHAR ADID                            
         B     VOPT90                                                           
                                                                                
* SHOW ALL UNITS (ALLUNITS)                                                     
                                                                                
VOPTALLU TM    WHEN,X'80'          ONLINE                                       
         BZ    VOPTHLP             BAD OPTION                                   
         OI    OPTNSW1,OPTALLU     SHOW ALL UNITS                               
         B     VOPT90                                                           
                                                                                
* PRD (PRODUCT)                                                                 
                                                                                
VOPTPRD  CLC   =C'POL',22(R4)      IS PRD=POL                                   
         BE    PRDERR              YES, ERROR                                   
         CLC   =C'ALL',22(R4)      IS PRD=ALL                                   
         BE    PRDERR              YES, ERROR                                   
                                                                                
         BAS   RE,VOP              GO VALIDATE OPTION PRODUCT                   
         B     VOPT90                                                           
                                                                                
* NTYPE (NETWORK TYPE)                                                          
                                                                                
VOPTNTYP CLI   1(R4),1             MUST BE 1 CHAR                               
         BNE   VOPT31               ERROR                                       
         CLI   22(R4),C'N'         NETWORK                                      
         BE    VOPT32                                                           
         CLI   22(R4),C'C'         CABLE                                        
         BE    VOPT32                                                           
         CLI   22(R4),C'S'         SYNDICATION                                  
         BE    VOPT32                                                           
         CLI   22(R4),C'O'         OTHER                                        
         BE    VOPT32                                                           
VOPT31   MVC   GERROR,=Y(BDNETTYP)                                              
         B     VOPTTRAP                                                         
                                                                                
VOPT32   MVC   OPTNTYP,22(R4)      SAVE IT                                      
         CLI   TRANETH+5,0         NETWORK ENTERED?                             
         BNE   VOPT90               YES                                         
         MVC   SVMEDIA,OPTNTYP                                                  
         B     VOPT90                                                           
                                                                                
* CML (COMMERCIAL)                                                              
                                                                                
VOPTCML  CLI   1(R4),8             OR 8 CHAR                                    
         BNL   VOPT35                                                           
         MVC   GERROR,=Y(NOT812)                                                
         B     VOPTTRAP                                                         
                                                                                
VOPT35   XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            ADDRESS OF TRPACK                            
         GOTO1 (RF),DMCB,(C'P',22(R4)),WORK                                     
         BNE   INVADIER                                                         
                                                                                
VOPT36   XC    KEY(13),KEY                                                      
         LA    R1,KEY                                                           
         USING CMLKEY,R1                                                        
         MVC   CMLKID,=XL2'0AC1'                                                
         MVC   CMLKAM(3),BAGYMD                                                 
         MVC   CMLKCML,WORK                                                     
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   VCMLERR                                                          
                                                                                
         L     R6,AIO                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         XC    SVADIDP,SVADIDP     ADID PACKED                                  
                                                                                
         MVI   ELCODE,X'A0'        ADID ELEMENT                                 
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVADIDP,14(R6)      SAVE PACKED ADID                             
                                                                                
         MVC   OPTCML,22(R4)                                                    
         B     VOPT90                                                           
                                                                                
* LEN (SPOT LENGTH)                                                             
                                                                                
VOPTLEN  TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERR                                                           
         MVC   ELEM,TRAOPTH                                                     
         PACK  ELEM+4(1),3(1,R4)  NUM, ALPHA, HEX BITS                          
         MVC   ELEM+5(1),1(R4)    DATA LEN                                      
         MVC   ELEM+8(10),22(R4)     SPOT LEN                                   
         MVI   ERROPT,C'Y'                                                      
         LA    R2,ELEM                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAOPTH                                                       
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   VOPTERX             GO PRINT ERROR                               
         MVC   OPTSLN,WORK                                                      
         B     VOPT90                                                           
                                                                                
* UNITS (B=BOTH, N=NETWORK, T=TRAFFIC)                                          
                                                                                
VOPTUNT  CLI   22(R4),C'B'                                                      
         BE    VOPT54                                                           
         CLI   22(R4),C'T'                                                      
         BE    VOPT54                                                           
         CLI   22(R4),C'N'                                                      
         BE    VOPT54                                                           
                                                                                
         MVC   GERROR,=Y(UNITERR)                                               
         B     VOPTTRAP                                                         
                                                                                
VOPT54   MVC   UNITOPT,22(R4)                                                   
         B     VOPT90                                                           
                                                                                
* ACTIVITY                                                                      
                                                                                
VOPTACT  OI    UNITSW,UNITSWAC     SET ON BUY ACTIVITY REPORT                   
                                                                                
VOPTACT2 TM    UNITSW,X'0C'        BUY ACTIVITY AND MINUS UNITS ?               
         BO    ACTUNTER             YES, ERROR                                  
                                                                                
         B     VOPT90                                                           
                                                                                
* SHOW ALL NEGATIVE UNITS                                                       
                                                                                
VOPTNEG  OI    UNITSW,UNITSWMI     SET ON UNIT MINUS SWITCH                     
         B     VOPTACT2                                                         
                                                                                
* AUTO T/A NO PRINT                                                             
                                                                                
VOPTNOP  TM    WHEN,X'C0'          IMMED/NOW                                    
         BNZ   VOPTHLP                                                          
         OI    GENSTAT2,NOREQDET   DO NOT PRINT REQUEST DETAILS                 
         OI    UNITSW,UNITSWNP     DO NOT PRINT AUTO T/A REPORT                 
         B     VOPT90                                                           
                                                                                
* SHOW ALL PATTERN REF #'S AND OTHER INFO                                       
                                                                                
VOPTPAT  OI    UNITSW,UNITSWPT     SET ON DISPLAY PATTERN INFO                  
         B     VOPT90                                                           
                                                                                
* UNASSIGNED                                                                    
                                                                                
VOPTUNA  OI    UNITSW,UNITSWAC+UNITSWUN SET ON BUY ACT & UNASSIGNED             
         B     VOPT90                                                           
                                                                                
* TRACE                                                                         
                                                                                
VOPTTRC  CLI   OFFLINE,C'Y'        IF OFFLINE, OK                               
         BE    *+12                                                             
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   VOPTHLP              NO, ONLY VALID FOR DDS                      
                                                                                
         OI    UNITSW,UNITSWTR     SET ON TRACE OPTION                          
         B     VOPT90                                                           
                                                                                
* POS=                                                                          
                                                                                
VOPTPOS  MVC   OPTPOS,22(R4)                                                    
         B     VOPT90                                                           
                                                                                
* POSF=                                                                         
                                                                                
VOPTPOSF CLI   1(R4),0                                                          
         BE    MISSERRB                                                         
         MVC   OPTPOSF,22(R4)                                                   
         CLC   =C'ALL',22(R4)      POS FILTER ALL                               
         BNE   *+8                                                              
         MVI   OPTPOSF,X'FF'                                                    
         TM    WHEN,X'C0'                                                       
         BZ    VOPT90                                                           
         MVI   ERROR,NOTONLIN                                                   
         B     VOPTERX                                                          
                                                                                
* DPT=                                                                          
                                                                                
VOPTDPT  DS    0H                                                               
         ZIC   R1,1(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OPTDPT(0),22(R4)                                                 
         OC    OPTDPT,SPACES                                                    
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),22(R4)       DAYPART                                      
         LA    R1,1(R1)                                                         
         STC   R1,FLDH+5           DATA LEN                                     
         LA    R2,FLDH                                                          
                                                                                
         XC    GERROR,GERROR                                                    
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALIDPT             VALIDATE DAYPART CODE                        
         MVI   ERROPT,0                                                         
         OC    GERROR,GERROR                                                    
         BZ    VOPT90                                                           
                                                                                
         LA    R2,TRAOPTH                                                       
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         STCM  R1,7,GASUBST                                                     
         MVI   DUB,8               L'SUBST TEXT + 1                             
         MVC   DUB+1(7),=C'DAYPART'                                             
         GOTO1 VTRAERR                                                          
                                                                                
* CCN (SHOW ONLY EST COPY CODE=N)                                               
                                                                                
VOPTCCN  OI    OPTNSW,OPTCCN                                                    
         B     VOPT90                                                           
                                                                                
VOPTRESG OI    OPTNSW1,OPTRESG                                                  
         B     VOPT90                                                           
                                                                                
*                                                                               
* INCLUDE DIGITAL BUYS (SUBMEDIA V) - OVERRIDE TN2+14 IF SET                    
VOPTDIGI OI    OPTNSW1,OPTDIGI                                                  
         B     VOPT90                                                           
                                                                                
*                                                                               
* BUY (ONLY SHOW BASE PROG CODE)                                                
                                                                                
VOPTBUY  OI    OPTNSW,OPTBASE                                                   
         B     VOPT90                                                           
                                                                                
* DSKAD (SHOW DISK ADDR IN REPORT)                                              
                                                                                
VOPTDSK  CLI   OFFLINE,C'Y'        IF OFFLINE, OK                               
         BE    *+12                                                             
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   VOPTHLP              NO, ONLY VALID FOR DDS                      
                                                                                
         OI    OPTNSW,OPTDSKAD                                                  
         B     VOPT90                                                           
                                                                                
* NAME (SHOW PROG NAME, NOT CODE)                                               
                                                                                
VOPTNAM  OI    OPTNSW,OPTPNAME                                                  
         B     VOPT90                                                           
                                                                                
* PGRP (BY PRODUCT GROUP)                                                       
                                                                                
VOPTPGRP BAS   RE,VPGR                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VOPTPGR5                                                         
         LA    R1,=C'DMWRT '                                                    
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)                                                 
         MVI   DMCB+8,1                                                         
         ICM   RF,12,=C'L='                                                     
         ICM   RF,3,=Y(OPTPLEN)                                                 
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,AOPTPGRL,,(RF)                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
VOPTPGR5 DS    0H                                                               
         B     VOPT90                                                           
                                                                                
* BB (FILTER ON BB ONLY)                                                        
                                                                                
VOPTBB   OI    OPTNSW,OPTBB                                                     
         B     VOPT90                                                           
                                                                                
* TS - FILTER ON TRAFFIC SUPPLIER                                               
                                                                                
VOPTTS   XC    SVTS(6),SVTS        CLEAR TRAFFIC SUPPLIER FIELD                 
         OI    OPTNSW1,OPTTS                                                    
         CLI   1(R4),0             FILTER ON TRAFFIC SUPPLIER                   
         BE    VOPT90                                                           
         CLI   1(R4),5                                                          
         BH    TSUPERR                                                          
         MVC   SVTSLEN,1(R4)       SAVE LEN AND                                 
         MVC   SVTS,22(R4)          TRAFFIC SUPPLIER                            
         B     VOPT90                                                           
                                                                                
* -TS - FILTER ON NON-TRAFFIC SUPPLIER                                          
                                                                                
VOPTNTS  DS    0H                                                               
         CLI   1(R4),0             -TS=XXXXX IS ILLIGAL                         
         BNE   VOPTHLP             BAD OPTION                                   
         OI    OPTNSW1,OPTNTS                                                   
         B     VOPT90                                                           
                                                                                
* ROT - SHOW ROTATION DAY                                                       
                                                                                
VOPTROT  OI    OPTNSW1,OPTROT                                                   
                                                                                
VOPT90   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
VOPT94   CLI   UNITOPT,0                                                        
         BNE   VOPTX                                                            
         MVI   UNITOPT,C'B'                                                     
VOPTX    XIT1                                                                   
         EJECT                                                                  
* VALIDATE PRODUCT & POSSIBLE PARTNER PAIR *                                    
                                                                                
VOP      NTR1                                                                   
         CLI   1(R4),2                                                          
         BL    PRDERR                                                           
         CLI   1(R4),7                                                          
         BH    PRDERR                                                           
         XC    DUB,DUB                                                          
         ZIC   R0,1(R4)                                                         
         LA    R1,22(,R4)                                                       
         LA    RE,DUB                                                           
         SR    RF,RF                                                            
VOP10    CLI   0(R1),C'/'                                                       
         BE    VOP14                                                            
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         CH    RF,=H'3'                                                         
         BH    PRLENER                                                          
         BCT   R0,VOP10                                                         
VOP14    LA    R1,1(,R1)                                                        
         MVI   0(RE),C' '                                                       
         CH    RF,=H'2'                                                         
         BL    PRLENER                                                          
         LA    RE,DUB+4                                                         
         SR    RF,RF                                                            
         LTR   R0,R0                                                            
         BZ    VOP20                                                            
         BCTR  R0,0                                                             
                                                                                
VOP16    MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         CH    RF,=H'3'                                                         
         BH    PRLENER                                                          
         BCT   R0,VOP16                                                         
         MVI   0(RE),C' '                                                       
         CH    RF,=H'2'                                                         
         BL    PRLENER                                                          
                                                                                
VOP20    DS    0H                                                               
         TM    SECFLAG,NECONPRD                                                 
         BO    VOP50                                                            
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
         LA    RF,DUB                                                           
VOP24    CLI   0(R1),0             AT END OF TABLE?                             
         BNH   INVPRDER             YES, ERROR                                  
         CLC   0(3,R1),DUB         THIS A VALID PROD CODE                       
         BE    VOP26                                                            
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,VOP24                                                         
         DC    H'0'                                                             
VOP26    MVC   OPTPROD(4),0(R1)    PROD AND BPRD                                
         OC    DUB+4(4),DUB+4                                                   
         BZ    VOPTX                                                            
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
         LA    RF,DUB+4                                                         
VOP30    CLI   0(R1),0             AT END OF TABLE?                             
         BNH   INVPRDER             YES, ERROR                                  
         CLC   0(3,R1),DUB+4       THIS A VALID PTR CODE                        
         BE    VOP34                                                            
         LA    R1,4(,R1)           BUMP PROD PTR                                
         BCT   R0,VOP30                                                         
         DC    H'0'                                                             
VOP34    MVC   OPTPROD2(4),0(R1)    PROD AND BPRD                               
                                                                                
         B     VOP90                                                            
                                                                                
VOP50    DS    0H                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,X'03'                                                     
         MVC   FLD(3),DUB                                                       
         MVI   ERROPT,C'Y'                                                      
         LR    R0,R2                                                            
         LA    R2,FLDH                                                          
         GOTO1 VALIPRD                                                          
         LR    R2,R0                                                            
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   VOPERR                                                           
         MVC   OPTPROD,FLD                                                      
         MVI   OPTPRD,0                                                         
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,X'03'                                                     
         MVC   FLD(3),DUB+4                                                     
         MVI   ERROPT,C'Y'                                                      
         LR    R0,R2                                                            
         LA    R2,FLDH                                                          
         GOTO1 VALIPRD                                                          
         LR    R2,R0                                                            
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   VOPERR                                                           
         XC    OPTPROD2,FLD                                                     
         MVI   OPTPRD2,0                                                        
         B     VOP90                                                            
                                                                                
VOPERR   MVC   GERROR,=Y(INVPRDE)                                               
         XC    WORK,WORK                                                        
         MVI   WORK,4                                                           
         MVC   WORK+1(3),=CL3'XXX'                                              
         LA    R1,WORK                                                          
         STCM  R1,7,GASUBST                                                     
         GOTO1 VTRAERR                                                          
                                                                                
VOP90    DS    0H                                                               
         CLC   OPTPROD,OPTPROD2                                                 
         BE    EQPRDER                                                          
         B     VOPTX                                                            
         EJECT                                                                  
* VALIDATE PRODUCT GROUP *                                                      
                                                                                
VPGR     NTR1                                                                   
         CLI   22(R4),C'A'         MUST BE A                                    
         BL    *+12                                                             
         CLI   22(R4),C'Z'           TO Z                                       
         BNH   VPGR04                                                           
         MVI   BYTE,X'FF'          VALIDATE PGROUP AFTER TN2 PROFILE            
         B     VOPT90                                                           
                                                                                
VPGR04   MVC   OPTPRGRS,22(R4)     SAVE PROD GROUP SCHEME                       
         MVC   SVTN2PR1,22(R4)     AND SET IT HERE FOR VALIPGR                  
         IC    R0,1(R4)                                                         
         BCTR  R0,0                                                             
         STC   R0,1(R4)                                                         
         ICM   R1,15,23(R4)        GET REMAINDER OF INPUT                       
         MVC   22(10,R4),SPACES                                                 
         STCM  R1,15,22(R4)        AND MOVE IT LEFT 1 BYTE                      
*                                                                               
         GOTO1 VALIPGR                                                          
         MVC   OPTPRGR,SVPGRP      MOVE GROUP CODE                              
         BRAS  RE,BLDPGRP                                                       
         J     EXIT                                                             
                                                                                
VOPTHLP  DS    0H                                                               
         MVC   GERROR,=Y(HELPMSG)                                               
         B     VOPTTRAP                                                         
                                                                                
VOPTGRD  DS    0H                                                               
         MVC   GERROR,=Y(INVGROPT)                                              
         B     VOPTTRAP                                                         
                                                                                
VOPTHLPR DS    0H                                                               
         MVC   GERROR,=Y(INVASSGN)                                              
         B     VOPTTRAP                                                         
                                                                                
VOPTHLPA DS    0H                                                               
         MVC   GERROR,=Y(INVPADID)                                              
         B     VOPTTRAP                                                         
                                                                                
BDADIDER DS    0H                                                               
         MVC   GERROR,=Y(FORMADID)                                              
         B     VOPTTRAP                                                         
                                                                                
INVADIER DS    0H                                                               
         MVC   GERROR,=Y(INVADID)                                               
         B     VOPTTRAP                                                         
                                                                                
TSUPERR  DS    0H                                                               
         MVC   GERROR,=Y(INVTSUP)                                               
         B     VOPTTRAP                                                         
*                                                                               
PRLENER  MVC   GERROR,=Y(PRDLENER)                                              
         B     VOPTTRAP                                                         
*                                                                               
PRDERR   MVC   GERROR,=Y(BADPRD)                                                
         B     VOPTTRAP                                                         
*                                                                               
ACTUNTER MVC   GERROR,=Y(UNTACTER) ACTIVITY AND NEGATIVE ERROR                  
         B     VOPTTRAP                                                         
*                                                                               
INVPRDER MVC   GERROR,=Y(INVPRDE)                                               
         XC    WORK,WORK                                                        
         MVI   WORK,4              L'SUBST TEXT + 1                             
         MVC   WORK+1(3),0(RF)                                                  
         LA    R1,WORK                                                          
         STCM  R1,7,GASUBST                                                     
VOPTTRAP GOTO1 VTRAERR                                                          
                                                                                
EQPRDER  MVI   ERROR,INVEQPRD      ENTERED 2 EQUAL PRODS FOR FILTER             
         B     VOPTERX                                                          
VCMLERR  MVI   ERROR,INVCOMM       NO SUCH COMMERCIAL FOR CLT                   
         B     VOPTERX                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     VOPTERX                                                          
MISSERRB MVI   ERROR,MISSING                                                    
VOPTERX  GOTO1 ERREX                                                            
                                                                                
*                                                                               
* OPTTABLE DESCRIPTION                                                          
*                                                                               
* BYTES 00-09 = OPTION KEYWORD                                                  
*     " 10-13 = A(VALI RTN)                                                     
*                                                                               
                                                                                
OPTTABLE DS    0CL14                                                            
*  THE FOLLOWING DOWN TO OPTGRENT ARE NOW GRIDS COLUMNS                         
         DC    CL10'EP       ',AL4(VOPTEP)    DISPLAY EST/PACKAGE               
OPTENT   EQU   *-OPTTABLE                                                       
         DC    CL10'REVISION ',AL4(VOPTREV)   SHOW REVISION  NUMBER             
         DC    CL10'NAME     ',AL4(VOPTNAM)   SHOW PROGRAM NAME                 
         DC    CL10'ROT      ',AL4(VOPTROT)   ROTATION                          
         DC    CL10'PADID    ',AL4(VOPTPADI)  PRINT AD-ID ON REPORT             
         DC    CL10'REASSIGN ',AL4(VOPTREAS)  PRT REASSIGN CML                  
OPTGRENT EQU   (*-OPTTABLE)/OPTENT                                              
                                                                                
*  THE FOLLOWING DOWN TO OPTTGRID ARE NOT VALID GRID OPTIONS                    
         DC    CL10'DSKAD    ',AL4(VOPTDSK)   SHOW DISK ADDR IN REPORT          
         DC    CL10'TRACE    ',AL4(VOPTTRC)   SET ON TRACE (DDS ONLY)           
         DC    CL10'NOPRT    ',AL4(VOPTNOP)   NO PRT REQ DTLS/ TA RPT           
         DC    CL10'POS      ',AL4(VOPTPOS)   POSITION                          
         DC    CL10'POSF     ',AL4(VOPTPOSF)  SORT REPORT BY POSITION           
                                                                                
*  THE FOLLOWING ARE VALID GRID OPTIONS                                         
OPTTGRID DS    0CL14                                                            
         DC    CL10'MR       ',AL4(VOPTMR)    SHOW MULTI-RUN UNITS              
         DC    CL10'ADID     ',AL4(VOPTADID)  FILTER ON AD-ID CML               
         DC    CL10'PRD      ',AL4(VOPTPRD)   FILTER ON PRODUCT                 
         DC    CL10'PGRP     ',AL4(VOPTPGRP)  FILTER ON PROD GROUP              
         DC    CL10'UNASSIGND',AL4(VOPTUNA)   SHOW UNASSIGNED UNITS             
         DC    CL10'NTYPE    ',AL4(VOPTNTYP)  FILTER ON NTYPE(NTYPE=)           
         DC    CL10'ALL      ',AL4(VOPTNTYP)  FILTER ON NTYPE(ALL=)             
         DC    CL10'ALLUNITS ',AL4(VOPTALLU)  SHOW ALL UNITS                    
         DC    CL10'CML      ',AL4(VOPTCML)   FILTER BY CMML CODE               
         DC    CL10'LENGTH   ',AL4(VOPTLEN)   FILTER ON SPOT LENGTH             
         DC    CL10'UNITS    ',AL4(VOPTUNT)   NET=N,TRF=T,BOTH=B                
         DC    CL10'ACTIVITY ',AL4(VOPTACT)   BUY ACTIVITY                      
         DC    CL10'DPT      ',AL4(VOPTDPT)   FILTER BY DAYPART                 
         DC    CL10'BB       ',AL4(VOPTBB)    LIST BILLBOARD UNITS              
         DC    CL10'PATTERN  ',AL4(VOPTPAT)   DISPLAY PATTERN REF               
         DC    CL10'NEGATIVE ',AL4(VOPTNEG)   FILTER ON NEGATIVE UNITS          
         DC    CL10'TS       ',AL4(VOPTTS)    FILTER ON TRAFF SUPPLIER          
         DC    CL10'-TS      ',AL4(VOPTNTS)   FILTER ON NON-TRAFF SUPPL         
         DC    CL10'BUY      ',AL4(VOPTBUY)   ONLY SHOW BASE PROG CODE          
         DC    CL10'CCN      ',AL4(VOPTCCN)   ONLY SHOW EST CODED N             
         DC    CL10'RSGN     ',AL4(VOPTRESG)  ONLY SHOW REASSIGNED              
         DC    CL10'DIGITAL  ',AL4(VOPTDIGI)  INCLUDE SUBMEDIA V                
*                                                                               
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
                                                                                
FCLT     NMOD1 0,**FCLT**                                                       
                                                                                
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         MVC   BBCLT,BCLT          SAVE BCLT IF ANY                             
                                                                                
FCLT05   MVC   SVKEY,KEY                                                        
                                                                                
         GOTO1 CLUNPK,DMCB,(SVCPROF6,SVBCLT),QCLT                               
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
                                                                                
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT08                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    OPTN2SW,OPTSRTMD    SORTING BY MEDIA?                            
         BZ    FCLT20               NO                                          
         B     FCLT50                                                           
                                                                                
FCLT08   DS    0H                                                               
         CLI   SVOFF,0             FILTER BY OFFICE?                            
         BE    FCLT12               NO                                          
         L     R6,AIO                                                           
         TM    OPTN2SW,OPTSRTMD    SORTING BY MEDIA?                            
         BZ    FCLT12               NO                                          
                                                                                
         BRAS  RE,COFF                                                          
         BNE   FCLT50                                                           
                                                                                
         CLC   SVOFF,SVCLTOFF      IS THIS THE OFFICE                           
         BNE   FCLT50                                                           
                                                                                
FCLT12   XC    WORK,WORK           * READ TA PROFILE *                          
                                                                                
         MVC   WORK(4),=C'S0TA'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),SVOFF                                                 
         GOTO1 GETPROF,DMCB,WORK,SVTAPROF,DATAMGR                               
                                                                                
         CLI   SVTAPROF,C'Y'       EXCLUDE CLIENT FROM LIST                     
         BE    FCLT20                                                           
                                                                                
         MVI   WORK+3,C'N'         * READ TN PROFILE *                          
         GOTO1 (RF),(R1),,SVPROF                                                
                                                                                
         MVC   LCTN2PR6,SVPROF+1   CAL/BROAD/WEEKLY PROFILE                     
         MVC   LCTN2PRC,WORK+31    SHOW ESTIMATE NAME AND NUMBER                
                                                                                
         MVI   WORK+3,C'0'        ** READ NETWORK N0 PROFILE *                  
         GOTO1 (RF),(R1),,NBUSER                                                
                                                                                
* MUST READ TN2 PROFILE                                                         
         MVI   WORK,X'A2'                                                       
         MVC   WORK+1(3),=C'TN2'   READ TN2                                     
         MVC   WORK+6(1),SVMEDIA   USE MEDIA FROM NETWORK REC                   
         GOTO1 (RF),(R1),,SVTN2PRO                                              
         CLI   SVTN2PRO+5,C'0'                                                  
         BE    *+18                                                             
         CLI   SVTN2PRO+5,0                                                     
         BE    *+10                                                             
         MVC   LCTN2PR6,SVTN2PRO+5 SAVE CAL/BROAD/WEEKLY PROFILE                
         MVC   LCTN2PR8,SVTN2PRO+7 SHOW UNIT COST                               
                                                                                
                                                                                
* CLEAR ESTIMATE/PROD/COPY CODE TABLE *                                         
                                                                                
*        LH    R0,=AL2(ESTBL-SYSD)                                              
*        AR    R0,R9                                                            
         L     R0,AESTBL                                                        
         LA    R1,L'ESTBL                                                       
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
* CLEAR EQUIVALENT PROGRAM TABLE *                                              
                                                                                
         LA    R0,EQVPTBL                                                       
         LA    R1,L'EQVPTBL                                                     
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         MVC   BCLT,BBCLT          RESTORE BCLT                                 
         CR    R0,R0               SET COND CODE TO PROCESS CLIENT              
FCLTX    XIT1                                                                   
                                                                                
* BYPASS CLIENT - TA PROFILE SAYS DON'T REPORT *                                
                                                                                
FCLT20   DS    0H                                                               
         TM    OPTN2SW,OPTSRTMD    SORT BY MEDIA?                               
         BZ    *+6                 SOEMTHING'S FUBAR IF WE TRY TO GO...         
         DC    H'0'                ...TO NETIO WHEN SORTING BY MEDIA            
         OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    NBSUBMSK,NBSBMCLI                                                
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    NBSELPRG,NBSELPRG                                                
         BZ    FCLT30                                                           
         TM    NBSUBMSK,NBSBMPRG                                                
         BO    FCLT40                                                           
                                                                                
FCLT30   CLI   NBMODE,NBREQLST                                                  
         BE    FCLT40                                                           
         CLI   NBMODE,NBPROCUN                                                  
         BNE   FCLT20                                                           
         CLC   SVBCLT,NBACTCLI                                                  
         BE    FCLT20                                                           
         MVC   SVBCLT,NBACTCLI                                                  
         B     FCLT05                                                           
                                                                                
FCLT40   MVC   BCLT,BBCLT          RESTORE BCLT                                 
         LTR   RB,RB               SET CC NE TO END RUN                         
         B     FCLTX                                                            
                                                                                
* BYPASS CLT - WRONG OFFICE                                                     
                                                                                
FCLT50   DS    0H                                                               
         BAS   RE,GETSRT2          GET KEY INTO NBKEY                           
         BZ    FCLT40              NO MORE CLTS                                 
         MVI   NBFUNCT,NBFGET                                                   
         OI    NBSBKEND,NBNODPT2   DON'T RD 2 CHAR DAYPART IN NETVALUE          
         XC    NBAPROD,NBAPROD                                                  
         GOTO1 ANETIO,DMCB,(R3)                                                 
         XC    NBAPROD,NBAPROD                                                  
         MVI   NBUSER+13,C'N'      DO NOT SUPPRESS PRE-EMPTS                    
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   NBMODE,NBREQLST                                                  
         BE    FCLT40                                                           
         CLI   NBMODE,NBPROCUN                                                  
         BNE   FCLT50                                                           
         CLC   SVBCLT,NBACTCLI                                                  
         BE    FCLT50                                                           
         MVC   SVBCLT,NBACTCLI                                                  
         B     FCLT05                                                           
                                                                                
GETSRT2  NTR1                                                                   
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         L     R2,4(R1)            R2=A(RECORD)                                 
         MVC   NBKEY(L'NUKPKEY+5),1(R2)     KEY + STAT & DA                     
         CLC   LMEDIA,0(R2)        SAME MEDIA?                                  
         BE    *+14                                                             
         MVC   LMEDIA,0(R2)                                                     
         MVI   FORCEHED,C'Y'       PAGE BREAK ON NEW MEDIA                      
*                                                                               
         LTR   R2,R2               SET CC FOR RETURN                            
         B     FCLTX                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE OFFICE CODE (ONLY SUPPORTED IN OFFLINE REPORT) *                     
*              -READ CLIENT HEADER RECORDS TO BUILD                             
*               TABLE OF CLIENTS FOR REQUESTED OFFICE                           
                                                                                
VOFF     NMOD1 0,**VOFF**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BNZ   VOFF5                                                            
         MVC   GERROR,=Y(OFFCNG)                                                
         GOTO1 VTRAERR             USE GETTXT CALL                              
                                                                                
VOFF5    XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
VOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   VOFFX                                                            
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   VOFFX                                                            
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
                                                                                
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     VOFF10                                                           
                                                                                
VOFF30   L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C'A'       IF THERE IS ONE                              
         BNL   VOFF34                                                           
                                                                                
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
                                                                                
         CLI   LAOFFICE,0          IS THIS CLT LIMITED ACCESS                   
         BE    VOFF34               NO                                          
         CLC   SVCLTOFF,LAOFFICE   SAME OFFICE                                  
         BNE   VOFF20                                                           
         B     VOFF40                                                           
                                                                                
VOFF34   DS   0H                                                                
         BRAS  RE,COFF                                                          
         BNE   VOFF20                                                           
                                                                                
VOFF40   CLC   SVOFF,SVCLTOFF      TEST RIGHT OFFICE                            
         BNE   VOFF20                                                           
*                                                                               
VOFF45   MVC   SVBCLT,CKEYCLT                                                   
                                                                                
         GOTO1 CLUNPK,DMCB,(SVCPROF6,SVBCLT),QCLT                               
                                                                                
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    *+8                                                              
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
                                                                                
         GOTO1 VALICLT                                                          
                                                                                
         MVC   SVBCLT,BCLT                                                      
         XC    BCLT,BCLT                                                        
         XC    FILENAME,FILENAME                                                
                                                                                
         MVC   KEY,SVKEY                                                        
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    VOFFX                                                            
                                                                                
         CLI   ERROR,SECLOCK       ERR IS SECURITY LOCK-OUT                     
         BE    VOFF20                                                           
         DC    H'0'                                                             
                                                                                
VOFFX    XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
* CHECK OFFICE TO BE VALID *                                                    
                                                                                
COFF     NMOD1 0,**COFF***                                                      
         L     RC,SVADGEND         RESTORE RC - CREAMED BY NMOD1                
         USING GEND,RC                                                          
                                                                                
         USING CLTHDRD,R6                                                       
                                                                                
         GOTO1 CLUNPK,DMCB,(SVCPROF6,2(R6)),DUB                                 
                                                                                
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
                                                                                
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         MVC   OFCCLT,DUB                                                       
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         XIT1                                                                   
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* TRACE I/O'S IF REQUESTED                                                      
                                                                                
TRA      NMOD1 0,**+TRA**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         MVC   HOLDP,P1                                                         
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P1(2),=C'2C'                                                     
         MVC   P1+3(8),SYSDIR                                                   
         MVI   P1+10,C'H'                                                       
         CLC   TRACEFW,HIGH                                                     
         BE    TRA10                                                            
         MVI   P1+10,C'S'                                                       
         CLC   TRACEFW,SEQ                                                      
         BE    TRA10                                                            
         MVI   P1+10,C'G'                                                       
         CLC   TRACEFW,GETREC                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P1+2(8),SYSFIL                                                   
TRA10    LA    R2,P1                                                            
         CLI   P1+10,C'G'                                                       
         BE    TRA20                                                            
         MVC   12(20,R2),KEYSAVE                                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,40(R2),20,0,0                                
         LA    R2,132(,R2)                                                      
         B     TRA30                                                            
                                                                                
TRA20    GOTO1 HEXOUT,DMCB,KEY+20,65(R2),5,0,0                                  
                                                                                
TRA30    MVC   12(20,R2),KEY                                                    
         GOTO1 HEXOUT,DMCB,KEY,40(R2),20,0,0                                    
         GOTO1 SPOOL,(R1),(R8)                                                  
         MVC   P1(256),HOLDP                                                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* CHECK ESTIMATES, BYPASS THOSE WITH COPY CODE N *                              
                                                                                
CKEST    NMOD1 0,**+CKE**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
CKEST00  LA    R0,L'ESTBL/25                                                    
*        LH    R1,=AL2(ESTBL-SYSD)                                              
*        AR    R1,R9                                                            
         L     R1,AESTBL                                                        
CKEST10  CLI   0(R1),0             EMPTY ENTRY                                  
         BE    CKEST30                                                          
         CLC   NBACTEST,0(R1)      SAME EST                                     
         BNE   CKEST14                                                          
         CLC   NBPR1CL3,1(R1)      SAME PRODUCT                                 
         BE    CKEST20                                                          
CKEST14  LA    R1,25(,R1)                                                       
         BCT   R0,CKEST10                                                       
                                                                                
* CLEAR TABLE AND START OVER *                                                  
                                                                                
*        LH    R0,=AL2(ESTBL-SYSD)                                              
*        AR    R0,R9                                                            
         L     R0,AESTBL                                                        
         LA    R1,L'ESTBL                                                       
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     CKEST00                                                          
                                                                                
CKEST20  TM    OPTNSW,OPTCCN       ONLY SHOW EST COPY CODED N                   
         BO    CKEST24                                                          
         CLI   4(R1),C'N'          BYPASS EST                                   
         B     CKESTX                                                           
CKEST24  CLI   4(R1),C'N'          BYPASS EST                                   
         BNE   CKEST26                                                          
         CR    RB,RD                                                            
         B     CKESTX                                                           
CKEST26  CR    RE,RE                                                            
CKESTX   XIT1                                                                   
                                                                                
CKEST30  MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD                                                  
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BNZ   CKEST32                                                          
         MVC   KEY+2(2),SVBCLT                                                  
         OC    SVBCLT,SVBCLT                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
CKEST32  CLC   NBPR1CL3,SPACES      UNALLOCATED                                 
         BNH   CKEST36              YES TRY AS POL                              
         LA    RE,NCLSTSIZ                                                      
         L     RF,ASVNCLST                                                      
CKEST34  CLC   NBPR1CL3,0(RF)                                                   
         BE    CKEST38                                                          
         LA    RF,4(,RF)                                                        
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         BCT   RE,CKEST34                                                       
                                                                                
* TRY UNKNOWN PRODUCT AS POL                                                    
                                                                                
CKEST36  LA    RF,=C'POL'          TRY AS PRODUCT POL                           
                                                                                
CKEST38  MVC   KEY+4(3),0(RF)                                                   
         MVC   KEY+7(1),NBACTEST                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CKEST40                                                          
         CLC   NBPR1CL3,SPACES     UNALLOCATED                                  
         BNH   CKEST50                                                          
         TM    NBKEY+12,X'C0'      THIS A CLOSED OUT UNIT                       
         BO    CKEST50              YES                                         
         DC    H'0'                                                             
CKEST40  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
         USING ESTHDRD,R6                                                       
*        R1 IS POINTING TO ESTBL - NO DSECT FOR THIS TABLE                      
         MVC   0(1,R1),NBACTEST                                                 
         MVC   1(3,R1),NBPR1CL3                                                 
         MVC   4(1,R1),ECOPY                                                    
         MVC   5(L'EDESC,R1),EDESC  ESTIMATE NAME                               
         B     CKEST20                                                          
         DROP  R6                                                               
CKEST50  MVC   0(1,R1),NBACTEST    NO EST HDR FOR POL                           
         MVC   1(3,R1),=XL3'00'                                                 
         MVI   4(R1),0                                                          
         B     CKEST20                                                          
         EJECT                                                                  
* FILTER UNITS ON ENTRIES IN OPTION FIELD WHILE DOING LIST FUNCTIONS *          
                                                                                
LFTR     NMOD1 0,**+LFT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         TM    OPTNSW1,OPTALLU     SHOW ALL UNITS                               
         BO    LFTR02               YES                                         
                                                                                
         TM    WHEN,X'20'          IS THIS A SOON RUN?                          
         BZ    LFTR01                                                           
         CLI   NBSELUOP,C'A'                                                    
         BNE   LFTR01                                                           
         TM    NBUNITST,X'02'                                                   
         BO    LFTRNE                                                           
                                                                                
LFTR01   DS    0H                                                               
         TM    UNITSW,UNITSWMI     FILTER BY NEGATIVE UNITS                     
         BNO   LFTR02               NO                                          
         TM    NBUNITST,X'C2'      MINUS/PREEMPT/MISSED UNITS                   
         BNZ   LFTR02               YES                                         
         L     R6,NBAIO                                                         
         TM    NURSTAT-NURECD(R6),X'80' DELETED UNITS                           
         BO    LFTR02                    YES                                    
                                                                                
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LFTRNE                                                           
         USING NUCMLEL,R6                                                       
         TM    NUCMLFLG,X'08'     TRAFFIC DELETE                                
         BNO   LFTRNE              NO                                           
                                                                                
LFTR02   OC    OPTIONS(L'OPTIONS),OPTIONS                                       
         BZ    LFTRX                                                            
         CLI   OPTDPT,0            FILTER BY DAYPART CODE                       
         BE    LFTR04               NO                                          
*        CLC   OPTDPT,NBACTDP                                                   
         CLC   OPTDPT,QDPT2                                                     
         BE    *+6                                                              
         DC    H'0'                INSURANCE                                    
         CLC   QDPT,NBACTDP                                                     
         BNE   LFTRNE                                                           
                                                                                
LFTR04   CLI   OPTNTYP,0           FILTER BY NETWORK TYPE                       
         BE    LFTR06               NO                                          
         L     R1,NBAIO                                                         
         MVC   BYTE,22(R1)                                                      
         NI    BYTE,X'7F'                                                       
         CLI   OPTNTYP,C'N'                                                     
         BNE   *+16                                                             
         CLI   BYTE,0                                                           
         BNE   LFTRNE                                                           
         B     LFTR06                                                           
         CLI   OPTNTYP,C'C'                                                     
         BNE   *+16                                                             
         CLI   BYTE,1                                                           
         BNE   LFTRNE                                                           
         B     LFTR06                                                           
         CLI   OPTNTYP,C'S'                                                     
         BNE   *+16                                                             
         CLI   BYTE,2                                                           
         BNE   LFTRNE                                                           
         B     LFTR06                                                           
         CLI   OPTNTYP,C'O'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,3                                                           
         BNE   LFTRNE                                                           
                                                                                
*        CLC   OPTNTYP,NBSTATYP                                                 
*        BNE   LFTRNE                                                           
LFTR06   OC    OPTPROD,OPTPROD     FILTER BY PRODUCT                            
         BZ    LFTR10               NO                                          
         CLC   NBPR1CL3,SPACES                                                  
         BH    LFTR06C                                                          
                                                                                
         LA    R1,SVCSPRDC         MAYBE IT'S A COPY SPLIT                      
         LA    R0,L'SVCSPRDC/3     MAX NUMBER OF PROD CODES                     
LFTR06E  OC    0(3,R1),0(R1)                                                    
         BZ    LFTR06C             NO CONTINUE W/OLD CODE                       
         CLC   OPTPROD,0(R1)                                                    
         BE    LFTR08                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,LFTR06E                                                       
         B     LFTRNE                                                           
                                                                                
LFTR06C  CLC   OPTPROD,NBPR1CL3                                                 
         BE    LFTR08                                                           
         OC    BPRD3C,BPRD3C       IS IT TRI-BACK                               
         BZ    *+14                 NO                                          
         CLC   OPTPROD,BPRD3C                                                   
         BE    LFTR08                                                           
                                                                                
         OC    OPTPROD2,OPTPROD2   FILTER BY PARTNER                            
         BNZ   LFTR07               YES                                         
         CLC   OPTPROD,NBPR2CL3                                                 
         BNE   LFTRNE                                                           
         B     LFTR10                                                           
LFTR07   CLC   OPTPROD2,NBPR1CL3    ARE PRODUCTS REVERSED                       
         BNE   LFTRNE                                                           
         CLC   OPTPROD,NBPR2CL3                                                 
         BNE   LFTRNE                                                           
         B     LFTR10                                                           
                                                                                
LFTR08   OC    OPTPROD2,OPTPROD2   FILTER BY PARTNER                            
         BZ    LFTR10               NO                                          
         CLC   NBPR2CL3,SPACES                                                  
         BH    LFTR09                                                           
                                                                                
         LA    R1,SVCSPRDC         MAYBE IT'S A COPY SPLIT                      
         LA    R0,6                MAX # OF PRODUCT CODES                       
LFTR08E  OC    0(3,R1),0(R1)                                                    
         BZ    LFTR09              NO CONTINUE W/OLD CODE                       
         CLC   OPTPROD2,0(R1)                                                   
         BE    LFTR10                                                           
         LA    R1,3(R1)                                                         
         BCT   R0,LFTR08E                                                       
         B     LFTRNE                                                           
LFTR09   CLC   OPTPROD2,NBPR2CL3                                                
         BNE   LFTRNE                                                           
                                                                                
LFTR10   OC    OPTPRGR,OPTPRGR      FILTER BY PRODUCT GROUP                     
         BZ    LFTR16                 NO                                        
         LA    R0,L'OPTPGRL/3                                                   
         L     R1,AOPTPGRL                                                      
LFTR12   CLC   NBPR1CL3,0(R1)                                                   
         BE    LFTR16                                                           
         CLC   NBPR2CL3,0(R1)                                                   
         BE    LFTR16                                                           
         LA    R1,3(,R1)                                                        
         CLI   0(R1),0                                                          
         BNH   LFTRNE                                                           
         BCT   R0,LFTR12                                                        
         B     LFTRNE                                                           
                                                                                
LFTR16   CLI   OPTSLN,0            FILTER ON UNIT LENGTH                        
         BE    LFTR20               NO                                          
         CLC   OPTSLN,NBLEN                                                     
         BNE   LFTRNE                                                           
                                                                                
LFTR20   OC    OPTDATE,OPTDATE     FILTER ON DATE                               
         BZ    LFTR30               NO                                          
         CLI   OPTDATES,0                                                       
         BE    LFTR22                                                           
         CLI   OPTDATES,X'4C'      LESS THAN                                    
         BE    LFTR24                                                           
         CLI   OPTDATES,X'6E'      GREATER THAN                                 
         BE    LFTR26                                                           
         DC    H'0'                                                             
LFTR22   OC    OPTDATE2,OPTDATE2                                                
         BNZ   LFTR28                                                           
         CLC   OPTDATE,NBACTDAT                                                 
         BE    LFTR30                                                           
         B     LFTRNE                                                           
LFTR24   CLC   OPTDATE,NBACTDAT                                                 
         BNH   LFTR30                                                           
         B     LFTRNE                                                           
LFTR26   CLC   OPTDATE,NBACTDAT                                                 
         BNL   LFTR30                                                           
         B     LFTRNE                                                           
LFTR28   CLC   OPTDATE,NBACTDAT                                                 
         BH    LFTRNE                                                           
         CLC   OPTDATE2,NBACTDAT                                                
         BL    LFTRNE                                                           
                                                                                
LFTR30   DS    0H                                                               
         OC    OPTCML,OPTCML       FILTER ON COMML                              
         BNZ   *+14                 YES                                         
         OC    OPTADID,OPTADID     FILTER ON CML ADID ?                         
         BZ    LFTR40               NO                                          
                                                                                
LFTR30A  DS    0H                                                               
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LFTRNE                                                           
         USING NUCMLEL,R6                                                       
                                                                                
         TM    NUCMLFLG,X'E0'      LEN/PROD/DATE CHANGED?                       
         BNZ   LFTRNE               BYPASS                                      
                                                                                
         MVI   SVCMADFL,0                                                       
         CLI   1(R6),X'34'         THIS NELSON SHORT EL?                        
         BE    *+10                                                             
         MVC   SVCMADFL,NUCMADFL   SAVE ADID FLAGS                              
                                                                                
* NOW CHECK OPTCML OR SVADIDP, WHICHEVER IS APPROPIATE                          
                                                                                
LFTR31   DS   0H                                                                
         TM    SVCMADFL,NUCMADF1   AD-ID FLAG                                   
         BO    LFTR31B                                                          
         CLI   OPTCML,0                                                         
         BE    LFTR31C                                                          
         CLC   OPTCML,NUCML1                                                    
         BE    LFTR40                                                           
         B     LFTR31C                                                          
LFTR31B  DS   0H                                                                
         CLC   SVADIDP,NUCML1                                                   
         BE    LFTR40                                                           
                                                                                
LFTR31C  DS   0H                                                                
         TM    SVCMADFL,NUCMADF2                                                
         BO    LFTR31D                                                          
         CLI   OPTCML,0                                                         
         BE    LFTR31E                                                          
         CLC   OPTCML,NUCML2                                                    
         BE    LFTR40                                                           
         B     LFTR31E                                                          
LFTR31D  DS   0H                                                                
         CLC   SVADIDP,NUCML2                                                   
         BE    LFTR40                                                           
                                                                                
LFTR31E  DS   0H                                                                
         TM    SVCMADFL,NUCMADF3                                                
         BO    LFTR31F                                                          
         CLI   OPTCML,0                                                         
         BE    LFTR31G                                                          
         CLC   OPTCML,NUCMLBSN                                                  
         BE    LFTR40                                                           
         B     LFTR31G                                                          
LFTR31F  DS   0H                                                                
         CLC   SVADIDP,NUCMLBSN                                                 
         BE    LFTR40                                                           
                                                                                
LFTR31G  DS   0H                                                                
         TM    SVCMADFL,NUCMADF4                                                
         BO    LFTR32                                                           
         CLI   OPTCML,0                                                         
         BE    LFTR33                                                           
         CLC   OPTCML,NUCMLBCN                                                  
         BE    LFTR40                                                           
         B     LFTR33                                                           
LFTR32   DS   0H                                                                
         CLC   SVADIDP,NUCMLBSN                                                 
         BE    LFTR40                                                           
                                                                                
LFTR33   L     R6,NBAIO                                                         
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LFTRNE                                                           
                                                                                
         USING NUFDCEL,R6                                                       
LFTR34   TM    NUFDCFL2,X'80'      DELETED FEED ELEMENT                         
         BO    LFTR36                                                           
                                                                                
         TM    NUFDADFL,NUFDADF1   AD-ID FLAG                                   
         BO    LFTR34B                                                          
         CLI   OPTCML,0                                                         
         BE    LFTR34C                                                          
         CLC   OPTCML,NUFDCML1                                                  
         BE    LFTR40                                                           
         B     LFTR34C                                                          
                                                                                
LFTR34B  DS    0H                                                               
         CLC   SVADIDP,NUFDCML1                                                 
         BE    LFTR40                                                           
                                                                                
LFTR34C  DS    0H                                                               
         TM    NUFDADFL,NUFDADF2                                                
         BO    LFTR34D                                                          
         CLI   OPTCML,0                                                         
         BE    LFTR34E                                                          
         CLC   OPTCML,NUFDCML2                                                  
         BE    LFTR40                                                           
         B     LFTR34E                                                          
                                                                                
LFTR34D  DS    0H                                                               
         CLC   SVADIDP,NUFDCML2                                                 
         BE    LFTR40                                                           
                                                                                
LFTR34E  DS    0H                                                               
         TM    NUFDADFL,NUFDADF3                                                
         BO    LFTR34F                                                          
         CLI   OPTCML,0                                                         
         BE    LFTR34G                                                          
         CLC   OPTCML,NUFDCBSN                                                  
         BE    LFTR40                                                           
         B     LFTR34G                                                          
LFTR34F  DS    0H                                                               
         CLC   SVADIDP,NUFDCBSN                                                 
         BE    LFTR40                                                           
                                                                                
LFTR34G  DS    0H                                                               
         TM    NUFDADFL,NUFDADF4                                                
         BO    LFTR35                                                           
         CLI   OPTCML,0                                                         
         BE    LFTR36                                                           
         CLC   OPTCML,NUFDCBCN                                                  
         BE    LFTR40                                                           
         B     LFTR36                                                           
                                                                                
LFTR35   DS    0H                                                               
         CLC   SVADIDP,NUFDCBCN                                                 
         BE    LFTR40                                                           
                                                                                
LFTR36   BRAS  RE,NEXTEL                                                        
         BE    LFTR34                                                           
         B     LFTRNE                                                           
                                                                                
LFTR40   OC    OPTPOS,OPTPOS       FILTER ON POSITION                           
         BZ    LFTR50               NO                                          
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LFTRNE                                                           
         USING NUCMLEL,R6                                                       
         CLC   OPTPOS,NUCMLPOS     FOR LIST SCREEN                              
         BNE   LFTRNE                                                           
                                                                                
LFTR50   TM    OPTNSW,OPTBB        FILTER ON BB'S?                              
         BZ    LFTR60               NO                                          
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LFTRNE                                                           
         TM    NUCMLFLG,X'04'     BILLBOARD REQUIRED                            
         BO    LFTR60                                                           
         CLI   NUCMLBSL,0                                                       
         BE    LFTRNE                                                           
                                                                                
LFTR60   CLI   OPTPOSF,0           SORT REPORT BY POSITION                      
         BE    LFTR70                                                           
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LFTRNE                                                           
         USING NUCMLEL,R6                                                       
         OC    NUCMLPOS,NUCMLPOS   TEST IF THERE IS A POSITION                  
         BZ    LFTRNE                                                           
                                                                                
         LA    R0,3                                                             
         LA    R1,NUCMLPOS+3                                                    
         CLI   0(R1),C' '          LOOK FOR NON-BLANK                           
         BH    LFTR64                                                           
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
LFTR64   CLI   OPTPOSF,X'FF'       IS THIS ALL POSITIONS                        
         BE    LFTR66                                                           
         CLC   OPTPOSF,0(R1)       CK IF THIS REQUESTED POS                     
         BNE   LFTRNE                                                           
         EJECT                                                                  
*                                                                               
*              BUILD RECORDS FOR SORT                                           
*                                                                               
LFTR66   LA    R2,WORK                                                          
         USING SORTD,R2                                                         
         MVC   SRTCLT,NBACTCLI     CLIENT                                       
         MVC   SRTPOSL,0(R1)                                                    
         MVC   SRTNET,NBACTNET     NETWORK                                      
         MVC   SRTDATE,NBACTDAT    DATE                                         
         MVC   SRTTIME,NBACTSQH    TIME                                         
         MVC   SRTPROG,NBACTPRG    PROGRAM                                      
         MVC   SRTEST,NBACTEST     EST CODE                                     
         MVC   SRTSUB,NBACTSUB     SUB-LINE                                     
         MVC   SRTDP,NBACTDP       DAY PART                                     
         MVC   SRTDSKAD,NBKEY+21   DISK ADDRESS                                 
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',(R2)                                        
         B     LFTRNE                                                           
         DROP  R2,R6                                                            
                                                                                
LFTR70   TM    OPTNSW1,OPTTS+OPTNTS TRAFFIC/NON-TRAFFIC SUPPLIER ?              
         BZ    LFTR80               NO                                          
                                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   LFTR75                                                           
                                                                                
         USING NUOTEL,R6                                                        
         CLI   NUOTTYP,C'H'         TRAFFIC SUPPLIER                            
         BE    *+16                  YES                                        
         BRAS  RE,NEXTEL                                                        
         BE    *-12                                                             
         BNE   LFTR75                                                           
                                                                                
         TM    OPTNSW1,OPTNTS      NON-TRAFFIC SUPPLIER FILTER                  
         BO    LFTRNE               YES, BYPASS THIS ONE                        
                                                                                
         OC    SVTS,SVTS           SPECIFIC TRAFFIC SUPPLIER REQUEST?           
         BZ    LFTREQ              NO, SHOW ALL                                 
                                                                                
         ZIC   R1,1(R6)            GET LEN                                      
         SH    R1,=H'3'            MINUS OVERHEAD                               
         CLM   R1,1,SVTSLEN        SAME LEN (IN ELEM AND SCREEN)                
         BNE   LFTRNE                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NUOTHER(0),SVTS     IS THIS IT?                                  
         BE    LFTREQ                                                           
                                                                                
         DROP  R6                                                               
LFTR75   TM    OPTNSW1,OPTNTS      NON-TRAFFIC SUPPLIER                         
         BZ    LFTRNE               NO                                          
                                                                                
LFTR80   TM    OPTNSW1,OPTMR       FILTER ON MULTI-RUN UNITS                    
         BZ    LFTREQ                                                           
                                                                                
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
LFTR85   BRAS  RE,NEXTEL                                                        
         BNE   LFTRNE                                                           
         CLI   2(R6),C'L'          IS THIS MULTI RUN UNIT                       
         BE    LFTREQ                                                           
         B     LFTR85                                                           
*                                                                               
LFTREQ   CR    R1,R1                                                            
         B     LFTRX                                                            
LFTRNE   LTR   RB,RB                                                            
                                                                                
LFTRX    XIT1                                                                   
         EJECT                                                                  
* READ REVISION RECORDS, AND STORE CABLE REV FOR BUY ACTIVITY REPORT            
                                                                                
RDREV    NMOD1 0,**RDREV*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         BRAS  RE,INITXSP          SET TO XSPOT                                 
                                                                                
         LH    RE,=AL2(REVRTBL-SYSD)                                            
         AR    RE,R9                                                            
         LH    RF,=AL2(REVRTBLN*L'REVRENT)                                      
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BNE   RDREV00                                                          
                                                                                
         L     RE,VADUMMY                                                       
         L     RF,=A(OFREVTBN*L'REVRENT)                                        
                                                                                
RDREV00  XCEF                                                                   
                                                                                
         MVC   WORK+32(L'ENDATEP),ENDATEP                                       
         MVC   WORK+40(L'STDATE3),STDATE3                                       
         MVC   WORK+46(L'ENDATE3),ENDATE3                                       
         MVC   WORK+52(L'STDATEPM),STDATEPM                                     
                                                                                
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BNE   RDREV02A                                                         
                                                                                
         MVC   WORK(12),STDATE                                                  
                                                                                
         L     RF,=V(GETBROAD)                                                  
         A     RF,SPTR2CRR         RELOCATE                                     
         ST    RF,VGTBROAD                                                      
                                                                                
         GOTO1 VGTBROAD,DMCB,(1,WORK),WORK+20,GETDAY,ADDAY                      
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),WORK+32,GETDAY,ADDAY                    
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   WORK+32(6),WORK+20   SAME BROADCAST MONTH?                       
         BNE   *+14                                                             
         MVC   WORK+26(6),WORK+6    YES - RESTORE END DATE ENTERED              
         B     RDREV01                                                          
         MVC   WORK+26(6),WORK+38                                               
                                                                                
         FIXDT02                                                                
RDREV01  GOTO1 DATCON,DMCB,(0,WORK+26),(2,WORK+32) ENDATEP                      
         GOTO1 DATCON,DMCB,(0,WORK+20),(3,WORK+40) STDATE3                      
         GOTO1 (RF),(R1),(0,WORK+26),(3,WORK+46)   ENDATE3                      
                                                                                
         GOTO1 GETDAY,(R1),(0,WORK+20),WORK                                     
         MVC   WORK(6),WORK+20                                                  
         CLI   0(R1),1             IF MONDAY                                    
         BE    RDREV02              DONE                                        
         ZIC   R3,DMCB                                                          
         BCTR  R3,0                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,(R1),WORK+20,WORK,(R3)                                     
         FIXDT02                                                                
RDREV02  GOTO1 DATCON,(R1),(0,WORK),(2,WORK+52)  STDATEPM                       
                                                                                
RDREV02A TM    SVFLAG,CONVSW                                                    
         BZ    RDREV03                                                          
                                                                                
         MVC   WORK(L'STDATE),STDATE                                            
         BRAS  RE,CONVPER                                                       
         MVC   WORK+52(L'CONVDTE),CONVDTE                                       
                                                                                
RDREV03  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
         MVC   REVXKID,=XL2'0A1D'                                               
         MVC   REVXKAM(3),BAGYMD AND BCLT                                       
         OC    BCLT,BCLT                                                        
         BNZ   RDREV04                                                          
         MVC   REVXKCLT,SVBCLT                                                  
         OC    SVBCLT,SVBCLT                                                    
         BNZ   RDREV04                                                          
         DC    H'0'                                                             
RDREV04  MVC   REVXKNET,NBACTNET                                                
                                                                                
RDREV06  DS    0H                                                               
         GOTO1 HIGH                                                             
*NOP     GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ,0                                        
         TM    UNITSW,UNITSWTR     TRACE REQUESTED                              
         BZ    RDREV08                                                          
         MVC   HOLDP,P1                                                         
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P1(2),=C'2C'                                                     
         MVC   P1+3(6),=C'UNTDIR'                                               
         MVI   P1+10,C'H'                                                       
         MVC   P1+12(32),KEYSAVE                                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,P1+52,32                                     
         MVC   P2+12(32),KEY                                                    
         GOTO1 (RF),(R1),KEY,P2+52,32                                           
         GOTO1 SPOOL,(R1),(R8)                                                  
         MVC   P1(256),HOLDP                                                    
                                                                                
RDREV08  DS    0H                                                               
         LA    R4,KEY                                                           
RDREV10  CLC   KEY(8),KEYSAVE      CK SAME TYPE, BAGYMD, CLIENT, NET            
         BNE   RDREV50                                                          
                                                                                
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    *+16                                                             
         TM    REVXKPER+1,X'F0'    FOR MONTHLY HOB = ZEROS                      
         BNZ   RDREV16             WEEKLY                                       
         B     *+12                                                             
         TM    REVXKPER,X'80'       THIS WEEKLY INSTR                           
         BO    RDREV16              YES                                         
                                                                                
         CLC   REVXKPER,WORK+40     ONLY COMP ON 1ST 2 BYTE(STDATE3)            
         BL    RDREV12                                                          
         CLC   REVXKPER,WORK+46     ONLY COMP ON 1ST 2 BYTES(ENDATE3)           
         BH    RDREV13                                                          
         B     RDREV20                                                          
                                                                                
RDREV12  MVC   REVXKPER,WORK+40     SET TO START DATE                           
         MVI   REVXKNUM,0                                                       
         MVC   REVXKPRD,=XL3'00'                                                
         XC    REVXKPGR,REVXKPGR                                                
         B     RDREV15                                                          
                                                                                
RDREV13  MVI   REVXKPER,X'FF'       FORCE TO NEXT PROG                          
         B     RDREV15                                                          
                                                                                
RDREV14  MVC   REVXKPER,WORK+52     SET TO START DATE                           
         MVI   REVXKNUM,0                                                       
         MVC   REVXKPRD,=XL3'00'                                                
         XC    REVXKPGR,REVXKPGR                                                
                                                                                
RDREV15  MVC   KEYSAVE,KEY                                                      
         B     RDREV06                                                          
* WEEKLY                                                                        
RDREV16  CLI   LCTN2PR6,C'W'       IS IT WEEKLY                                 
         BNE   RDREV13                                                          
                                                                                
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    RDREV18              NO                                          
                                                                                
         LA    R1,REVXKPER                                                      
         BAS   RE,FPERDTE          FIND ACTUAL PERIOD DATE                      
         GOTO1 DATCON,DMCB,(3,NEWPER),(0,REVDATES)                              
         B     RDREV19                                                          
                                                                                
RDREV18  CLC   REVXKPER,WORK+52     CK PACKED DATES                             
         BL    RDREV14                                                          
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,REVXKPER),(0,REVDATES)                            
                                                                                
RDREV19  GOTO1 ADDAY,(R1),REVDATES,REVDATEE,F'6'                                
         FIXDT02                                                                
         GOTO1 DATCON,(R1),(0,REVDATEE),(2,ENDREVD)                             
                                                                                
*NOP     CLC   REVXKPER,WORK+32     ENDATEP                                     
         CLC   ENDREVD,WORK+32      ENDATEP                                     
         BL    RDREV13                                                          
                                                                                
RDREV20  L     R6,AIO1                                                          
         GOTO1 GETREC                                                           
*NOP     GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,AIO1                                 
         CLC   KEY(32),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    UNITSW,UNITSWTR     TRACE REQUESTED                              
         BZ    RDREV26                                                          
                                                                                
         MVC   HOLDP,P1                                                         
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P1(2),=C'2C'                                                     
         MVC   P1+3(6),=C'UNTFIL'                                               
         MVI   P1+10,C'G'                                                       
         MVC   P1+12(32),KEY                                                    
         GOTO1 HEXOUT,DMCB,KEY,P1+52,32                                         
         GOTO1 SPOOL,(R1),(R8)                                                  
         MVC   P1(256),HOLDP                                                    
                                                                                
RDREV26  LA    R6,42(,R6)                                                       
         CLI   0(R6),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
                                                                                
         TM    REVFLAG,REVINS+REVCAB+REVPAT PAT/NIN/CAB GEN?                    
         BZ    RDREV40                 NO                                       
                                                                                
         LH    R2,=AL2(REVRTBL-SYSD)                                            
         AR    R2,R9                                                            
         LA    R5,REVRTBLN                                                      
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BNE   RDREV21                                                          
                                                                                
         L     R2,VADUMMY                                                       
         LA    R5,OFREVTBN                                                      
                                                                                
         USING REVRECTD,R2                                                      
                                                                                
RDREV21  BAS   RE,RDTE             GET START/END DATES FOR THIS                 
                                                                                
RDREV22  OC    REVRENT,REVRENT     EMPTY ENTRY                                  
         BZ    RDREV30                                                          
                                                                                
         CLC   REVXKPRG,REVRPRG                                                 
         BNE   RDREV24                                                          
         CLC   REVRSDT,DUB                                                      
         BE    RDREV36                                                          
RDREV24  LA    R2,REVRNEXT                                                      
         BCT   R5,RDREV22                                                       
                                                                                
         GOTO1 PERVERT,DMCB,STDATE,ENDATE                                       
         CLI   17(R1),1             MORE THAN 1 YEAR                            
         BH    RDREVERR                                                         
                                                                                
         MVI   ERROR,NOTONLIN       JOB CANNOT BE RUN ONLINE                    
         SR    R2,R2                                                            
         GOTO1 ERREX                                                            
                                                                                
RDREV30  MVC   REVRPRG,REVXKPRG                                                 
                                                                                
         MVC   REVRSDT(4),DUB                                                   
                                                                                
* SAVE HIGHEST REV # WITH INSTR RUN                                             
RDREV36  DS    0H                                                               
         CLC   REVRVN,REVNNUM                                                   
         BH    *+10                                                             
         MVC   REVRVN,REVNNUM       NETWORK REVISION NUMBER                     
RDREV40  DS   0H                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*NOP     GOTO1 AIOCALL,DMCB,SEQQ+DIRQ,0                                         
                                                                                
         TM    UNITSW,UNITSWTR     TRACE REQUESTED                              
         BZ    RDREV10                                                          
         MVC   HOLDP,P1                                                         
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P1(2),=C'2C'                                                     
         MVC   P1+3(6),=C'UNTDIR'                                               
         MVI   P1+10,C'S'                                                       
         MVC   P1+12(32),KEYSAVE                                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,P1+52,32                                     
         MVC   P2+12(32),KEY                                                    
         GOTO1 (RF),(R1),KEY,P2+52,32                                           
         GOTO1 SPOOL,(R1),(R8)                                                  
         MVC   P1(256),HOLDP                                                    
                                                                                
         B     RDREV10                                                          
                                                                                
RDREV50  GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'UNTDIR',NBKEY,KEY             
         BRAS  RE,INITNET          SET TO NET                                   
                                                                                
RDREVX   XIT1                                                                   
                                                                                
RDREVERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RDREVMSG),RDREVMSG                                     
         LA    R2,TRAPERH                                                       
         GOTO1 ERREX2                                                           
RDREVMSG DC    C'* ERROR * RUN OFFLINE OR TRY A SHORTER PERIOD *'               
         DS    0H                                                               
         EJECT                                                                  
* GET START/END DATES FOR REVISION                                              
                                                                                
RDTE     NTR1                                                                   
         TM    SVFLAG,CONVSW       CONVERTED RECORDS?                           
         BZ    RDTE05                                                           
         TM    REVXKPER+1,X'F0'    FOR MONTHLY HOB = ZEROS                      
         BZ    RDTE10              MONTHLY                                      
         LA    R1,REVXKPER                                                      
         BAS   RE,FPERDTE          FIND ACTUAL PERIOD DATE                      
         GOTO1 DATCON,DMCB,(3,NEWPER),(0,WORK)                                  
         B     RDTE06                                                           
                                                                                
RDTE05   TM    REVXKPER,X'80'       THIS WEEKLY INSTR                           
         BZ    RDTE10                                                           
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(2,REVXKPER),(0,WORK)                                
RDTE06   GOTO1 ADDAY,(R1),WORK,WORK+6,F'6'                                      
         B     RDTE30                                                           
                                                                                
RDTE10   MVC   DUB(2),REVXKPER                                                  
         MVI   DUB+2,01                                                         
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)                                     
                                                                                
         TM    REVFLAG,REVBRD                                                   
         BZ    RDTE20                                                           
*NOP     CLI   LCTN2PR6,C'B'       THIS BROADCAST                               
*NOP     BNE   RDTE20                                                           
                                                                                
         MVC   WORK+4(2),=C'15'                                                 
         MVC   WORK+12(6),WORK                                                  
                                                                                
         L     RF,=V(GETBROAD)                                                  
         A     RF,SPTR2CRR         RELOCATE                                     
         ST    RF,VGTBROAD                                                      
                                                                                
         GOTO1 VGTBROAD,DMCB,(1,WORK+12),WORK,GETDAY,ADDAY                      
         B     RDTE30                                                           
                                                                                
RDTE20   GOTO1 ADDAY,(R1),WORK,WORK+6,F'31'                                     
                                                                                
RDTE24   GOTO1 (RF),(R1),WORK+6,WORK+6,F'-1'                                    
         CLC   WORK(4),WORK+6                                                   
         BNE   RDTE24                                                           
                                                                                
         FIXDT02                                                                
RDTE30   GOTO1 DATCON,DMCB,(0,WORK),(2,DUB)  *** NBACTDAT                       
         FIXDT02                                                                
         GOTO1 (RF),(R1),(0,WORK+6),(2,DUB+2)                                   
         B     RDREVX                                                           
                                                                                
         LTORG                                                                  
         DROP  R2,R4,R6                                                         
*                                                                               
* FIND PERIOD DATE                                                              
* ON ENTRY R1 = PERIOD = 1 BYTE YEAR/4 BITS MONTH#/4 BITS WEEK#                 
* WHEN DONE R1 = YYMMDD                                                         
*                                                                               
FPERDTE  NTR1                                                                   
         MVC   NEWPER(1),0(R1)     SAVE YEAR                                    
         LLC   R4,1(R1)            MD OF YMD                                    
         SRDL  R4,4                                                             
         STC   R4,NEWPER+1         MM                                           
         SRL   R5,28               WEEK NUMBER                                  
         MVI   NEWPER+2,1          START WITH 1ST OF THE MONTH                  
*                                                                               
         GOTO1 DATCON,DMCB,(3,NEWPER),(0,DUB)                                   
         MVC   WORK(6),DUB                                                      
*                                                                               
FPER02   GOTO1 GETDAY,DMCB,DUB,WORK+10                                          
         CLI   0(R1),1             MUST BE MONDAY                               
         BE    FPER05                                                           
*                                                                               
         GOTO1 ADDAY,DMCB,DUB,DUB,1                                             
         CLC   WORK+2(2),DUB+2    SAME MONTH?                                   
         BE    FPER02                                                           
         DC    H'0'                BUG CATCHER                                  
*                                                                               
FPER05   CHI   R5,1                WEEK ONE ?                                   
         BE    FPER06              YES, THIS IS THE DATE                        
         CR    R1,R1                                                            
         BCTR  R5,0                WEEK# MINUS 1                                
         MH    R5,=H'7'                                                         
         GOTO1 ADDAY,DMCB,DUB,DUB,(R5)     YYMMDD                               
*                                                                               
FPER06   GOTO1 DATCON,DMCB,(0,DUB),(3,NEWPER)                                   
         XIT1                                                                   
*                                                                               
*                                                                               
****************************************************************                
* ON ENTRY WORK CONTAINS PERIOD (YYMMDD)                                        
* CONVERT PERIOD INTO CONVDTE:                                                  
* 1ST BYTE = YEAR                                                               
* 2ND BYTE = HOB MONTH AND LOB WEEK NUMBER FOR THAT MONTH                       
* EG. 1/27/20 CONVERT X'7814' YEAR= X'78' MONTH=1 WEEK=4                        
****************************************************************                
CONVPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(3,WORK+6) YYMMDD TO YMD                    
*                                                                               
         MVC   CONVDTE,WORK+6      SAVE YM                                      
*                                                                               
* CALC WEEK NUMBER FOR THIS MONTH                                               
         SR    R5,R5               INIT WEEK NUMBER                             
         MVC   DUB(6),WORK                                                      
CONV02   AHI   R5,1                INCR WEEK NUMBER                             
         GOTO1 ADDAY,(R1),DUB,DUB,F'-7'                                         
         CLC   WORK+2(2),DUB+2     SAME MONTH?                                  
         BE    CONV02                                                           
*                                                                               
         SLL   R5,28               WEEK NUMBER IN HOB                           
         LLC   R4,CONVDTE+1                                                     
         SLDL  R4,28               HOB MONTH/LOB WEEK NUMBER                    
         STCM  R4,8,CONVDTE+1      MONTH/WEEK NO (X'C4'= DEC/WEEK 4)            
         XIT1                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
                                                                                
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         OC    STDATE(12),STDATE                                                
         BZ    HDHK20                                                           
                                                                                
         TM    UNITSW,UNITSWMI     NEGATIVE UNITS REPORT                        
         BZ    HDHK05                                                           
                                                                                
         MVC   H1+33(24),=CL24' NEGATIVE UNITS REPORT'                          
         MVC   H2+33(24),=CL24' ---------------------'                          
                                                                                
HDHK05   TM    UNITSW,UNITSWAC     BUY ACTIVITY REPORT                          
         BZ    HDHK10                                                           
                                                                                
         MVC   H1+33(24),=CL24'   BUY ACTIVITY REPORT  '                        
         MVC   H2+33(24),=CL24'   -------------------  '                        
                                                                                
         TM    UNITSW,UNITSWUN    THIS BUY ACTIVITY - UNASSIGNED                
         BZ    HDHK10                                                           
         MVC   H1+56(5),=CL5'(UNA)'                                             
                                                                                
HDHK10   MVC   H3+33(6),=C'PERIOD'                                              
         GOTO1 DATCON,DMCB,STDATE,(8,H3+40)                                     
         MVI   H3+48,C'-'                                                       
         GOTO1 (RF),(R1),ENDATE,(8,H3+49)                                       
*                              COMMERCIAL SCHEDULE LIST                         
HDHK20   DS    0H                                                               
         TM    OPTN2SW,OPTSRTMD                                                 
         BZ    HDHK21                                                           
         LA    RF,NBKEY                                                         
         USING NURECD,RF                                                        
         MVC   H4+2(5),=C'MEDIA'                                                
*                                                                               
         TM    NUKSTAT,X'04'            DIGITAL BUY                             
         BNO   *+14                                                             
         MVC   H4+8(5),=C'VIDEO'                                                
         B     HDHK21                                                           
*                                                                               
         TM    NUKSTAT,X'03'                                                    
         BNO   *+14                                                             
         MVC   H4+8(5),=C'OTHER'                                                
         B     HDHK21                                                           
         TM    NUKSTAT,X'02'                                                    
         BZ    *+14                                                             
         MVC   H4+8(4),=C'SYND'                                                 
         B     HDHK21                                                           
         TM    NUKSTAT,X'01'                                                    
         BZ    *+14                                                             
         MVC   H4+8(5),=C'CABLE'                                                
         B     HDHK21                                                           
         MVC   H4+8(3),=C'NET'                                                  
         DROP  RF                                                               
*                                                                               
HDHK21   CLI   SVOFF,0                                                          
         BE    HDHK22                                                           
         MVC   H5+40(6),=C'OFFICE'                                              
         BAS   RE,CNVOFF           CONVER 1 BYTE OFFICE CODE                    
         BE    HDHK22              OFFICE PRINTED                               
                                                                                
         GOTO1 =V(OFFOUT),DMCB,SVOFF,HEXOUT,H5+48                               
                                                                                
HDHK22   MVC   H5+10(L'QCLT),QCLT                                               
         MVC   H5+15(L'CLTNM),CLTNM                                             
         MVC   H6+10(L'NETWORK),SVNET                                           
                                                                                
         CLI   SVTNPR8,C'Y'       SHOW ALL NETS ON SAME PAGE                    
         BNE   HDHK30                                                           
         TM    UNITSW,UNITSWAC    THIS BUY ACTIVITY                             
         BZ    HDHK30              NO                                           
         MVC   H6+2(15),SPACES                                                  
         CLC   P1+2(9),=C'NETWORK ='  IF NETWORK ALREADY PRINTING               
         BE    HDHK24                                                           
         MVC   H11+2(9),=C'NETWORK ='                                           
         MVC   H11+12(4),SVNET                                                  
         B     HDHK30                                                           
                                                                                
HDHK24   MVC   H11+2(30),SPACES    BLANK HEADING                                
                                                                                
* ANY FILTER OPTIONS TO PRINT                                                   
                                                                                
HDHK30   OC    OPTIONS(L'OPTIONS),OPTIONS                                       
         BZ    HDHKX                                                            
         MVC   H4+33(7),=C'FILTERS'                                             
         LA    R2,H4+41                                                         
         LR    R4,R2                                                            
         OC    OPTPROD,OPTPROD                                                  
         BZ    HDHK32                                                           
         MVC   0(5,R2),=C'PROD='                                                
         MVC   5(3,R2),OPTPROD                                                  
         CLI   7(R2),C' '                                                       
         BH    *+6                                                              
         BCTR  R2,0                                                             
         LA    R2,8(,R2)                                                        
         OC    OPTPROD2,OPTPROD2                                                
         BZ    HDHK32                                                           
         MVI   0(R2),C'/'                                                       
         MVC   1(3,R2),OPTPROD2                                                 
         LA    R2,4(,R2)                                                        
         CLI   OPTPROD2+2,C' '                                                  
         BH    *+6                                                              
         BCTR  R2,0                                                             
HDHK32   OC    OPTPOS,OPTPOS                                                    
         BZ    HDHK34                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(4,R2),=C'POS='                                                 
         MVC   4(4,R2),OPTPOS                                                   
         CLI   7(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,8(,R2)                                                        
HDHK34   CLI   OPTPOSF,0                                                        
         BE    HDHK36                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(5,R2),=C'POSF='                                                
         MVC   5(1,R2),OPTPOSFL                                                 
         LA    R2,6(,R2)                                                        
HDHK36   OC    OPTCML,OPTCML                                                    
         BZ    HDHK37                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(4,R2),=C'CML='                                                 
         MVC   4(8,R2),OPTCML                                                   
         CLI   11(R2),C' '                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,12(,R2)                                                       
         B     HDHK38                                                           
                                                                                
HDHK37   OC    OPTADID,OPTADID     FILTERED ON AD-ID                            
         BZ    HDHK38                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(6,R2),=C'AD-ID='                                               
         MVC   6(12,R2),OPTADID                                                 
         CLI   17(R2),C' '                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,18(,R2)                                                       
                                                                                
HDHK38   OC    OPTNTYP,OPTNTYP     NETWORK TYPE                                 
         BZ    HDHK40                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(5,R2),=C'NTYP='                                                
         MVC   5(1,R2),OPTNTYP                                                  
         LA    R2,6(,R2)                                                        
                                                                                
HDHK40   CLI   OPTSLN,0                                                         
         BE    HDHK46                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(4,R2),=C'LEN='                                                 
         EDIT  (B1,OPTSLN),(3,4(R2)),ALIGN=LEFT                                 
         CLI   6(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,7(,R2)                                                        
                                                                                
HDHK46   CLI   OPTDPT,0                                                         
         BE    HDHK48                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(4,R2),=C'DPT='                                                 
         CLC   OPTDPT,QDPT2                                                     
         BE    *+6                                                              
         DC    H'0'                INSURANCE                                    
         MVC   4(2,R2),OPTDPT                                                   
         LA    R2,5(,R2)                                                        
         CLI   0(R2),C' '                                                       
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
                                                                                
HDHK48   CLI   OPTPRGR,0           FILTER BY PRODUCT GROUP                      
         BE    HDHK50                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(4,R2),=C'PGR='                                                 
         MVC   4(1,R2),OPTPRGRS                                                 
*                                                                               
         UNPK  DUB(5),OPTPRGR(3)                                                
         LLC   RE,PGRLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R2),DUB                                                      
*                                                                               
         LA    R2,9(,R2)                                                        
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
                                                                                
HDHK50   TM    OPTNSW,OPTBASE      FILTER BY BASE PROGRAM                       
         BZ    HDHK60                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(3,R2),=C'BUY'                                                  
         LA    R2,3(,R2)                                                        
                                                                                
HDHK60   TM    OPTNSW,OPTBB        FILTER BY BB REQUIRED BY MEDIA               
         BZ    HDHK70                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(6,R2),=C'BB REQ'                                               
         LA    R2,6(,R2)                                                        
                                                                                
HDHK70   TM    OPTNSW,OPTBILL      FILTER BY BB                                 
         BZ    HDHK80                                                           
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         MVC   0(4,R2),=C'BILL'                                                 
         LA    R2,4(,R2)                                                        
                                                                                
HDHK80   TM    OPTNSW1,OPTTS+OPTNTS FILTER ON TRA/NON-TRAFFIC SUPPLIER          
         BZ    HDHKX                                                            
         CR    R4,R2                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
                                                                                
         TM    OPTNSW1,OPTNTS      FILTER ON NON-TRAFFIC SUPPLIER               
         BO    HDHK85                                                           
                                                                                
         OC    SVTS,SVTS                                                        
         BZ    *+20                                                             
         MVC   0(3,R2),=C'TS='                                                  
         MVC   3(5,R2),SVTS                                                     
         B     HDHKX                                                            
         MVC   0(5,R2),=C'TSUPP'                                                
         B     HDHKX                                                            
HDHK85   MVC   0(6,R2),=C'-TSUPP'                                               
                                                                                
HDHKX    XIT1                                                                   
                                                                                
         EJECT                                                                  
* CONVERT 1 BYTE OFFICE CODE AND PRINT 2 CHAR CODE                              
                                                                                
CNVOFF   NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    WORKSEC,WORKSEC                                                  
         LA    R5,WORKSEC                                                       
         USING OFFICED,R5                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVOFF                                                     
                                                                                
         CLI   LAOFFICE,0          LIMITED ACCESS                               
         BNE   *+16                 YES, JUST CONVERT                           
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   CNVOFFX                                                          
                                                                                
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    CNVOFFX             2 CHAR OFFICE IS NOT ON                      
                                                                                
         MVC   H5+48(2),OFCOFC2                                                 
         CR    RB,RB               SET EQ CC                                    
                                                                                
CNVOFFX  XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
* GET EQUIVALENT PROGRAM RECORDS FOR CLIENT *                                   
                                                                                
GTEQV    NMOD1 0,**GTEQV*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         TM    OPTNSW,OPTBASE      SHOW ALL AS BASE                             
         BO    GTEQVX                                                           
                                                                                
         LA    R2,EQVPTBL                                                       
         LA    R5,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,R2                                                      
                                                                                
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM,BAGYMD                                                    
         MVC   PGEKCLT,SVBCLT                                                   
         MVC   PGEKNET,NETWORK                                                  
                                                                                
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
                                                                                
GTEQV10  CLC   KEY(4),KEYSAVE                                                   
         BNE   GTEQV70                                                          
         OC    NETWORK,NETWORK     WAS NETWORK ENTERED                          
         BZ    *+14                                                             
         CLC   PGEKNET,NETWORK                                                  
         BNE   GTEQV70                                                          
                                                                                
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    GTEQV22                                                          
         DC    H'0'                                                             
         USING PGEDTAEL,R6                                                      
                                                                                
GTEQV20  BRAS  RE,NEXTEL                                                        
         BNE   GTEQV60                                                          
                                                                                
GTEQV22  OC    PROGRAM,PROGRAM     WAS PROGRAM ENTERED                          
         BZ    GTEQV24                                                          
         CLC   PGEPROG,PROGRAM                                                  
         BNE   GTEQV20                                                          
                                                                                
GTEQV24  OC    STDATE3(6),STDATE3  ANY DATES ENTERED                            
         BZ    GTEQV26              NO                                          
                                                                                
         CLC   PGESTR,ENDATE3                                                   
         BH    GTEQV20                                                          
         CLC   PGEEND,STDATE3                                                   
         BL    GTEQV20                                                          
                                                                                
GTEQV26  MVC   EQVNET,PGEKNET                                                   
         MVC   EQVEPROG,PGEKPRG                                                 
         FIXDT02                                                                
         GOTO1 DATCON,DMCB,(3,PGESTR),(2,EQVSDT)                                
                                                                                
* BYPASS FUNNY (I THINK) USELESS DATE GENERATION (BG)                           
         B     GTEQVX10                                                         
         GOTO1 (RF),(R1),,(0,WORK)                                              
                                                                                
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    GTEQV34                                                          
         CLI   LCTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    GTEQV30                                                          
         CLI   NBUSER+3,C'B'       BROADCAST MONTH                              
         BE    GTEQV34                                                          
                                                                                
GTEQV30  MVC   WORK+4(2),=C'01'                                                 
         B     GTEQV40                                                          
                                                                                
GTEQV34  GOTO1 VGTBROAD,(R1),(1,WORK),WORK+6,GETDAY,ADDAY                       
                                                                                
         MVC   WORK(6),WORK+6                                                   
         FIXDT02                                                                
GTEQV40  GOTO1 DATCON,(R1),(0,WORK),(2,EQVSMODT)                                
                                                                                
         MVC   EQVEDT,=F'-1'       ONLY USES 1ST 2                              
         MVC   EQVEMODT,=F'-1'     SAME                                         
         CLC   PGEEND,=F'-1'                                                    
         BE    GTEQV50                                                          
GTEQVX10 DS    0H                                                               
         FIXDT02                                                                
         GOTO1 (RF),(R1),(3,PGEEND),(2,EQVEDT)                                  
         B     GTEQV50                                                          
         GOTO1 (RF),(R1),,(0,WORK)                                              
         CLI   LCTN2PR6,C'B'       TRAFFIC OVERRIDE BROADCAST MONTH             
         BE    GTEQV46                                                          
         CLI   LCTN2PR6,C'C'       TRAFFIC OVERRIDE CALENDAR MONTH              
         BE    GTEQV42                                                          
         DC    H'0'                                                             
                                                                                
GTEQV42  MVC   WORK+6(4),WORK                                                   
         GOTO1 ADDAY,(R1),WORK,WORK,F'31'                                       
GTEQV44  GOTO1 (RF),(R1),,,F'-1'                                                
         CLC   WORK(4),WORK+6                                                   
         BNE   GTEQV44                                                          
         B     GTEQV48                                                          
                                                                                
GTEQV46  GOTO1 VGTBROAD,(R1),(1,WORK),WORK+6,GETDAY,ADDAY                       
                                                                                
         MVC   WORK(6),WORK+12                                                  
         FIXDT02                                                                
GTEQV48  GOTO1 DATCON,(R1),(0,WORK),(2,EQVEMODT)                                
                                                                                
GTEQV50  MVC   EQVBPROG,PGEPROG                                                 
         LA    R2,EQVNEXT                                                       
         BCT   R5,GTEQV20                                                       
         DC    H'0'                                                             
GTEQV60  MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     GTEQV10                                                          
                                                                                
* GO THRU TABLE AND FIND LATEST BASE PGM FOR ENTRY & SAVE IN ENTRY              
                                                                                
*INITSPOT                                                                       
         BRAS  RE,INITSPT          SET FROM NET TO SPOT                         
                                                                                
GTEQV70  DS    0H                                                               
         CLI   KEYSAVE+2,0         DID WE LOOK ACROSS ALL CLIENT                
         BE    GTEQV75              YES WE ARE DONE                             
         MVC   KEY(2),KEYSAVE      RESTORE ID/ACY/MED                           
         XC    PGEKCLT(18),PGEKCLT  CLEAR CLT/NET/EQV PROG                      
         MVC   PGEKNET,NETWORK                                                  
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         B     GTEQV10                                                          
                                                                                
GTEQV75  MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
                                                                                
         LA    R2,EQVPTBL                                                       
         LA    R3,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,R2                                                      
         XC    FULL,FULL                                                        
*                                                                               
GTEQV80  OC    EQVENT,EQVENT       ANY ENTRY?                                   
         BZ    GTEQV100                                                         
         CLC   EQVNET,FULL                                                      
         BE    GTEQV84                                                          
                                                                                
         MVC   FULL,EQVNET                                                      
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY       PRE-FILL THE KEY WITH ZEROES                 
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVI   STAKMED,C'N'                                                     
         MVC   STAKCALL(4),EQVNET                                               
         MVI   STAKCALL+4,C'N'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,BCLT                                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO1                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1                                                          
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
                                                                                
GTEQV84  LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING NPGRECD,R4                                                       
         MVC   NPGKTYP,=X'0D20'                                                 
         MVC   NPGKAM,BAGYMD                                                    
         MVC   NPGKNET,HALF                                                     
         MVC   NPGKPROG,EQVBPROG                                                
         XC    NPGKEND,NPGKEND                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   NPGKTYP(11),KEYSAVE                                              
         BE    GTEQV90                                                          
         DC    H'0'                                                             
*GTEQV90  MVC   EQVBENDT,NPGKEND                                                
GTEQV90  MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   NPGKTYP(11),KEYSAVE                                              
         BE    GTEQV90                                                          
         LA    R2,EQVNEXT          NEXT ENTRY                                   
         BCT   R3,GTEQV80                                                       
*                                                                               
GTEQV100 MVC   KEY(L'SVKEY),SVKEY                                               
GTEQVX   XIT1                                                                   
                                                                                
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
* CONVERT EQUIVALENT PROGRAM CODES TO BASE AND SAVE COPY SPLITS *               
                                                                                
CNVRTP   NMOD1 0,*CNVRTP*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
                                                                                
         XC    SVCSPRD,SVCSPRD                                                  
         MVI   BPRD3,0                                                          
         XC    SVCSPRDC,SVCSPRDC                                                
         XC    BPRD3C,BPRD3C        CLEARS BPRD1C/BPRD2C/BPRD3C                 
                                                                                
         MVI   ELCODE,X'19'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CRTP18                                                           
                                                                                
         USING NUPDED,R6                                                        
         TM    NUPDEIND,X'80'      IS THIS A MULTI-BACK                         
         BO    *+12                                                             
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    CRTP35               NO                                          
         CLC   NBPR1CL3,=XL3'00'  ALREADY HAVE PRODUCT                          
         BNE   CRTP35               YES                                         
                                                                                
         CLC   NBPR2CL3,=XL3'00'  PARTNER                                       
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         D     R0,=F'7'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         CHI   R0,1                MUST BE MORE THAN 1 PRODUCT                  
         BH    *+6                                                              
         DC    H'0'                                                             
         CHI   R0,6                MUST NO MORE THAN 6 PRODUCTS                 
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    NUPDEIND,X'80'      IS THIS A MULTI-BACK                         
         BZ    CRTP10               NO                                          
         CHI   R0,3                MUST BE 3 PRODUCTS                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
CRTP10   DS    0H                                                               
         LA    R1,NUPDEPR                                                       
         LA    RF,SVCSPRDC                                                      
CRTP15   MVC   0(3,RF),0(R1)                                                    
         LA    R1,7(,R1)                                                        
         LA    RF,3(,RF)                                                        
         BCT   R0,CRTP15                                                        
                                                                                
         CLI   SVCSPRDC,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    NUPDEIND,X'80'      IS THIS A MULTI-BACK                         
         BO    CRTP17               YES                                         
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BO    CRTP35               YES                                         
         DC    H'0'                                                             
                                                                                
CRTP17   DS   0H                                                                
         MVC   NBPR1CL3,SVCSPRDC                                                
         MVC   QPRD,SVCSPRDC                                                    
         MVC   BPRD1C,SVCSPRDC                                                  
                                                                                
         MVC   NBPR2CL3,SVCSPRDC+3                                              
         MVC   QPRD2,SVCSPRDC+3                                                 
         MVC   BPRD2C,SVCSPRDC+3                                                
                                                                                
         MVC   BPRD3C,SVCSPRDC+6                                                
         XC    SVCSPRDC,SVCSPRDC                                                
         B     CRTP35                                                           
CRTP18   DS    0H                                                               
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'14'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CRTP35                                                           
                                                                                
         USING NUPRDD,R6                                                        
         TM    NUPRDIND,X'80'      IS THIS A MULTI-BACK                         
         BO    *+12                                                             
         TM    NBUNST3,X'40'       COPY SPLIT                                   
         BZ    CRTP35               NO                                          
         CLC   NBPR1CL3,=XL3'00'   ALREADY HAVE PRODUCT                         
         BNE   CRTP35               YES                                         
                                                                                
         CLC   NBPR2CL3,=XL3'00'   PARTNER                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         D     R0,=F'6'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         CHI   R0,1                MUST BE MORE THAN 1 PRODUCT                  
         BH    *+6                                                              
         DC    H'0'                                                             
         CHI   R0,6                MUST NO MORE THAN 6 PRODUCTS                 
         BNH   *+6                                                              
         DC    H'0'                                                             
                                                                                
         TM    NUPRDIND,X'80'      IS THIS A MULTI-BACK                         
         BZ    CRTP20               NO                                          
         CHI   R0,3                MUST BE 3 PRODUCTS                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
CRTP20   DS    0H                                                               
         LA    R1,NUPRDPR                                                       
         LA    RF,SVCSPRD                                                       
CRTP22   MVC   0(1,RF),0(R1)                                                    
         LA    R1,6(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,CRTP22                                                        
         LA    R2,SVCSPRD              BINARY PRODUCT TABLE                     
         LA    R4,SVCSPRDC             CHARACTER PRODUCT TABLE                  
         LA    R5,6                    MAX # OF ENTRIES                         
                                                                                
CRTP26   LA    RF,NCLSTSIZ                                                      
         L     RE,ASVNCLST                                                      
CRTP27   CLC   0(1,R2),3(RE)                                                    
         BE    CRTP28                                                           
         LA    RE,4(,RE)                                                        
*        CLI   0(RE),C' '                                                       
*        BNH   *+8                                                              
         BCT   RF,CRTP27                                                        
         DC    H'0'                                                             
                                                                                
CRTP28   MVC   0(3,R4),0(RE)                                                    
         LA    R2,1(R2)                                                         
         LA    R4,3(R4)                                                         
         BCT   R5,CRTP26                                                        
                                                                                
         CLI   SVCSPRD,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    NUPRDIND,X'80'      IS THIS A MULTI-BACK                         
         BO    CRTP30               YES                                         
         TM    NBUNST3,X'40'       IS THIS A COPY SPLIT                         
         BO    CRTP35               YES                                         
         DC    H'0'                                                             
                                                                                
CRTP30   DS    0H                                                               
         MVC   NBPRD,SVCSPRD                                                    
         MVC   BPRD,SVCSPRD                                                     
         MVC   NBPRD2,SVCSPRD+1                                                 
         MVC   BPRD2,SVCSPRD+1                                                  
         MVC   BPRD3,SVCSPRD+2                                                  
         XC    SVCSPRD,SVCSPRD                                                  
         MVC   NBPR1CL3,SVCSPRDC                                                
         MVC   QPRD,SVCSPRDC                                                    
         MVC   BPRD1C,SVCSPRDC                                                  
                                                                                
         MVC   NBPR2CL3,SVCSPRDC+3                                              
         MVC   QPRD2,SVCSPRDC+3                                                 
         MVC   BPRD2C,SVCSPRDC+3                                                
                                                                                
         MVC   BPRD3C,SVCSPRDC+6                                                
         XC    SVCSPRDC,SVCSPRDC                                                
                                                                                
CRTP35   MVI   EQVPRGSW,C'N'                                                    
         XC    SVPENDT,SVPENDT     CLEAR PGM END DATE                           
                                                                                
         TM    OPTNSW,OPTBASE      ONLY SHOW BASE                               
         BO    CRTPX                                                            
                                                                                
         LA    R2,EQVPTBL                                                       
         LA    R4,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         SR    R5,R5                                                            
         USING EQVPTBLD,R2                                                      
                                                                                
CRTP40   OC    EQVNET,EQVNET       AT END OF ENTRIES                            
         BZ    CRTP55                                                           
                                                                                
* SEE IF EQUAL ENTRY                                                            
                                                                                
         CLC   EQVNET,NBACTNET     SAME NETWORK                                 
         BNE   CRTP50                                                           
                                                                                
         CLC   EQVEPROG,NBACTPRG   SAME PROGRAM                                 
         BNE   CRTP50                                                           
                                                                                
         BCTR  R5,0                                                             
                                                                                
         CLC   NBACTDAT,EQVSDT     COVERED BY THESE DATES                       
         BL    CRTP50                                                           
         CLC   NBACTDAT,EQVEDT                                                  
         BNH   CRTP80                                                           
                                                                                
CRTP50   LA    R2,EQVNEXT                                                       
         BCT   R4,CRTP40                                                        
                                                                                
* UNIT NOT COVERED BY EXACT DATES, BUT SHOULD IT BE                             
                                                                                
CRTP55   LTR   R5,R5                                                            
         BZ    CRTPX                                                            
         LA    R2,EQVPTBL                                                       
         LA    R4,L'EQVPTBL/(EQVNEXT-EQVENT)                                    
         USING EQVPTBLD,R2                                                      
                                                                                
CRTP60   OC    EQVNET,EQVNET       AT END OF ENTRIES                            
         BZ    CRTPX                                                            
                                                                                
         CLC   EQVNET,NBACTNET     SAME NETWORK                                 
         BNE   CRTP74                                                           
                                                                                
         CLC   EQVEPROG,NBACTPRG   SAME PROGRAM                                 
         BNE   CRTP74                                                           
                                                                                
         CLC   NBACTDAT,EQVSDT     COVERED BY THIS MONTH                        
         BL    CRTP74                                                           
         CLC   NBACTDAT,EQVEDT                                                  
         BNH   CRTP76                                                           
         B     CRTP74                                                           
                                                                                
* WILL REMOVE NEXT 4 LINES OF CODE IF THIS WORKS - 3/17/98 L105                 
                                                                                
         CLC   NBACTDAT,EQVSMODT   COVERED BY THIS MONTH                        
         BL    CRTP74                                                           
         CLC   NBACTDAT,EQVEMODT                                                
         BNH   CRTP76                                                           
                                                                                
CRTP74   LA    R2,EQVNEXT                                                       
         BCT   R4,CRTP60                                                        
         B     CRTPX                                                            
                                                                                
CRTP76   MVI   EQVPRGSW,C'X'       SET UP AS UNCOVERED EQUIV PROG               
         MVC   SVPENDT,EQVEDT                                                   
         MVC   SVPSTDT,EQVSDT                                                   
         B     CRTPX                                                            
                                                                                
CRTP80   MVC   SVEQVPRG,NBACTPRG                                                
         MVC   NBACTPRG,EQVBPROG                                                
         MVI   EQVPRGSW,C'Y'                                                    
         B     CRTPX               BRANCH AROUND NEEDLESS CODE (TEMP)           
*                                                                               
         CLC   NBACTDAT,EQVBENDT   ACT REC NOT COVERED?                         
         BNH   *+10                                                             
         MVC   SVPENDT,EQVBENDT                                                 
*                                                                               
CRTPX    XIT1                                                                   
                                                                                
         DROP  R2                                                               
         EJECT                                                                  
* REPORT SORTED BY MEDIA TYPE                                                   
                                                                                
MEDSRT   NMOD1 0,*MEDSRT*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         BRAS  RE,INITNET                                                       
*                                                                               
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
         MVI   NUKPTYPE,X'84'      UNIT RECORD, PASSIVE KEY                     
         MVC   NUKPAM,BAGYMD                                                    
         MVC   NUKPCLT,BCLT                                                     
         MVC   NUKPNET,NETWORK                                                  
         MVC   NUKPPROG,PROGRAM                                                 
         MVC   NUKPDATE,STDATEP                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         TM    UNITSW,UNITSWTR     SET ON TRACE OPTION                          
         BZ    MS10                                                             
                                                                                
         MVI   P+1,C'H'                                                         
         MVI   P+2,C'1'                                                         
         MVC   P+3(3),=C'KEYSAVE'                                               
         MVC   P+12(25),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY,P+40,25,0,0                                      
         MVC   P2+3(7),=C'KEYSAVE'                                              
         MVC   P2+12(25),KEYSAVE                                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,P2+40,25,0,0                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* IF CLT, NET, OR PROG HAS CHANGED, RE-SEED DATE                                
*                                                                               
MS10     DS    0H                                                               
         CLI   KEY,X'84'           STILL ON UNIT RECS?                          
         BNE   MSRSET                                                           
         CLC   BAGYMD,NUKPAM       STILL ON THIS AGY/MED?                       
         BNE   MSRSET                                                           
*                                                                               
* SEE IF NET OR PROGRAM HAS CHANGED                                             
*                                                                               
         CLC   NUKPCLT(12),KEYSAVE+(NUKPCLT-NUKPKEY)                            
         BE    MS20                                                             
         MVC   NUKPDATE,STDATEP                                                 
         XC    NUKPEST(4),NUKPEST                                               
*                                                                               
* IF CLT HAS CHANGED, READ NEW CLIENT HEADER TO GET OFFICE,...                  
* ...THEN READ PROFILE.                                                         
         CLC   NUKPCLT,KEYSAVE+(NUKPCLT-NUKPKEY)  CLIENT CHANGED?               
         BE    MS15                 NO - IT WAS SOMETHING ELSE                  
         OC    BCLT,BCLT           FILTERING ON CLIENT?                         
         BNZ   MSRSET               YES                                         
*                                                                               
         LA    R4,SAVEKEY                                                       
         USING NURECD,R4                                                        
*                                                                               
         BRAS  RE,INITSPT                                                       
         MVC   SAVEKEY,KEY                                                      
         LA    R1,KEY                                                           
         USING CLTHDR,R1                                                        
         XC    KEY,KEY                                                          
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,NUKPCLT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         DROP  R1                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF6,NUKPCLT),QCLT                              
                                                                                
         XC    WORK,WORK           * READ TA PROFILE *                          
         MVC   WORK(4),=C'S0TA'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVTAPROF,DATAMGR                               
                                                                                
         BRAS  RE,INITNET                                                       
                                                                                
         MVC   KEY,SAVEKEY                                                      
         LA    R4,KEY                                                           
MS15     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     MS10                                                             
*                                                                               
* CK IF FILTER ON NET, PROG                                                     
MS20     DS    0H                                                               
         OC    NETWORK,NETWORK                                                  
         BZ    *+14                                                             
         CLC   NETWORK,NUKPNET                                                  
         BNE   MSRSET                                                           
         OC    PROGRAM,PROGRAM                                                  
         BZ    *+14                                                             
         CLC   PROGRAM,NUKPPROG                                                 
         BNE   MSRSET                                                           
*                                                                               
         CLC   ENDATEP,NUKPDATE    AIR DATE > END DATE?                         
         BNL   MS30                                                             
         MVC   NUKPDATE,=X'FFFF'   FORCE NEXT PROG                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         TM    UNITSW,UNITSWTR     SET ON TRACE OPTION                          
         BZ    MS10                                                             
                                                                                
         MVI   P+1,C'H'                                                         
         MVI   P+2,C'1'                                                         
         MVC   P+3(3),=C'KEYSAVE'                                               
         MVC   P+12(25),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY,P+40,25,0,0                                      
         MVC   P2+3(7),=C'KEYSAVE'                                              
         MVC   P2+12(25),KEYSAVE                                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,P2+40,25,0,0                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   P+1,C'H'                                                         
         MVI   P+2,C'2'                                                         
         MVC   P+3(3),=C'KEYSAVE'                                               
         MVC   P+12(25),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY,P+40,25,0,0                                      
         MVC   P2+3(7),=C'KEYSAVE'                                              
         MVC   P2+12(25),KEYSAVE                                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,P2+40,25,0,0                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     MS10                                                             
*                                                                               
* CHECK PROFILE IF THIS CLIENT OR MEDIA S.B. EXCLUDED                           
*                                                                               
MS30     DS    0H                                                               
         OC    BCLT,BCLT           DON'T EXCLUDE IF THEY REQ'D IT!!!            
         BNZ   MS35                                                             
         CLI   SVTAPROF,C'Y'                                                    
         BNE   MS35                                                             
         MVC   NUKPNET,=X'FFFFFFFF'  FORCE NEXT CLT                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     MS10                                                             
MS35     DS    0H                                                               
         OC    NETWORK,NETWORK     FILTERING ON NET?                            
         BNZ   MS60                 YES - DON'T EXCLUDE MEDIA                   
         TM    NUKSTAT,X'03'       OTHER?                                       
         BZ    *+16                MUST BE NET                                  
         BM    MS40                MUST BE SYND OR OTHER                        
         LA    R1,SVTAPROF+10      EXCLUDE OTHER                                
         B     MS50                                                             
*                                                                               
         LA    R1,SVTAPROF+11      EXCLUDE NET                                  
         B     MS50                                                             
*                                                                               
MS40     TM    NUKSTAT,X'01'       SYND?                                        
         BZ    MS44                                                             
         LA    R1,SVTAPROF+8       EXCLUDE CABLE                                
         B     MS50                                                             
*                                                                               
MS44     LA    R1,SVTAPROF+9       EXCLUDE SYND                                 
*                                                                               
MS50     CLI   0(R1),C'Y'          IF Y, EXCLUDE REC                            
         BNE   MS60                                                             
*                                                                               
         MVC   NUKPPROG,=X'FFFFFFFFFFFF'  FORCE NEXT NET                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     MS10                                                             
*                                                                               
MS60     BAS   RE,PUTSRT           PUT REC TO SORT                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
                                                                                
         TM    UNITSW,UNITSWTR     SET ON TRACE OPTION                          
         BZ    MS10                                                             
         MVI   P+1,C'S'                                                         
         MVC   P+3(3),=C'KEYSAVE'                                               
         MVC   P+12(25),KEY                                                     
         GOTO1 HEXOUT,DMCB,KEY,P+40,25,0,0                                      
         MVC   P2+3(7),=C'KEYSAVE'                                              
         MVC   P2+12(25),KEYSAVE                                                
         GOTO1 HEXOUT,DMCB,KEYSAVE,P2+40,25,0,0                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     MS10                                                             
*                                                                               
MSRSET   BRAS  RE,INITSPT                                                       
MSX      XIT1                                                                   
         DROP  R4                                                               
*                                                                               
* ROUTINE TO PUT KEYS TO SORTER                                                 
*                                                                               
PUTSRT   NTR1                                                                   
         LA    R2,WORK                                                          
         USING SORT2D,R2                                                        
         USING NURECD,R4           R4 STILL AT KEY                              
*                                                                               
         MVC   SRT2TYPE,NUKPTYPE                                                
         MVC   SRT2AM,NUKPAM                                                    
         MVC   SRT2CLT,NUKPCLT                                                  
         MVC   SRT2NET,NUKPNET                                                  
         MVC   SRT2PROG,NUKPPROG                                                
         MVC   SRT2DATE,NUKPDATE                                                
         MVC   SRT2EST,NUKPEST                                                  
         MVC   SRT2SUB,NUKPSUB                                                  
         MVC   SRT2DP,NUKPDP                                                    
         MVC   SRT2STAT,NUKSTAT    STATUS                                       
         MVC   SRT2DA,NUDA         DSKADDR                                      
         MVC   SRT2MED,NUKSTAT                                                  
         NI    SRT2MED,X'03'       0=NET, 1=CABLE, 2=SYND, 3=OTHER              
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',(R2)                                        
         B     MSX                 EXIT                                         
         DROP  R2,R4                                                            
*                                                                               
SAVEKEY  DS    XL48                                                             
*                                                                               
         EJECT                                                                  
                                                                                
* INITIALIZE NETIO                                                              
                                                                                
NETI     NMOD1 0,**NETI**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         LR    RE,R3               CLEAR NETBLOCK                               
         LA    RF,NBBLKEND-NETBLOCK                                             
         XCEFL                                                                  
                                                                                
         MVC   NBSELAGY,AGENCY                                                  
         MVC   NBEFFAGY,AGENCY                                                  
         MVC   NBSELMED,QMED                                                    
         MVC   NBSELAM,BAGYMD                                                   
         MVC   NBSELCLI,QCLT                                                    
         MVC   NBSELCL2,BCLT                                                    
                                                                                
         OC    BCLT,BCLT                                                        
         BNZ   NETI10                                                           
         MVC   NBSELCLI,=C'ALL'                                                 
         B     *+10                                                             
NETI10   DS    0H                                                               
         MVC   NBACLI,ACLTREC                                                   
         MVC   NBSELPRD,=C'ALL'                                                 
         MVC   NBSELOFF,SVOFF                                                   
         MVC   NBSELNET,NETWORK                                                 
         CLI   STDATE,0                                                         
         BE    *+10                                                             
         MVC   NBSELSTR(12),STDATE                                              
                                                                                
         OC    PROGRAM,PROGRAM     WAS PROG CODE ENTERED                        
         BZ    NETI24               NO                                          
         TM    OPTNSW,OPTBASE      REQUEST BASE ONLY                            
         BO    NETI20               YES                                         
         OC    EQVPTBL(L'EQVENT),EQVPTBL  ANY EQUIV PROG CODES                  
         BNZ   NETI24                      YES                                  
                                                                                
NETI20   MVC   NBSELPRG,PROGRAM                                                 
                                                                                
NETI24   DS    0H                                                               
         TM    UNITSW,UNITSWTR     TRACE REQUESTED                              
         BZ    NETI30               NO                                          
         MVI   NBTRCOPT,C'Y'                                                    
         MVC   NBPRINT,VPRINT                                                   
                                                                                
NETI30   MVI   NBNOWRIT,C'N'       STOP LOCKING RECORDS                         
         MVI   NBSELPST,0                                                       
         MVI   NBSELUOP,C'A'                                                    
         TM    UNITSW,UNITSWMI     NEGATIVE UNITS?                              
         BZ    *+8                                                              
         MVI   NBSELUOP,C'B'                                                    
                                                                                
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'Q'          PRESET READ '84' PASSIVE KEY                 
         CLC   NETWORK,SPACES      WAS NETWORK ENTERED                          
         BH    *+8                 YES                                          
         MVI   NBSEQ,C'D'          ELSE READ'04' KEY                            
         MVI   NBMODE,NBPROCUN                                                  
         MVC   NBSELTRF,UNITOPT                                                 
         CLI   NBSELTRF,C'N'       IF NETWORK ONLY                              
         BNE   *+8                                                              
         MVI   NBSELTRF,0          SET                                          
         MVC   NBAIO,AIO3                                                       
         L     R1,SYSPARMS                                                      
         L     RF,16(,R1)           COMFACS ADDRESS                             
         ST    RF,NBACOM                                                        
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A27')                                 
         L     RF,0(,R1)                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ANETIO                                                        
         XIT1                                                                   
         EJECT                                                                  
                                                                                
         LTORG                                                                  
ESTBL    DS    XL2500                                                           
ESTBLEN  EQU   *-ESTBL                                                          
                                                                                
OPTPGRL  DS    CL1500                                                           
OPTPLEN  EQU   *-OPTPGRL                                                        
CLTREC   DS    0H                                                               
         DS    CL1500                                                           
CLTRLEN  EQU   *-CLTREC                                                         
BOTHLEN  EQU   *-OPTPGRL                                                        
                                                                                
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNREV                                                       
         EJECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
* NETWORK PROGRAM RECORD                                                        
       ++INCLUDE SPGENPROG                                                      
         EJECT                                                                  
       ++INCLUDE SPTRNEQPRG                                                     
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE FASSB                                                          
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
NETBLOCKD      DSECT                                                            
       ++INCLUDE NETBLOCKN                                                      
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAACD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRA9CD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR2CRR DS    A                                                                
AGFT     DS    A                                                                
ANETIO   DS    F                                                                
AESTBL   DS    A                                                                
AOPTPGRL DS    A                                                                
ACLTREC  DS    A                                                                
*------------------------------------------------------------------*            
*    BE VERY CAREFUL OF ANY STORAGE YOU ADD BELOW ASVSTOR - THERE  *            
*    IS A TEMPSTR CALL THAT BEGINS WITH THIS ADDRESS AND GOES TO   *            
*    EQUATE SVSTREQ - THIS IS SAVED AND RESTORED MULTIPLE TIME PER *            
*    MF TRANSACTION                                                *            
*------------------------------------------------------------------*            
ASVSTOR  DS    F   THIS IS WRONG                                                
**ASVSTOR  DS    A   THIS IS CORRECT                                            
TRACEFW  DS    F                                                                
TEMPADDR DS    A                                                                
                                                                                
* SAVED STORAGE STARTS HERE FOR 6144 *                                          
                                                                                
VSORTER  DS    V                                                                
VGTBROAD DS    V                                                                
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
PRNTNOW  DS    CL1                 PRINT SWITCH (USING SORTER)                  
SAVCLT   DS    CL2                 SAVE CLIENT FOR POS FILTER                   
SAVNET   DS    CL4                 SAVE NETWORK   "                             
                                                                                
SVWORK   DS    CL64                SAVE WORK AREA                               
                                                                                
SVCODE   DS    XL8                 SAVE KEY                                     
SVANET   DS    XL8                 SAVED BITS FOR APPROVED NETS                 
IDATE    DS    XL3                 INACTIVE DATE FROM STATION APPROVAL          
                                                                                
SVTS     DS    CL5                 SAVE TRAFFIC SUPPLIER                        
SVTSLEN  DS    CL1                 (TS) ENTRY LENGTH                            
*                                                                               
SVSTDATE DS    XL3                 UNIT START                                   
SVENDATE DS    XL3                 AND END DATES                                
*                                                                               
SVADIDP  DS    XL8                 SAVE PACKED ADID                             
*                                                                               
CMLFLAG1 DS    XL1                                                              
NOAIR    EQU   X'08'               NOT APPROVED TO AIR                          
MAXDTE   EQU   X'04'               MAX DATE ERROR                               
CADTE    EQU   X'02'               CLT APPROVAL DATE ERROR                      
                                                                                
*                                                                               
SVOFF    DS    CL1                 SAVE OFFICE FOR BUY ACTIVITY                 
LAOFFICE DS    CL1                 SAVE LIMITED ACCESS OFFICE                   
OCLT     DS    XL2                 FIRST CLIENT FOR OFFICE                      
FIRSTIME DS    CL1                                                              
TOTUNITS DS    H                                                                
CURKEY   DS    H                   CURRENT KEY FOR LIST (PFKEY)                 
*                                                                               
BYTE1    DS    CL1                 USED TO UPDATE NUCMLFLG                      
*                                                                               
SVFLAG   DS    CL1                                                              
SVKEYSW  EQU   X'80'               SAVE KEY IN KEY TABLE                        
RECCHGSW EQU   X'40'               RECORD CHANGED                               
NETCHGSW EQU   X'20'               NETWORK CHANGED                              
PREASGN  EQU   X'10'               PRINT CML THAT NEEDS REASSIGN                
GETISCII EQU   X'08'               JUST GET 8 CHAR ISCII                        
GRIDCTU  EQU   X'04'               CONTINUING WITH GRID                         
GRIDNOUN EQU   X'02'               NO GRID UNITS                                
CONVSW   EQU   X'01'               CONVERTED REVISION RECORDS                   
*                                                                               
BBCLT    DS    XL2                                                              
SVBCLT   DS    XL2                                                              
SVNUSLN1 DS    XL1                 UNIT LENGTH                                  
SVNUSLN2 DS    XL1                                                              
                                                                                
SVCMLFLG DS    XL1                 NUCMLFLG SAVED FROM UNIT FOR FEEDS           
SVCMLFL2 DS    XL1                 X'FF' = FEED W/O NATIONAL                    
*                                  USED FOR TRI-BACKS                           
SVLEN1   DS    XL1                 SLN1                                         
SVLEN2   DS    XL1                 SLN2                                         
SVLEN3   DS    XL1                 SLN3                                         
CMLALLSW DS    XL1                 X'80' = CML1 = ALL                           
*                                  X'40' = CML2 = ALL                           
*                                  X'20' = CML3 = ALL                           
*                                                                               
SVNUCML1 DS    CL8                                                              
SVNUCML2 DS    CL8                                                              
SVNUCML3 DS    CL8                                                              
*                                                                               
SVTYPE   DS    XL4                 CML TYPE                                     
*                                                                               
JUSTLEN  DS    CL1                 JUST GET CML LENGTH                          
                                                                                
BPRD3    DS    XL1                 USED FOR TRI-BACK                            
BPRD1C   DS    CL3                                                              
BPRD2C   DS    CL3                                                              
BPRD3C   DS    CL3                                                              
PPRDPRD  DS    CL3                 TO PASS PROD CODE TO PPRD ROUTINE            
                                                                                
SVCSPRD  DS    XL6                                                              
SVCSPRDC DS    CL18                                                             
                                                                                
SVPRGDAY DS    XL1                                                              
                                                                                
SVCMLS   DS   0CL16                                                             
SVCML1   DS    CL8                                                              
SVCML2   DS    CL8                                                              
SVCMTITL DS    CL15                                                             
HCML1    DS    CL8                                                              
HCML2    DS    CL8                                                              
SVPRD    DS    CL3                                                              
                                                                                
SVCMLADI DS    CL12                AD-ID                                        
                                                                                
DATE     DS    CL6                                                              
                                                                                
OPTN2SW  DS    X                   SECOND OPTION SWITCH                         
OPTSRTMD EQU   X'80'               SORT BY MEDIA TYPE                           
*UNUSED  EQU   X'40'                                                            
*UNUSED  EQU   X'20'                                                            
*UNUSED  EQU   X'10'                                                            
*UNUSED  EQU   X'08'                                                            
*UNUSED  EQU   X'04'                                                            
*UNUSED  EQU   X'02'                                                            
OPTNACT  EQU   X'01'               USED IN FACT RTN - NEEDS TO BE PRTD          
                                                                                
BNET     DS    H                   BIN NET                                      
LMEDIA   DS    X                   LAST MEDIA READ                              
SVPENDT  DS    XL2                 SAVE PGM END DATE (FRM BASE REC)             
SVPSTDT  DS    XL2                 SAVE PGM START DATE (FRM BASE REC)           
SVTAPROF DS    XL16                SAVE TA PROFILE                              
SVMEDIA  DS    C                                                                
LCTN2PR6 DS    C                  CALENDAR/BROADCAST MONTH (TRAFFIC)            
*                                  W=PERIOD = WEEK, DATE MUST BE MONDAY         
LCTN2PR8 DS    C                  SHOW UNIT COST                                
LCTN2PRC DS    C                  SHOW ESTIMATE NAME AND NUMBER                 
SVTN2PR0 DS    C                  BY PROD OR PGROUP                             
                                                                                
* STDATE, ENDATE, STDATEP, AND ENDATEP MUST BE TOGETHER AND IN ORDER            
                                                                                
DATES    DS    0CL25                                                            
PERIOD   DS    XL3                                                              
STDATE   DS    CL6                                                              
ENDATE   DS    CL6                                                              
STDATEP  DS    XL2                                                              
ENDATEP  DS    XL2                                                              
STDATE3  DS    XL3                 KEEP STDATE3/ENDATE3/STDATEPM                
ENDATE3  DS    XL3                      TOGETHER & IN ORDER                     
STDATEPM DS    XL2                 START DATE (MONDAY)                          
                                                                                
CONVDTE  DS    XL2                                                              
NEWPER   DS    XL3                                                              
                                                                                
REVDATES DS    CL6                 REVISION START DATE                          
REVDATEE DS    CL6                 REVISION END DATE                            
ENDREVD  DS    XL2                                                              
                                                                                
UNITSW   DS    XL1                                                              
UNITSWAL EQU   X'80'               80 - FOUND ALLOCATED PRODUCTS                
UNITSWTR EQU   X'40'               40 - TRACE ALL I/O'S                         
UNITSWPT EQU   X'20'               20 - SHOW ALL PATTERN REF #'S                
UNITSWUN EQU   X'10'               10 - SHOW UNASSIGNED                         
UNITSWAC EQU   X'08'               08 - BUY ACTIVITY REPORT                     
UNITSWMI EQU   X'04'               04 - SHOW MINUS UNITS                        
UNITSWNP EQU   X'02'               02 - AUTO T/A NO PRINT                       
UNITSWIC EQU   X'01'               01 - COMMLS HAVE BEEN INVERTED               
*                                     BECAUSE PRD/PTR OUT OF ALPHA SEQ          
NETWORK  DS    CL4                                                              
PROGRAM  DS    CL6                                                              
                                                                                
SVEQVPRG DS    CL6               SAVED EQUIVALENT PROGRAM CODE                  
EQVPRGSW DS    CL1               Y, X, OR N                                     
                                                                                
SVNET    DS    CL4                                                              
SVCML    DS    CL8                                                              
*                                  DEFAULT IS BOTH                              
UNITOPT  DS    CL1                 B(OTH), N(ETWORK), T(RAFFIC)                 
OPTIONS  DS   0CL(ENDOPTS-OPTNSW)                                               
OPTNSW   DS    XL1                                                              
OPTEP    EQU   X'80'               DDS ONLY - SHOW EST/PACK ONLINE LIST         
OPTRV    EQU   X'40'                ""     SHOW REVISION # FOR UNIT             
OPTCCN   EQU   X'20'               ONLY SHOW EST CODED N                        
OPTBASE  EQU   X'10'               ONLY SHOW BASE PROG CODE                     
OPTDSKAD EQU   X'08'               SHOW DISK ADDR IN REPORT                     
OPTPNAME EQU   X'04'               SHOW PROG NAME, NOT CODE                     
OPTBB    EQU   X'02'               FILTER ONLY ON BB REQUIRED BY MEDIA          
OPTBILL  EQU   X'01'               FILTER ONLY ON UNITS WITH BILLBOARDS         
                                                                                
OPTNSW1  DS    XL1                                                              
OPTROT   EQU   X'80'               SHOW ROTATION                                
OPTTS    EQU   X'40'               SHOW TRAFFIC SUPPLIER UNITS                  
OPTNTS   EQU   X'20'               SHOW NON-TRAFFIC SUPPLIER UNITS              
OPTMR    EQU   X'10'               SHOW MULTI-RUN UNITS                         
OPTALLU  EQU   X'08'               SHOW ALL UNITS                               
OPTPADID EQU   X'04'               PRINT ADID ON REPORT                         
OPTRESG  EQU   X'02'               SHOW ONLY REASSIGN UNITS                     
OPTDIGI  EQU   X'01'               INCLUDE DIGITAL SUBMEDIA V                   
                                                                                
OPTPROD  DS    CL3                 KEEP OPTPROD & OPTPRD TOGETHER               
OPTPRD   DS    XL1                 AND IN ORDER                                 
OPTPROD2 DS    CL3                 KEEP OPTPROD2 & OPTPRD2 TOGETHER             
OPTPRD2  DS    XL1                 AND IN ORDER                                 
OPTNTYP  DS    XL1                 NETWORK TYPE                                 
OPTPRGRS DS    CL1                                                              
OPTPRGR  DS    XL2                                                              
OPTCML   DS    CL8                                                              
OPTSLN   DS    XL1                                                              
OPTDATE  DS    XL2                                                              
OPTDATE2 DS    XL2                                                              
OPTDATES DS    CL1                                                              
OPTPOS   DS    CL4                                                              
OPTPOSF  DS    CL1                 X'FF' = ALL                                  
OPTPOSFL DS    CL1                 LAST POS FOR HEAD HOOK                       
OPTDPT   DS    CL2                                                              
OPTADID  DS    CL12                FILTER ON AD-ID                              
*OPTPGRL  DS    CL450                                                           
ENDOPTS  EQU   *                                                                
WORKPROD DS    CL3        REPLACES WORK+24 PROD SAVE SPACE                      
SVLISTAR DS    CL(L'LISTAR)                                                     
SVADIDLS DS    CL(L'SVCMLAD2)                                                   
ADIDSW   DS    XL1                                                              
SKIPNTIO EQU   X'80'               SKIP NETIO READ                              
PRNTBOTH EQU   X'40'               TWO LINES LEFT TO PRINT                      
PRNTSCND EQU   X'20'               ONE LINE LEFT TO PRINT                       
SVCMLAD1 DS    CL12                AD-ID                                        
SVCMLAD2 DS    CL12                AD-ID                                        
ADIDSTAT DS    XL1                                                              
CML1AD   EQU   X'80'                                                            
CML2AD   EQU   X'40'                                                            
CML3AD   EQU   X'20'                                                            
CML4AD   EQU   X'10'                                                            
CML5AD   EQU   X'08'                                                            
CML6AD   EQU   X'04'                                                            
                                                                                
SVCMADFL DS    XL1                 SAVED ADID FLAGS FOR FILTER                  
                                                                                
SVBBFLG  DS    XL1     SAVE BILLBOARD FLAG (NUCMADFL/NUFDADFL)                  
SVBBSADI EQU   X'20'   FLAG FOR AD-ID BB SLIDE                                  
SVBBCADI EQU   X'10'   FLAG FOR AD-ID BB COPY                                   
                                                                                
SVBBSLN  DS    CL17    BILLBOARD SPOT LEN/CML SLIDE/CML COPY                    
SVBBPOS  DS    CL4               POSITION                                       
                                                                                
ENDOPT   EQU   *                                                                
FEEDFLG  DS    CL1                 SET BY FACT RTN IF FEED NEEDS ATTN           
CUTNFLG  DS    CL1                 SET BY FACT RTN IF CUTIN NEEDS ATTN          
         DS    0D                                                               
                                                                                
SVSTREQ  EQU   *-ASVSTOR           LENGTH DEFINITION FOR TEMPSTR CALL           
                                                                                
HOLDP    DS    CL256                                                            
*                                                                               
*GRID                                                                           
GCOSEL   DS    X                   GRID COLUMN SELECTOR                         
GSTAT    DS    X                   GRID STATUS INDICATOR                        
GMSG     DS    CL60                GRID MESSAGE COMMAND                         
DLCB     DS    XL256               DOWNLOAD CONTROL BLOCK                       
                                                                                
*     GRIDS WORK FIELD                                                          
SVTITL1  DS    CL24                                                             
SVTITL2  DS    CL24                                                             
SVTITL3  DS    CL24                                                             
SVTITALL EQU   *-SVTITL1                                                        
                                                                                
SVREV    DS    CL3                                                              
SVPID    DS    CL8                                                              
SVPIDHEX DS    XL2                                                              
SVFEED   DS    CL4                                                              
                                                                                
*     SAVE GRIDS FIELDS                                                         
GRDFLDS1 DS    0C                                                               
*GRDAY    DS    CL3                                                             
GRDAY    DS    CL7                                                              
GRTIME   DS    CL11                                                             
GRCML1   DS    CL12                                                             
GRPRD1   DS    CL7                                                              
GRPRD2   DS    CL7                                                              
GRCML    DS    0C                                                               
GRCML2ST DS    CL1                                                              
GRCML2   DS    CL12                                                             
GRCMLLEN EQU   *-GRCML                                                          
GRCML3ST DS    CL1                                                              
GRCML3   DS    CL12                                                             
GRCML4ST DS    CL1                                                              
GRCML4   DS    CL12                                                             
GRCML5ST DS    CL1                                                              
GRCML5   DS    CL12                                                             
GRCML6ST DS    CL1                                                              
GRCML6   DS    CL12                                                             
                                                                                
GRPRDLN  DS    0C                                                               
GRPRDT3  DS    0CL7                                                             
GRPRD3   DS    CL3                                                              
GRPRDL3  DS    CL4                                                              
GRPRDT4  DS    0CL7                                                             
GRPRD4   DS    CL3                                                              
GRPRDL4  DS    CL4                                                              
GRPRDT5  DS    0CL7                                                             
GRPRD5   DS    CL3                                                              
GRPRDL5  DS    CL4                                                              
GRPRDT6  DS    0CL7                                                             
GRPRD6   DS    CL3                                                              
GRPRDL6  DS    CL4                                                              
GRPRDLNQ EQU   *-GRPRDLN                                                        
                                                                                
GRREASS  DS    CL28                                                             
GRR2ASS  DS    CL28                                                             
                                                                                
GRPTREF  DS    CL6                                                              
GRPID    DS    CL8                                                              
GRPIDNA  DS    CL8                                                              
GRPIDNC  DS    CL8                                                              
GRDTADD  DS    CL10                                                             
GRDTCHAT DS    CL10                                                             
GRDTCHAN DS    CL10                                                             
**GRBTYPE  DS    CL1                                                            
**GRTMADD  DS    CL10                                                           
GRFLDLN1 EQU   *-GRDFLDS1                                                       
                                                                                
                                                                                
GRDFLDS2 DS    0C                                                               
GFEED1   DS    CL4                                                              
GFEED2   DS    CL4                                                              
GFEED3   DS    CL4                                                              
GFEED4   DS    CL4                                                              
GFEED5   DS    CL4                                                              
GFEED6   DS    CL4                                                              
                                                                                
GRADID1  DS    CL12                                                             
GRADID2  DS    CL12                                                             
GRADID3  DS    CL12                                                             
GRADID4  DS    CL12                                                             
GRADID5  DS    CL12                                                             
GRADID6  DS    CL12                                                             
                                                                                
GRLIN#B  DS    XL1                                                              
GRLIN#   DS    CL3                                                              
GRDPT2   DS    CL2                                                              
GRESPK   DS    CL7                                                              
GRNTYPE  DS    CL1                                                              
GRBB     DS    CL2                                                              
GRBBCML  DS    CL12                                                             
GROT     DS    CL7                                                              
                                                                                
GRREV    DS    CL3                                                              
GRCMLO   DS    CL12                                                             
GRCMLO2  DS    CL12                                                             
         DS    CL5                                                              
GRACTUAL DS    CL13                                                             
         DS    CL5                                                              
GRASSIGN DS    CL13                                                             
GRFLDLN2 EQU   *-GRDFLDS2                                                       
                                                                                
                                                                                
GRDFLDS3 DS    0C                                                               
GRCM1T1  DS    CL24                                                             
GRCM1T2  DS    CL24                                                             
GRCM1T3  DS    CL24                                                             
                                                                                
GRCM2T1  DS    CL24                                                             
GRCM2T2  DS    CL24                                                             
GRCM2T3  DS    CL24                                                             
                                                                                
GRCM3T1  DS    CL24                                                             
GRCM3T2  DS    CL24                                                             
GRCM3T3  DS    CL24                                                             
                                                                                
GRSTAT   DS    CL8                                                              
GRCUTIN  DS    CL6                                                              
                                                                                
GRFLDLN3 EQU   *-GRDFLDS3                                                       
                                                                                
GRDFLDS4 DS    0C                                                               
GRCM4T1  DS    CL24                                                             
GRCM4T2  DS    CL24                                                             
GRCM4T3  DS    CL24                                                             
                                                                                
GRCM5T1  DS    CL24                                                             
GRCM5T2  DS    CL24                                                             
GRCM5T3  DS    CL24                                                             
                                                                                
GRCM6T1  DS    CL24                                                             
GRCM6T2  DS    CL24                                                             
GRCM6T3  DS    CL24                                                             
GRFLDLN4 EQU   *-GRDFLDS4                                                       
                                                                                
                                                                                
*------------------------------------------------------------*                  
*GRID                                                                           
*                                                                               
* TABLE OF EQUIVALENT PROGRAMS                          *                       
* EACH ENTRY = 18 BYTES, 6 EQV PR, 3 STDATE, 3 ENDATE, 6 BASE PROG *            
                                                                                
EQVPTBL  DS    XL(90*L'EQVENT)                                                  
                                                                                
STNETBLK EQU   *                                                                
         DS    CL1024                                                           
                                                                                
* TABLE OF CABLE REVISION RECORDS - SEE REVRECTD DSECT                          
* EACH ENTRY = 11 BYTES, 6 PROG, 2 STR DATE, 2 END DATE, 1 REV NUMBER           
                                                                                
REVRTBL  DS    XL(300*L'REVRENT)                                                
REVRTBLN EQU   (*-REVRTBL)/L'REVRENT                                            
                                                                                
* TABLE OF FIRST KEY FOR EACH SCREEN (FOR PFKEY FUNCTION)                       
                                                                                
LKEYENT  EQU   20                                                               
LKEYTBL  DS    XL(20*LKEYENT)     20 KEYS, ONE FOR EACH SCREEN                  
LKEYTBLN EQU   (*-LKEYTBL)/LKEYENT NUMBER OF ENTRIES                            
                                                                                
* TABLE OF ESTIMATES AND COPY CODES                                             
* EACH ENTRY = 23 BYTES, 1 EST, 1 PRD, 1 COPY CODE, 20 ESTIMATE NAME *          
* EACH ENTRY = 23 BYTES, 1 EST, 3 PRD, 1 COPY CODE, 20 ESTIMATE NAME *          
                                                                                
**ESTBL    DS    XL2500                                                         
                                                                                
EDSVSYSD EQU   *                                                                
ENDSYSD  EQU   *       IF THIS ADDRESS EXCEEDS 2F08, PAST END OF                
*                                  STORAGE                                      
                                                                                
         EJECT                                                                  
* EQUIVALENT PROGRAM DSECT                                                      
*                                                                               
EQVPTBLD DSECT                                                                  
*                                                                               
EQVENT   DS    0CL(EQVNEXT-EQVNET)                                              
EQVNET   DS    CL4                 NETWORK                                      
EQVEPROG DS    CL6                 EQUIVALENT PROGRAM CODE                      
EQVSDT   DS    XL2                            EFFECTIVE START DATE              
EQVEDT   DS    XL2                                      END                     
EQVBPROG DS    CL6                 BASE PROG CODE                               
EQVUSED  DS    CL1                 Y = THIS ENTRY USED                          
EQVSMODT DS    XL2                     MONTH START DATE                         
EQVEMODT DS    XL2                           END                                
EQVBENDT DS    XL2                 BASE PROG END DATE                           
EQVNEXT  EQU   *                                                                
                                                                                
* SORT REC DSECT                                                                
*                                                                               
SORTD    DSECT                                                                  
*                                                                               
SRTCLT   DS    CL2                 CLIENT                                       
SRTPOSL  DS    CL1                 POSITION (LAST NON-BLANK)                    
SRTNET   DS    CL4                 NETWORK                                      
SRTDP    DS    CL1                 DAYPART                                      
SRTDATE  DS    CL2                 AIR DATE                                     
SRTTIME  DS    CL1                 START 1/4 HR                                 
SRTPROG  DS    CL6                 PROGRAM                                      
SRTEST   DS    CL1                 ESTIMATE                                     
SRTSUB   DS    CL1                 SUBLINE                                      
SKEYLEN  EQU   *-SORTD             LEN OF SORT KEY                              
SRTDSKAD DS    CL4                 DISK ADDRESS                                 
SRTLN    EQU   *-SORTD             LEN OF SORT REC                              
                                                                                
SORT2D   DSECT                                                                  
*                                                                               
SRT2MED  DS    CL1                 MEDIA                                        
SRT2TYPE DS    CL1                 KEY TYPE (X'84' - JUST TO SAVE TIME)         
SRT2AM   DS    CL1                 AGENCY/MEDIA                                 
SRT2CLT  DS    CL2                 CLIENT                                       
SRT2NET  DS    CL4                 NETWORK                                      
*  THE ACTUAL X'84' KEY IS PROGRAM/AIRDATE - NEEDS TO BE VERIFIED               
*  THAT AIRDATE/PROGRAM IS INTENTIONAL NOT A MISTAKE                            
SRT2DATE DS    CL2                 AIR DATE                                     
SRT2PROG DS    CL6                 PROGRAM                                      
SRT2EST  DS    CL1                 ESTIMATE                                     
SRT2SUB  DS    CL1                 SUBLINE                                      
SRT2DP   DS    CL1                 DAYPART                                      
         DS    CL1                                                              
SKEYLEN2 EQU   *-SORT2D            LEN OF SORT KEY                              
SRT2STAT DS    CL1                 KEY STATUS (JUST TO SAVE TIME)               
SRT2DA   DS    CL4                 DISK ADDRESS                                 
SRTLN2   EQU   *-SORT2D            LEN OF SORT REC                              
                                                                                
* DSECT FOR CABLE REVISION RECORDS                                              
                                                                                
REVRECTD DSECT                                                                  
REVRENT  DS    0XL11                                                            
REVRPRG  DS    CL6                 PROG CODE                                    
REVRSDT  DS    XL2                                                              
REVREDT  DS    XL2                                                              
REVRVN   DS    XL1                 HIGHEST REV NUM CABLE INSTR RUN              
REVRNEXT EQU   *                                                                
                                                                                
* OFFLINE REPORT LINE                                                           
                                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PSTART   DS    CL2    1-  2                                                     
PDATE    DS    CL6    3-  8                                                     
PSUB     DS    CL3    9- 11                                                     
         DS    CL1   12                                                         
PPROG    DS    CL6   13- 18                                                     
         DS    CL1   19                                                         
PPROGNM  DS    CL16  20- 35                                                     
         DS    CL1   36                                                         
PPROD    DS    CL7   37- 43                                                     
         DS    CL1   44                                                         
PPRODNM  DS    CL20  45- 64                                                     
         DS    CL1   65                                                         
PCMML    DS    CL12  66- 77                                                     
         DS    CL1   78                                                         
PCMMLT   DS    CL15  79- 93                                                     
         DS    CL1   94                                                         
PTIME    DS    CL11  95-105                                                     
         DS    CL1  106                                                         
PDP      DS    CL2  107-108                                                     
         DS    CL1  109                                                         
PDAY     DS    CL8  110-117                                                     
         DS    CL1  118                                                         
PFEED    DS    CL1  119            FEED                                         
PCUTIN   DS    CL1  120            CUTIN                                        
                                                                                
* ONLINE LIST LINE                                                              
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LNET     DS    CL4                                                              
         DS    CL1                                                              
LPROG    DS    CL6                                                              
         DS    CL1                                                              
LPRODTB  DS   0CL14                                                             
LPROD    DS    CL7                                                              
LPROD2   DS    CL7                                                              
         DS    CL1                                                              
LDATE    DS    CL5                                                              
LSUBL    DS    CL1                                                              
         DS    CL1                                                              
LDAY     DS    CL7                                                              
         DS    CL1                                                              
LTIME    DS    CL11                                                             
LFEED    DS    CL4                                                              
         DS    CL1                                                              
LCML     DS    CL12                                                             
         DS    CL1                                                              
         DS    CL1                                                              
LREASON  DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'203SPTRA2C   11/09/20'                                      
         END                                                                    
