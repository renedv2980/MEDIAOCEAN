*          DATA SET SPTRA11    AT LEVEL 077 AS OF 12/06/16                      
****************************                                                    
*PHASE T21611C                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE XSORT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*  LEV  3- 5 MAR28/89 ADD EXCLUDE CLIENT                                        
*  LEV  6- 7 MAR28/89 BYPASS ANY RECS WITH MISSING CLIENT HDR                   
*  LEV  8    APR27/89 SHOW EST DESCRIPTION                                      
*  LEV  9    APR27/89 CHANGE CLTPEQ TO SVPROF13 AND USE SVT1 FOR TA             
*  LEV 10    JUL27/89 BLOCK X'90' BUY ACTIVITY                                  
*  LEV 11    APR19/90 ALLOW SINGLE CLIENT OFFLINE TO OVERRIDE TA PROF *         
*  LEV 12    JUN27/90 BYPASS X'20' BIT ON FROM SAME DAY INSTR         *         
*  LEV 13-14 OCT10/90 SHOW ORIGIN OF 'T' AND LATER 'M' AS FLAGGED     *         
*                                    TRAFFIC        MEDIA             *         
*  LEV 15    NOV06/90 BYPASS DELETED ESTIMATE                         *         
*  LEV 16    DEC17/90 ALLOW TA PROFILE FOR NUMBER OF WEEKS TO SHOW    *         
*  LEV 17-19 AUG13/91 NEW TA PROFILE - SVTANOTR DON'T SHOW *T         *         
*  LEV 20-21 DEC13/91 ADD CODE IN CKDELEST FOR DIFF CLT CODE BSNY     *         
*  LEV 22    JAN09/92 FIX READING T0 PROFILE                          *         
*  LEV 23    MAR17/92 SHOW ??? FOR UNKNOWN PRODUCTS - DON'T DUMP      *         
*  LEV 24    MAR19/92 SHOW STATION AFFILIATE                          *         
*  LEV 25    NOV11/92 SHOW PROD REGARDLES OF PROD OR PARTNER          *         
*  LEV 26    DEC03/92 FIX MSUNPK                                      *         
*  LEV 27    MAR02/93 ADD CABLE HEAD CODE                             *         
*  LEV 28    MAR25/93 MAKE MARKET NAME 24 - FROM 20 CHAR              *         
*  LEV 29    AUG20/93 MAKE ONLINE LIST WORK LIKE OFFLINE REPORT FOR T *         
*  LEV 30    SEP07/93 STOP ACTION SELECT OF DELETE                    *         
*  LEV 31    NOV05/93 ADD NEW TRAFFIC SYSTEM                          *         
*  LEV 32    JAN13/94 FIX V/NOFF BUG - COND CODE SET WRONG            *         
*  LEV 33    APR13/94 ADD EXTRA VSWITCH AFTER PRDGRP                  *         
*  LEV 34    JUL28/94 CHANGE TO FILENAME                              *         
*  LEV 35    AUG02/94 FIX TO FILENAME IN FCLT                         *         
*  LEV 36    NOV17/94 FIX FOR WHEN SET WRONG (STOP ONLINE SORT)       *         
*  LEV 37    JUL14/95 FIX FOR BIG FILES                               *         
*  LEV 38    JAN05/96 ADD CODE IN CKDELEST FOR DIFF CLT CODE ZENY     *         
*  LEV 39    NOV06/96 CHANGE PRINTING FOR OFFICE CODE- SPRI           *         
*  LEV 40    FEB14/97 SHOW BILLBOARDS                                 *         
*  LEV 41    APR09/97 FORCE ALL BILLBOARDS TO SHOW, EVEN IF TRAFFICED *         
*  LEV 42    JUN19/98 OPTION TO SHOW DISK ADDRESS                     *         
*  LEV 43    NOV10/99 USE RECUP FROM FACPAK                           *         
*  LEV 44 SMUR MAY19/00 IF SOON TURN OFF VALIDATED BIT                *         
*  LEV 46 SMUR AUG15/00 TURN ON GENSTAT4 - DELETE IS INVALID ACTION   *         
*  LEV 47 SMUR SEP25/00 LIMITED ACCESS                                *         
*  LEV 49 BGRI MAR28/01 PUT IN TRAFFIC OFFICE                         *         
*  LEV 50 BGRI JUN25/01 FIX RELO AND USE VALICLT                      *         
*  LEV 52 DEIS MAY??/02 CHGE BAL TO BAS                               *         
*  LEV 53 SMUR JUN25/02 FOR BY CLIENT IF CLIENT STRING SECURITY       *         
*  LEV 54 BGRI APR12/04 ADD EST DESC TO KEY FIELDS IF BY EST          *         
*  LEV 55 SMUR AUG24/05 2 CHAR OFFICE CODE, FIX FOR OFFICE LIST ($)   *         
*  LEV 56 MHER JUN/10   SUPPORT FOR 8A ACTIVITY POINTERS              *         
*  LEV 73 SMUR MAY10/12 SUPPORT NEW BB LENGTHS AND TYPES              *         
*                       BUG FIX FOR RUNNING BEYOND END OF TABLE       *         
*  LEV 74 MNAS JAN22/13 MORE BANDS                                    *         
*  LEV 75 SMUR JAN05/16 NEW BAND CM FOR IHEART RADIO                  *         
*  LEV 76 SMUR NOV18/16 2 CHAR OFFICE LIST ($A1)                      *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21611 TRAFFIC BUY ACTIVITY LIST/DISPLAY'                       
T21611   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*21611**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR11RR                                                      
*                                                                               
         L     RF,SYSPARMS         TEST THIS IS A TRANSFER OF CONTROL           
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         ST    RF,VGLOBBER                                                      
                                                                                
*        DO NOT REMOVE THE CODE BELOW - IT DOESN'T HURT ANYTHING!               
         MVC   TRACEIT+6(1),MODE                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',TRACEIT                        
*                                                                               
         CLI   CONREC,C'B'         TEST BUYACT SCREEN                           
         BE    TBA00                                                            
                                                                                
* TRAFFIC ACTIVITY SCREEN                                                       
                                                                                
         OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
TBA00    CLI   TRLSTPGM,X'11'                                                   
         BNE   *+12                                                             
         TM    CONRECH+4,X'20'     TEST REC FIELD HAS CHANGED                   
         BO    TBA01               NO - SO BEEN HERE BEFORE                     
         MVI   TRLSTPGM,X'11'                                                   
         OI    CONRECH+4,X'20'                                                  
         NI    TRAMEDH+4,X'FF'-X'20' SET KEY NOT VALIDATED                      
*                                                                               
TBA01    DS    0H                                                               
         CLC   =C'BCM',CONREC      TEST BUYACT/LIST SCREEN                      
         BNE   TBA02                                                            
         CLI   PFKEY,12            TEST PFKEY TO RETURN                         
         BNE   TBA02                                                            
* BUILD RETURN ELEMENT                                                          
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING GLVXFRSY,R5                                                      
         MVC   GLVXFRSY,=C'STR'    FROM SPOT/TRA                                
         MVC   GLVXFRPR,=C'TRA'                                                 
         MVC   GLVXTOSY,=C'STR'    TO                                           
         MVC   GLVXTOPR,=C'TRA'                                                 
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         DROP  R5                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
         J     EXIT                                                             
TRACEIT  DC    X'07',C'MODE=X'                                                  
*                                                                               
TBA02    LA    RE,TRAPFKSH                                                      
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+8                                                              
         LA    RE,TRAPFKLH                                                      
         LR    RF,RE               SAVE LOCATION                                
*                                                                               
         CLC   =C'BCM',CONREC                                                   
         BNE   TBA04                                                            
         CLC   =C'PF',0(RE)                                                     
         BE    TBA06                                                            
         MVC   8(13,RE),=C'PF12=TCM/LIST'                                       
         B     TBA06                                                            
*                                                                               
TBA04    CLC   =C'PF',0(RE)                                                     
         BNE   TBA06                                                            
         XC    8(60,RE),8(RE)                                                   
*                                                                               
TBA06    OI    6(RE),X'80'                                                      
*                                                                               
TBA08    CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         J     EXIT                                                             
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         J     EXIT                                                             
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   TBA20                                                            
         BRAS  RE,DR                                                            
*                                  REMEMBER RF POINTS TO PFK FIELD              
         LA    RF,CONRECH                                                       
         SR    R0,R0                                                            
TBA10    IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         CLI   0(RF),0                                                          
         BNE   TBA10                                                            
         MVC   0(3,RF),=X'000101'  ERASE TO EOS                                 
         J     EXIT                                                             
*                                                                               
TBA20    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*=================================================                              
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
*=================================================                              
                                                                                
LR       DS    0H                                                               
*MNMB                                                                           
*        MVI   TWAFIRST,2          SEND RUN LAST MODE                           
*MNMB                                                                           
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
*                                                                               
         CLI   CONREC,C'B'         TEST BUYACT SCREEN                           
         BE    LR00                                                             
         BRAS  RE,LISTCLTS                                                      
         J     EXIT                                                             
*                                                                               
LR00     CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BNE   LR01                                                             
         TM    WHEN,X'C0'          MUST BE RUN OFFLINE                          
         BNZ   RUNERR              TELL'EM CAN'T BE DONE IMMED OR NOW           
*                                                                               
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   RUNERR              TELL'EM CAN'T BE DONE IMMED OR NOW           
*                                                                               
LR01     DS    0H                                                               
         OC    TABLESZ,TABLESZ      TEST FIRST TIME THRU                        
         BZ    LR02                                                             
*                                                                               
         OC    TBLINDEX,TBLINDEX   WILL BE CLEAR IF ALL DISPLAYED               
         BNZ   LRL                                                              
*                                                                               
         XC    TABLESZ,TABLESZ     ZERO NUMBER OF RECORDS IN TABLE              
         XC    TBLINDEX,TBLINDEX   ZERO INDEX INTO TBA TABLE                    
         XC    SCRINDEX,SCRINDEX   ZERO INDEX INTO TBA TABLE FOR SCREEN         
         OI    TRAMEDH+4,X'20'     VALIDATED                                    
*                                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE                                 
         JE    EXIT                                                             
         OC    SAVEKEY,SAVEKEY     ANY KEY TO CONTINUE WITH                     
         JZ    EXIT                NO                                           
*                                                                               
LR02     CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BE    LR06                                                             
*                                                                               
         USING TBLRECD,R5          TABLE OF TBA RECORDS                         
         LA    R5,TABLE                                                         
         XC    TABLESZ,TABLESZ     ZERO NUMBER OF RECORDS IN TABLE              
         XC    TBLINDEX,TBLINDEX   ZERO INDEX INTO TBA TABLE                    
         XC    SCRINDEX,SCRINDEX   ZERO INDEX INTO TBA TABLE FOR SCREEN         
         B     LR08                                                             
*                                                                               
LR06     TM    WHEN,X'C0'          MUST BE RUN OFFLINE                          
         BNZ   RUNERR              TELL'EM CAN'T BE DONE IMMED OR NOW           
*                                                                               
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   RUNERR              TELL'EM CAN'T BE DONE IMMED OR NOW           
*                                                                               
         LM    R4,R5,=A(SORTCARD,RECCARD)                                       
         GOTO1 =V(SORTER),DMCB,(R4),(R5)                                        
*                                                                               
* BUILD KEY *                                                                   
*                                                                               
LR08     MVI   TBLSW,0             MUST BUILD DATE TABLE                        
*                                                                               
LR10     OC    SAVEKEY,SAVEKEY     TEST CONTINUE FROM PREVIOUS                  
         BZ    LR10A               NO                                           
         MVC   KEY,SAVEKEY         SET KEY TO START WITH                        
         XC    SAVEKEY,SAVEKEY     CLEAR FOR NEXT TIME                          
* CLEAR THE TABLE                                                               
         LA    R0,TABLE                                                         
         LHI   R1,X'2800'                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     LR10B                                                            
*                                                                               
LR10A    MVC   KEY(2),=X'0A2E'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(5),BMKTSTA                                                 
*        MVC   KEY+10(1),BPRD                                                   
         MVC   KEY+11(1),BEST                                                   
         MVC   KEY+5(2),MKTFTR     IF NOT ENTERED, BINARY ZEROS                 
         CLC   KEY+5(2),=X'FFFF'   TEST MKT 0 ENTERED                           
         BNE   *+10                YES - FFFF IS REALLY MKT 0                   
         XC    KEY+5(2),KEY+5      SO CLEAR IT                                  
*                                                                               
         CLI   BOFFCD,0            THIS BY OFFICE                               
         BE    *+10                                                             
         MVC   KEY+3(2),OCLT                                                    
*                                                                               
LR10B    GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE      TEST SAME AGENCY/MEDIA                       
         BNE   LR10X                                                            
         OC    BCLT,BCLT                                                        
         BZ    LR24                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BE    LR24                                                             
*                                                                               
LR10X    CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BNE   NOACTERR            ONLINE ERROR ROUTINE                         
         CLI   BOFFCD,0            TEST OFFICE CODE WAS ENTERED                 
         BNE   LRRPRT                                                           
         B     NOACTIV                                                          
         EJECT                                                                  
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
*                                                                               
LR22     CLC   KEY(3),KEYSAVE      SAME TYPE/A-M                                
         BE    LR24                YES                                          
         CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BE    LRRPRT              GO FORMAT FOR OFFLINE REPORT                 
         B     LRL                 PROCESS ONLINE TABLE                         
*                                                                               
LR24     DS    0H                                                               
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   LR24A                                                            
         XC    DUB,DUB                                                          
         XC    ELEM,ELEM                                                        
         GOTO1 MSUNPK,DMCB,(X'80',KEY+5),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    LR20                                                             
         CLI   DUB+4,C'S'                                                       
         BE    LR20                                                             
         CLI   DUB+4,C'C'          CM FOR IHEART RADIO                          
         BE    LR20                                                             
LR24A    DS    0H                                                               
*MNMB                                                                           
         LA    R4,KEY                                                           
         USING TBAKEY,R4                                                        
         OC    BCLT,BCLT           WAS CLIENT ENTERED                           
         BZ    LR26                NO                                           
         CLC   KEY+3(2),BCLT       SAME CLIENT                                  
         BE    LR30                YES                                          
LR25     DS    0H                                                               
         CLI   MODE,PRINTREP       TEST OFFLINE REPORT                          
         BE    LRRPRT              GO FORMAT FOR OFFLINE REPORT                 
         B     LRL                 PROCESS ONLINE TABLE                         
*                                                                               
LR26     DS    0H                                                               
         CLI   TRACLT,C'$'                                                      
         BNE   LR26K                                                            
*                                                                               
         CLC   SVBCLT,KEY+3                                                     
         BE    *+14                                                             
         MVC   SVBCLT,KEY+3                                                     
         BRAS  RE,FCLT                                                          
         BNE   LR25                END OF THIS AGENCY                           
         B     LR34                                                             
                                                                                
LR26K    DS    0H                                                               
         CLC   SVBCLT,KEY+3        NEW CLIENT                                   
         BE    LR30                                                             
         MVC   SVBCLT,KEY+3                                                     
         BRAS  RE,FCLT                                                          
         BNE   LR25                END OF THIS AGENCY                           
*                                                                               
LR30     CLI   BOFFCD,0            THIS AN OFFICE REQUEST                       
         BE    LR34                                                             
         CLC   OCLT,KEY+3          THIS THIS CLT                                
         BE    LR34                                                             
         BRAS  RE,NOFF               GET NEW CLIENT CODE                        
         BNE   LRRPRT                ALL OF OFFICE CODE DONE?                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2E'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),OCLT                                                    
         GOTO1 HIGH                                                             
         B     LR22                                                             
*                                                                               
LR34     OC    BMKTSTA,BMKTSTA     WAS STATION ENTERED                          
         BZ    LR40                                                             
         CLC   BMKTSTA,TBAKMKT                                                  
         BNE   LR20                                                             
*                                                                               
LR40     SR    R0,R0                                                            
         ICM   R0,3,MKTFTR         TEST ANY MARKET FILTER                       
         BZ    LR42                NO                                           
         CLC   MKTFTR,=X'FFFF'     TEST MARKET 0 FILTER                         
         BNE   *+6                 YES                                          
         SR    R0,R0               SO SET MARKET = 0                            
         CLM   R0,3,TBAKMKT                                                     
         BNE   LR20                                                             
*                                                                               
LR42     CLI   BPRD,0              TEST ONE PRD REQUEST                         
         BE    LR44                NO - ALL PRODUCTS                            
         CLC   BPRD,TBAKPRD        YES - TEST MATCH PROD                        
         BE    LR44                NO                                           
         CLC   BPRD,TBAKPRD2       YES - TEST MATCH PARTNER                     
         BNE   LR20                NO                                           
         EJECT                                                                  
LR44     CLI   BEST,0              WAS ESTIMATE ENTERED                         
         BE    LR46                NO                                           
         CLC   BEST,TBAKEST        TEST MATCH                                   
         BNE   LR20                                                             
*                                                                               
LR46     OC    MGRPTBLE(2),MGRPTBLE  WAS MARKET GROUP ENTERED                   
         BZ    LR50                   NO                                        
         LA    R0,150                                                           
         LA    R1,MGRPTBLE                                                      
LR48     CLC   TBAKMKT,0(R1)       THIS A MARKET                                
         BE    LR50                                                             
         LA    R1,2(,R1)                                                        
         OC    0(2,R1),0(R1)       END OF TABLE                                 
         BZ    LR20                                                             
         BCT   R0,LR48                                                          
         B     LR20                                                             
*                                                                               
LR50     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   DSPSW,0                                                          
         MVI   ELCODE,X'05'                                                     
*                                                                               
         CLI   MODE,LISTRECS       TEST ONLINE LIST                             
         BE    LO30                                                             
         EJECT                                                                  
* PROCESS ELEMENTS FOR OFFLINE REPORT                                           
*                                                                               
         BAS   RE,GETEL                                                         
         BNE   LR70                                                             
         CLI   TBLSW,1             HAS WEEK TABLE BEEN BUILT                    
         BE    LR56                YES                                          
         BAS   RE,TBL              GO BUILD WEEK TABLE AND HEADINGS             
         MVI   TBLSW,1                                                          
         B     LR56                                                             
*                                                                               
         USING TBADTAEL,R6                                                      
*                                                                               
LR54     BAS   RE,NEXTEL                                                        
         BNE   LR70                                                             
LR56     TM    TBADTAAC,X'80'      TEST ACTIVE                                  
         BO    LR57                 YES                                         
*                                                                               
         CLI   BBFILT,C'Y'         REQUESTED ALL BILLBOARDS                     
         BNE   LR60                 NO                                          
*                                                                               
*NOP     TM    TBADTAAC,X'05'      EITHER 5 OR 10 SEC BB'S                      
*                                                                               
         TM    TBADTAAC,TBADTABF+TBADTABT  5 OR 10 SEC BB'S                     
         BNZ   LR57                                                             
*                                                                               
         CLI   TBADTLN,TBADTEQ     NEW ELEM                                     
         BNE   LR60                 NO                                          
*                                                                               
         OC    TBADTBB(2),TBADTBB   AND  TBADTBB1 (ANY NEW BB LENS)             
         BZ    LR60                 NO                                          
*                                                                               
LR57     CLI   SVTANOTR,C'Y'       DON'T SHOW *T TRAFFIC ACTIVITY               
         BNE   LR58                                                             
         TM    TBADTAAC,X'10'      TEST MEDIA BUY ADDED AFTER TRAFFIC           
         BO    LR58                                                             
         TM    TBADTAAC,X'40'      TEST TRAFFIC TBA                             
         BO    LR60                 BYPASS                                      
*                                                                               
LR58     CLC   STDATE,TBADTAWK     MUST BE AFTER START DATE                     
         BH    LR60                                                             
         CLC   ENDATE,TBADTAWK     MUST BE BEFORE END DATE                      
         BNL   LR64                                                             
LR60     GOTO1 VRECUP,DMCB,AIO,(R6)                                             
         CLI   TBADTAEL,X'05'                                                   
         BNE   LR70                                                             
         B     LR56                                                             
*                                                                               
LR64     CLI   SLNFTR,0            WAS SPOT LEN FILTER ENTERED                  
         BE    LR66                                                             
         CLC   SLNFTR,TBADTASL     SAME                                         
         BNE   LR60                NO                                           
LR66     ZIC   R1,DSPSW                                                         
         LA    R1,1(,R1)                                                        
         STC   R1,DSPSW                                                         
         MVC   WORK(4),TBADTAWK    INVERT DATE & SLNS FOR SORT                  
         MVC   TBADTAWK(2),WORK+2                                               
         MVC   TBADTASL(2),WORK                                                 
         B     LR54                                                             
LR70     CLI   DSPSW,0             ANY VALID                                    
         BE    LR20                NO, NEXT REC                                 
*                                                                               
         BAS   RE,CKDELEST         CHECK FOR DELETED ESTIMATE                   
         BNE   LR20                 YES BYPASS                                  
*                                                                               
         LLC   R3,DSPSW                                                         
*                                                                               
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,FIXELEM          MAKE ALL ELEMS SAME LEN FOR XSORT            
*                                                                               
         LLC   R2,1(R6)                                                         
*                                                                               
         L     RF,=V(XSORT)                                                     
         A     RF,SPTR11RR                                                      
         GOTO1 (RF),DMCB,(R6),(R3),(R2),4,2                                     
*                                                                               
         MVC   WORK(4),TBADTAWK    INVERT DATE & SLNS AFTER SORT                
         MVC   TBADTAWK(2),WORK+2                                               
         MVC   TBADTASL(2),WORK                                                 
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRRSRT              GO SORT FOR OFFLINE REPORT                   
         J     EXIT                                                             
         EJECT                                                                  
* BUILD TABLE OF ACTIVE ELEMENTS FOR ONLINE LIST                                
*                                                                               
LO30     XC    SLNWKLST(96),SLNWKLST   LIST OF SLNS AND ACTIVITY WEEKS          
         XC    NUMSLN,NUMSLN       CLEAR NUMBER OF SPOTLEN PAIRS FOUND          
         BAS   RE,GETEL            GET FIRST ELEMENT                            
         BNE   LR20                NONE, SO GET NEXT RECORD                     
         CLI   TBLSW,1             HAS WEEK TABLE BEEN BUILT                    
         BE    LO33                YES                                          
         BAS   RE,TBL              GO BUILD WEEK TABLE AND HEADINGS             
         MVI   TBLSW,1                                                          
         B     LO33                GET REMAINING ELEMENTS                       
*                                                                               
         USING TBADTAEL,R6                                                      
*                                                                               
LO32     BAS   RE,NEXTEL           GET NEXT ACTIVITY ELEMENT                    
         BNE   LO60                NONE REMAINING                               
                                                                                
LO33     TM    TBADTAAC,X'80'      TEST ACTIVE                                  
         BO    LO34                 YES                                         
*                                                                               
         CLI   BBFILT,C'Y'         REQUESTED ALL BILLBOARDS                     
         BNE   LO32                 NO                                          
*                                                                               
*NOP     TM    TBADTAAC,X'05'      EITHER 5 OR 10 SEC BB'S                      
*                                                                               
         TM    TBADTAAC,TBADTABF+TBADTABT  5 OR 10 SEC BB'S                     
         BNZ   LO34                                                             
*                                                                               
         CLI   TBADTLN,TBADTEQ     NEW ELEM                                     
         BNE   LO32                 NO                                          
*                                                                               
         OC    TBADTBB(2),TBADTBB  AND TBADTBB1 (NEW BB LENS)                   
         BZ    LO32                 NO                                          
*                                                                               
LO34     CLI   SVTANOTR,C'Y'       DON'T SHOW *T TRAFFIC ACTIVITY               
         BNE   LO35                                                             
         TM    TBADTAAC,X'10'      TEST MEDIA BUY ADDED AFTER TRAFFIC           
         BO    LO35                                                             
         TM    TBADTAAC,X'40'      TEST TRAFFIC TBA                             
         BO    LO32                 BYPASS                                      
*                                                                               
LO35     CLC   STDATE,TBADTAWK     MUST BE AFTER START DATE                     
         BH    LO32                                                             
         CLC   ENDATE,TBADTAWK     MUST BE BEFORE END DATE                      
         BL    LO32                                                             
*                                                                               
         CLI   SLNFTR,0            WAS SPOT LEN FILTER ENTERED                  
         BE    LO36                ELEMENT IS OK                                
         CLC   SLNFTR,TBADTASL     SAME                                         
         BNE   LO32                NO                                           
*                                                                               
LO36     BAS   RE,CKDELEST         CHECK FOR DELETED ESTIMATE                   
         BNE   LR20                 YES BYPASS                                  
*                                                                               
         XC    SLNWKKEY,SLNWKKEY   CLEAR SPOT LENGTH/WEEK KEY                   
         MVC   SLNWKKEY(1),TBADTASL     MOVE SPOT LENGTHS TO KEY                
         MVC   SLNWKKEY+1(1),TBADTAS2                                           
*                                                                               
         LA    R3,WKTABLE          FIND ACTIVITY DATE IN TABLE                  
LO38     CLC   0(2,R3),TBADTAWK                                                 
         BE    LO40                                                             
         LA    R3,2(R3)                                                         
         CLI   0(R3),0             CHECK FOR END OF DATE TABLE                  
         BNE   LO38                                                             
         B     LO41                                                             
*                                                                               
LO40     LA    R1,WKTABLE          BEGINNING OF WEEK TABLE                      
         LR    R0,R3               LOCATION OF DATE IN TABLE                    
         SR    R0,R1               DISPLACEMENT INTO TABLE                      
         SRL   R0,1                NUMBER OF WEEKS                              
         SRDL  R0,3                DIVIDE BY 8 (8 WEEKS/BYTE)                   
         LR    RF,R0               DISPLACEMENT IN BYTES                        
         LR    RE,R0               DISPLACEMENT IN BYTES                        
         LA    RF,SLNWKKEY+2(RF)   POINT TO BYTE                                
         SRL   R1,29               BIT POSITION                                 
         IC    R1,BITTAB(R1)                                                    
         STC   R1,0(RF)            SET BIT IN APPROPRIATE KEY SLOT              
*                                                                               
* CHECK IF ANY BILLBOARDS                                                       
*                                                                               
         TM    TBADTAAC,TBADTABF+TBADTABT                                       
         BNZ   LO40C                                                            
*                                                                               
         CLI   TBADTLN,TBADTEQ     NEW ELEM                                     
         BNE   LO41                 NO                                          
*                                                                               
         OC    TBADTBB(2),TBADTBB  AND TRADTBB1 (NEW BB LENS)                   
         BZ    LO41                                                             
*                                                                               
LO40C    LA    RE,SLNWKKEY+6(RE)   POINT TO BYTE                                
         STC   R1,0(RE)            SET BIT IN APPROPRIATE KEY SLOT              
*                                                                               
LO41     MVC   DMCB+8(4),NUMSLN    NUMBER OF RECORDS IN TABLE                   
         GOTO1 =V(BINSRCH),DMCB,(1,SLNWKKEY),SLNWKLST,,10,(0,2),16,    +        
               RR=SPTR11RR                                                      
         L     R3,0(R1)            RETURNED TABLE LOCATION                      
         CLI   0(R1),X'01'         SEE IF A MATCH WAS FOUND                     
         BNE   LO42                THERE WAS A MATCH, SO MERGE DATES            
         LTR   R3,R3               SEE IF TABLE IS FULL                         
         BZ    LO64                FULL - SAVE KEY AND START DISPLAY            
*                                                                               
         L     R1,NUMSLN           INCREMENT THE NUMBER OF SLNS FOUND           
         LA    R1,1(R1)                                                         
         ST    R1,NUMSLN                                                        
         B     LO32                NEXT ACTIVITY ELEMENT                        
*                                                                               
LO42     OC    2(8,R3),SLNWKKEY+2  OR ACTIVITY BITS FOR THIS SLN PAIR           
         B     LO32                NEXT ACTIVITY ELEMENT                        
*                                                                               
LO60     L     R0,NUMSLN           GENERATE TABLE RECORDS FOR SLN PAIRS         
         LTR   R0,R0               TEST ANY ACTIVE ELEMENTS FOUND               
         BZ    LR20                NO, GET NEXT TBA RECORD                      
         LA    R1,SLNWKLST         EACH SPOT LENGTH WITH ACTIVITY WEEKS         
         CLI   ESTSTAT,C'N'        SEE IF ESTIMATE = 'NO'                       
         BE    LO80                IT IS                                        
*                                                                               
LO62     MVC   TBLMKT,TBAKMKT      ENTER KEYS IN TABLE                          
         MVC   TBLSTA,TBAKSTA                                                   
         MVC   TBLPRD,TBAKPRD                                                   
         MVC   TBLEST,TBAKEST                                                   
         MVC   TBLPRD2,TBAKPRD2                                                 
         MVC   TBLSLN,0(R1)                                                     
         MVC   TBLSLN2,1(R1)                                                    
*                                                                               
         MVC   TBLWKS,2(R1)                                                     
         MVC   TBLBBS,6(R1)                                                     
         MVC   TBLDSKAD,KEY+14                                                  
*                                                                               
         LA    R1,10(R1)           NEXT SPOT LENGTH PAIR                        
         LA    R5,TBLNEXT(,R5)     ADDRESS OF NEXT RECORD IN TABLE              
         LR    R3,R9                                                            
         AHI   R3,X'2800'          END OF SAVED STORAGE                         
         AHI   R3,-TBLNEXT                                                      
         CR    R5,R3               TEST TABLE TOO LARGE                         
         BNH   LO66                NO-                                          
*                                                                               
LO64     MVC   SAVEKEY,KEY         SAVE KEY TO PROCESS NEXT                     
         B     LRL                 AND GO START DISPLAY                         
*                                                                               
LO66     L     RF,TABLESZ          INCREMENT THE NUMBER OF RECORDS              
         LA    RF,1(RF)                                                         
         ST    RF,TABLESZ                                                       
         BCT   R0,LO62             NEXT SPOT LENGTH PAIR                        
         B     LR20                GET NEXT TBA RECORD                          
*                                                                               
LO80     MVC   TABLEKEY(2),TBAKMKT      CREATE KEY WITHOUT ESTIMATE             
         MVC   TABLEKEY+2(3),TBAKSTA                                            
         MVC   TABLEKEY+5(1),TBAKPRD                                            
         MVC   TABLEKEY+7(1),TBAKPRD2                                           
         MVC   TABLEKEY+6(1),0(R1)      SLN1                                    
         MVC   TABLEKEY+8(1),1(R1)      SLN2                                    
*                                                                               
         LA    RF,TABLE                                                         
LO82     CR    RF,R5               TEST END OF TABLE                            
         BE    LO86                YES, ADD NEW TABLE ENTRY                     
         CLC   TABLEKEY(9),0(RF)   TEST KEY MATCH                               
         BE    LO84                YES, MERGE ACTIVITY DATES                    
         LA    RF,TBLNEXT(,RF)     NO, TRY NEXT TABLE ENTRY                     
         B     LO82                                                             
*                                                                               
LO84     MVI   9(RF),0             ESTIMATE = 0                                 
         OC    14(8,RF),2(R1)      MERGE ACTIVITY DATES                         
         LA    R1,10(R1)           NEXT SPOT LENGTH PAIR                        
         B     LO88                                                             
*                                                                               
LO86     MVC   TBLRECKY,TABLEKEY     MOVE KEY TO TABLE                          
         MVC   TBLEST,TBAKEST                                                   
         MVC   TBLWKS,2(R1)                                                     
         LA    R1,10(R1)           NEXT SPOT LENGTH PAIR                        
         LA    R5,TBLNEXT(,R5)     NEXT RECORD IN TABLE                         
         LR    R3,R9                                                            
         AHI   R3,X'2800'          END OF SAVED STORAGE                         
         AHI   R3,-TBLNEXT                                                      
         CR    R5,R3               TEST TABLE TOO LARGE                         
         BH    LO64                STOP HERE                                    
         L     RF,TABLESZ          INCREMENT THE NUMBER OF RECORDS              
         LA    RF,1(RF)                                                         
         ST    RF,TABLESZ                                                       
*                                                                               
LO88     BCT   R0,LO80             NEXT SPOT LENGTH PAIR                        
         B     LR20                GET NEXT TBA RECORD                          
*                                                                               
BITTAB   DC    XL1'80,40,20,10,08,04,02,01'                                     
         EJECT                                                                  
* FORMAT ONLINE LIST *                                                          
*                                                                               
LRL      MVI   NLISTS,14           ONLY 14 LINES ON SCREEN                      
*                                                                               
         OC    TABLESZ,TABLESZ     TEST TABLE EMPTY                             
         BZ    NOACTERR            IT IS EMPTY                                  
*                                                                               
         CLC   TABLESZ,TBLINDEX    FIX RUNNING BEYOND EOT                       
         BE    LRL50                                                            
*                                                                               
         MVC   SCRINDEX,TBLINDEX                                                
         LA    R4,TABLE                                                         
         DROP  R5                                                               
         USING TBLRECD,R4                                                       
         L     R0,TBLINDEX                                                      
         LTR   R0,R0               TEST BEGINNING OF LIST                       
         BZ    LRL06                                                            
LRL04    LA    R4,TBLNEXT(,R4)     FIND NEXT TABLE ENTRY                        
         BCT   R0,LRL04                                                         
*                                                                               
LRL06    DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   SVMKTSTA(2),TBLMKT                                               
         MVC   SVMKTSTA+2(3),TBLSTA                                             
         BRAS  RE,FMTSTA                                                        
         MVC   LSTMKT,QMKT                                                      
         MVC   LSTSTA,STAPRNT                                                   
*                                                                               
         CLC   =C'BCM',CONREC      TEST BUYACT/LIST SCREEN                      
         BNE   *+12                NO                                           
         CLI   SVTANOAD,C'Y'       TEST SUPPRESS NO ADDR FLAG                   
         BE    LRL07                                                            
         MVC   LSTSTA-1(1),STADRSW                                              
         MVC   LSTSTA+7(1),STADRSW                                              
*                                                                               
LRL07    OC    STANET,STANET       CABLE HEAD STATION                           
         BZ    *+10                                                             
         MVC   LSTSTA(8),STANET                                                 
*                                                                               
         EDIT  (B1,TBLEST),(3,QEST)                                             
         CLI   ESTSTAT,C'N'        SEE IF ESTIMATE KEY IS 'NO'                  
         BE    LRL08               IT IS                                        
         MVC   LSTEST,QEST                                                      
*                                                                               
* READ MARKET RECORD FOR NAME *                                                 
*                                                                               
LRL08    MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO3                                                          
         USING MKTRECD,R5                                                       
*                                                                               
         CLC   KEY(8),0(R5)                                                     
         BE    LRL10                                                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO3                     
LRL10    LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   LSTMKTNM,0(R1)                                                   
         EJECT                                                                  
* FIND PRINTABLE PROD 1 AND EDIT WITH SPOT LENGTH *                             
*                                                                               
         L     R5,ASVCLIST                                                      
LRL12    CLC   3(1,R5),TBLPRD      MATCH PRD CODE                               
         BE    LRL14                                                            
         LA    R5,4(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    LRL12                                                            
         LA    R5,=C'???'          SET UNKNOWN PRODUCT                          
*                                                                               
LRL14    MVC   PROD,0(R5)                                                       
         MVC   LSTPRD(3),PROD                                                   
         LA    R3,LSTPRD+3                                                      
         CLI   LSTPRD+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  (B1,TBLSLN),(3,1(R3)),ALIGN=LEFT                                 
*                                                                               
* FIND PRINTABLE PROD 2 AND EDIT WITH SPOT LENGTH *                             
*                                                                               
         CLI   TBLPRD2,0                                                        
         BE    LRL20                                                            
         L     R5,ASVCLIST                                                      
LRL16    CLC   3(1,R5),TBLPRD2     MATCH PRD CODE                               
         BE    LRL18                                                            
         LA    R5,4(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    LRL16                                                            
         LA    R5,=C'???'          SET UNKNOWN PRODUCT                          
*                                                                               
LRL18    MVC   PROD2,0(R5)                                                      
         MVC   LSTPTR(3),PROD2                                                  
         LA    R3,LSTPTR+3                                                      
         CLI   LSTPTR+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  (B1,TBLSLN2),(3,1(R3)),ALIGN=LEFT                                
         EJECT                                                                  
* GET FIRST ACTIVE DATE *                                                       
*                                                                               
LRL20    OC    TBLBBS,TBLBBS       ANY BILLBOARDS                               
         BZ    *+10                                                             
         MVC   LSTIND+1(2),=C'BB'                                               
*                                                                               
         ICM   R1,15,TBLWKS        FIND FIRST ACTIVITY DATE                     
         LA    R3,WKTABLE                                                       
         LA    R0,31                                                            
LRL22    LTR   R1,R1                                                            
         BM    LRL30               TEST DATE ACTIVE                             
         LA    R3,2(R3)                                                         
         SLL   R1,1                                                             
         BCT   R0,LRL22                                                         
         MVC   LSTDTE(10),=C'SUBSEQUENT'                                        
         B     LRL34                                                            
LRL30    MVC   ACTVDATE,0(R3)                                                   
         GOTO1 DATCON,DMCB,(2,ACTVDATE),(8,LSTDTE)                              
*                                                                               
LRL34    L     R1,TBLINDEX         INCREMENT INDEX INTO TABLE                   
         LA    R1,1(R1)                                                         
         ST    R1,TBLINDEX                                                      
*        MVC   DMDSKADD,TBLINDEX   PASS TO GENCON                               
         XC    DMDSKADD,DMDSKADD   STOP READ                                    
*                                                                               
         CLI   DSKADSW,C'Y'        SHOW DISK ADDRESS                            
         BNE   LRL40                                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,TBLDSKAD,LSTPTR,4                                    
*                                                                               
LRL40    EQU   *                                                                
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         L     R1,TABLESZ                                                       
         C     R1,TBLINDEX         TEST ALL RECORDS DISPLAYED                   
         BNH   LRL50               YES                                          
         LA    R4,TBLNEXT(,R4)     NEXT TABLE ENTRY                             
         OC    0(5,R4),0(R4)       SINCE TABLE COUNTER IS WRONG                 
         BNZ   LRL06               CONTINUE IF MORE DATA                        
*                                                                               
LRL50    XC    TBLINDEX,TBLINDEX   RESET TABLE INDEX                            
         OC    SAVEKEY,SAVEKEY     TEST HAVE MORE                               
         BNZ   LRL52                                                            
         NI    TRAMEDH+4,X'FF'-X'20'  FORCE MEDIA VALIDATION                    
         J     EXIT                                                             
*                                                                               
LRL52    OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
LRL54    MVC   LISTAR,SPACES       SEND SPACES TO REST OF LINES                 
         GOTO1 LISTMON                                                          
         B     LRL54                                                            
         DROP  R5                                                               
         EJECT                                                                  
* FORMAT SORT RECS FOR OFFLINE REPORT HERE *                                    
*                                                                               
LRRSRT   L     R5,AIO2                                                          
         USING SORTREC,R5                                                       
         L     R4,AIO1                                                          
         USING TBAKEY,R4                                                        
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         OC    8(3,R6),8(R6)                                                    
         BZ    *+14                                                             
         MVC   DATEUPDT,8(R6)                                                   
         B     *+10                                                             
         MVC   DATEUPDT,2(R6)                                                   
         GOTO1 DATCON,DMCB,(3,DATEUPDT),(2,SRTUPDT)                             
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   SRTEST,TBAKEST                                                   
         CLI   ESTSTAT,C'Y'        TEST ESTIMATE STATUS                         
         BE    *+8                  IT IS NOT 'NO'                              
         MVI   SRTEST,0            SORT ACROSS ALL ESTIMATES                    
*                                                                               
         MVC   SRTCLT,TBAKCLT                                                   
*                                                                               
         CLC   SVBCLT,TBAKCLT                                                   
         BE    LRRS02                                                           
         MVC   SVBCLT,TBAKCLT                                                   
         BRAS  RE,FCLTR                                                         
         BNE   LR25                END OF AGENCY                                
*                                                                               
LRRS02   L     R1,ASVCLIST                                                      
*                                                                               
LRRS03   CLC   3(1,R1),TBAKPRD     MATCH PRD CODE                               
         BE    LRRS04                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    LRRS03                                                           
         LA    R1,=C'???'          SET UNKNOWN PRODUCT                          
*                                                                               
LRRS04   MVC   SRTPROD,0(R1)                                                    
         MVC   SRTMKST,TBAKMKT                                                  
         XC    SRTPROD2,SRTPROD2                                                
         CLI   TBAKPRD2,0          ANY PTR                                      
         BE    LRRS10              NO                                           
         L     R1,ASVCLIST                                                      
LRRS06   CLC   3(1,R1),TBAKPRD2    MATCH PRD CODE                               
         BE    LRRS08                                                           
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    LRRS06                                                           
         LA    R1,=C'???'          SET UNKNOWN PRODUCT                          
*                                                                               
LRRS08   MVC   SRTPROD2,0(R1)                                                   
*                                                                               
LRRS10   MVC   SRTSLN,TBADTASL                                                  
         MVC   SRTSLN2,TBADTAS2                                                 
         MVC   SVSLNS,TBADTASL                                                  
*                                                                               
LRRS12   MVC   SRTDT(2),TBADTAWK   WEEK DATE                                    
         MVC   SRTSTAT(1),TBADTAAC STATUS                                       
*                                                                               
         CLI   TBADTLN,TBADTEQ     NEW ELEM                                     
         BNE   *+10                 NO                                          
         MVC   SRTSTAT+1(2),TBADTBB AND TBADTBB1 (MORE BB)                      
                                                                                
         LA    R5,L'SRTENT(,R5)                                                 
LRRS20   BAS   RE,NEXTEL                                                        
         BNE   LRRS30                                                           
         MVC   WORK(4),TBADTAWK    INVERT DATE & SLNS AFTER SORT                
         MVC   TBADTAWK(2),WORK+2                                               
         MVC   TBADTASL(2),WORK                                                 
         CLC   SVSLNS,TBADTASL                                                  
         BE    LRRS12                                                           
LRRS30   L     R3,AIO2                                                          
         SR    R5,R3                                                            
         LA    R5,23(,R5)                                                       
         STH   R5,0(,R3)                                                        
         GOTO1 =V(SORTER),DMCB,=C'PUT',(R3)                                     
         LR    R5,R3                                                            
         CLI   TBADTAEL,X'05'      ANY MORE TO PROCESS                          
         BE    LRRS10              YES                                          
         B     LR20                                                             
         EJECT                                                                  
* FORMAT OFFLINE REPORT HERE *                                                  
*                                                                               
LRRPRT   LM    R0,R1,=A(HDHK,HEADING)     HEADING ROUTINE FOR REPORT            
         A     R0,SPTR11RR                                                      
         ST    R0,HEADHOOK         STORE FOR CONTROLLER                         
         A     R1,SPTR11RR                                                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         L     RE,AIO2                                                          
         LA    RF,2000             ZERO TABLE AREA                              
         XCEF                                                                   
*                                                                               
         OC    BCLT,BCLT           WAS SPECIFIC CLT ENTERED                     
         BNZ   *+10                 YES                                         
         XC    SVBCLT,SVBCLT                                                    
*                                                                               
         MVI   EOFSORT,C'N'                                                     
         USING SORTREC,R5                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET' GET FIRST RECORD                         
         ICM   R5,15,4(R1)                                                      
         BNZ   LRRP04              AT LEAST 1 SORT REC                          
*                                                                               
NOACTIV  MVC   P(44),=C' *** NO ACTIVITY SINCE LAST INSTRUCTIONS ***'           
         XC    HEADHOOK,HEADHOOK   DON'T GO TO HEADHOOK ROUTINE                 
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         J     EXIT                                                             
*                                                                               
LRRP04   LH    RF,SRTLEN          GET RECORD LENGTH                             
         LR    R3,RF                                                            
         LR    RE,R5                                                            
         L     R2,AIO1                                                          
         LR    R5,R2                                                            
         MVCL  R2,RE              MOVE SORT RECORD TO AIO1                      
         LA    R2,P               ADDRESS OF PRINT AREA                         
         USING PRTLINE,R2                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'   NEXT RECORD                            
         ICM   R6,15,4(R1)                                                      
         BNZ   LRRP06              TEST NOT EOF                                 
         MVI   EOFSORT,C'Y'                                                     
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         B     LRRP14                                                           
*                                                                               
LRRP06   CLC   4(16,R6),4(R5)     TEST IDENTICAL KEY ACROSS ESTIMATE            
         BNE   LRRP14                                                           
         MVI   SPOOLOK,C'N'       FILL UP PRINT LINE, BUT DON'T PRINT           
         B     LRRP16              (PRINT LINES GET MERGED)                     
*                                                                               
LRRP14   MVI   SPOOLOK,C'Y'       OK TO PRINT                                   
LRRP16   CLC   SVBCLT,SRTCLT      EJECT ON CHANGE OF CLIENT                     
         BE    LRRP20                                                           
         MVC   SVBCLT,SRTCLT                                                    
         BRAS  RE,FCLTR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FORCEHED,C'Y'                                                    
LRRP20   CLC   SRTPROD,PROD       EJECT ON CHANGE OF PRODUCT                    
         BE    LRRP22                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   PROD,SRTPROD                                                     
LRRP22   CLC   SRTEST,EST         EJECT ON CHANGE OF ESTIMATE                   
         BE    LRRP24                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVC   EST,SRTEST                                                       
*                                                                               
LRRP24   CLI   FORCEHED,C'Y'       IF FORCING HEADLINE, PRINT MARKET            
         BE    LRRP26                                                           
         CLC   SVMKTSTA(2),SRTMKST CHANGE IN MARKET                             
         BE    LRRP28               NO                                          
*                                                                               
* READ MARKET RECORD FOR NAME AND PRINT ON SEPARATE LINE *                      
*                                                                               
LRRP26   MVC   SVMKTSTA(2),SRTMKST CHANGE IN MARKET                             
         SR    R0,R0                                                            
         ICM   R0,3,SRTMKST                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
*                                                                               
         BAS   RE,RMKTNM                                                        
*                                                                               
         MVC   PRTMKT,QMKT                                                      
         MVC   PRTMKTNM,MKTNM                                                   
*                                                                               
         MVI   PRTMKTSW,C'Y'       SET PRINTING MARKET NAME LINE                
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     LET CONTROLLER BUILD PAGE                    
*                                                                               
         MVI   PRTMKTSW,C'N'       SET OFF PRINTING MARKET NAME LINE            
*                                                                               
LRRP28   MVC   SVMKTSTA,SRTMKST                                                 
         BRAS  RE,FMTSTA                                                        
         MVC   PRTSTA,STAPRNT                                                   
*                                                                               
         OC    STANET,STANET       CABLE HEAD STATION                           
         BZ    *+10                                                             
         MVC   PRTSTA(8),STANET                                                 
*                                                                               
         CLI   STADRSW,C'*'                                                     
         BNE   LRRP29B                                                          
         CLC   =C'BCM',CONREC      TEST BUYACT/LIST SCREEN                      
         BNE   LRRP29A             NO                                           
         CLI   SVTANOAD,C'Y'                                                    
         BE    LRRP29B                                                          
*                                                                               
LRRP29A  MVC   PRTSTA-3+132(14),=C'* NO ADDRESS *'                              
*                                                                               
LRRP29B  MVC   PRTAFF,STAAFFL                                                   
*                                                                               
         CLC   TODAY,SRTUPDT                                                    
         BNE   *+8                                                              
         MVI   PRTNEW,C'*'                                                      
*                                                                               
LRRP30   EDIT  (B1,SRTSLN),(3,PRTSLN),ALIGN=LEFT                                
*                                                                               
         OC    SRTPROD2,SRTPROD2                                                
         BZ    LRRP40                                                           
         MVC   PRTPTR(3),SRTPROD2                                               
         LA    R3,PRTPTR+3                                                      
         CLI   PRTPTR+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  (B1,SRTSLN2),(3,1(R3)),ALIGN=LEFT                                
*                                                                               
LRRP40   LH    R1,SRTLEN                                                        
         AHI   R1,-23              SUBT FIXED REC LEN                           
         SR    R0,R0                                                            
         D     R0,=F'5'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1                                                            
         LA    R1,PRTDTES+1                                                     
         DROP  R2                                                               
         LA    R2,WKTABLE                                                       
         LA    R3,SRTDT                                                         
LRRP42   CLC   0(2,R2),0(R3)       LOOK FOR DATE IN TABLE                       
         BE    LRRP44                                                           
         LA    R1,4(,R1)                                                        
         LA    R2,2(,R2)                                                        
         B     LRRP42                                                           
LRRP44   MVI   0(R1),C'*'          SHOW ACTIVE DATE WITH ASTERISK               
         MVI   1(R1),C'M'          SHOW AS MEDIA ENTRY                          
*                                                                               
         TM    2(R3),X'40'         SET ON BY TRAFFIC                            
         BZ    LRRP50               NO                                          
         MVI   1(R1),C'T'          SHOW ACTIVE DATE WITH TRAFFIC IND            
*                                                                               
         TM    2(R3),X'10'         SET ON BY MEDIA (AFTER TRAFFIC)              
         BZ    LRRP50                                                           
         MVI   2(R1),C'M'          SHOW ACTIVE DATE W TRAF/MEDIA INDS           
*                                                                               
* DISPLAY LONGEST BILLBOARD LEN (IF ANY)                                        
LRRP50   DS    0H                                                               
         ST    R4,FULL             SAVE R4                                      
*                                                                               
         LLC   R4,3(R3)            BB BIT FROM SORT                             
         LA    R5,BBTABLE0         BB TABLE LEN=15                              
         BAS   RE,FBB                                                           
         BE    LRRP55                                                           
                                                                                
         TM    2(R3),TBADTABF      TEST FOR 5 SEC BILLBOARD                     
         BZ    *+10                                                             
         MVC   132(2,R1),=C'B5'                                                 
                                                                                
         TM    2(R3),TBADTABT      TEST FOR 10SEC BILLBOARD                     
         BZ    *+14                                                             
         MVC   132(3,R1),=C'B10'                                                
         B     LRRP55                                                           
*                                                                               
         LLC   R4,3(R3)            BB BIT FROM SORT                             
         LA    R5,BBTABLE1         BB TABLE                                     
         BAS   RE,FBB                                                           
         BE    LRRP55                                                           
*                                                                               
         LLC   R4,4(R3)            BB BIT FROM SORT                             
         LA    R5,BBTABLE2         BB TABLE                                     
         BAS   RE,FBB                                                           
*                                                                               
LRRP55   L     R4,FULL             RESTORE R4                                   
*                                                                               
         LA    R1,4(,R1)           BUMP IN PRINT LINE                           
         LA    R2,2(,R2)           BUMP IN WKTABLE                              
         LA    R3,L'SRTENT(,R3)                                                 
         BCT   R0,LRRP42                                                        
*                                                                               
         LR    R5,R6               SAVE ADDRESS OF NEXT RECORD                  
         CLI   SPOOLOK,C'Y'        TEST OK TO PRINT                             
         BNE   LRRP04              NO, LEAVE                                    
         GOTO1 SPOOL,DMCB,(R8)     LET CONTROLLER BUILD PAGE                    
*                                                                               
         CLI   EOFSORT,C'N'        TEST END OF SORT FILE                        
         BE    LRRP04                                                           
*                                                                               
         J     EXIT                                                             
*                                                                               
* MAKE ALL ELEMENTS SAME LENGTH  (IF THEY ARE NOT ALREADY)                      
*                                                                               
FIXELEM  NTR1                                                                   
*                                                                               
* COMPARE ELEM LENGTHS                                                          
         LLC   R2,1(R6)            ELEM LEN                                     
FIXE02   BRAS  RE,NEXTEL           GET NEXT ACTIVITY ELEMENT                    
         BNE   FIXEX                                                            
         LLC   R1,1(R6)                                                         
         CR    R2,R1                                                            
         BE    FIXE02                                                           
*                                                                               
* MAKE ALL ELEMS SAME LEN (9)                                                   
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FIXE05   CLI   1(R6),TBADTEQ       NEW ELEM LEN                                 
         BE    FIXE10                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(7),0(R6)     SAVE ELEM INFO                                 
*                                                                               
         GOTO1 VRECUP,DMCB,AIO1,(R6)  REMOVE OLD ELEM                           
*                                                                               
         MVI   ELEM+1,9          ADD BIGGER ELEM                                
         GOTO1 VRECUP,DMCB,AIO1,ELEM,(C'R',(R6))                                
*                                                                               
FIXE10   BRAS  RE,NEXTEL                                                        
         BE    FIXE05                                                           
*                                                                               
FIXEX    J     EXIT                                                             
*                                                                               
*                                                                               
* FIND BILLBOARD IN TABLE                                                       
*                                                                               
FBB      NTR1                                                                   
         LA    R0,BBTABLN1         NUM OF ENTRIES IN THE TABLE                  
                                                                                
FBB02    EX    R4,TMBB             MATCH ON BB?                                 
         BNZ   FBB10                                                            
                                                                                
         LA    R5,BBTBLENT(R5)     BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,FBB02                                                         
         B     FBBNE                                                            
                                                                                
FBB10    MVC   132(3,R1),1(R5)                                                  
                                                                                
         CR    RB,RB               SET CC EQ                                    
         B     FBBX                                                             
                                                                                
FBBNE    LTR   RB,RB               SET CC NEQ                                   
FBBX     XIT1                                                                   
                                                                                
TMBB     TM    0(R5),0             BB SECONDS LEN                               
                                                                                
*                                                                               
* TABLE OF VALID BILLBOARDS                                                     
*                                                                               
         DS    0D                                                               
BBTABLE0 DC    X'80',C'B15'            TBABB15=X'80'                            
         DC    X'08',C'A15'            TBABBA15=X'08'                           
         DC    X'01',C'S15'            TBABBS15=X'01'                           
BBTABLN0  EQU   (*-BBTABLE0)/BBTBLENT                                           
BBTBLENT  EQU   4                                                               
         DS    0D                                                               
BBTABLE1 DC    X'04',C'A10'            TBABBA10=X'04'                           
         DC    X'40',C'S10'            TBABBS10=X'40'                           
         DC    X'10',C'B7 '            TBABB7=X'10'                             
         DC    X'02',C'A7 '            TBABBA7=X'02'                            
         DC    X'20',C'S7 '            TBABBS7=X'20'                            
BBTABLN1  EQU   (*-BBTABLE1)/BBTBLENT                                           
*                                                                               
         DS    0D                                                               
BBTABLE2 DC    X'80',C'A5 '      TBABBA5=X'80'                                  
         DC    X'40',C'S5 '      TBABBS5=X'40'                                  
BBTABLN2  EQU   (*-BBTABLE2)/BBTBLENT                                           
*                                                                               
* BUILD TABLE OF WEEKS                                                          
*                                                                               
         USING TBADTAEL,R6                                                      
TBL      NTR1                                                                   
         XC    WKTABLE,WKTABLE                                                  
         XC    WKHDA,WKHDA                                                      
         XC    WKHDB,WKHDB                                                      
         MVC   WKTABLE,TBADTAWK                                                 
         GOTO1 DATCON,DMCB,(2,WKTABLE),(0,WORK)                                 
         CLC   WKTABLE,STDATE                                                   
         BE    TBL20                                                            
         BL    TBL14                                                            
TBL10    GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-7' SUB 1 WEEK                          
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WKTABLE+2)                             
         MVC   WORK(6),WORK+6                                                   
         CLC   WKTABLE+2(2),STDATE                                              
         BH    TBL10                                                            
         MVC   WKTABLE(2),WKTABLE+2                                             
         B     TBL20                                                            
TBL14    GOTO1 ADDAY,DMCB,WORK,WORK+6,F'7' ADD 1 WEEK                           
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WKTABLE+2)                             
         GOTO1 (RF),(R1),(0,WORK+6),(2,WKTABLE+2)                               
         MVC   WORK(6),WORK+6                                                   
         MVC   WKTABLE(2),WKTABLE+2                                             
         CLC   WKTABLE+2(2),STDATE                                              
         BL    TBL14                                                            
TBL20    ZIC   R2,PERWKS                                                        
         LA    R3,WKTABLE                                                       
         LA    R4,WKHDA                                                         
         LA    R5,WKHDB                                                         
TBL22    GOTO1 DATCON,DMCB,(2,(R3)),(0,WORK)                                    
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'7' ADD 1 WEEK                           
         GOTO1 DATCON,(R1),(0,WORK+6),(2,WORK+12)                               
         MVC   2(2,R3),WORK+12                                                  
         GOTO1 (RF),(R1),(2,(R3)),(5,WORK)                                      
         MVC   0(3,R4),WORK                                                     
         MVC   1(2,R5),WORK+3                                                   
         CLC   ENDATE,WORK+12                                                   
         BL    EXIT                                                             
         LA    R3,2(,R3)                                                        
         LA    R4,4(,R4)                                                        
         LA    R5,4(,R5)                                                        
         BCT   R2,TBL22                                                         
         J     EXIT                                                             
         EJECT                                                                  
* READ MARKET RECORD FOR NAME *                                                 
*                                                                               
RMKTNM   NTR1                                                                   
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R4,AIO2                                                          
         LA    R5,1960(,R4)        END OF TABLE-1                               
RMKTNM10 LA    R1,4(,R4)           MKT NAME IF ANY                              
         OC    0(4,R4),0(R4)       END OF TABLE                                 
         BZ    RMKTNM20                                                         
         CLC   QMKT,0(R4)                                                       
         BE    RMKTNM30                                                         
         LA    R4,28(,R4)                                                       
         CR    R4,R5                                                            
         BL    RMKTNM10                                                         
*                                                                               
RMKTNM20 L     R3,AIO3                                                          
         USING MKTRECD,R3                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO3                     
         LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(15),0(R3)                                                    
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
*                                                                               
         MVC   0(4,R4),QMKT                                                     
         MVC   4(24,R4),0(R1)                                                   
*                                                                               
RMKTNM30 MVC   MKTNM,0(R1)                                                      
*                                                                               
RMKTNMX  J     EXIT                                                             
         EJECT                                                                  
* CHECK FOR DELETED ESTIMATE *                                                  
*                                                                               
CKDELEST NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY             ESTIMATE HEADER                              
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,SVKEY+TBAKCLT-TBAKEY                                     
         L     RE,ASVCLIST                                                      
         LA    RF,220                                                           
*                                                                               
CKDEL10  CLC   3(1,RE),SVKEY+TBAKPRD-TBAKEY                                     
         BE    CKDEL14                                                          
         LA    RE,4(,RE)                                                        
         CLI   0(RE),C' '                                                       
         BNH   CKDEL26                                                          
         BCT   RF,CKDEL10                                                       
         B     CKDEL26                                                          
CKDEL14  MVC   EKEYPRD,0(RE)                                                    
         MVC   EKEYEST,SVKEY+TBAKEST-TBAKEY                                     
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,120                                                           
CKDEL20  OC    0(4,RE),0(RE)       EMPTY ENTRY (END)                            
         BZ    CKDEL30                                                          
         CLC   KEY+4(4),0(RE)                                                   
         BE    CKDEL26                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,CKDEL20                                                       
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         B     CKDEL30                                                          
*                                                                               
CKDEL26  MVC   KEY,SVKEY           RESTORE KEY                                  
         CR    RE,RE               SET COND CODE FOR GOOD RETURN                
         J     EXIT                                                             
         DROP  R4                                                               
*                                                                               
CKDEL30  DS   0H                                                                
*                                                                               
         MVC   FILENAME,=C'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM                 
*                                                                               
         GOTO1 HIGH                                                             
         MVC   WORK(96),KEY        SAVE BOTH KEY AND KEYSAVE                    
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   KEY,SVKEY           RESTORE ORIG KEY                             
         GOTO1 HIGH                SET FOR SEQ                                  
         CLC   KEY,SVKEY                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(5),WORK+48     WAS THIS AGYMD AND CLIENT                    
         BNE   CKDEL50              BYPASS STORE                                
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,120                                                           
CKDEL40  OC    0(6,RE),0(RE)       EMPTY ENTRY (END)                            
         BZ    CKDEL44                                                          
         LA    RE,4(,RE)                                                        
         BCT   RF,CKDEL40                                                       
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         LA    RE,BLOCK                                                         
CKDEL44  MVC   0(4,RE),WORK+4      SAVE WHATEVER PRD/EST WAS FOUND              
*                                                                               
CKDEL50  CLC   WORK(13),WORK+48    WAS EST HDR FOUND                            
         BE    EXIT                 YES                                         
*                                                                               
         CLC   AGENCY,=C'TH'       IS THIS ZENITH                               
         BE    CKDEL60                                                          
         CLC   AGENCY,=C'BS'       IS THIS BACKER                               
         BNE   EXIT                                                             
CKDEL60  MVI   SVKEY+TBAKEST-TBAKEY,0                                           
         L     RF,AIO1                                                          
         MVI   TBAKEST-TBAKEY(RF),0                                             
         J     EXIT                                                             
         EJECT                                                                  
* ROUTINE TO CLEAR LIST SCREEN                                                  
*                                                                               
CLEAR    NTR1                                                                   
         LA    R4,TRASELH          CLEAR LIST LINES OF ALL ACTIVITY             
         LA    R1,TRAPFKLH                                                      
*                                                                               
CLR02    XC    8(3,R4),8(R4)       CLEAR FIELD AND TRANSMIT                     
         OI    6(R4),X'80'                                                      
         LA    R4,11(R4)           LIST FIELD HEADER                            
*                                                                               
         XC    8(74,R4),8(R4)      CLEAR FIELD AND TRANSMIT                     
         OI    6(R4),X'80'                                                      
*                                                                               
         LA    R4,82(R4)           NEXT SELECT FIELD HEADER                     
         CR    R4,R1               DON'T SPILL OVER SCREEN                      
         BL    CLR02                                                            
         J     EXIT                                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
NONNUM   DS   0H                                                                
         LA    RE,NUMMSG                                                        
         LA    RF,L'NUMMSG                                                      
         J     ERREXIT                                                          
TABERR   DS   0H                                                                
         LA    RE,TBLMSG                                                        
         LA    RF,L'TBLMSG                                                      
         J     ERREXIT                                                          
ESTERR   DS   0H                                                                
         LA    RE,ESTMSG                                                        
         LA    RF,L'ESTMSG                                                      
         J     ERREXIT                                                          
NOPRDERR DS   0H                                                                
         LA    RE,NOPRDMSG                                                      
         LA    RF,L'NOPRDMSG                                                    
         LA    R2,TRAPRDH                                                       
         J     ERREXIT                                                          
NOESTERR DS   0H                                                                
         LA    RE,NOESTMSG                                                      
         LA    RF,L'NOESTMSG                                                    
         J     ERREXIT                                                          
TABLEERR DS   0H                                                                
         LA    RE,TABMSG                                                        
         LA    RF,L'TABMSG                                                      
         CLI   BPRD,0                                                           
         BNE   TBLERR1                                                          
         LA    R2,TRAPRDH                                                       
         J     ERREXIT                                                          
TBLERR1  CLI   BEST,0                                                           
         BNE   TBLERR2                                                          
         CLI   ESTSTAT,C'N'                                                     
         BE    TBLERR2                                                          
         LA    R2,TRAESTH                                                       
         J     ERREXIT                                                          
TBLERR2  OC    BMKTSTA,BMKTSTA                                                  
         BNZ   TBLERR3                                                          
         LA    R2,TRASTAH                                                       
         J     ERREXIT                                                          
OFFLNERR DS   0H                                                                
         LA    RE,OFFLNMS                                                       
         LA    RF,L'OFFLNMS                                                     
         J     ERREXIT                                                          
INVOFERR DS   0H                                                                
         LA    RE,INVOFMS                                                       
         LA    RF,L'INVOFMS                                                     
         J     ERREXIT                                                          
                                                                                
INVOFLER DS   0H                                                                
         LA    RE,INVOFLM                                                       
         LA    RF,L'INVOFLM                                                     
         J     ERREXIT                                                          
                                                                                
TBLERR3  LA    R2,TRAPERH                                                       
         J     ERREXIT                                                          
NOACTERR BAS   RE,CLEAR                                                         
         LA    RE,NOACTMSG                                                      
         LA    RF,L'NOACTMSG                                                    
         XC    CONHEAD,CONHEAD                                                  
         LA    R2,TRAMEDH                                                       
         J     ERREXIT                                                          
RUNERR   DS   0H                                                                
         LA    RE,RUNMSG                                                        
         LA    RF,L'RUNMSG                                                      
ERREXIT  DS   0H                                                                
         XC    CONHEAD,CONHEAD                                                  
         EX    RF,MVCMSG                                                        
         GOTO1 ERREX2                                                           
MVCMSG   MVC   CONHEAD(0),0(RE)                                                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
OFFLNMS  DC    C'* ERROR * OFFICE MUST BE * AND 1 OR 2 CHARACTER *'             
INVOFMS  DC    C'* ERROR * INVALID OFFICE *'                                    
INVOFLM  DC    C'* ERROR * INVALID OFFICE LIST *'                               
RUNMSG   DC    C'* ERROR * CAN''T BE RUN ONLINE *'                              
NOPRDMSG DC    C'* ERROR * ESTIMATE REQUIRES PRODUCT *'                         
NOESTMSG DC    C'* ERROR * NO SUCH ESTIMATE FOR THIS PRODUCT *'                 
TABMSG   DC    C'* ERROR * ENTER A PRD, EST OR SHORTEN THE PERIOD*'             
NOACTMSG DC    C'* NOTE * NO ACTIVITY SINCE LAST INSTRUCTIONS *'                
TBLMSG   DC    C'* ERROR * TOO MANY CLIENTS IN REQUESTED OFFICE *'              
NUMMSG   DC    C'* ERROR * OFFICE MUST BE NUMERIC *'                            
ESTMSG   DC    C'* ERROR * ESTIMATE MUST BE NUMERIC (1-255) OR ''NO'' *+        
               '                                                                
SORTCARD DC    CL80'SORT FIELDS=(5,16,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=600'                                   
*                                                                               
HEADING  SSPEC H1,3,C'MEDIA'                                                    
         SSPEC H1,39,C'TRAFFIC BUY ACTIVITY REPORT'                             
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'CLIENT'                                                   
         SSPEC H2,39,C'---------------------------'                             
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,3,C'PRODUCT'                                                  
         SSPEC H3,40,C'PERIOD'                                                  
         SSPEC H5,3,C'ESTIMATE'                                                 
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H8,3,C'MARKET'                                                   
         SSPEC H8,22,C'STATION  AFF'                                            
         SSPEC H8,37,C'SLN PARTNER'                                             
         SSPEC H8,50,C'---------------------- ACTIVITY WEEKS ----------+        
               -----------'                                                     
         SSPEC H9,3,C'------------------ -------  ---   --- -------'            
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
*================================================================               
* VALIDATE KEY ROUTINE                                                          
*================================================================               
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
VK00     TM    TRAMEDH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRACLTH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAPRDH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRASTAH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAESTH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAPERH+4,X'20'     VALIDATED                                    
         BZ    VK10                 NO                                          
         TM    TRAFLTRH+4,X'20'    VALIDATED                                    
         JO    EXIT                 YES                                         
*                                                                               
VK10     XC    SAVEKEY,SAVEKEY     CLEAR SAVED KEY                              
         XC    TBLINDEX,TBLINDEX         BUG FIX                                
*                                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT,BCLT                                                        
         XC    QCLT,QCLT                                                        
         XC    SVBCLT,SVBCLT                                                    
         MVI   BOFFCD,0                                                         
         CLI   5(R2),0             TEST DATA                                    
         BNE   VK20                                                             
*                                                                               
         CLI   CONREC,C'B'         TEST BUYACT                                  
         BNE   VK72                NO - TAM JUST NEEDS MEDIA                    
*                                                                               
         TM    WHEN,X'30'          SOON/OV                                      
         BZ    VK13                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY (SOON/OV)                         
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
VK13     TM    WHEN,X'80'          ONLINE LIST REQUIRES CLT                     
         BNZ   MISSERR                                                          
         CLI   ACTNUM,ACTLIST      OMIT ONLY FOR LIST                           
         BNE   MISSERR                                                          
         MVI   BYTE,0                                                           
         B     VK40                                                             
*                                                                               
VK20     CLI   8(R2),C'*'          TEST OFFICE CODE                             
         BE    VK25                                                             
         CLI   8(R2),C'$'          TEST OFFICE LIST                             
         BE    VK24                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BE    VK25                                                             
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT         SAVE BCLT TO STOP TA PROFILE LOOKUP          
         MVC   BYTE,SVCLTOFF                                                    
         B     VK40                                                             
                                                                                
VK24     DS    0H                                                               
         XC    WORK,WORK           READ T0 PROFILE                              
         MVC   WORK(2),=C'S0'                                                   
         MVC   WORK+2(2),TRACLT                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVI   WORK+10,C'*'                                                     
         GOTO1 GETPROF,DMCB,WORK,SVPROFFC,DATAMGR                               
         OC    SVPROFFC,SVPROFFC                                                
         BZ    INVOFLER                                                         
                                                                                
VK25     CLI   5(R2),3                                                          
         BH    OFFLNERR                                                         
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         BNZ   TRAPERR                                                          
*                                                                               
         MVI   ERROR,0                                                          
         CLI   8(R2),C'*'          BY OFFICE                                    
         BNE   VK35                                                             
*                                                                               
* VALIDATE AND CONVERT TO 1 BYTE IF NEEDED                                      
*                                                                               
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,9(R2)       2 CHAR OFFICE CODE                           
         CLI   5(R2),3                                                          
         BE    *+10                                                             
         OC    OFCOFC2,SPACES      1 CHAR OFFICE PADDED W/SPACE                 
*                                                                               
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VK30                    YES                                      
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VK28                                                             
*                                                                               
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VK28                VALIDATE AND CONVERT                         
*                                                                               
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VK28                                                             
*                                                                               
         MVI   LAOFFICE,0          INIT LIMITED ACCESS OFFICE                   
*                                                                               
         BRAS  RE,GOFF             GET OFFICE FOR THIS CLIENT                   
         B     VK30                                                             
*                                                                               
VK28     MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
*                                                                               
VK30     XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   INVOFERR                                                         
*                                                                               
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    VK35                                                             
*                                                                               
         MVC   BOFFCD,OFCOFC       SAVE 1 BYTE OFFICE CODE                      
         B     *+10                                                             
VK35     MVC   BOFFCD(1),9(R2)                                                  
         CLI   8(R2),C'$'                                                       
         BNE   *+8                                                              
         MVI   BOFFCD,0                                                         
*                                                                               
         BRAS  RE,VOFF                                                          
         MVC   BYTE,BOFFCD                                                      
         DROP  R3                                                               
*                                                                               
VK40     XC    WORK,WORK           READ T0 PROFILE                              
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),BYTE                                                  
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
*MNMB                                                                           
* READ T3 PROFILE *                                                             
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT3PR06,SVT1PROF+5                                              
*MNMB                                                                           
*                                                                               
         MVI   WORK+3,C'A'         READ TA PROFILE                              
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
VK45     CLI   CONREC,C'B'         TEST BUYACT                                  
         BNE   VK72                MIGHT WANT TO VALIDATE #DAYS                 
*                                                                               
         LA    R2,TRAPRDH                                                       
         MVI   BPRD,0              CLEAR PRD                                    
         CLI   5(R2),0             TEST DATA                                    
         BNE   VK50                                                             
         CLI   ACTNUM,ACTLIST      OMIT ONLY FOR LIST                           
         BNE   MISSERR                                                          
         B     VK55                                                             
*                                                                               
VK50     CLC   =C'ALL',8(R2)                                                    
         BE    VK55                                                             
         CLC   =C'POL',8(R2)                                                    
         BE    VK55                                                             
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
*                                                                               
VK55     XC    BMKTSTA,BMKTSTA                                                  
         LA    R2,TRASTAH                                                       
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   MISSERR                                                          
         XC    TRAMKNM,TRAMKNM                                                  
         OI    TRAMKNMH+6,X'80'                                                 
         B     VK70                                                             
*                                                                               
VK60     GOTO1 VALISTA                                                          
         MVC   TRAMKNM,MKTNM                                                    
         OI    TRAMKNMH+6,X'80'                                                 
*                                                                               
VK70     LA    R2,TRAPERH                                                       
         BRAS  RE,VPER             CALL TO VPER                                 
*                                                                               
         LA    R2,TRAFLTRH         VALIDATE FILTERS                             
         BRAS  RE,VFTR                                                          
*                                                                               
         LA    R2,TRAESTH          VALIDATE ESTIMATE                            
         BAS   RE,VEST                                                          
*                                                                               
VK72     OI    TRACLTH+4,X'20'     VALIDATED                                    
         OI    TRAMEDH+4,X'20'     VALIDATED                                    
         OI    TRAPRDH+4,X'20'     VALIDATED                                    
         OI    TRASTAH+4,X'20'     VALIDATED                                    
         OI    TRAESTH+4,X'20'     VALIDATED                                    
         OI    TRAPERH+4,X'20'     VALIDATED                                    
         OI    TRAFLTRH+4,X'20'    VALIDATED                                    
*                                                                               
         TM    WHEN,X'20'          TEST SOON                                    
         BZ    VK80                                                             
*                                                                               
         NI    TRACLTH+4,X'FF'-X'20'  TURN OFF VALIDATED                        
*                                                                               
* BUILD KEY                                                                     
*                                                                               
VK80     XC    KEY,KEY                                                          
         CLI   CONREC,C'B'         TEST BUYACT                                  
         BE    VK82                                                             
         MVI   KEY,X'8A'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         J     EXIT                                                             
*                                                                               
VK82     MVC   KEY(2),=X'0A2E'                                                  
         MVC   KEY+2(3),BAGYMD   & BCLT                                         
         MVC   KEY+5(5),BMKTSTA                                                 
         MVC   KEY+11(1),BEST                                                   
         XC    TABLESZ,TABLESZ     ZERO NUMBER OF RECORDS IN TABLE              
         XC    TBLINDEX,TBLINDEX   ZERO INDEX INTO TBA TABLE                    
         XC    SCRINDEX,SCRINDEX   ZERO INDEX INTO TBA TABLE FOR SCREEN         
         J     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* VALIDATE ESTIMATE                                                             
*============================================================                   
                                                                                
VEST     NTR1                                                                   
         MVI   BEST,0                                                           
         MVC   QEST,SPACES                                                      
         MVC   QESTDESC,SPACES                                                  
         MVC   TRAESTD,SPACES                                                   
         OI    TRAESTDH+6,X'80'                                                 
         MVI   ESTSTAT,C'Y'                                                     
         CLI   5(R2),0             ANY DATA                                     
         BE    VEXIT               NO                                           
         CLC   =C'NO',8(R2)                                                     
         BNE   VEST1                                                            
         MVI   ESTSTAT,C'N'                                                     
         B     VEXIT                                                            
VEST1    TM    4(R2),X'08'         TEST DATA NUMERIC                            
         BO    VEST2               YES                                          
         J     ESTERR                                                           
*                                                                               
VEST2    ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,VESTPACK                                                      
         CVB   R1,DUB                                                           
         CHI   R1,1                TEST ESTIMATE BETWEEN 1 AND 255              
         JL    ESTERR                                                           
         CHI   R1,255                                                           
         JH    ESTERR                                                           
         CLI   BPRD,0              TEST PRODUCT ENTERED                         
         BE    NOPRDERR            NO, ERROR                                    
         XC    KEY,KEY             ESTIMATE HEADER                              
         MVC   KEY+1(3),BAGYMD     AGENCY, MEDIA, BCLT                          
         MVC   KEY+4(3),QPRD       PRODUCT                                      
         STC   R1,KEY+7            THE ALLEGED ESTIMATE NUMBER                  
         STC   R1,BEST                                                          
*                                                                               
         MVC   FILENAME,=C'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   NOESTERR                                                         
         EDIT  (B1,BEST),(3,QEST),ALIGN=LEFT                                    
*                                                                               
         MVC   FILENAME,=C'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM                 
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLC   KEY(13),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ESTHDRD,R6                                                       
         MVC   QESTDESC,EDESC                                                   
         MVC   TRAESTD,QESTDESC                                                 
         OI    TRAESTDH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         XC    FILENAME,FILENAME                                                
VEXIT    J     EXIT                                                             
*                                                                               
VESTPACK PACK  DUB,TRAEST(0)                                                    
         LTORG                                                                  
         EJECT                                                                  
*========================================================                       
* FORMAT STATION FOR PRINTING                                                   
*========================================================                       
                                                                                
         USING TBAKEY,R4                                                        
FMTSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 MSUNPK,DMCB,(X'80',SVMKTSTA),QMKT,WORK                           
         XC    STANET,STANET                                                    
         CLC   WORK+5(3),SPACES                                                 
         BE    FMTSTA06                                                         
         MVC   STANET,WORK                                                      
         MVI   STANET+4,C'/'                                                    
*                                                                               
FMTSTA06 CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   WORK+4,C'T'                                                      
         CLC   SVSTAKEY,WORK       SAME STATION                                 
         JE    EXIT                                                             
         MVC   SVSTAKEY,WORK                                                    
*                                                                               
         L     R2,AIO3                                                          
         LA    R2,3000(,R2)        USE LAST 1,000 BYTES OF AIO3                 
         XC    0(256,R2),0(R2)     CLEAR FIRST                                  
         LA    R3,900(,R2)                                                      
*                                                                               
         CLI   MODE,PRINTREP       THIS SPOOLED REPORT                          
         BNE   FMTSTA20             NO, TABLE BUILD                             
*                                                                               
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         BNE   FMTSTA20             NO, TABLE BUILD                             
*                                                                               
         L     R2,VADUMMY                                                       
         LR    R3,R2                                                            
         AHI   R3,9000             ROOM FOR 1000 STATIONS                       
FMTSTA10 OC    0(5,R2),0(R2)       END OF TABLE                                 
         BZ    FMTSTA20                                                         
         CLC   WORK(5),0(R2)                                                    
         BE    FMTSTA30                                                         
         LA    R2,9(,R2)                                                        
         CR    R2,R3                                                            
         BL    FMTSTA10                                                         
*                                                                               
         AHI   R2,-9                                                            
*                                                                               
FMTSTA20 MVC   0(5,R2),WORK                                                     
         MVC   5(4,R2),SPACES                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         MVI   5(R2),C'*'                                                       
*                                                                               
         MVC   6(3,R2),SPACES      AFFILIATE                                    
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   FMTSTA30                                                         
*                                                                               
* READ STATION MASTER RECORD *                                                  
*                                                                               
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,WORK                                                    
*                                                                               
         CLI   STAKCALL+4,C' '                                                  
         BH    *+10                                                             
         MVC   STAKCALL+4(1),QMED                                               
*                                                                               
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
*                                                                               
         L     R4,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,(R4)                     
         CLI   8(R1),0                                                          
         BE    FMTSTA26                                                         
         MVC   SNETWRK,=C'???'                                                  
*                                                                               
FMTSTA26 MVC   6(3,R2),SNETWRK                                                  
         MVC   STAAFFL,6(R2)                                                    
*                                                                               
FMTSTA30 MVC   STADRSW,5(R2)                                                    
         MVC   STAAFFL,6(R2)                                                    
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),WORK                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),WORK+4                                                   
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTSTAX                                                          
         MVI   3(RE),C' '                                                       
FMTSTAX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SUBROUTINE DISPLAYS SINGLE DATA RECORD                                        
* SHOWS ALL DATES FOR THIS MARKET/STATION                                       
* PRODUCT                                                                       
*==============================================================                 
                                                                                
DR       NTR1  BASE=*,LABEL=*                                                   
         LA    R4,TRADSP1H         CLEAR ANY PREVIOUS ACTIVITY DATES            
         LA    R1,TRAPFKLH                                                      
*                                                                               
DR02     LA    R5,8(R4)            SKIP OVER FIELD HEADER                       
         XC    8(78,R4),8(R4)      CLEAR FIELD AND TRANSMIT                     
         OI    6(R4),X'80'                                                      
         LA    R4,86(R4)           NEXT FIELD                                   
         CR    R4,R1               DON'T SPILL OVER SCREEN                      
         BL    DR02                                                             
*                                                                               
         L     R0,TBLINDEX         INDEX INTO TABLE                             
         LA    R6,TABLE                                                         
         LTR   R0,R0                                                            
         BZ    *+12                                                             
*                                                                               
DR10     LA    R6,TBLNEXT(,R6)     LOCATE TABLE ENTRY                           
         BCT   R0,DR10                                                          
         USING TBLRECD,R6                                                       
         L     R5,ASVCLIST                                                      
*                                                                               
DR11     CLC   3(1,R5),TBLPRD      MATCH PRD CODE                               
         BE    DR12                                                             
         LA    R5,4(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    DR11                                                             
         LA    R5,=C'???'          SET UNKNOWN PRODUCT                          
*                                                                               
DR12     MVC   PROD,0(R5)                                                       
*                                                                               
         XC    PROD2,PROD2                                                      
         CLI   TBLPRD2,0                                                        
         BE    DR18                                                             
         L     R5,ASVCLIST                                                      
DR14     CLC   3(1,R5),TBLPRD2     MATCH PRD CODE                               
         BE    DR16                                                             
         LA    R5,4(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    DR14                                                             
         LA    R5,=C'???'          SET UNKNOWN PRODUCT                          
*                                                                               
DR16     MVC   PROD2,0(R5)                                                      
DR18     LA    R2,TRADSP1H         SET DISPLAY LINE LIMITS                      
         MVI   COL,1                                                            
*                                                                               
         MVC   EST,TBLEST          ESTIMATE                                     
*                                                                               
         MVC   SAVES,TBLBBS        GET POSSIBLE BILLBOARDS & ACTIVITY           
         LM    R0,R1,SAVEBBS                                                    
         LA    R5,WKTABLE          TABLE OF WEEKS                               
         LTR   R1,R1               TEST FIRST BIT                               
         BM    DR24                WEEK IS ACTIVE                               
DR20     LM    R0,R1,SAVEBBS                                                    
*                                                                               
DR22     LA    R5,2(R5)            CHECK ALL WEEKS FOR ACTIVITY                 
         CLI   0(R5),0             TEST END OF TABLE                            
         BNE   DR23                                                             
         SLL   R1,1                                                             
         C     R1,=X'80000000'     TEST OVERFLOW BIT IS ON                      
         BNE   DRX                 IT IS NOT                                    
         B     DR30                                                             
DR23     SLL   R1,1                                                             
         SLL   R0,1                                                             
         LTR   R1,R1                                                            
         BZ    DRX                 NO MORE ACTIVITY DATES                       
         BP    DR22                TRY AGAIN                                    
DR24     STM   R0,R1,SAVEBBS                                                    
*                                                                               
* CALCULATE DISPLAY LINE DISPLACEMENT                                           
*                                                                               
         ZIC   RE,COL                                                           
         BCTR  RE,0                                                             
         MHI   RE,L'DSPLINE+3                                                   
         LA    R4,8(RE,R2)         POINT TO DISPLAY POSITION                    
*                                                                               
         USING DSPLINED,R4                                                      
*                                                                               
         MVC   DSPLINE,SPACES                                                   
         MVC   DSPPRD(3),PROD                                                   
         LA    R3,DSPPRD+3                                                      
         CLI   DSPPRD+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  (B1,TBLSLN),(3,1(R3)),ALIGN=LEFT                                 
         OC    PROD2,PROD2                                                      
         BZ    DR26                                                             
         MVC   DSPPTR(3),PROD2                                                  
         LA    R3,DSPPTR+3                                                      
         CLI   DSPPTR+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R3,0                                                             
         MVI   0(R3),C'-'                                                       
         EDIT  (B1,TBLSLN2),(3,1(R3)),ALIGN=LEFT                                
DR26     MVC   ACTVDATE,0(R5)                                                   
         GOTO1 DATCON,DMCB,(2,ACTVDATE),(8,DSPDTE)                              
*                                                                               
         TM    SAVEBBS,X'80'       THIS A BILLBOARD WEEK                        
         BZ    *+10                                                             
         MVC   DSPDTE+5(3),=C' BB'                                              
*                                                                               
         OI    6(R2),X'80'         SET ON TRANSMIT BIT                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             TEST EOS                                     
         BH    DR20                                                             
         IC    RE,COL                                                           
         LA    RE,1(RE)            BUMP COLUMN                                  
         STC   RE,COL                                                           
         CLI   COL,3               MAXIMUM NUMBER OF COLUMNS                    
         BH    DRX                                                              
         LA    R2,TRADSP1H                                                      
         B     DR20                                                             
*                                                                               
DR30     ZIC   RE,COL              DEAL WITH DATE OVERFLOW                      
         BCTR  RE,0                                                             
         MHI   RE,L'DSPLINE+3                                                   
         LA    R4,8(RE,R2)         POINT TO DISPLAY POSITION                    
         MVC   DSPLINE,SPACES                                                   
         MVC   DSPLINE,=C'SUBSEQUENT DATES'                                     
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DRX      J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY KEY *                                                                 
*                                                                               
DK       NTR1  BASE=*,LABEL=*                                                   
         CLI   CONREC,C'B'         TEST BUYACT SCREEN                           
         BE    DK02                YES -                                        
         BRAS  RE,GOXFRCTL         IF TAM, TRANSFER CONTROL                     
         J     EXIT                                                             
*                                                                               
DK02     LA    R4,TABLE                                                         
         LLC   R0,SELLISTN                                                      
         A     R0,SCRINDEX                                                      
         ST    R0,TBLINDEX                                                      
*                                                                               
         LTR   R0,R0                                                            
         BZ    DK03                                                             
*                                                                               
         LA    R4,TBLNEXT(,R4)     FIND SELECTED TABLE ENTRY                    
         BCT   R0,*-4                                                           
         USING TBLRECD,R4                                                       
*                                                                               
DK03     XC    WORK,WORK                                                        
         MVC   WORK(3),QCLT                                                     
         CLC   TRACLT,WORK                                                      
         BE    *+14                                                             
         MVC   TRACLT,WORK                                                      
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
         MVC   TRAPRD,SPACES                                                    
         L     R5,ASVCLIST                                                      
DK04     CLC   3(1,R5),TBLPRD      MATCH PRD CODE                               
         BE    DK06                                                             
         LA    R5,4(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BH    DK04                                                             
         LA    R5,=C'???'          SET UNKNOWN PRODUCT                          
*                                                                               
DK06     MVC   TRAPRD(3),0(R5)                                                  
         OI    TRAPRDH+6,X'80'                                                  
*                                                                               
         MVC   SVMKTSTA(2),TBLMKT                                               
         MVC   SVMKTSTA+2(3),TBLSTA                                             
         BRAS  RE,FMTSTA                                                        
         MVC   TRASTA,SPACES                                                    
         MVC   TRASTA(7),STAPRNT                                                
         MVC   TRASTA+7(1),STADRSW    SHOW * IF NO TRAF ADDR REC                
         OI    TRASTAH+6,X'80'                                                  
*                                                                               
         OC    STANET,STANET       CABLE HEAD STATION                           
         BZ    *+10                                                             
         MVC   TRASTA,STANET                                                    
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
* READ MARKET RECORD FOR NAME *                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R5,AIO3                                                          
         USING MKTRECD,R5                                                       
*                                                                               
         CLC   KEY(8),0(R5)                                                     
         BE    DK08                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO3                     
*                                                                               
DK08     LA    R1,=CL24'*** UNKNOWN ***'                                        
         CLC   KEY(8),0(R5)                                                     
         BNE   *+8                                                              
         LA    R1,MKTNAME                                                       
         MVC   TRAMKNM,0(R1)                                                    
         OI    TRAMKNMH+6,X'80'                                                 
*                                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
         CLI   ESTSTAT,C'Y'        TEST ESTIMATE STATUS                         
         BE    DK10                ESTIMATE IS NOT 'NO'                         
         MVC   TRAEST,=C'NO '                                                   
         OI    TRAESTH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK10     EDIT  (B1,TBLEST),(3,QEST),ALIGN=LEFT                                  
         CLC   TRAEST,QEST                                                      
         BE    DKX                                                              
         MVC   TRAEST,QEST                                                      
         OI    TRAESTH+6,X'80'                                                  
         MVC   TRAESTD,QESTDESC                                                 
         OI    TRAESTDH+6,X'80'                                                 
*                                                                               
DKX      J     EXIT                                                             
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* BUILD GLOBBER ELEMENTS TO TRANSFER CONTROL FOR THIS CLIENT                    
*==================================================================             
                                                                                
GOXFRCTL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MYKEYSV,KEY          REMEMBER KEY IS 0A2E REC KEY                
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),MYKEYSV+2   MOVE A-M/CLT                                
         MVC   FILENAME,=C'SPTDIR'                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         MVC   FILENAME,=C'SPTFIL'                                              
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         LLC   R0,CPROF+6                                                       
         GOTO1 CLUNPK,DMCB,((R0),KEY+2),DUB  GET CLT CODE IN DUB                
         XC    FILENAME,FILENAME                                                
         MVC   KEY,MYKEYSV                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',QMED,1,GLVSPMD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGLOBBER,(R1),,DUB,3,GLVSPCLT                                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DUB(3),=C'BCM'                                                   
         GOTO1 (RF),(R1),,DUB,3,GLVXREC                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DUB(4),=C'LIST'                                                  
         GOTO1 (RF),(R1),,DUB,3,GLVXACT                                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'STR'                                                 
         MVC   GLVXFRPR,=C'TRA'                                                 
         MVC   GLVXTOSY,=C'STR'                                                 
         MVC   GLVXTOPR,=C'TRA'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',ELEM,GLVXLENQ,GLVXCTL                     
*                                                                               
         MVI   XFRCALL,C'N'                                                     
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* LIST CLIENTS WITH DATES OF FIRST ACTIVITY                                     
* IF CLIENT IS ENTERED, START WITH THAT CLIENT                                  
* READ THE FILE HEADER RECORD TO GET THE DATE OF THE FILE LOAD                  
* THEN GO BACK 1 WEEK TO GET THE DATE THAT IS DAY 0 IN DATE FIELD               
*================================================================               
                                                                                
LISTCLTS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,14               SET MAX LINE COUNTER                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME THROUGH                      
         BZ    LCLT2                                                            
         OC    SVLSTKEY,SVLSTKEY   IF WE FINISHED LAST TIME                     
         BZ    LCLT2               THEN START AGAIN                             
         MVC   KEY,SVLSTKEY        RESTORE LAST KEY READ                        
         GOTO1 HIGH                                                             
         B     LCLT10              AND START WITH NEXT                          
*                                                                               
LCLT2    MVC   TRAHL1(28),=C'CLIENT  -----CLIENT NAME----'                      
         XC    TRAHL1+1(6),=X'4040404040'  MAKE LOWERCASE                       
         XC    TRAHL1+13(11),=X'0040404040400000404040'                         
         OI    TRAHL1H+6,X'80'                                                  
         MVC   TRAHL2,SPACES                                                    
         MVC   TRAHL2(9),=C'FIRST AIR'                                          
         XC    TRAHL2(9),=X'004040404000004040'                                 
         OI    TRAHL2H+6,X'80'                                                  
*                                                                               
         XC    MYKEYSV,MYKEYSV     WILL BE NON-ZERO IF ANY DISPLAY              
         CLI   SVMAXDYS,0                                                       
         BNE   *+8                                                              
         MVI   SVMAXDYS,128                                                     
         XC    KEY,KEY             READ TRFDIR FOR HEADER                       
         MVI   KEY+12,1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'       READ HEADER RECORD FROM TRFFIL               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO1                                                          
         LA    RE,58(RE)           POINT TO MM/DD/YY                            
         MVC   WORK(2),6(RE)       MOVE YY                                      
         MVC   WORK+2(2),0(RE)     MOVE MM                                      
         MVC   WORK+4(2),3(RE)     MOVE DD                                      
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,-7                                        
         MVC   SVBASEDT,WORK+6                                                  
         MVI   SVMINDYS,0                                                       
*&&DO                                                                           
         GOTO1 DATCON,DMCB,(5,0),WORK  GET TODAY                                
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
         CLI   0(R1),1             TEST MONDAY                                  
         BE    LCLT4                                                            
         LA    R0,7                                                             
         LLC   RE,0(R1)                                                         
         SR    R0,RE                                                            
         LNR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)  BACK UP TO MONDAY                   
*                                                                               
LCLT4    MVC   WORK(6),WORK+6                                                   
         CLC   SVBASEDT,WORK       TEST BASE DATE PRIOR                         
         BNL   LCLT8               NO                                           
*                                                                               
LCLT6    LA    R0,-1               BACK UP ONE DAY                              
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   WORK(6),WORK+6                                                   
         CLC   SVBASEDT,WORK       AND SEE IF WE'RE BACK TO BASE DATE           
         BE    LCLT6X                                                           
         LA    R4,1(R4)            BUMP COUNTER                                 
         B     LCLT6                                                            
*                                                                               
LCLT6X   LA    R4,1(R4)            SET MINIMUM DAYS FOR DISPLAY                 
         STC   R4,SVMINDYS                                                      
*&&                                                                             
LCLT8    XC    KEY,KEY                                                          
         MVI   KEY,X'8A'                                                        
         MVC   KEY+1(3),BAGYMD     A-M/<START AT> CLT                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE      SAME TYPE/A-M                                
         BNE   LCLTX                                                            
         B     LCLT12                                                           
*                                                                               
LCLT10   MVC   KEY+4(4),=X'FFFFFFFF'  FORCE NEXT CLIENT                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
LCLT11   CLC   KEY(2),KEYSAVE      SAME TYPE/A-M                                
         BNE   LCLTX                                                            
                                                                                
* SEE IF CLIENT SHOULD BE DISPLAYED                                             
                                                                                
LCLT12   TM    KEY+13,X'80' EY+KEY(TEST DELETED                                 
         BO    LCLT13                                                           
*                                                                               
         CLC   TBAPFRST-TBAKEY+KEY(1),SVMAXDYS                                  
         BH    LCLT10              FIRST TLCST TOO FAR AWAY                     
*                                                                               
         CLC   TBAPFRST-TBAKEY+KEY(1),SVMINDYS  TEST BEFORE THIS MON            
         BNL   LCLT14                                                           
LCLT13   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     LCLT11                                                           
*                                                                               
LCLT14   MVC   LISTAR,SPACES                                                    
         MVC   MYKEYSV,KEY         SAVE TRFDIR KEY                              
*                                                                               
         XC    KEY,KEY              READ CLTHDR FOR DECODE PROF CHAR            
         MVC   KEY+1(3),MYKEYSV+1   A-M/CLT                                     
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFILE'                                           
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
         LLC   R0,CPROF+6                                                       
         GOTO1 CLUNPK,DMCB,((R0),KEY+2),CLSTCLT                                 
         MVC   CLSTCNAM,CNAME                                                   
         DROP  R6                                                               
*                                                                               
         MVC   KEY,MYKEYSV             RESTORE TRFDIR KEY                       
         MVC   SVLSTKEY,MYKEYSV        SAVE LAST KEY DISPLAYED                  
*                                                                               
         MVC   AIO,AIO2                LISTMON KEEPS TRACK OF D/A               
         GOTO1 GETREC                                                           
*                                                                               
         LLC   R0,TBAPFRST-TBAKEY+KEY                                           
         GOTO1 ADDAY,DMCB,SVBASEDT,DUB,(R0)    GET FIRST TLCST DT               
         GOTO1 DATCON,DMCB,DUB,(5,CLSTDATE)                                     
*                                                                               
         GOTO1 LISTMON                                                          
         BCT   R0,LCLT10                                                        
*                                                                               
LCLTX    OC    MYKEYSV,MYKEYSV     TEST ANY ACTIVITY                            
         JZ    NOACTERR                                                         
         XC    SVLSTKEY,SVLSTKEY                                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* GET OFFICE FOR SINGLE CLIENT LIMITED ACCESS                                   
*===========================================================                    
                                                                                
GOFF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),T216FFD+6  LIMITED ACCESS CLT                           
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+6                                                              
         DC    H'0'                WHAT'S WRONG                                 
*                                                                               
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
*                                                                               
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   LAOFFICE,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   LAOFFICE,C'A'       IF THERE IS ONE                              
         BNL   *+10                                                             
*                                                                               
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         LTORG                                                                  
         DROP  R7                                                               
         EJECT                                                                  
* VALIDATE PERIOD (ALSO NOT MORE THAN 15 WEEKS) *                               
*                                                                               
VPER     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY)                                 
         XC    PERDTS,PERDTS                                                    
         CLI   5(R2),0                                                          
         BE    VPER10                                                           
         GOTO1 DATVAL,(R1),(0,8(R2)),WORK                                       
         L     R3,0(R1)                                                         
         LTR   R3,R3                                                            
         BZ    DATERR                                                           
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         CLI   0(R1),1             TEST MONDAY                                  
         BNE   DAYERR                                                           
         LA    R3,1(R2,R3)                                                      
         GOTO1 DATCON,(R1),(0,WORK),(2,STDATE)                                  
*                                                                               
         OC    8(2,R3),8(R3)                                                    
         BZ    VPER20                                                           
         GOTO1 DATVAL,(R1),(0,8(R3)),WORK                                       
         OC    0(4,R1),0(R1)                                                    
         BZ    DATERR                                                           
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         CLI   0(R1),7             TEST SUNDAY                                  
         BNE   DAYERR                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,ENDATE)                                  
*                                                                               
         CLC   STDATE,ENDATE                 START NOT GREATER END              
         BH    DATERR                                                           
         GOTO1 DATCON,(R1),(2,STDATE),(0,SPDATE)                                
         GOTO1 DATCON,(R1),(2,ENDATE),(0,EPDATE)                                
         TM    WHEN,X'80'          TEST ONLINE                                  
         BZ    VPER5               OFFLINE REPORT                               
         GOTO1 ADDAY,(R1),SPDATE,DUB,F'217'    ADD 31 WEEKS FOR ONLINE          
         B     VPER7                                                            
*                                                                               
VPER5    GOTO1 ADDAY,(R1),SPDATE,DUB,F'105'                                     
*                                                                               
VPER7    CLC   DUB(6),EPDATE                                                    
         BL    PERERR                                                           
         B     VPER30                                                           
*                                                                               
VPER10   MVC   STDATE,TODAY        START OF PERIOD IS TODAY                     
*                                                                               
VPER20   GOTO1 DATCON,(R1),(2,STDATE),(0,SPDATE)                                
         GOTO1 GETDAY,(R1),SPDATE,WORK+6                                        
         CLI   0(R1),0             TEST DAY RETURNED                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,0(R1)            GET DAY NUMBER                               
         BCTR  R0,0                GIVES DAYS TO MONDAY                         
         LCR   R0,R0               SET TO BACK UP TO PREVIOUS MONDAY            
         GOTO1 ADDAY,(R1),SPDATE,WORK,(R0)                                      
         MVC   SPDATE,WORK                                                      
         GOTO1 DATCON,(R1),(0,SPDATE),(2,STDATE)                                
         TM    WHEN,X'80'          TEST ONLINE                                  
         BZ    VPER25              OFFLINE REPORT                               
*                                                                               
         LA    R0,31               31 WEEKS FOR ONLINE                          
         CLC   =C'BCM',CONREC                                                   
         BNE   *+8                                                              
         LA    R0,4                4 WEEKS FOR BCM                              
*                                                                               
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         STC   RE,PERWKS                                                        
         MHI   R0,7                CONVERT WEEKS TO DAYS                        
         BCTR  R0,0                AND GET A SUNDAY DATE                        
         GOTO1 ADDAY,(R1),SPDATE,EPDATE,(R0)                                    
*                                                                               
         CLC   =C'BCM',CONREC      SHOW DATES FOR BCM                           
         BNE   VPER27                                                           
         GOTO1 DATCON,DMCB,SPDATE,(5,8(R2))                                     
         MVI   16(R2),C'-'                                                      
         GOTO1 (RF),(R1),EPDATE,(5,17(R2))                                      
         OI    TRAPERH+6,X'80'     AND XMT FIELD                                
         B     VPER27                                                           
*                                                                               
VPER25   SR    R0,R0                                                            
         ICM   R0,1,SVTAMWKS                                                    
         BNZ   *+8                                                              
         LA    R0,15               STANDARD IS 15 WEEKS                         
*                                                                               
         STC   R0,PERWKS                                                        
         MHI   R0,7                                                             
         BCTR  R0,0                                                             
*                                                                               
         GOTO1 ADDAY,(R1),SPDATE,EPDATE,(R0)   ADD 15 WEEKS FOR OFFLINE         
*                                                                               
VPER27   GOTO1 DATCON,(R1),(0,EPDATE),(2,ENDATE)                                
*                                                                               
VPER30   MVC   DUB,SPDATE                                                       
         GOTO1 DATCON,(R1),(0,DUB),(5,SPDATE)                                   
         MVC   DUB,EPDATE                                                       
         GOTO1 DATCON,(R1),(0,DUB),(5,EPDATE)                                   
         J     EXIT1                                                            
EXIT1    XIT1                                                                   
*                                                                               
PERERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PERMSG),PERMSG                                         
         J     ERREXIT1                                                         
DAYERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DAYMSG),DAYMSG                                         
ERREXIT1 GOTO1 ERREX2                                                           
DATERR   MVI   ERROR,INVDATE                                                    
         GOTO1 ERREX                                                            
*                                                                               
DAYMSG   DC    C'* ERROR * PERIOD MUST BEGIN ON MONDAY AND END ON SUNDA+        
               Y *'                                                             
PERMSG   DC    C'* ERROR * PERIOD CAN''T BE MORE THAN 15 WEEKS *'               
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - MARKET, LEN                                        
*                                                                               
VFTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    FILTERS,FILTERS                                                  
         XC    MGRPTBLE(200),MGRPTBLE                                           
         XC    MGRPTBLE+100(100),MGRPTBLE+100                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELP),FTRHELP                                       
         B     VFTRERRX                                                         
VFTR08   GOTO1 SCANNER,DMCB,TRAFLTRH,(5,BLOCK)                                  
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA            NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR14              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
VFTR14   EX    R1,VFTRCLCA         MKT=                                         
         BE    VFTR16                                                           
         EX    R1,VFTRCLCB         MARKET=                                      
         BNE   VFTR20                                                           
VFTR16   LA    R5,22(,R4)                                                       
         TM    3(R4),X'80'         WAS MARKET ENTRY NUMERIC                     
         BZ    NUMERRA                                                          
*                                                                               
         MVC   MKTFTR,BMKT                                                      
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      MARKET                                       
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALIMKT                                                          
*                                                                               
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERRA            GO PRINT ERROR                               
*                                                                               
         MVC   WORK(2),MKTFTR                                                   
         MVC   MKTFTR,BMKT                                                      
         MVC   BMKT,WORK                                                        
*                                                                               
         OC    MKTFTR,MKTFTR       TEST FILTER IS FOR MKT 0                     
         BNZ   *+10                                                             
         MVC   MKTFTR,=X'FFFF'     SET A NON-ZERO VALUE                         
         B     VFTR50                                                           
*                                                                               
VFTR20   EX    R1,VFTRCLCC         SPOT LEN                                     
         BNE   VFTR30                                                           
         TM    3(R4),X'80'         WAS SPOT LEN NUMERIC                         
         BZ    NUMERRA                                                          
         MVC   FLDH,TRAFLTRH                                                    
         PACK  FLDH+4(1),3(1,R4)   NUM, ALPHA, HEX BITS                         
         MVC   FLDH+5(1),1(R4)     DATA LEN                                     
         MVC   FLD(10),22(R4)      SPOT LEN                                     
         MVI   ERROPT,C'Y'                                                      
         LA    R2,FLDH                                                          
         GOTO1 VALISLN                                                          
         LA    R2,TRAFLTRH                                                      
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BNE   TRAPERRA            GO PRINT ERROR                               
         MVC   SLNFTR,WORK                                                      
         B     VFTR50                                                           
*                                                                               
VFTR30   EX    R1,VFTRCLCD         MARKET GROUP                                 
         BNE   VFTR40                                                           
         CLI   1(R4),2                                                          
         BL    MGRPERR                                                          
         CLI   1(R4),5                                                          
         BH    MGRPERR                                                          
         CLI   22(R4),C'A'                                                      
         BL    MGRPERR                                                          
         CLI   22(R4),C'Z'                                                      
         BH    MGRPERR                                                          
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LA    R1,23(,R4)                                                       
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
VFTR32   CLI   0(R1),C'0'                                                       
         BL    MGRPERR                                                          
         CLI   0(R1),C'9'                                                       
         BH    MGRPERR                                                          
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,VFTR32                                                        
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   SVPMGRP(1),22(R4)                                                
         MVC   SVPMGRP+1(4),DUB                                                 
         MVC   SVDMGRP(5),22(R4)                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(1),22(R4)                                                  
         MVC   KEY+9(2),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   BDMGRPER                                                         
         LA    R5,MGRPTBLE                                                      
         LA    R6,150                                                           
VFTR36   MVC   0(2,R5),KEY+11                                                   
         LA    R5,2(,R5)                                                        
         BCT   R6,*+6                                                           
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BE    VFTR36                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
         B     VFTR50                                                           
*                                                                               
VFTR40   EX    R1,VFTRCLCE         BB - PRINT ALL BILLBOARDS                    
         BNE   VFTR44                                                           
*                                                                               
         MVI   BBFILT,C'Y'         SET TO SHOW ALL BILLBOARDS                   
*                                                                               
VFTR44   EX    R1,VFTRCLCF         SHOW DISK ADDRESS                            
         BNE   VFTR60                                                           
*                                                                               
         MVI   DSKADSW,C'Y'        SHOW DISK ADDRESS                            
*                                                                               
VFTR50   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    XIT1                                                                   
*                                                                               
NUMERRA  MVI   ERROR,NOTNUM                                                     
         B     TRAPERRA                                                         
MISSERRA MVI   ERROR,MISSING                                                    
TRAPERRA GOTO1 ERREX                                                            
*                                                                               
VFTR60   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELP),FTRMSG                               
VFTRERRX GOTO1 ERREX2                                                           
*                                                                               
MGRPERR  MVC   CONHEAD,MGRPERMS                                                 
         B     VFTRERRX                                                         
*                                                                               
BDMGRPER MVC   CONHEAD,BDMGRPMS                                                 
         MVC   CONHEAD+26(5),SVDMGRP                                            
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         B     VFTRERRX                                                         
*                                                                               
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC    C'VALID FILTERS - MKT/LEN/MGRP/BB= *'                            
VFTRCLCA CLC   12(0,R4),=CL4'MKT'                                               
VFTRCLCB CLC   12(0,R4),=CL7'MARKET'                                            
VFTRCLCC CLC   12(0,R4),=CL4'LEN'                                               
VFTRCLCD CLC   12(0,R4),=CL5'MGRP'                                              
VFTRCLCE CLC   12(0,R4),=CL3'BB'                                                
VFTRCLCF CLC   12(0,R4),=CL6'DSKAD '                                            
MGRPERMS DC    CL60'* ERROR * MKT GROUP MUST BE LETTER AND 1-4 DIGITS *C        
               '                                                                
BDMGRPMS DC    CL60'* ERROR * NO MARKET GROUP X0000 FOUND *'                    
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE OFFICE CODE (ONLY SUPPORTED IN OFFLINE REPORT) *                     
*              -READ CLIENT HEADER RECORDS TO BUILD                             
*               TABLE OF CLIENTS FOR REQUESTED OFFICE                           
*                                                                               
VOFF     NTR1  BASE=*,LABEL=*                                                   
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERRA                                                          
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
VOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   OFFERR                                                           
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   OFFERR                                                           
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
*                                                                               
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         B     VOFF10                                                           
*                                                                               
VOFF30   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         USING CLTHDRD,R6                                                       
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
*                                                                               
*NOP     CLI   TRACLT,C'$'         USE OFFICER                                  
*****    BNE   VOFF40                                                           
*                                                                               
         BRAS  RE,COFF             CHECK OFFICE VALIDITY                        
         BNE   VOFF20                                                           
*                                                                               
         CLI   TRACLT,C'*'         REQUESTED BY OFFICE                          
         BNE   VOFF35                                                           
         CLC   BOFFCD,SVCLTOFF                                                  
         BNE   VOFF20                                                           
         B     VOFFX                                                            
*                                                                               
VOFF35   MVC   BOFFCD,SVCLTOFF                                                  
         B     VOFFX                                                            
*                                                                               
* THIS CODE IS DONE ABOVE BY OFFICER (CAN BE REMOVED)                           
*                                                                               
VOFF40   LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,SVCLTOFF                                                      
         LA    R0,1                                                             
*                                                                               
VOFF42   CLC   BOFFCD,0(R1)        TEST RIGHT OFFICE                            
         BE    VOFF43                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VOFF42                                                        
         B     VOFF20                                                           
*                                                                               
VOFF43   CLI   CACCESS,C' '                                                     
         BNH   VOFF44                                                           
*                                                                               
         MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
*                                                                               
*OFF44   L     R4,ATWA             TEST FOR SECURITY BREACHES                   
*        USING T216FFD,R4                                                       
*                                                                               
VOFF44   OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VOFFX                                                            
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    VOFF60                                                           
         CLI   T216FFD+6,C'$'          TEST OFFICE LOCKOUT                      
         BE    VOFF70                                                           
*                                                                               
         CLC   T216FFD+6(2),CKEYCLT    ELSE SINGLE CLIENT ACCESS                
         BNE   VOFF20                                                           
         B     VOFFX                                                            
*                                                                               
VOFF60   CLC   T216FFD+7(1),BOFFCD    MATCH OFFICE CODE                         
         BNE   VOFF20                                                           
*                                                                               
VOFFX    MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
*                                                                               
         XC    FILENAME,FILENAME                                                
         CR    R1,R1                                                            
*                                                                               
         XIT1                                                                   
OFFERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OFFMSG),OFFMSG                                         
         GOTO1 ERREX2                                                           
OFFMSG   DC    C'* ERROR * NO CLIENTS FOUND FOR OFFICE *'                       
         DS    0H                                                               
*                                                                               
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
*                                                                               
VOFF70   XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),DMCB,DUB,ACOMFACS                                           
         CLI   0(R1),0                                                          
         BNE   VOFF20                                                           
         B     VOFFX                                                            
REPERRA  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSGA),REPMSGA                                       
         GOTO1 ERREX2                                                           
REPMSGA  DC    C'* ERROR * OFFICE CODE SUPPORTED OFFLINE ONLY *'                
         DROP  R6                                                               
         EJECT                                                                  
* FIND CLIENT HEADER, CK FOR SECURITY, AND SAVE CLIST                           
*                                                                               
FCLT     NTR1  BASE=*,LABEL=* 0,**FCLT***                                       
         XC    BLOCK(240),BLOCK    CLEAR EST TABLE AREA                         
         XC    BLOCK+240(240),BLOCK+240                                         
*                                                                               
* SAVE CURRENT RECORD                                                           
*                                                                               
FCLT10   DS   0H                                                                
         MVC   SVKEY,KEY                                                        
*                                                                               
         L     R0,AIO3                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         CLI   ACTNUM,ACTADD       IF AN ADD                                    
         BE    *+8                                                              
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   OCLT,BCLT                                                        
         MVC   SVBCLT,BCLT                                                      
         XC    BCLT,BCLT                                                        
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   KEY,SVKEY                                                        
                                                                                
         CLI   TRACLT,C'$'         BY OFFICE LIST                               
         BNE   FCLT12                                                           
                                                                                
         L     R6,AIO1                                                          
         USING CLTHDRD,R6                                                       
                                                                                
* GO VALIDATE CLIENT OFFICE IS IN LIST                                          
         OC    TRACLT,SPACES                                                    
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
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         MVC   OFCCLT,QCLT                                                      
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),TRACLT                                                 
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1,R6                                                            
                                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   FCLT14              GET NEXT CLIENT                              
         B     FCLT13                                                           
                                                                                
FCLT12   CLI   TRACLT,C'*'         BY OFFICE                                    
         BNE   FCLT13                                                           
         CLC   SVCLTOFF,TRACLT+1                                                
         BNE   FCLT14                                                           
                                                                                
FCLT13   MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
*                                                                               
         CLI   ERROR,SECLOCK       ERR IS SECURITY LOCK-OUT                     
         BE    FCLT14                                                           
*                                                                               
         CLI   ERROR,INVCLI        CLIENT NOT FOUND                             
         BE    FCLT14                                                           
         DC    H'0'                                                             
*                                                                               
FCLT14   DS    0H                                                               
         MVI   KEY+5,X'FF'         GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(3),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCLTNE                                                           
*                                                                               
         MVC   SVBCLT,KEY+3        SAVE CLIENT                                  
*                                                                               
* DO GETREC - WILL SAVE REC AT FCLT10                                           
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         B     FCLT10                                                           
*                                                                               
FCLT20   DS    0H                                                               
         L     R0,AIO1             MOVE BUY ACT RECORD BACK                     
         L     RE,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
* SEE IF CLIENT USES PRODUCT EQUIVALENCY *                                      
* READ T0 PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
*                                                                               
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* READ TA PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'A'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         CLI   SVTAXMED,C'Y'       EXCLUDE CLIENT                               
         BE    FCLT14                                                           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      MUST BE THERE                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         CR    RB,RB                                                            
         B     FCLTX                                                            
*                                                                               
FCLTNE   DS    0H                                                               
         CR    RC,RB                                                            
FCLTX    XIT1                                                                   
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLTR    NTR1  BASE=*,LABEL=*                                                   
         XC    BLOCK(240),BLOCK    CLEAR EST TABLE AREA                         
         XC    BLOCK+240(240),BLOCK+240                                         
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+CKEYAM-CLTHDRD(1),BAGYMD                                     
         MVC   KEY+CKEYCLT-CLTHDRD(2),SVBCLT                                    
*                                                                               
         MVC   FILENAME,=C'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE  BYPASS IF NO CLIENT HEADER                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FILENAME,=C'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM                 
*                                                                               
         L     R4,AIO3                                                          
         ST    R4,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   CLTNM,CNAME-CLTHDRD(R4)                                          
*                                                                               
         MVC   SVCLTOFF,COFFICE-CLTHDRD(R4)                                     
         CLI   CTRAFOFC-CLTHDRD(R4),0                                           
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC-CLTHDRD(R4)                                    
*                                                                               
         CLI   TRACLT,C'$'         BY OFFICER OFFICE                            
         BNE   *+10                                                             
         MVC   BOFFCD,SVCLTOFF                                                  
*                                                                               
         LA    R2,CLIST-CLTHDRD(R4)                                             
*                                                                               
         LA    R3,880                                                           
         L     RE,ASVCLIST                                                      
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
*                                                                               
* SEE IF CLIENT USES PRODUCT EQUIVALENCY *                                      
* READ T0 PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* READ TA PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'A'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
* GET NEXT CLIENT FOR THIS OFFICE CODE (ONLY IN OFFLINE REPORT) *               
*          END OF CLIENTS, RETURN NE COND CODE                                  
*                                                                               
NOFF     NTR1  BASE=*,LABEL=*                                                   
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERR                                                           
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),OCLT       LAST CLIENT THIS OFFICE                      
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
*                                                                               
         GOTO1 HIGH                                                             
         MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
*                                                                               
NOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   NEQXIT                                                           
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   NEQXIT                                                           
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    NOFF30               YES                                         
*                                                                               
NOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 HIGH                                                             
         B     NOFF10                                                           
*                                                                               
NOFF30   L     R6,AIO3                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVCLTOFF,COFFICE                                                 
         CLI   CTRAFOFC,0                                                       
         BE    *+10                                                             
         MVC   SVCLTOFF,CTRAFOFC                                                
*                                                                               
*NOP     CLI   TRACLT,C'$'         USING OFFICER AS FILTER                      
*****    BNE   NOFF34                                                           
         BRAS  RE,COFF             GO CK OFFICE                                 
         BNE   NOFF20               NOT OK                                      
*                                                                               
         CLI   TRACLT,C'*'         REQUESTED BY OFFICE                          
         BNE   NOFF32              NO, OFFICE LIST                              
*                                                                               
         CLC   BOFFCD,SVCLTOFF     SAME OFFICE                                  
         BNE   NOFF20                                                           
         B     NOFFX                                                            
*                                                                               
NOFF32   MVC   BOFFCD,SVCLTOFF                                                  
         B     NOFFX                                                            
*                                                                               
* THIS CODE IS DONE IN OFFICER (DELETE THIS CODE WHEN DONE TESTING)             
*                                                                               
NOFF34   LA    R1,CACCESS                                                       
         LA    R0,3                                                             
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,SVCLTOFF                                                      
         LA    R0,1                                                             
*                                                                               
NOFF35   CLC   BOFFCD,0(R1)        TEST RIGHT OFFICE                            
         BE    NOFF35C                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,NOFF35                                                        
         B     NOFF20                                                           
*                                                                               
NOFF35C  CLI   CACCESS,C' '                                                     
         BNH   NOFF36                                                           
*                                                                               
         MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
*                                                                               
*OFF36   L     R4,ATWA             TEST FOR SECURITY BREACHES                   
*        USING T216FFD,R4                                                       
*                                                                               
NOFF36   OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    NOFFX                                                            
*                                                                               
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    NOFF60                                                           
*                                                                               
         CLI   T216FFD+6,C'$'          TEST OFFICE LOCKOUT                      
         BE    NOFF70                                                           
*                                                                               
         CLC   T216FFD+6(2),CKEYCLT   ELSE SINGLE CLIENT ACCESS                 
         BNE   NOFF20                                                           
         B     NOFFX                                                            
*                                                                               
NOFF60   CLC   T216FFD+7(1),BOFFCD    MATCH OFFICE CODE                         
         BNE   NOFF20                                                           
         B     NOFFX                                                            
*                                                                               
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
*                                                                               
NOFF70   BRAS  RE,COFF             GO CK OFFICE                                 
         BNE   NOFF20                                                           
*                                                                               
NOFFX    MVC   OCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
*                                                                               
NOFFXX   XC    FILENAME,FILENAME                                                
         CR    R1,R1               SET CC EQ                                    
*                                                                               
         XIT1                                                                   
*                                                                               
NEQXIT   XC    FILENAME,FILENAME                                                
*                                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
* CHECK OFFICE TO BE VALID *                                                    
*                                                                               
COFF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+2,DUB                                            
*                                                                               
         USING CLTHDRD,R6                                                       
*                                                                               
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
*                                                                               
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
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         XIT1                                                                   
REPERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'REPMSG),REPMSG                                         
         GOTO1 ERREX2                                                           
REPMSG   DC    C'* ERROR * OFFICE CODE SUPORTED OFFLINE ONLY *'                 
         DROP  R6                                                               
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
         DS   0H                                                                
HDHK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   H1+10(L'QMED),QMED                                               
         MVC   H1+15(L'MEDNM),MEDNM                                             
         MVC   H3+47(8),SPDATE                                                  
         MVI   H3+55,C'-'                                                       
         MVC   H3+56(8),EPDATE                                                  
*                                                                               
         CLI   TRACLT,C'$'         TEST OFFICER CODE GIVEN                      
         BNE   HDHK10                                                           
         MVI   H4+60,C'('                                                       
         MVC   H4+61(3),TRACLT     SHOW OFFICE CODE                             
         MVI   H4+64,C')'                                                       
*                                                                               
HDHK10   CLI   BOFFCD,0            TEST OFFICE CODE GIVEN                       
         BE    HDHK14                                                           
         MVC   H4+47(6),=C'OFFICE'                                              
         BAS   RE,CNVOFF           CONVERT 1 BYTE OFFICE CODE                   
         BE    HDHK14              OFFICE PRINTED                               
*                                                                               
         GOTO1 =V(OFFOUT),DMCB,BOFFCD,HEXOUT,H4+56                              
*                                                                               
HDHK14   OC    MGRPTBLE(2),MGRPTBLE   WAS MARKET GROUP ENTERED                  
         BZ    HDHK16                                                           
         MVC   H5+47(6),=C'MGROUP'                                              
         MVC   H5+56(5),SVDMGRP    SHOW MARKET GROUP                            
*                                                                               
HDHK16   MVC   H2+10(L'QCLT),QCLT                                               
         MVC   H2+15(L'CLTNM),CLTNM                                             
         MVC   H3+10(L'PROD),PROD                                               
         CLI   EST,0               TEST ESTIMATE = 'NO'                         
         BNE   HDHK20                                                           
         MVC   H5+11(3),=C'NO '                                                 
         B     HDHK40                                                           
*                                                                               
HDHK20   EDIT  (B1,EST),(3,QEST),ALIGN=LEFT                                     
         MVC   H5+11(L'QEST),QEST                                               
*                                                                               
HDHK40   XC    KEY,KEY             PRODUCT HEADER                               
         CLC   PROD,=C'???'        IS IT UNKNOWN PRODUCT                        
         BE    HDHK80                                                           
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT                                                  
         MVC   KEY+4(3),PROD                                                    
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING PRDHDRD,R1                                                       
         MVC   H3+15(20),PNAME                                                  
         DROP  R1                                                               
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
* SEE IF PRODUCT IS EQUIVALENT OR BASE *                                        
*                                                                               
         CLI   SVPROF13,C'Y'                                                    
         BNE   HDHK50                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQKID,=X'0A37'                                                  
         MVC   PEQKAM,BAGYMD                                                    
         MVC   PEQKCLT,SVBCLT                                                   
         MVC   PEQKPRD,PROD                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A BASE                                  
         BNE   HDHK44               NO                                          
         MVC   H4+9(6),=C'(BASE)'                                               
         B     HDHK50                                                           
*                                                                               
HDHK44   XC    KEY,KEY                                                          
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQKAM,BAGYMD                                                    
         MVC   PEQKCLT,SVBCLT                                                   
         MVC   PEQPEPRD,PROD                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS AN EQUIV                                
         BNE   HDHK50               NO                                          
         MVC   H4+10(26),=C'EQUIVALENT TO BASE PRODUCT'                         
         MVC   H4+10+27(3),PEQPBPRD                                             
         DROP  R4                                                               
*                                                                               
HDHK50   CLI   EST,0                                                            
         BE    HDHK80                                                           
         XC    KEY,KEY             ESTIMATE HEADER                              
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT                                                  
         MVC   KEY+4(3),PROD                                                    
         MVC   KEY+7(1),EST                                                     
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT MEDIA SYSTEM               
*                                                                               
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   KEY(13),KEYSAVE                                                  
         BNE   HDHK60                                                           
         TM    KEY+13,X'80'        DELETED REC                                  
         BZ    *+6                                                              
HDHK60   DC    H'0'                SHOULD HAVE DROPPED ALL DELETED EST          
         MVC   AIO,AIO3                                                         
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT MEDIA SYSTEM               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R3,AIO                                                           
         USING ESTHDRD,R3                                                       
         MVC   H5+15(20),EDESC                                                  
         GOTO1 DATCON,DMCB,(0,ESTART),(8,H6+15)                                 
         GOTO1 (RF),(R1),(0,EEND),(8,H6+24)                                     
         MVI   H6+23,C'-'                                                       
         DROP  R3                                                               
*                                                                               
HDHK80   MVC   H9+49(60),WKHDA     MOVE IN MONTH HEADING                        
         MVC   H10+49(60),WKHDB     MOVE IN DAYS HEADING                        
*                                                                               
         CLI   PRTMKTSW,C'Y'       IS THIS A MARKET NAME LINE                   
         BE    HDHKX                YES                                         
         MVC   P4,P3                                                            
         MVC   P3,P2                                                            
         MVC   P2,P1                                                            
         MVC   P1,SPACES                                                        
*                                                                               
         LA    R2,P               ADDRESS OF PRINT AREA                         
         USING PRTLINE,R2                                                       
         MVC   PRTMKT,QMKT                                                      
         MVC   PRTMKTNM,MKTNM                                                   
*                                                                               
HDHKX    XIT1                                                                   
         DROP  R2                                                               
*                                                                               
* CONVERT 1 BYTE OFFICE CODE AND PRINT 2 CHAR CODE                              
*                                                                               
CNVOFF   NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,BOFFCD                                                    
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   CNVOFFX                                                          
*                                                                               
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    CNVOFFX             2 CHAR OFFICE IS NOT ON                      
*                                                                               
         MVC   H4+56(2),OFCOFC2                                                 
         CR    RB,RB               SET EQ CC                                    
*                                                                               
CNVOFFX  XIT1                                                                   
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRTBAE                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAA1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAB1D                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* OFFLINE SORT RECORD DSECT                                                     
*                                                                               
SORTREC  DSECT                                                                  
SRTLEN   DS    F                   LENGTH 23 AND UP IN INCREMENTS OF 4          
SRTCLT   DS    XL2                                                              
SRTPROD  DS    CL3                                                              
SRTEST   DS    XL1                                                              
SRTMKST  DS    XL5                                                              
SRTSLN   DS    XL1                                                              
SRTPROD2 DS    CL3                                                              
SRTSLN2  DS    XL1                                                              
SRTUPDT  DS    XL2                 DATE OF LAST RECORD UPDATE                   
*                                                                               
SRTFLAG  DS    XL1                 00 = PROD AND PTR ARE IN ORDER               
*                                  80 = PROD AND PTR ARE INVERTED               
*                                  VARIABLE PART OF RECORD                      
SRTENT   DS   0XL5                                                              
SRTDT    DS    XL2                                                              
SRTSTAT  DS    XL3                 BB                                           
*                                                                               
* ONLINE ACTIVITY TABLE DSECT                                                   
*                                                                               
TBLRECD  DSECT                     TABLE OF BUY ACTIVITY                        
TBLREC   DS    0XL22               ONE ENTRY FOR EACH LIST LINE                 
TBLRECKY DS    0XL14                                                            
*                                                                               
TBLMKT   DS    XL2                                                              
TBLSTA   DS    XL3                                                              
TBLPRD   DS    XL1                                                              
TBLSLN   DS    XL1                                                              
TBLPRD2  DS    XL1                                                              
TBLSLN2  DS    XL1                                                              
TBLEST   DS    XL1                 ZERO WHEN ESTIMATE = 'NO' OR BLANK           
TBLBBS   DS    XL4                 BITS FOR ANY BILLBOARDS                      
TBLWKS   DS    XL4                 BITS INDICATING 31 WEEKS OF ACTIVITY         
TBLDSKAD DS    XL4                                                              
*                                   LAST BIT INDICATES IF THERE IS              
*                                   OVERFLOW ACTIVITY                           
TBLNEXT  EQU   *-TBLREC                                                         
         EJECT                                                                  
* DSECT FOR DISPLAY RECORD ONLINE (ALL DATES FOR A STATION) *                   
*                                                                               
DSPLINED DSECT                                                                  
*                                                                               
DSPLINE  DS    0CL24               ***** DSECT FOR DISPLAY LINE DATA            
*                                                                               
DSPPRD   DS    CL7                 PROD/SLN                                     
         DS    CL1                                                              
DSPPTR   DS    CL7                 PARTNER/SLN                                  
         DS    CL1                                                              
DSPDTE   DS    CL8                 ACTIVITY DATE                                
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL1                                                              
LSTMKT   DS    CL4                                                              
         DS    CL3                                                              
LSTMKTNM DS    CL20                                                             
         DS    CL2                                                              
LSTSTA   DS    CL7                                                              
         DS    CL2                                                              
LSTPRD   DS    CL7                                                              
         DS    CL2                                                              
LSTPTR   DS    CL7                                                              
         DS    CL2                                                              
LSTEST   DS    CL3                                                              
         DS    CL2                                                              
LSTDTE   DS    CL8                                                              
LSTIND   DS    CL3                                                              
*                                                                               
         ORG   LISTAR                                                           
         DS    CL1                                                              
CLSTCLT  DS    CL3                                                              
         DS    CL4                                                              
CLSTCNAM DS    CL20                                                             
         DS    CL2                                                              
CLSTDATE DS    CL8                                                              
*                                                                               
PRTLINE  DSECT                     ***** DSECT FOR PRINT LINE DATA              
         DS    CL2                                                              
PRTMKT   DS    CL4                                                              
         DS    CL1                                                              
PRTMKTNM DS    CL24                                                             
         ORG   *-11                                                             
PRTNEW   DS    CL1                                                              
PRTSTA   DS    CL7                                                              
         DS    CL2                                                              
PRTAFF   DS    CL3                                                              
         DS    CL3                                                              
PRTSLN   DS    CL3                                                              
         DS    CL1                                                              
PRTPTR   DS    CL7                                                              
         DS    CL1                                                              
PRTDTES  DS    CL60                                                             
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
SPTR11RR DS    F                                                                
ADSPLINE DS    A                                                                
VGLOBBER DS    A                                                                
SVMAXDYS DS    X                   MAX DAYS TO FIRST TLCST                      
SVMINDYS DS    X                   MIN DAYS TO FIRST TLCST                      
XFRCALL  DS    X                   C'Y' IF GOT HERE BY XFRCTL                   
SAVEKEY  DS    XL24                LAST KEY PROCESSED WHEN TABLE FULL           
         DS    0D                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
         ORG   FLD                                                              
MYKEYSV  DS    XL24                                                             
         ORG                                                                    
SVBASEDT DS    CL6                                                              
COL      DS    PL1                 NUMBER OF COLUMNS ON DISPLAY SCREEN          
DSPSW    DS    XL1                                                              
PRTMKTSW DS    CL1                 Y = THIS A MARKET NAME PRINT LINE            
*                                                                               
PERWKS   DS    XL1                 WEEKS TO COVER IN PERIOD                     
*                                                                               
PROD     DS    CL3                                                              
PROD2    DS    CL3                                                              
TODAY    DS    XL2                                                              
*                                                                               
PERDTS   DS    0XL20                                                            
STDATE   DS    XL2                 START OF PERIOD                              
ENDATE   DS    XL2                 END OF PERIOD                                
SPDATE   DS    CL8                 START OF PERIOD                              
EPDATE   DS    CL8                 END OF PERIOD                                
SVSLNS   DS    XL2                                                              
SVMKTSTA DS    XL5                                                              
SVBCLT   DS    XL2                                                              
OCLT     DS    XL2                                                              
WKHDA    DS    CL128                                                            
WKHDB    DS    CL128                                                            
TBLSW    DS    XL1                                                              
*                                                                               
SVSTAKEY DS    CL5                                                              
STADRSW  DS    CL1                                                              
STAAFFL  DS    CL3                                                              
*                                                                               
SVMGRP   DS    XL3                                                              
SVPMGRP  DS    CL5                 MGRP WITH TRAILING ZEROS                     
SVDMGRP  DS    CL5                 DISPLAYABLE MGRP AS ENTERED                  
*                                                                               
WKTABLE  DS    32XL2               31 WEEKS, WITH X'00' IN LAST BYTE            
*                                                                               
SVPROFFC DS    CL16                                                             
LOCKEY   DS    CL(L'KEY)                                                        
LOCKEYSV DS    CL(L'KEYSAVE)                                                    
*MNMB                                                                           
SVT3PR06 DS    CL1                 EXCLUDE CM/DV/SM FROM TRAFFIC                
*MNMB                                                                           
FILTERS  DS    0CL6                                                             
MKTFTR   DS    XL2                                                              
SLNFTR   DS    XL1                                                              
HOLDSIGN DS    CL1                                                              
BBFILT   DS    CL1                                                              
DSKADSW  DS    CL1                                                              
*                                                                               
MGRPTBLE DS    XL300               ROOM FOR 150 MARKETS                         
*                                                                               
EST      DS    XL1                                                              
ESTSTAT  DS    CL1                 'N' IF EST= 'NO', OTHERWISE 'Y'              
BOFFCD   DS    CL1                 OFFICE CODE                                  
LAOFFICE DS    CL1                 OFFICE FOR THIS LIMITED ACCESS               
ACTVDATE DS    XL2                                                              
SPOOLOK  DS    CL1                                                              
EOFSORT  DS    CL1                                                              
DATEUPDT DS    XL3                 DATE RECORD LAST UPDATED                     
NUMSLN   DS    F                   NUMBER OF SPOT LENGTH PAIRS                  
SLNWKKEY DS    XL10                SLNS AND ACTIVITY WEEK BITS                  
SLNWKLST DS    16XL10              UP TO 16 PAIRS PER LIST RECORD               
         DS   0F                                                                
SAVES    DS   0XL8                                                              
SAVEBBS  DS    F                                                                
SAVEWKS  DS    F                                                                
*                                  SCREEN                                       
SCRINDEX DS    F                   INDEX INTO ACTIVITY DATA TABLE               
TBLINDEX DS    F                   INDEX INTO ACTIVITY DATA TABLE               
TABLESZ  DS    F                   NUMBER OF RECORDS IN DATA TABLE              
TABLEKEY DS    XL14                TBA KEY AND ACTIVE WEEK BITS                 
TABLE    DS    0XL14               MUST BE LAST IN DSECT                        
*                                                                               
* TA PROFILE DEFINITIONS                                                        
*                                                                               
SVTAXMED EQU   SVT1PROF+0          EXCLUDE CLIENT FROM MEDIA BUY                
SVTAXTBY EQU   SVT1PROF+1             "      "      "  TRAFFIC                  
SVTAXGOL EQU   SVT1PROF+2             "      "      "  GOAL                     
SVTAMWKS EQU   SVT1PROF+3          WEEKS TO SHOW ON MEDIA BUY ACTIVITY          
SVTANOTR EQU   SVT1PROF+4          DON'T SHOW *T TRAFFIC ACTIVITY               
SVTANOAD EQU   SVT1PROF+15         DON'T SHOW 'NO ADDR' ON BCM                  
*                                                                               
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077SPTRA11   12/06/16'                                      
         END                                                                    
