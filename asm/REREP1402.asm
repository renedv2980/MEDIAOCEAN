*          DATA SET REREP1402  AT LEVEL 053 AS OF 05/01/02                      
*PHASE RE1402C,*                                                                
*INCLUDE HEXIN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         PRINT NOGEN                                                            
         TITLE 'REREP1402 - RE1402 - CONTRACT BATCH CONFIRM MODULE'             
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP1402 --- CONTRACT OFFLINE BATCH CONFIRM MODULE        *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 03FEB97 RHV VOILA!                                                *           
*                                                                   *           
* JAN22/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
RE1402   CSECT                                                                  
         NMOD1 STOREX-STORED,**RE1402,R7,RR=R5                                  
         USING STORED,RC                                                        
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
         ST    R5,RELO                                                          
         LA    RE,HEADLINE                                                      
         ST    RE,HEADHOOK                                                      
         L     RE,=V(HELLO)                                                     
         ST    RE,VHELLO                                                        
*                                                                               
***********************************************************************         
* MODE - RUNFRST - PERFORMS GENERAL MODULE SETUP                      *         
*                - INITIALIZE SORTER                                  *         
***********************************************************************         
         CLI   MODE,RUNFRST        RELOCATE VCONS AT RUNFRST                    
         BNE   REQF00                                                           
*                                                                               
* GET REP PROGRAM PROFILES (SPECIFICALLY CONTRACT PROFILES - CAN BE             
* FOUND IN RECNTPROF).                                                          
         XC    PROFILES,PROFILES   START CLEAN                                  
         LA    R6,RREPREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   RUNF30              NO PROFILE ELEMENT                           
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
RUNF10   CLI   RREPPGM1,RREPQCNT   CONTRACT?                                    
         BE    RUNF20                                                           
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,RUNF10                                                        
         B     RUNF30              CONTRACT NOT FOUND. USE DEFAULTS.            
*                                                                               
RUNF20   MVC   PROFILES,RREPPGM1+2 SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
RUNF30   DS    0H                                                               
*                                                                               
* SAVE OFF SOME IMPORTANT DATES                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
* GET MONDAY WEEK DATE                                                          
         GOTO1 (RF),(R1),(3,TODAY),DUB                                          
* GET DAY OF WEEK                                                               
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),SPACES                                                   
         BNE   *+6                                                              
         DC    H'0'                WRONG COMPUTER DATE                          
         SR    R4,R4                                                            
         IC    R4,DMCB                                                          
         BCTR  R4,R0               DAY                                          
         LNR   R4,R4                                                            
         GOTO1 ADDAY,(R1),DUB,WORK,(R4)                                         
         GOTO1 DATCON,(R1),WORK,(2,MONDATE)                                     
*                                                                               
         MVC   CFTIME,=P'235959'   CONFIRM TIME IS 23:59:59                     
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
* MODE - REQFRST                                                      *         
***********************************************************************         
REQF00   DS    0H                                                               
         CLI   MODE,REQFRST                                                     
         BNE   PROC00                                                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   REQUESTR,QUESTOR    SAVE REQUESTOR                               
         BAS   RE,GETFILT          READ REQ CARD FOR FILTERS                    
         MVI   UPDATE,C'N'         DEFAULT NOT UPDATIVE                         
         CLI   QOPTION1,C'U'       OPTION TO UPDATE?                            
         BNE   *+8                                                              
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         XC    KEY,KEY             GET PRD REC                                  
         MVI   KEY,X'09'                                                        
         MVC   KEY+18(4),QADV                                                   
         MVC   KEY+22(3),QPRODUCT                                               
         MVC   KEY+25(2),QREP                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RPRDREC,DMWORK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             GET AGY REC                                  
         MVI   KEY,X'0A'                                                        
         MVC   KEY+19(6),QAGENCY                                                
         MVC   KEY+25(2),QREP                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RAGYREC,DMWORK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    KEY,KEY             GET ADV REC                                  
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),QADV                                                   
         MVC   KEY+25(2),QREP                                                   
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY,0                             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),(R1),GETREC,REPFILE,KEY+28,RADVREC,DMWORK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    REQDATES,REQDATES                                                
         GOTO1 DATCON,DMCB,QSTART,(11,REQDATES)                                 
         LA    R3,REQDATES                                                      
         CLI   0(R3),0                                                          
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   1(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,QEND,(11,3(R3))                                      
*                                                                               
* INITIALIZE SORTER                                                             
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
*                                                                               
         B     XIT                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,24,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=39'                                    
*                                                                               
***********************************************************************         
* MODE - PROCCONT                                                     *         
***********************************************************************         
PROC00   DS    0H                                                               
         CLI   MODE,PROCCONT                                                    
         BNE   REQL00                                                           
*                                                                               
         B     PROC05              ***> TESTING ONLY                            
         MVI   P,0                                                              
         MVC   PSECOND(6),=C'READ: '                                            
         GOTO1 HEXOUT,DMCB,RCONKCON,PSECOND+6,4                                 
         MVC   PSECOND+15(2),RCONKREP                                           
         GOTO1 REPORT                                                           
PROC05   DS    0H                                                               
*                                                                               
         MVC   KADDR,KEY+28        SAVE DISK ADDR OF CONREC                     
*                                                                               
         BAS   RE,CHECKCON         VALIDATE CONTRACT                            
         BNE   XIT                 INVALID, DON'T CONFIRM                       
*                                                                               
         B     PROC15              ***> TESTING ONLY                            
         MVC   P(6),=C'KEPT: '                                                  
         GOTO1 HEXOUT,DMCB,RCONKCON,P+6,4                                       
         GOTO1 REPORT                                                           
PROC15   DS    0H                                                               
*                                                                               
         BAS   RE,GETCTY           GET CONTYPE RECORD                           
*                                                                               
         BAS   RE,CONFIRM          PERFORM CONFIRMATION                         
*                                                                               
         BAS   RE,TOSORT           PASS K INFO TO SORTER FOR REPORT             
*                                                                               
         BAS   RE,DELBCMT          DELETE BUY ORD COMMENTS                      
*                                                                               
*                                  RESTORE REPORTER'S READ SEQ LOOP             
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),REPDIR,RCONKEY,KEY,0                 
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
***********************************************************************         
* MODE - REQLAST                                                      *         
***********************************************************************         
REQL00   DS    0H                                                               
         CLI   MODE,REQLAST                                                     
         BNE   REQL100                                                          
*                                                                               
         SR    R3,R3               ACCUMULATE GRAND TOTAL                       
         XC    COUNT,COUNT         CONTRACT TALLY                               
*                                                                               
REQL20   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'GET'   GET NEXT CONTRACT FROM SORTER              
         OC    DMCB+4(4),DMCB+4          NO MORE CONTRACTS?                     
         BZ    REQL80                    ALL DONE - EXIT                        
*                                                                               
         LH    R4,COUNT              INCREMENT COUNTER                          
         LA    R4,1(R4)                                                         
         STCM  R4,3,COUNT                                                       
*                                                                               
         ZICM  R2,DMCB+4,4           ADDRESS REC FROM SORTER                    
         USING SORTK,R2                                                         
*                                                                               
         MVI   P,0                                                              
         GOTO1 HEXOUT,DMCB,SKCON,PSECOND+2,4                                    
         LA    R5,PSECOND+2                                                     
         CLI   0(R5),C'0'                                                       
         BNE   *+16                                                             
         MVI   0(R5),C' '                                                       
         LA    R5,1(R5)                                                         
         B     *-16                                                             
*                                                                               
         MVC   PSECOND+14(4),SKSTA   STATION                                    
         LA    R5,PSECOND+18                                                    
         OI    0(R5),X'40'                                                      
         CLI   0(R5),C' '                                                       
         BNE   *+10                                                             
         BCTR  R5,0                                                             
         B     *-14                                                             
         MVC   1(3,R5),=C'-TV'                                                  
         CLI   SKSTA+4,C'F'                                                     
         BNE   *+10                                                             
         MVC   2(2,R5),=C'FM'                                                   
         CLI   SKSTA+4,C'A'                                                     
         BNE   *+10                                                             
         MVC   2(2,R5),=C'AM'                                                   
         CLI   SKSTA+4,C'L'                                                     
         BNE   *+10                                                             
         MVC   2(2,R5),=C'L '                                                   
*                                                                               
         MVC   PSECOND+25(20),SKMKT     MARKET                                  
*                                                                               
*                                       FLIGHT DATES                            
         GOTO1 DATCON,DMCB,(3,SKFLT),(11,PSECOND+49)                            
         MVI   PSECOND+57,C'-'                                                  
         GOTO1 DATCON,DMCB,(3,SKFLT+3),(11,PSECOND+58)                          
*                                                                               
         EDIT  SKTOT,(12,PSECOND+70),2,FLOAT=$   BUCKET TOTAL                   
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         ZICM  R5,SKTOT,4          ADD K TO GRAND TOTAL                         
         SLL   R5,1                ROUND TO DOLLAR                              
         A     R5,=F'100'                                                       
         SR    R4,R4                                                            
         D     R4,=F'100'                                                       
         SRL   R5,1                                                             
         AR    R3,R5               ADD                                          
*                                                                               
         B     REQL20              BACK FOR NEXT CONTRACT                       
         DROP  R2                                                               
*                                                                               
REQL80   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'  END SORT                                    
         OC    COUNT,COUNT                                                      
         BNZ   REQL90                                                           
         MVI   P,0                                                              
         MVC   PSECOND+2(54),=C'*** NO CONTRACTS SELECTED BASED ON REPO+        
               RT FILTERS ***'                                                  
         MVI   PTHIRD,0                                                         
         GOTO1 REPORT                                                           
*                                                                               
REQL90   DS    0H                                                               
         MVI   P,0                                                              
         MVI   PSECOND+2,C'*'                                                   
         MVC   PSECOND+3(79),PSECOND+2                                          
         MVC   PTHIRD+62(6),=C'TOTAL:'                                          
         EDIT  (R3),(12,PTHIRD+70),FLOAT=$,COMMAS=YES,ZERO=NOBLANK              
         MVC   PTHIRD+2(20),=C'CONTRACTS CONFIRMED:'                            
         EDIT  COUNT,(4,PTHIRD+23),ALIGN=LEFT,ZERO=NOBLANK                      
         MVI   PFOURTH+2,C'*'                                                   
         MVC   PFOURTH+3(79),PFOURTH+2                                          
         GOTO1 REPORT                                                           
         B     XIT                                                              
***********************************************************************         
* NO OTHER REPORTER MODES ARE SUPPORTED IN THIS PROGRAM               *         
***********************************************************************         
REQL100  DS    0H                                                               
         B     XIT                                                              
***********************************************************************         
* GETCTY - GET CONTYPE RECORD & SAVE NECESSARY PROFILE BITS                     
***********************************************************************         
GETCTY   NTR1                                                                   
         MVI   CTYPRFK,0           DEFAULT VALUE                                
         CLI   RCONTYPE,0                                                       
         BE    GETCTYX                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'                                                        
         MVC   KEY+24(2),RCONKREP                                               
         MVC   KEY+26(1),RCONTYPE                                               
         GOTO1 DATAMGR,DMCB,DMREAD,REPDIR,KEY,KEY                               
         CLI   DMCB+8,0                                                         
         BNE   GETCTYX                                                          
         GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KEY+28,IOAREA,DMWORK                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   GETCTYX                                                          
         USING RCTYFEL,R6                                                       
         MVC   CTYPRFK,RCTYFPRC    SAVE K PROF BITS                             
         DROP  R6                                                               
GETCTYX  B     XIT                                                              
***********************************************************************         
* CHECKCON - VALIDATE CONTRACT CC=TRUE IF CONTRACT IS OK TO CONFIRM             
***********************************************************************         
CHECKCON NTR1                                                                   
         TM    RCONCNTL,X'80'      DELETED?                                     
         BO    NO                                                               
*                                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET?                                    
         BZ    NO                                                               
*                                                                               
         MVI   ELCODE,X'1F'                                                     
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'80'      NOT CONFIRMED?                               
         BZ    NO                                                               
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'1D'                                                     
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CKCON010                                                         
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'      LINKED TO DARE?                              
         BO    NO                  YES - SKIP                                   
         DROP  R6                                                               
*                                                                               
CKCON010 DS    0H                                                               
         MVI   ELCODE,X'20'                                                     
         LA    R6,RCONREC                                                       
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BZ    NO                                                               
         TM    RCONSENF,X'20'      REP VERSION NOT ADVANCED                     
         BZ    NO                                                               
         DROP  R6                                                               
*                                                                               
         B     YES                                                              
***********************************************************************         
* TOSORT - FEED CONTRACT INFO TO SORTER TO PREPARE FOR REPORT PRINTING          
***********************************************************************         
TOSORT   NTR1                                                                   
         XC    WORK2,WORK2                                                      
         LA    R4,WORK2                                                         
         USING SORTK,R4                                                         
         MVC   SKCON,RCONKCON                                                   
         MVC   SKMKT,RSTAMKT                                                    
         MVC   SKSTA,RCONKSTA                                                   
         MVC   SKFLT,RCONDATE                                                   
*                                                                               
         SR    R5,R5               ACCUMULATE K TOTAL                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
TOS010   BAS   RE,NEXTEL                                                        
         BNE   TOS020                                                           
         ZICM  R3,6(R6),4                                                       
         AR    R5,R3                                                            
         B     TOS010                                                           
TOS020   STCM  R5,15,SKTOT                                                      
*                                                                               
         DROP  R4                                                               
         GOTO1 SORTER,DMCB,=C'PUT',WORK2                                        
         B     XIT                                                              
***********************************************************************         
* HEADLINE - REPORT HEADLINES                                                   
***********************************************************************         
HEADLINE NTR1                                                                   
*                                                                               
         MVC   HEAD1(L'RREPNAME),RREPNAME                                       
         MVC   HEAD2(L'RREPADDR),RREPADDR                                       
*                                                                               
         MVC   HEAD1+40(25),=C'BATCH CONFIRMATION REPORT'                       
         MVC   HEAD2+40(25),=C'-------------------------'                       
*                                                                               
         MVC   HEAD1+101(4),=C'PAGE'                                            
         EDIT  PAGE,(2,HEAD1+106)                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(11,HEAD2+91)                                  
         MVC   HEAD2+100(2),=C'AT'                                              
         GOTO1 TIMCON,DMCB,REQSTART,HEAD2+103                                   
*                                                                               
         MVC   HEAD3(L'REQUESTR),REQUESTR                                       
*                                                                               
         CLI   UPDATE,C'N'         TEST RUN?                                    
         BNE   *+10                                                             
         MVC   HEAD3+32(44),=C'*** TEST REPORT - NO CONTRACTS CONFIRMED+        
                ***'                                                            
*                                                                               
         MVI   HEAD4,0                                                          
*                                                                               
         MVC   HEAD5(11),=C'ADVERTISER:'                                        
         MVC   HEAD5+12(4),RADVKADV                                             
         MVC   HEAD5+18(20),RADVNAME                                            
         MVC   HEAD5+50(7),=C'AGENCY:'                                          
         MVC   HEAD5+58(6),RAGYKAGY                                             
         MVC   HEAD5+66(20),RAGYNAM1                                            
         MVC   HEAD6(8),=C'PRODUCT:'                                            
         MVC   HEAD6+9(3),RPRDKPRD                                              
         MVC   HEAD6+14(20),RPRDNAME                                            
         MVC   HEAD6+50(6),=C'DATES:'                                           
         MVC   HEAD6+57(19),REQDATES                                            
*                                                                               
         MVC   HEAD7(27),=C'ADDITIONAL REQUEST FILTERS:'                        
         MVC   HEAD7+28(104),FILTERS                                            
*                                                                               
         MVI   HEAD8,0                                                          
*                                                                               
         LA    R2,HEAD9                                                         
         MVC   2(08,R2),=C'CONTRACT'                                            
         MVC   14(07,R2),=C'STATION'                                            
         MVC   25(06,R2),=C'MARKET'                                             
         MVC   49(06,R2),=C'FLIGHT'                                             
         MVC   70(14,R2),=C'CONTRACT TOTAL'                                     
         LA    R2,L'HEAD1(R2)                                                   
         MVC   2(08,R2),=C'--------'                                            
         MVC   14(07,R2),=C'-------'                                            
         MVC   25(20,R2),=C'--------------------'                               
         MVC   49(17,R2),=C'-----------------'                                  
         MVC   70(14,R2),=C'--------------'                                     
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
* GETFILT - FORMAT FILTERS FOR REPORT FOOTER                                    
***********************************************************************         
GETFILT  NTR1                                                                   
         LA    R5,FILTERS                                                       
         MVI   FRSTIME,0          FLAG IF ANY FILTERS ON                        
         MVC   0(132,R5),SPACES                                                 
*                                                                               
FT10     CLC   QREGION,SPACES                                                   
         BE    FT20                                                             
         MVC   0(7,R5),=C'REGION='                                              
         MVC   7(2,R5),QREGION                                                  
         LA    R5,10(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT20     CLC   QOFFICE,SPACES                                                   
         BE    FT25                                                             
         MVC   0(7,R5),=C'OFFICE='                                              
         MVC   7(2,R5),QOFFICE                                                  
         LA    R5,10(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT25     CLI   QGROUP,C' '                                                      
         BE    FT30                                                             
         MVC   0(6,R5),=C'GROUP='                                               
         MVC   6(1,R5),QGROUP                                                   
         LA    R5,8(R5)                                                         
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT30     CLI   QSBGROUP,C' '                                                    
         BE    FT40                                                             
         MVC   0(8,R5),=C'SBGROUP='                                             
         MVC   8(1,R5),QSBGROUP                                                 
         LA    R5,10(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT40     CLC   QMAN,SPACES                                                      
         BE    FT50                                                             
         MVC   0(12,R5),=C'SALESPERSON='                                        
         MVC   8(3,R5),QMAN                                                     
         LA    R5,16(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT50     CLI   QDIV,C' '                                                        
         BE    FT60                                                             
         MVC   0(4,R5),=C'DIV='                                                 
         MVC   8(1,R5),QDIV                                                     
         LA    R5,6(R5)                                                         
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT60     CLI   QTEAM,C' '                                                       
         BE    FT70                                                             
         MVC   0(5,R5),=C'TEAM='                                                
         MVC   8(1,R5),QTEAM                                                    
         LA    R5,7(R5)                                                         
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT70     CLC   QCLASS,SPACES                                                    
         BE    FT80                                                             
         MVC   0(6,R5),=C'CLASS='                                               
         MVC   6(2,R5),QCLASS                                                   
         LA    R5,9(R5)                                                         
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT80     CLC   QCATY,SPACES                                                     
         BE    FT90                                                             
         MVC   0(9,R5),=C'CATEGORY='                                            
         MVC   9(2,R5),QCATY                                                    
         LA    R5,12(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT90     CLI   QCONTYPE,C' '                                                    
         BE    FT500                                                            
         MVC   0(8,R5),=C'CONTYPE='                                             
         MVC   8(1,R5),QCONTYPE                                                 
         LA    R5,10(R5)                                                        
         MVI   FRSTIME,1          A REQUEST FILTER IS ON                        
FT500    DS    0H                                                               
         CLI   FRSTIME,0          ANY REQUEST FILTER?                           
         BNE   FT120                                                            
*                                                                               
FT110    MVC   0(4,R5),=C'NONE'                                                 
*                                                                               
FT120    B     XIT                                                              
***********************************************************************         
* CONFIRM - PERFORM CONTRACT CONFIRMATION                             *         
***********************************************************************         
CONFIRM  NTR1                                                                   
*                                                                               
CNF10    DC    0H'0'                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   CNF15                                                            
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'20'           CONFIRMED PREVIOUSLY?                   
         BO    CNF15                                                            
         DROP  R6                                                               
*                                                                               
* DATE STAMP EPL/SPL ELEM ON ORIGINAL CONFIRM IF CONTYPE OPTION SET             
         TM    CTYPRFK,X'20'            CONTYPE K OPTION #3?                    
         BZ    CNF11                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'             EPL/SPL ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   CNF10A                                                           
         USING RCONSPEL,R6                                                      
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSPYR)   TODAY'S DATE                    
         OI    RCONSPES,X'08'           FLAG NEW DATE STYLE                     
         B     CNF11                                                            
CNF10A   DS    0H                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         MVI   RCONSPCO,X'06'                                                   
         MVI   RCONSPLN,X'12'                                                   
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSPYR)   TODAY'S DATE                    
         MVI   RCONSPES,X'0C'                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RCONSPDT)   TODAY'S DATE                    
         MVI   RCONSPNU,1                                                       
         MVC   RCONSPST,RCONKSTA                                                
        GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK2,=C'ADD=CODE'         
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'                                                     
         BAS   RE,GETEL                                                         
         BE    CNF11                                                            
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING RCONACEL,R6                                                      
         MVI   RCONACCO,X'08'                                                   
         MVI   RCONACLN,X'0C'                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,RCONACTA)   TODAY'S DATE                    
        GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK2,=C'ADD=CODE'         
         DROP  R6                                                               
*                                                                               
CNF11    DS    0H                                                               
         CLI   RCONKSTA+4,C' '                                                  
         BE    CNF11A                                                           
         CLI   RCONKSTA+4,C'L'                                                  
         BNE   CNF15                                                            
CNF11A   DS    0H                                                               
         TM    PROFILES+CNTSETBB,CNTSETBA  CHECK PROFILE #27                    
         BZ    CNF15                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
CNF12    BNE   CNF15                                                            
         USING RCONBKEL,R6                                                      
         MVC   RCONBKWK,MONDATE         TODAY'S MONDAY DATE                     
         DROP  R6                                                               
         BAS   RE,NEXTEL                                                        
         B     CNF12                                                            
*                                                                               
CNF15    DS    0H                                                               
         TM    CTYPRFK,X'20'            CONTYPE K OPTION #3?                    
         BZ    CNF18                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'             SPL/EPL ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   CNF18                    NO ELEM? - SKIP                         
         USING RCONSPEL,R6                                                      
         CLI   RCONSPNU,0               IF NO STATIONS, SKIP THIS PART          
         BE    CNF17                                                            
         ZIC   R1,RCONSPNU              # OF STATIONS                           
         LA    RE,RCONSPAM              FIRST MINI ELEM AMOUNT                  
CNF16    XC    0(4,RE),0(RE)            CLEAR MINI ELEM AMOUNT                  
         LA    RE,9(RE)                 NEXT MINI ELEM AMOUNT                   
         BCT   R1,CNF16                                                         
CNF17    NI    RCONSPES,X'FF'-X'40'     CLEAR SOME FLAGS                        
         NI    RCONSPES,X'FF'-X'80'                                             
         TM    RCONSPES,X'20'           OVERIDE VALUE IN 08 ELEM?               
         BZ    CNF18                    NO - SKIP                               
         NI    RCONSPES,X'FF'-X'20'     YES - CLEAR FLAG                        
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'             GET 08 ELEM                             
         BAS   RE,GETEL                                                         
         BNE   CNF18                                                            
         USING RCONACEL,R6                                                      
         XC    RCONAC$$,RCONAC$$        ZERO OVERIDE AMOUNT                     
         DROP  R6                                                               
*                                                                               
CNF18    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONELEM,R6                                                      
         ZIC   R1,RCONMOD                                                       
         LA    R1,1(R1)                                                         
         STC   R1,RCONMOD                                                       
         MVC   RCONMODD,TODAY                                                   
         SPACE 1                                                                
*  MARK CONTRACT CONFIRMED                                                      
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONXEL,R6                                                       
         NI    RCONCONF,X'7F'      TURN OFF NOT CONFIRMED                       
         OI    RCONCONF,X'40'      TURN ON CONFIRMED NOW                        
         OI    RCONSTAT,X'01'      BATCH CONFIRM FLAG                           
*                                                                               
*  UPDATE X'22' MOD DATE/TIME ELEMENT, CREATE IF NECESSARY                      
CNF30    LA    R6,RCONREC                                                       
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BE    CNF35                IF IT DOESN'T EXIST, MAKE IT                
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         MVI   0(R6),X'22'                                                      
         MVI   1(R6),RMODELLQ                                                   
        GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK2,=C'ADD=CODE'         
         B     CNF30                                                            
CNF35    LR    R5,R6                                                            
         USING RMODELEM,R5                                                      
         MVC   WORK2(RMODEL3M-RMODEL1M),RMODEL1M                                
         MVC   RMODEL2M(RMODEL3M-RMODEL1M),WORK2                                
         MVC   RMODEL1M,RCONMOD                                                 
         GOTO1 DATCON,DMCB,(5,0),(2,RMODEL1D)                                   
         UNPK  RMODEL1T(6),CFTIME                                               
*                                                                               
*  MARK CONTRACT WITH DATE/TIME/SENT FLAG                                       
         SPACE 1                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    CNF40                                                            
         DC    H'0'                                                             
         SPACE 1                                                                
         USING RCONSEND,R6                                                      
CNF40    DS    0H                   PUT HIGHER OF REP OR STA VERSION#           
         MVC   RMODEL1V,RCONSRV     IN X'22' ELEMENT                            
         CLC   RCONSSV,RCONSRV                                                  
         BNH   *+10                                                             
         MVC   RMODEL1V,RCONSRV                                                 
         DROP  R5                                                               
*                                                                               
         OI    RCONSENF,X'10'      STA VERS. NOT ADVANCED                       
         NI    RCONSENF,X'7F'      TURN OFF SENT BY REP                         
         OI    RCONSENF,X'40'      TURN ON SENT BY STATION                      
         OI    RCONSENF,X'02'      TURN ON CONF BY STATION                      
CNF50    NI    RCONSENF,X'FF'-X'01' TURN OFF TAKEOVER FLAG, IF ON               
         GOTO1 DATCON,DMCB,(5,0),(2,RCONSSDT) COMPRESSED DATE                   
         UNPK  RCONSSTI(6),CFTIME                                               
*                                                                               
*  DELETE ORDER COMMENTS                                                        
CNF51    EQU   *                                                                
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'82',RCONREC),0                 
         TM    DMCB+12,X'FF'-X'06'             REP COMMENT                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'92',RCONREC),0                 
         TM    DMCB+12,X'FF'-X'06'             STATION COMMENT                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* FOR EDI/DARE ORDERS, UPDATE EDI ELEMENT FOR MOD NUMBERS                       
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        FOR SPECIAL EDI/DARE ORDERS                  
         BAS   RE,GETEL            ADD A PASSIVE KEY X'E1'                      
         BNE   CNF27                                                            
         USING RCONDREL,R6                                                      
         CLI   RCONDRFG,X'02'                                                   
         BE    CNF25B              SPECIAL EDI CONVERTED ORDER                  
         TM    RCONDRFG,X'80'+X'40'+X'04'                                       
         BNO   CNF27               MUST BE LINKED, APPROVED AND EDI             
         DROP  R6                                                               
*                                                                               
CNF25B   DS    0H                                                               
         LA    R6,RCONREC          IF ELEMENT EXISTS, UPDATE THIS               
         MVI   ELCODE,X'ED'        ELEMENT ONLY ONCE PER DAY                    
         BAS   RE,GETEL                                                         
         BNE   CNF26                                                            
         USING RCONEDEL,R6                                                      
         CLC   RCONEDDT,TODAY                                                   
         BE    CNF27                                                            
         MVC   RCONEDDT,TODAY                                                   
         ZIC   RF,RCONEDMD                                                      
         LA    RF,1(RF)                                                         
         STC   RF,RCONEDMD                                                      
         B     CNF27                                                            
         DROP  R6                                                               
*                                                                               
CNF26    DS    0H                  ELEMENT NOT FOUND, ADD ONE                   
         LA    R6,WORK2                                                         
         USING RCONEDEL,R6                                                      
         XC    WORK2(80),WORK2                                                  
         MVI   RCONEDCD,X'ED'                                                   
         MVI   RCONEDLN,6                                                       
         MVI   RCONEDMD,1          EDI/KATZ ORDER STARTS AT MOD 1               
         MVC   RCONEDDT,TODAY                                                   
*                                                                               
         CLC   =C'1224',RCONKAGY   EXCEPT FOR LEO,                              
         BE    CNF26A               WHICH IS TO START AT MOD 0                  
         CLC   =C'LEOB',RCONKAGY                                                
         BNE   CNF26B                                                           
*                                                                               
CNF26A   DS    0H                                                               
         MVI   RCONEDMD,0                                                       
*                                                                               
CNF26B   DS    0H                                                               
        GOTO1 VHELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,WORK2,=C'ADD=CODE'         
         DROP  R6                                                               
*                                                                               
*  PUT CONTRACT                                                                 
*                                                                               
CNF27    DS    0H             RE-READ K FOR UPDATE (INTO BOGUS AREA)            
         CLI   UPDATE,C'Y'    CHECK FOR WRITE=NO                                
         BNE   CNF28                                                            
         GOTO1 DATAMGR,DMCB,(X'80',GETREC),REPFILE,KADDR,IO2,DMWORK             
         B     CNF29                                                            
CNF28    GOTO1 DATAMGR,DMCB,GETREC,REPFILE,KADDR,IO2,DMWORK                     
CNF29    TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   UPDATE,C'Y'    CHECK FOR WRITE=YES                               
         BNE   CNF29A              NO                                           
         GOTO1 DATAMGR,DMCB,PUTREC,REPFILE,KADDR,RCONREC,DMWORK                 
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
CNF29A   DS    0H                                                               
*                                                                               
CNF70    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        FOR SPECIAL EDI/DARE ORDERS                  
         BAS   RE,GETEL            ADD A PASSIVE KEY X'E1'                      
         BNE   CNFX                                                             
         USING RCONDREL,R6                                                      
         CLI   RCONDRFG,X'02'                                                   
         BE    CNF80               SPECIAL FOR EDI CONVERTED ORDERS             
         TM    RCONDRFG,X'80'+X'40'+X'04'                                       
         BNO   CNFX                MUST BE LINKED, APPROVED AND EDI             
         DROP  R6                                                               
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS CONFIRM, ADD A PASSIVE KEY          
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
CNF80    DS    0H                                                               
         CLC   =C'1342',RCONKAGY   SKIP FOR DMBB                                
         BE    CNFX                                                             
         CLC   =C'2905',RCONKAGY   SKIP FOR DMBB                                
         BE    CNFX                                                             
         CLC   =C'DMBB',RCONKAGY   SKIP FOR DMBB                                
         BE    CNFX                                                             
         XC    IOAREA(32),IOAREA                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REDIKEY,R6                                                       
         MVI   REDIKTYP,REDIKTYQ                                                
         MVC   REDIKREP,RCONKREP                                                
         MVI   REDIKACT,C'C'       ACTION IF CONFIRM                            
*                                                                               
         MVC   REDIKCON,RCONKCON   CONTRACT NUMBER                              
*                                  DATE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,REDIKDTE)                                
*                                  FETCH TODAY'S DATE                           
         ICM   R1,15,CFTIME                                                     
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,REDIKTIM       TIME IN HHMMSS                               
         DROP  R6                                                               
*                                                                               
         XC    IOAREA(256),IOAREA                                               
         MVC   IOAREA(27),KEY                                                   
*                                                                               
         MVC   KEY+28(4),KADDR     CONTRACT RECORD ADDRESS                      
*                                                                               
         CLI   UPDATE,C'Y'    CHECK FOR WRITE=NO                                
         BNE   CNF85                                                            
         GOTO1 DATAMGR,DMCB,DMADD,REPDIR,KEY,KEY,0                              
CNF85    DS    0H                                                               
*                                                                               
         LA    R6,IOAREA                                                        
         USING REDBKEY,R6                                                       
         MVI   REDBKTYP,REDBKTYQ   ADD X'0E' RECORD AS A BACKUP                 
         MVI   REDBLEN+1,34+REDBELLQ                                            
         MVI   REDBCODE,1          TO THE X'E1' KEYS                            
         MVI   REDBELLN,REDBELLQ                                                
         MVC   REDBMODN,RCONMOD                                                 
         DROP  R6                                                               
*                                                                               
         CLI   UPDATE,C'Y'    CHECK FOR WRITE=NO                                
         BNE   CNF95                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,REPFILE,0,IOAREA,DMWORK                      
CNF95    DS    0H                                                               
*                                                                               
CNFX     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DELBCMT - DELETE BUY ORD COMMENTS                                   *         
***********************************************************************         
DELBCMT  NTR1                                                                   
*                                                                               
         XC    KEY,KEY                 BUILD BUYREC KEY                         
         MVI   KEY,X'0B'                                                        
         MVC   KEY+16(2),RREPKREP                                               
         MVC   FULL,RCONKCON           GET 9'S COMP REVERSED                    
         L     R0,=X'99999999'                                                  
         S     R0,FULL                                                          
         STCM  R0,15,FULL                                                       
         PACK  KEY+18(1),FULL+3(1)                                              
         PACK  KEY+19(1),FULL+2(1)                                              
         PACK  KEY+20(1),FULL+1(1)                                              
         PACK  KEY+21(1),FULL+0(1)                                              
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
         B     DBC020                                                           
*                                                                               
DBC010   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),REPDIR,KEY,KEY,0                     
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
DBC020   DS    0H                                                               
         CLC   KEY(22),KEYSAVE                                                  
         BNE   DCBX                GOOD - CONTINUE ON                           
*                                                                               
         CLI   UPDATE,C'Y'    CHECK FOR WRITE=YES                               
         BNE   DBC025                                                           
         GOTO1 (RF),(R1),(X'88',GETREC),REPFILE,KEY+28,RBUYREC,DMWORK           
         B     DBC030                                                           
DBC025   DS    0H                                                               
         GOTO1 (RF),(R1),(X'08',GETREC),REPFILE,KEY+28,RBUYREC,DMWORK           
DBC030   DS    0H                                                               
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                BLOW UP FOR DISK ERROR                       
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'REPFILE'),(X'84',RBUYREC),0                 
         TM    DMCB+12,X'FF'-X'06'  DELETE ORDER COMMENTS                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   UPDATE,C'Y'    CHECK FOR WRITE=YES                               
         BNE   DBC040                                                           
         GOTO1 DATAMGR,DMCB,PUTREC,REPFILE,KEY+28,RBUYREC,DMWORK                
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
DBC040   DS    0H                                                               
*                                                                               
         B     DBC010              NEXT BUY                                     
*                                                                               
DCBX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
NO       SR    R0,R0                                                            
         CR    RB,R0                                                            
         B     *+6                                                              
YES      CR    RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
* SAVED STORAGE                                                                 
*                                                                               
PROFILES DS    XL8                                                              
TODAY    DS    CL3       B         TODAY'S DATE                                 
MONDATE  DS    CL2                 PACKED MONDAY WEEK DATE                      
UPDATE   DS    C                   Y=UPDATE N=NO UPDATE                         
CFTIME   DS    CL4                 CONFIRM TIME                                 
REQUESTR DS    CL12                REQUESTOR                                    
REQDATES DS    CL19                REQ DATE RANGE                               
FILTERS  DS    CL132               FILTERS FOR FOOTER                           
*                                                                               
* LITERALS                                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* STORAGE AREAS                                                                 
*                                                                               
STORED   DSECT                                                                  
RELO     DS    A                                                                
FRSTIME  DS    X                                                                
ELCODE   DS    C                                                                
KADDR    DS    F                                                                
COUNT    DS    H                   NUMBER OF CONTRACTS CONFIRMED                
VHELLO   DS    A                   A(HELLO)                                     
CTYPRFK  DS    X                   CONTYPE REC K PROFILES                       
SVCONKEY DS    CL27                SAVE K KEY                                   
WORK2    DS    CL240                                                            
         DS    0D                                                               
IOAREA   DS    CL1000                                                           
IO2      DS    CL2000                                                           
STOREX   EQU   *                                                                
*              FILE CONTROL AND WORKD DSECTS                                    
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE RECNTPROF                                                      
       DSECT                                                                    
       ++INCLUDE REGENDAR                                                       
       DSECT                                                                    
       ++INCLUDE REGENEDI                                                       
SORTK    DSECT                                                                  
SKMKT    DS    CL20                MARKET NAME                                  
SKCON    DS    CL4                 K NUMBER                                     
SKSTA    DS    CL5                 STATION                                      
SKFLT    DS    CL6                 FLIGHT DATES                                 
SKTOT    DS    CL4                 K $ TOTAL                                    
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053REREP1402 05/01/02'                                      
         END                                                                    
