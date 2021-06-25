*          DATA SET PPEZF11    AT LEVEL 014 AS OF 05/01/02                      
*PHASE T23011A,*                                                                
*INCLUDE SORTER                                                                 
*INCLUDE KHDUMMY                                                                
***********************************************************************         
*                                                                     *         
*  TITLE: T23011 - EASI BATCH CLOSE - DELETE BATCHES ON/PRIOR TO DATE *         
*            AND PRIOR                                                *         
*  COMMENTS: THIS PROGRAM DELETES EASI INVOICES BATCHES FROM THE      *         
*            WORKER FILE.                                             *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE BATCHES                                   *         
*           LISTING OF BATCHES WITH CLOSED OR NON-CLOSED STATUS       *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - POINTER TO DEFERRED PRINT AREA                        *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPEZF00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             ATIA - WORKER INDEX                                     *         
*             ATIA - WORKER RECORD FOR UPDATE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  LEV 03    OCT16/92 ALSO CHECK WRITE=NO                             *         
*  LEV 04    OCT28/92 FIX BAD LENGTH TOTALS TITLE                     *         
*  LEV 05    APR23/93 CHANGE TO EASIWK FROM CONTROLLER                *         
*  LEV 06    JUL27/93 ADD MEDIAS TO CLOSED AGENCY LIST                *         
*  LEV 07    OCT28/93 CHANGE FROM XSORT TO SORTER AND ADD TOTALS      *         
*  LEV 08    NOV29/93 ALLOW UNKNOWN AGENCY TO PRINT, NOT PROG CHECK   *         
*  LEV 09    AUG01/94 CK FOR SHORT 34 REC                             *         
*  LEV 10    APR11/95 CK FOR MAX SIZE BATCH                           *         
*  LEV 11    JUN06/95 ADD NWK FILES                                   *         
*  LEV 12    AUG06/96 MORE NWK FILES STUFF                            *         
*  LEV 13    AUG21/96 FIX 34 REC COST AND GROSS ONLY                  *         
*                                                                     *         
***********************************************************************         
         TITLE 'T23011 - EASI BATCH DELETE/RESTORE'                             
T23011   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**3011**,R7,RR=R2                                              
         PRINT NOGEN                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,T23011RR                                                      
         MVC   AIO,AIO1                                                         
         MVI   QMED,C'T'                                                        
         CLI   OFFLINE,C'Y'                                                     
         BE    *+20                                                             
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTLIST      CLIENT'S CAN ONLY DO A LIST                  
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    BADACTER                                                         
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    BADACTER                                                         
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    BADACTER                                                         
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE ON SCREEN                
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LIST                                                             
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                  VALIDATE KEY                                 
VKEY     LA    R2,CONACTH                                                       
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        DDS TERMINAL                                 
         BNE   BADACTER                                                         
         CLI   ACTNUM,ACTCHA                                                    
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTADD                                                    
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTDEL                                                    
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTREST                                                   
         BE    BADACTER                                                         
         CLI   ACTNUM,ACTDIS                                                    
         BE    BADACTER                                                         
         SPACE                                                                  
         LA    R2,EZFSTAH          STATION                                      
         XC    SVSTA,SVSTA                                                      
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK100                                                            
         SPACE                                                                  
         GOTO1 VALISTA                                                          
         MVC   SVSTA,QSTA                                                       
         SPACE                                                                  
VK100    LA    R2,EZFDTEH          DATE                                         
         XC    SVDTE,SVDTE                                                      
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDTE)                                   
         SPACE                                                                  
VK200    LA    R2,EZFSRCEH         SOURCE                                       
         XC    SVSRCE,SVSRCE                                                    
         CLI   5(R2),0                                                          
         BE    VK260                                                            
         SPACE                                                                  
         OC    EZFSRCE,SPACES                                                   
         LA    RE,SOURCE                                                        
         LA    RF,SOURCES                                                       
VK220    CLC   EZFSRCE,0(RE)                                                    
         BE    VK230                                                            
         LA    RE,L'SOURCE(,RE)                                                 
         BCT   RF,VK220                                                         
         B     BADSRCE                                                          
VK230    MVC   SVSRCE,0(RE)                                                     
         SPACE                                                                  
VK260    LA    R2,EZFSTATH         STATUS                                       
         XC    SVSTAT,SVSTAT                                                    
         CLI   5(R2),0                                                          
         BE    VK300                                                            
         SPACE                                                                  
         CLI   EZFSTAT,C'C'        ONLY CONVERTED                               
         BE    VK270                                                            
         CLI   EZFSTAT,C'U'        ONLY UNCONVERTED                             
         BNE   STATERR                                                          
         SPACE                                                                  
VK270    MVC   SVSTAT,EZFSTAT                                                   
         SPACE                                                                  
VK300    LA    R2,EZFDRFH          DRAFT                                        
         SPACE                                                                  
         MVI   SVDRAFT,C'Y'        DEFAULT IS DRAFT RUN                         
         CLI   5(R2),0                                                          
         BE    VK320                                                            
         SPACE                                                                  
         CLI   EZFDRF,C'Y'         YES, NO FILE UPDATE                          
         BE    VK310                                                            
         CLI   EZFDRF,C'N'         NO, REAL FILE UPDATE                         
         BNE   DRAFERR                                                          
         SPACE                                                                  
VK310    MVC   SVDRAFT,EZFDRF                                                   
         SPACE                                                                  
VK320    LA    R2,EZFRESH          RESTORE                                      
         XC    SVREST,SVREST                                                    
         CLI   5(R2),0                                                          
         BE    VK340                                                            
         SPACE                                                                  
         CLI   EZFRES,C'Y'         YES, RESTORE RUN                             
         BE    VK330                                                            
         CLI   EZFRES,C'N'         NO, REAL FILE UPDATE                         
         BNE   RESTERR                                                          
         SPACE                                                                  
VK330    MVC   SVREST,EZFRES                                                    
         SPACE                                                                  
VK340    DS    0H                                                               
*        GOTO1 SCANNER,DMCB,(R2),(1,WORK+32),C',=,:'                            
*        CLI   4(R1),0             TEST FOR SCAN ERROR                          
*        BE    TRAPERR                                                          
*        CLI   WORK+34,X'80'                                                    
*        BZ    TRAPERR                                                          
*        CLI   WORK+35,X'80'                                                    
         SPACE                                                                  
         XC    SVUIDNUM,SVUIDNUM                                                
         LA    R2,EZFOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         MVI   BYTE,1                                                           
         LA    R4,WORK                                                          
         XC    WORK,WORK                                                        
         MVC   CONHEAD(14),=C'INVALID OPTION'                                   
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
         SPACE                                                                  
VK500    CLI   0(R4),0                                                          
         BE    VK700                                                            
*                                  USER CODE                                    
         CLI   12(R4),C'U'                                                      
         BNE   VK590                                                            
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VK590                                                            
         MVC   CONHEAD(15),=C'INVALID USER ID'                                  
         CLI   1(R4),8                                                          
         BH    VKERR                                                            
         MVC   SVUID,22(R4)                                                     
         SPACE                                                                  
         MVI   SVUIDNUM,X'FF'      'ALL' IDS                                    
         CLC   SVUID(3),=C'ALL'                                                 
         BE    VK600                                                            
         SPACE                                                                  
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,MVCUID                                                        
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO                     
         MVC   AIO,AIO1                                                         
         CLI   8(R1),0                                                          
         BE    VK520                                                            
         MVC   CONHEAD(15),=C'INVALID USER ID'                                  
         B     MYERR2                                                           
         SPACE                                                                  
VK520    L     R6,AIO2                                                          
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   SVUIDNUM,2(R6)      BINARY USER ID (ORIGIN)                      
         SPACE                                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(2),2(R6)                                                    
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WORK,AIO1,ATIA             
         SPACE                                                                  
         LA    R1,WORK                                                          
         MVC   EASIWK,UKUSRINF-UKRECD(R1)                                       
         SPACE                                                                  
         B     VK600                                                            
         SPACE                                                                  
VK590    B     VKERR                                                            
*                                  UP ERROR FIELD & NEXT BLOCK                  
VK600    ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,32(R4)                                                        
         B     VK500                                                            
         SPACE                                                                  
VK700    MVC   CONHEAD,SPACES                                                   
         SPACE                                                                  
VKXIT    B     EXIT                                                             
MVCUID   MVC   KEY+15(0),22(R4)                                                 
         SPACE                                                                  
VKERR    OI    BYTE,X'F0'                                                       
         MVC   CONHEAD+17(5),=C'FIELD'                                          
         MVC   CONHEAD+23(1),BYTE                                               
MYERR    LA    R2,EZFOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE                                                                  
LIST     MVI   USEIO,C'Y'                                                       
         XC    TCTRS,TCTRS                                                      
         ZAP   TEZNET,=P'0'        TOTAL NET DOLLARS                            
         ZAP   TEZNETD,=P'0'                                                    
         ZAP   TEZGRS,=P'0'                                                     
         ZAP   TEZGRSD,=P'0'                                                    
         MVI   REPORTSW,0                                                       
         SPACE                                                                  
         OC    SVUIDNUM,SVUIDNUM   HAS ID BEEN SET AS OTPION                    
         BNZ   *+10                                                             
         MVC   SVUIDNUM,TWAORIG    NO, USE SIGN ON                              
         SPACE                                                                  
         L     R3,=V(DUMMY)        CLEAR LIST OF SAVED ENTRIES                  
         A     R3,T23011RR                                                      
         USING SVLEZLSD,R3                                                      
         LR    RE,R3                                                            
         L     RF,SVEZTBLN                                                      
         LR    R0,RE                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        THIS OFFLINE                                 
         BNE   LS050                                                            
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         LA    R3,SVEZWORK                                                      
         SPACE                                                                  
LS050    LA    R4,SVEZKEY                                                       
         USING EZWKRIXD,R4                                                      
         SPACE                                                                  
         CLI   MODE,PRINTREP                                                    
         BNE   LS100                                                            
         SPACE                                                                  
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         CLI   SVUIDNUM,X'FF'      UNLESS 'ALL' IDS                             
         BNE   LS100                                                            
         MVC   EASIWK,=CL8'WRKF0'                                               
         SPACE                                                                  
LS070    ZIC   RF,EASIWK+4                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,EASIWK+4                                                      
         XC    SVEZKEY,SVEZKEY                                                  
         SPACE                                                                  
LS100    GOTO1 DATAMGR,DMCB,=C'INDEX',EASIWK,SVEZKEY,AIO1,ATIA                  
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    LS150                                                            
         CLI   SVUIDNUM,X'FF'      UNLESS 'ALL' IDS                             
         BNE   LSEOJ                                                            
         CLI   EASIWK+4,C'8'                                                    
         BL    LS070                                                            
         XC    SVEZKEY,SVEZKEY                                                  
         B     LSEOJ                                                            
         SPACE                                                                  
LS150    L     RF,TWKRECS                                                       
         LA    RF,1(,RF)                                                        
         ST    RF,TWKRECS                                                       
         SPACE                                                                  
         CLI   EZWIDAY,X'99'       ONLY WANT EASI BATCHES                       
         BNE   LS100                                                            
         SPACE                                                                  
         L     RF,TEZRECS                                                       
         LA    RF,1(,RF)                                                        
         ST    RF,TEZRECS                                                       
         SPACE                                                                  
         CLI   SVUIDNUM,X'FF'      UNLESS 'ALL' IDS                             
         BE    *+14                                                             
         CLC   EZWIUID,SVUIDNUM    TEST RIGHT ID                                
         BNE   LS100                                                            
         SPACE                                                                  
         MVC   SVEZKEY,EZWIKEY                                                  
         MVC   WORK(4),EZWISTN         FORMAT STATION                           
         CLI   WORK+3,C' '                                                      
         BH    *+8                                                              
         MVI   WORK+3,C' '                                                      
         MVC   WORK+4(1),EZWIMED                                                
         SPACE                                                                  
* IF FILTERS MATCH ON FILTER                                                    
         SPACE                                                                  
         OC    SVSTA,SVSTA         STATION                                      
         BZ    *+14                                                             
         CLC   WORK(5),SVSTA                                                    
         BNE   LS100                                                            
         SPACE                                                                  
* READ FIRST RECORD                                                             
         SPACE                                                                  
LS200    GOTO1 DATAMGR,DMCB,=C'READ',EASIWK,SVEZKEY,AIO1,ATIA                   
         TM    DMCB+8,X'80'        IF EOF ON FIRST READ                         
         BNZ   LS100                                                            
         SPACE                                                                  
         L     R5,ATIA                                                          
         USING W_RECD,R5                                                        
         MVC   SVFILNO,W_FILENO                                                 
         MVC   SVDATEC,W_AGELD                                                  
         MVC   SVWKCMT,W_DESC                                                   
         DROP  R5                                                               
         SPACE                                                                  
* IF FILTERS MATCH ON FILTER                                                    
         SPACE                                                                  
         OC    SVSRCE,SVSRCE       BATCH SRCE                                   
         BZ    LS210                                                            
         CLC   SVWCSRCE,SVSRCE                                                  
         BNE   LS100                                                            
         SPACE                                                                  
LS210    CLI   SVSTAT,0            SPECIAL STATUS                               
         BE    LS240                NO                                          
         SPACE                                                                  
         CLI   SVSTAT,C'C'         CONVERTED STATUS                             
         BE    LS220                YES                                         
         CLI   SVSTAT,C'U'         UNCONVERTED STATUS                           
         BE    LS230                YES                                         
         DC    H'0'                                                             
         SPACE                                                                  
LS220    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BZ    LS100                NO                                          
         B     LS240                                                            
         SPACE                                                                  
LS230    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BO    LS100                YES                                         
         SPACE                                                                  
LS240    L     RF,EZBATCT          ADD TO TOTAL BATCHES                         
         LA    RF,1(,RF)                                                        
         ST    RF,EZBATCT                                                       
         XC    BCTRS,BCTRS                                                      
         SPACE                                                                  
         L     R6,AIO1                                                          
         SPACE                                                                  
*                                  CK FOR 31 RECS THAT ARE CONVRTD/DEL          
*                                                                               
STR10    CLC   =C'31',4(R6)        THIS AN INVOICE HEADER                       
         BNE   STR20                                                            
         CLI   6(R6),X'5E'         THIS A FIELD SEPARATOR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         L     R1,BEZINV                 INVOICES                               
         LA    R1,1(,R1)                                                        
         ST    R1,BEZINV                                                        
         SPACE                                                                  
         B     STR50                                                            
*                                  CK FOR 34 TOTAL RECS                         
*                                                                               
STR20    CLC   =C'34',4(R6)        THIS AN INVOICE TOTAL                        
         BNE   STR40                                                            
         CLI   6(R6),X'5E'         THIS A FIELD SEPARATOR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,2                GET GROSS DOLLARS                            
         LA    RF,4(,R6)                                                        
STR22    CLI   0(RF),X'5E'                                                      
         BE    STR24                                                            
         CLI   0(RF),X'15'         THIS A SHORT REC                             
         BE    STR50                                                            
         LA    RF,1(,RF)                                                        
         B     STR22                                                            
STR24    LA    RF,1(,RF)                                                        
         BCT   RE,STR22                                                         
         SPACE                                                                  
         SR    R1,R1                                                            
         LA    R0,11                                                            
         LR    RE,RF               SAVE START ADDRESS                           
STR26    CLI   0(RF),X'5E'                                                      
         BE    STR28                                                            
         CLI   0(RF),X'15'                                                      
         BE    STR28                                                            
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,STR26                                                         
STR28    LTR   R1,R1                                                            
         BZ    STR29                                                            
         BCTR  R1,0                                                             
         EX    R1,STRPACK                                                       
         SPACE                                                                  
         CVB   R0,DUB                                                           
         SPACE                                                                  
         C     R0,MAXDOLG                                                       
         BNH   *+8                                                              
         ST    R0,MAXDOLG                                                       
         SPACE                                                                  
         A     R0,BEZGRS                                                        
         ST    R0,BEZGRS                                                        
STR29    CLI   0(RF),X'15'         IF END OF REC, QUIT                          
         BE    STR50                                                            
         SPACE                                                                  
         LA    RE,1                GET NET DOLLARS                              
         LA    RF,1(,RF)                                                        
STR30    CLI   0(RF),X'5E'                                                      
         BE    STR32                                                            
         CLI   0(RF),X'15'                                                      
         BE    STR50                                                            
         LA    RF,1(,RF)                                                        
         B     STR30                                                            
STR32    LA    RF,1(,RF)                                                        
         BCT   RE,STR30                                                         
         SPACE                                                                  
         SR    R1,R1                                                            
         LA    R0,11                                                            
         LR    RE,RF               SAVE START ADDRESS                           
STR34    CLI   0(RF),X'5E'                                                      
         BE    STR36                                                            
         CLI   0(RF),X'15'                                                      
         BE    STR36                                                            
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,STR34                                                         
STR36    LTR   R1,R1               IS THERE A NET DOLLARS FIELD                 
         BZ    STR50                                                            
         BCTR  R1,0                                                             
         EX    R1,STRPACK                                                       
         SPACE                                                                  
         CVB   R0,DUB                                                           
         SPACE                                                                  
         C     R0,MAXDOLN                                                       
         BNH   *+8                                                              
         ST    R0,MAXDOLN                                                       
         SPACE                                                                  
         A     R0,BEZNET                                                        
         ST    R0,BEZNET                                                        
         B     STR50                                                            
STRPACK  PACK  DUB,0(0,RE)                                                      
         SPACE                                                                  
*                                  CK FOR 51 SPOT RECORDS                       
*                                                                               
STR40    CLC   =C'51',4(R6)        THIS AN SPOT RECORD                          
         BNE   STR50                                                            
         CLI   6(R6),X'5E'         THIS A FIELD SEPARATOR                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   7(R6),C'Y'          DID SPOT RUN                                 
         BNE   STR50                                                            
         SPACE                                                                  
         L     R1,BEZSPT                 SPOTS                                  
         LA    R1,1(,R1)                                                        
         ST    R1,BEZSPT                                                        
         SPACE                                                                  
         C     R1,MAXSPTS                                                       
         BNH   *+8                                                              
         ST    R1,MAXSPTS                                                       
         SPACE                                                                  
* READ NEXT RECORD                                                              
         SPACE                                                                  
STR50    GOTO1 DATAMGR,DMCB,=C'READ',EASIWK,SVEZKEY,AIO1,ATIA                   
         SPACE                                                                  
         TM    DMCB+8,X'80'        IS IT EOF                                    
         BZ    STR10                NO                                          
         SPACE                                                                  
         MVC   LSTA,EZWISTN                                                     
         LA    RE,LSTA+3                                                        
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         LA    RF,MEDTBL                                                        
         LA    R0,8                                                             
LS300    CLC   EZWIMED,0(RF)                                                    
         BE    LS320                                                            
         LA    RF,4(RF)                                                         
         BCT   R0,LS300                                                         
LS320    MVC   1(2,RE),1(RF)                                                    
         MVC   BATMEDIA,3(RF)      SAVE MEDIA FOR THIS BATCH                    
         SPACE                                                                  
         OC    SVDATEC,SVDATEC                                                  
         BZ    LS340                                                            
         GOTO1 DATCON,DMCB,(2,SVDATEC),(8,LDATE)                                
LS340    OC    SVFILNO,SVFILNO                                                  
         BZ    LS360                                                            
         SR    R0,R0                                                            
         ICM   R0,3,SVFILNO                                                     
         EDIT  (R0),(6,LSEQ),COMMAS=YES                                         
         SPACE                                                                  
LS360    OC    SVWCPDAT,SVWCPDAT      TEST CONVERTED AT ALL                     
         BZ    LS364                                                            
         GOTO1 DATCON,DMCB,(1,SVWCPDAT),(8,LSTAT)                               
         SPACE                                                                  
         TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BO    LS370                YES                                         
         MVC   LSTAT+8(3),=C'(P)'     NO, MARK PARTIAL                          
         B     LS370                NO                                          
         SPACE                                                                  
LS364    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BZ    LS370                NO                                          
         MVC   LSTAT+8(3),=C'(D)'     NO, MARK DELETED                          
         SPACE                                                                  
LS370    OC    SVWCSRCE,SVWCSRCE                                                
         BZ    LS400                                                            
         MVC   LSRCE,SVWCSRCE                                                   
         SPACE                                                                  
LS400    MVC   LUID(7),=C'UNKNOWN'                                              
         LA    R0,ENDAGYID         END OF TABLE                                 
         LA    R1,AGYIDTAB                                                      
LS410    OC    0(8,R1),0(R1)       EMPTY ENTRY?                                 
         BZ    LS430                                                            
         CLC   8(2,R1),EZWIUID     THIS IN TABLE                                
         BE    LS420                                                            
         LA    R1,12(,R1)                                                       
         CR    R0,R1                                                            
         BH    LS410                                                            
         DC    H'0'                                                             
         SPACE                                                                  
LS420    MVC   LUID,0(R1)                                                       
         B     LS500                                                            
         SPACE                                                                  
LS430    OC    EZWIUID,EZWIUID                                                  
         BZ    LS500                                                            
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),EZWIUID                                                
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R6)                    
         SPACE                                                                  
         CLI   8(R1),0                                                          
         BNE   LS440               NO ID, PRINT AS UNKNOWN                      
*                                  LUID PREFILLED 'UNKNOWN'                     
         SPACE                                                                  
         CLC   KEY(25),0(R6)       KEY HAD BETTER BE THE SAME                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LUID,CTDSC-CTDSCD(R6)                                            
         SPACE                                                                  
LS440    LA    R0,ENDAGYID         END OF TABLE                                 
         LA    R1,AGYIDTAB                                                      
LS450    OC    0(8,R1),0(R1)       EMPTY ENTRY?                                 
         BZ    LS460                                                            
         LA    R1,12(,R1)                                                       
         CR    R0,R1                                                            
         BNL   LS450                                                            
         DC    H'0'                                                             
LS460    MVC   0(8,R1),LUID                                                     
         MVC   8(2,R1),EZWIUID                                                  
         SPACE                                                                  
LS500    EDIT  BEZINV,(7,LINVS),COMMAS=YES                                      
         SPACE                                                                  
         EDIT  BEZSPT,(5,LSPTS),COMMAS=YES                                      
         SPACE                                                                  
         EDIT  BEZNET,(13,LNETD),2,COMMAS=YES                                   
         SPACE                                                                  
         EDIT  BEZGRS,(13,LGRSD),2,COMMAS=YES                                   
         SPACE                                                                  
         L     R0,TEZINV                                                        
         A     R0,BEZINV                                                        
         ST    R0,TEZINV                                                        
         L     R0,TEZSPT                                                        
         A     R0,BEZSPT                                                        
         ST    R0,TEZSPT                                                        
         L     R0,BEZNET                                                        
         CVD   R0,DUB                                                           
         AP    TEZNET,DUB                                                       
         L     R0,BEZGRS                                                        
         CVD   R0,DUB                                                           
         AP    TEZGRS,DUB                                                       
         XC    SVEZWORK,SVEZWORK                                                
         SPACE                                                                  
* SEE IF BATCH TO BE DELETED                                                    
         SPACE                                                                  
         CLC   SVDATEC,SVDTE       SEE IF TO BE DELETED                         
         BH    LS600                NO                                          
         SPACE                                                                  
         MVI   10(R1),C'D'                                                      
         MVC   LDEL,=C'DELETED'                                                 
         OI    SVLSTAT,SVLSTATD                                                 
         SPACE                                                                  
         L     R0,TEZINVD                                                       
         A     R0,BEZINV                                                        
         ST    R0,TEZINVD                                                       
         L     R0,TEZSPTD                                                       
         A     R0,BEZSPT                                                        
         ST    R0,TEZSPTD                                                       
         L     R0,BEZNET                                                        
         CVD   R0,DUB                                                           
         AP    TEZNETD,DUB                                                      
         L     R0,BEZGRS                                                        
         CVD   R0,DUB                                                           
         AP    TEZGRSD,DUB                                                      
         OC    11(1,R1),BATMEDIA   SAVED MEDIA FOR THIS BATCH                   
         SPACE                                                                  
         L     RF,TEZDEL                                                        
         LA    RF,1(,RF)                                                        
         ST    RF,TEZDEL                                                        
         SPACE                                                                  
         CLI   EZFDRF,C'Y'         THIS A DRAFT RUN                             
         BE    LS600                YES, NO FILE UPDATE                         
         SPACE                                                                  
         CLI   TWAWRITE,C'Y'       THIS WRITE = YES RUN                         
         BNE   LS600                NO, NO FILE UPDATE                          
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=CL8'DELETE',EASIWK,SVEZKEY,AIO1,ATIA               
         SPACE                                                                  
LS600    DS   0H                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
* SAVE FOR SORTED LISTING AT EOJ                                                
         SPACE                                                                  
         MVC   SVLUID,EZWIUID                                                   
         MVC   WORK(4),=C'0000'                                                 
         MVC   WORK+4(4),EZWISTN                                                
         OI    WORK+7,X'40'                                                     
         MVC   WORK+8(1),EZWIMED                                                
         GOTO1 MSPACK,DMCB,WORK,WORK+4,WORK+9                                   
         MVC   SVLSTA,WORK+11                                                   
*        GOTO1 DATCON,DMCB,(1,SVDATEC),(2,SVLBDTE)                              
         MVC   SVLBDTE,SVDATEC                                                  
         MVC   SVLSRCE,SVWCSRCE                                                 
         MVC   SVLSEQ,SVFILNO                                                   
         SPACE                                                                  
         OC    SVWCPDAT,SVWCPDAT                                                
         BZ    LS620                                                            
         GOTO1 DATCON,(R1),(1,SVWCPDAT),(2,SVLCDTE)                             
         SPACE                                                                  
LS620    TM    SVWCSTAT,X'40'      TEST COMPLETELY CONVERTED                    
         BZ    *+8                  NO                                          
         OI    SVLSTAT,SVLSTATC                                                 
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE, USE SORT                         
         BNE   LS650                                                            
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',SVEZWORK                                 
         SPACE                                                                  
         B     LS100                                                            
         SPACE                                                                  
LS650    LA    R3,L'SVLEZLS(,R3)                                                
         L     RF,=V(DUMMY)        START OF LIST OF SAVED ENTRIES               
         A     RF,T23011RR                                                      
         A     RF,SVEZTBLN                                                      
         CR    R3,RF                                                            
         BNH   LS100                                                            
         DC    H'0'                                                             
         DROP  R4                                                               
         SPACE                                                                  
LSEOJ    GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (B4,TWKRECS),(7,P+1),COMMAS=YES                                  
         MVC   P+10(14),=C'WORKER BATCHES'                                      
         SPACE                                                                  
         EDIT  (B4,TEZRECS),(7,P+30),COMMAS=YES                                 
         MVC   P+39(12),=C'EASI BATCHES'                                        
         SPACE                                                                  
         EDIT  (B4,TEZDEL),(7,P+55),COMMAS=YES                                  
         MVC   P+64(12),=C'EASI BATCHES'                                        
         MVC   P+77(7),=C'DELETED'                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         EDIT  (B4,TEZINV),(11,P+26),COMMAS=YES                                 
         MVC   P+39(8),=C'INVOICES'                                             
         SPACE                                                                  
         EDIT  (B4,TEZINVD),(11,P+51),COMMAS=YES                                
         MVC   P+64(7),=C'DELETED'                                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         EDIT  (B4,TEZSPT),(11,P+26),COMMAS=YES                                 
         MVC   P+39(5),=C'SPOTS'                                                
         SPACE                                                                  
         EDIT  (B4,TEZSPTD),(11,P+51),COMMAS=YES                                
         MVC   P+64(7),=C'DELETED'                                              
         SPACE                                                                  
         EDIT  (B4,MAXSPTS),(11,P+90),COMMAS=YES                                
         MVC   P+103(10),=C'MAX IN BAT'                                         
         SPACE                                                                  
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         EDIT  (P8,TEZNET),(17,P+20),2,COMMAS=YES                               
         MVC   P+39(3),=C'NET'                                                  
         SPACE                                                                  
         EDIT  (P8,TEZNETD),(17,P+45),2,COMMAS=YES                              
         MVC   P+64(7),=C'DELETED'                                              
         SPACE                                                                  
         EDIT  (B4,MAXDOLN),(17,P+84),2,COMMAS=YES                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         EDIT  (P8,TEZGRS),(17,P+20),2,COMMAS=YES                               
         MVC   P+39(5),=C'GROSS'                                                
         SPACE                                                                  
         EDIT  (P8,TEZGRSD),(17,P+45),2,COMMAS=YES                              
         MVC   P+64(7),=C'DELETED'                                              
         SPACE                                                                  
         EDIT  (B4,MAXDOLG),(17,P+84),2,COMMAS=YES                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,ENDAGYID         END OF TABLE                                 
         LA    R3,AGYIDTAB                                                      
         SPACE                                                                  
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         OC    0(8,RE),0(RE)                                                    
         BZ    *+14                                                             
         LA    RE,12(,RE)                                                       
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
         LPR   R4,RF                                                            
         BZ    LSEOJ04                                                          
         SPACE                                                                  
         GOTO1 XSORT,DMCB,(R3),(R4),12,12,0                                     
         SPACE                                                                  
LSEOJ04  OC    0(8,R3),0(R3)       EMPTY ENTRY?                                 
         BZ    LSEOJ06                                                          
         MVC   P+1(8),0(R3)                                                     
         CLI   10(R3),C'D'                                                      
         BNE   *+10                                                             
         MVC   P+10(7),=C'DELETES'                                              
         SPACE                                                                  
         LA    RE,P+20                                                          
         TM    11(R3),X'01'        TV                                           
         BZ    *+12                                                             
         MVI   0(RE),C'T'                                                       
         LA    RE,2(,RE)                                                        
         SPACE                                                                  
         TM    11(R3),X'02'        RADIO                                        
         BZ    *+12                                                             
         MVI   0(RE),C'R'                                                       
         LA    RE,2(,RE)                                                        
         SPACE                                                                  
         TM    11(R3),X'04'        NETWORK RADIO                                
         BZ    *+12                                                             
         MVI   0(RE),C'X'                                                       
         LA    RE,2(,RE)                                                        
         SPACE                                                                  
         TM    11(R3),X'10'        CABLE                                        
         BZ    *+12                                                             
         MVI   0(RE),C'C'                                                       
         LA    RE,2(,RE)                                                        
         SPACE                                                                  
         TM    11(R3),X'20'        SYNDICATED                                   
         BZ    *+12                                                             
         MVI   0(RE),C'S'                                                       
         LA    RE,2(,RE)                                                        
         SPACE                                                                  
         TM    11(R3),X'40'        NETWORK                                      
         BZ    *+12                                                             
         MVI   0(RE),C'N'                                                       
         LA    RE,2(,RE)                                                        
         SPACE                                                                  
         TM    11(R3),X'80'        UNKNOWN                                      
         BZ    *+8                                                              
         MVI   0(RE),C'?'                                                       
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,12(,R3)                                                       
         CR    R2,R3                                                            
         BNL   LSEOJ04                                                          
LSEOJ06  GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         XC    SVUIDNUM,SVUIDNUM                                                
         MVI   REPORTSW,1                                                       
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE, USE SORTER                       
         BE    LSEOJ08                                                          
         SPACE                                                                  
* NOW SORT BY LUID AND PRINT AGENCY REPORTS *                                   
         SPACE                                                                  
         L     RE,=V(DUMMY)                                                     
         A     RE,T23011RR                                                      
         LR    R3,RE                                                            
         SR    RF,RF                                                            
         OC    0(L'SVLEZLS,RE),0(RE)                                            
         BZ    *+12                                                             
         LA    RE,L'SVLEZLS(,RE)                                                
         BCT   RF,*-14                                                          
         LPR   R5,RF                                                            
         BZ    EXIT                                                             
         SPACE                                                                  
         C     R5,EZBATCT          THIS AGREE WITH TOTAL BATCHES READ           
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 XSORT,DMCB,(R3),(R5),L'SVLEZLS,L'SVLSORT,0                       
         SPACE                                                                  
         SR    R5,R5                                                            
         B     LSEOJ14                                                          
         SPACE                                                                  
LSEOJ08  DS   0H                                                                
         SR    R5,R5                                                            
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SPACE                                                                  
         ICM   R2,15,4(R1)          GET REC ADDRESS                             
         BZ    NOBATERR                                                         
         SPACE                                                                  
         MVC   SVEZWORK,0(R2)                                                   
         SPACE                                                                  
         LA    R3,SVEZWORK                                                      
         BCTR  R5,0                                                             
         B     LSEOJ14                                                          
         DC    H'0'                                                             
         SPACE                                                                  
LSEOJ10  CLC   SVUIDNUM,SVLUID                                                  
         BE    LSEOJ20                                                          
LSEOJ12  GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (B4,TEZRECS),(7,P+1),COMMAS=YES                                  
         MVC   P+10(7),=C'BATCHES'                                              
         SPACE                                                                  
         EDIT  (B4,TEZPCON),(7,P+20),COMMAS=YES                                 
         MVC   P+29(14),=C'PARTIALLY DONE'                                      
         SPACE                                                                  
         EDIT  (B4,TEZCON),(7,P+45),COMMAS=YES                                  
         MVC   P+54(12),=C'DONE BATCHES'                                        
         SPACE                                                                  
         EDIT  (B4,TEZDEL),(7,P+68),COMMAS=YES                                  
         MVC   P+77(15),=C'DELETED BATCHES'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LTR   R5,R5               ALL DONE                                     
         BZ    EXIT                                                             
         SPACE                                                                  
LSEOJ14  MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
         MVC   PAGE,=H'1'                                                       
         XC    TCTRS,TCTRS                                                      
         MVC   SVUIDNUM,SVLUID                                                  
         SPACE                                                                  
         MVC   SVUID(7),=C'UNKNOWN'                                             
         MVI   SVUID+7,C' '                                                     
         SPACE                                                                  
         OC    SVLUID,SVLUID                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),SVLUID                                                 
         L     R6,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R6)                    
         SPACE                                                                  
         CLI   8(R1),0                                                          
         BNE   LSEOJ20             IF NOT FOUND, USE UNKNOWN                    
         SPACE                                                                  
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVUID,CTDSC-CTDSCD(R6)                                           
         SPACE                                                                  
LSEOJ20  L     RF,TEZRECS                                                       
         LA    RF,1(,RF)                                                        
         ST    RF,TEZRECS                                                       
         SPACE                                                                  
         MVC   LUID,SVUID                                                       
         XC    WORK(2),WORK                                                     
         MVC   WORK+2(3),SVLSTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,WORK+5,WORK+9                                   
         MVC   LSTA(4),WORK+9                                                   
         LA    RF,LSTA+4                                                        
         CLI   LSTA+3,C' '                                                      
         BH    *+6                                                              
         BCTR  RF,0                                                             
         MVI   0(RF),C'-'                                                       
         MVC   1(1,RF),WORK+13                                                  
         CLI   1(RF),C' '                                                       
         BH    *+8                                                              
         MVI   1(RF),C'T'                                                       
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,SVLBDTE),(5,LDATE)                                
         SPACE                                                                  
         MVC   LSRCE,SVLSRCE                                                    
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,SVLSEQ                                                      
         EDIT  (R0),(6,LSEQ),FILL=0                                             
         SPACE                                                                  
         OC    SVLCDTE,SVLCDTE                                                  
         BZ    LSEOJ24                                                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,SVLCDTE),(5,LSTAT)                                
         TM    SVLSTAT,SVLSTATC                                                 
         BO    LSEOJ26                                                          
         MVC   LSTAT+8(3),=C'(P)'     NO, MARK PARTIAL                          
         L     RF,TEZPCON                                                       
         LA    RF,1(,RF)                                                        
         ST    RF,TEZPCON                                                       
         B     LSEOJ30                                                          
         SPACE                                                                  
LSEOJ24  TM    SVLSTAT,SVLSTATC                                                 
         BZ    LSEOJ30                                                          
         MVC   LSTAT+8(3),=C'(D)'     NO, MARK DELETE                           
         SPACE                                                                  
LSEOJ26  L     RF,TEZCON                                                        
         LA    RF,1(,RF)                                                        
         ST    RF,TEZCON                                                        
         SPACE                                                                  
LSEOJ30  TM    SVLSTAT,SVLSTATD                                                 
         BZ    LSEOJ40                                                          
         MVC   LDEL,=C'DELETED'                                                 
         L     RF,TEZDEL                                                        
         LA    RF,1(,RF)                                                        
         ST    RF,TEZDEL                                                        
         SPACE                                                                  
LSEOJ40  DS   0H                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE, USE SORTER                       
         BE    LSEOJ50                                                          
         SPACE                                                                  
         LA    R3,L'SVLEZLS(,R3)                                                
         SPACE                                                                  
         OC    0(L'SVLEZLS,R3),0(R3) AT END                                     
         BZ    LSEOJ60                                                          
         BCT   R5,LSEOJ10                                                       
         DC    H'0'                                                             
         SPACE                                                                  
LSEOJ50  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SPACE                                                                  
         ICM   R2,15,4(R1)          GET REC ADDRESS                             
         BZ    LSEOJ60                                                          
         SPACE                                                                  
         MVC   SVEZWORK,0(R2)                                                   
         BCT   R5,LSEOJ10                                                       
         DC    H'0'                                                             
         SPACE                                                                  
* E-O-F                                                                         
         SPACE                                                                  
LSEOJ60  LPR   R5,R5                                                            
         C     R5,EZBATCT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         SR    R5,R5                                                            
         B     LSEOJ12                                                          
         EJECT                                                                  
* HEAD HOOK RTN *                                                               
         SPACE                                                                  
HDHK     NTR1                                                                   
         MVC   H3+32(24),=C'ALL BATCHES ON OR BEFORE'                           
         GOTO1 DATCON,(R1),(2,SVDTE),(5,H3+57)                                  
         SPACE                                                                  
         LA    R4,H3+1                                                          
         CLI   SVSRCE,0                                                         
         BE    HDHK10                                                           
         SPACE                                                                  
         MVC   0(11,R4),=C'ONLY SOURCE'                                         
         MVC   12(4,R4),SVSRCE                                                  
         LA    R4,132(,R4)                                                      
         SPACE                                                                  
HDHK10   CLI   SVSTAT,0            STATUS                                       
         BE    HDHK20                                                           
         SPACE                                                                  
         MVC   0(14,R4),=C'ONLY CONVERTED'                                      
         CLI   SVSTAT,C'C'         STATUS                                       
         BE    HDHK16                                                           
         MVC   5(11,R4),=C'UNCONVERTED'                                         
HDHK16   LA    R4,132(,R4)                                                      
         SPACE                                                                  
HDHK20   CLI   SVDRAFT,C'Y'        ONLY UNCONVERTED                             
         BNE   HDHK30                                                           
         MVC   0(9,R4),=C'DRAFT RUN'                                            
         LA    R4,132(,R4)                                                      
         SPACE                                                                  
HDHK30   CLI   SVREST,C'Y'                                                      
         BNE   HDHK40                                                           
         MVC   0(9,R4),=C'RESTORE'                                              
         SPACE                                                                  
HDHK40   CLI   REPORTSW,0                                                       
         BE    EXIT                                                             
         MVC   H4+36(17),=C'BY AGENCY LISTING'                                  
         B     EXIT                                                             
         SPACE                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
BADNUM   MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
BADATE   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
BADACTER MVI   ERROR,INVACT                                                     
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
BADSRCE  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDSRCMS),BDSRCMS                                       
         B     ERREXIT                                                          
STATERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'STATMS),STATMS                                         
         B     ERREXIT                                                          
RESTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'RESTMS),RESTMS                                         
         B     ERREXIT                                                          
DRAFERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DRAFMS),DRAFMS                                         
         B     ERREXIT                                                          
NOBATERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOBATMS),NOBATMS                                       
ERREXIT  GOTO1 ERREX2                                                           
         DC    H'0'                                                             
         SPACE 3                                                                
SVEZTBLN DC    A(SVEZLNE*1000)                                                  
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,13,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=16'                                    
         SPACE                                                                  
BDSRCMS  DC    C'* ERROR * INVALID SOURCE CODE *'                               
STATMS   DC    C'* ERROR * STATUS MUST BE C OR U *'                             
RESTMS   DC    C'* ERROR * RESTORE MUST BE Y OR N *'                            
DRAFMS   DC    C'* ERROR * DRAFT MUST BE Y OR N *'                              
NOBATMS  DC    C'* ERROR * NO BATCHES ON FILE *'                                
         SPACE                                                                  
MEDTBL   DC    CL3' TV',X'01'                                                   
         DC    CL3'TTV',X'01'                                                   
         DC    CL3'AAM',X'02'                                                   
         DC    CL3'FFM',X'02'                                                   
         DC    CL3'CC ',X'10'                                                   
         DC    CL3'SS ',X'20'                                                   
         DC    CL3'NN ',X'40'                                                   
         DC    CL3'XX ',X'04'                                                   
         DC    CL3' ??',X'80'                                                   
         SPACE                                                                  
SOURCE   DC    CL5'CBS '                                                        
         DC    CL5'ENTR'                                                        
         DC    CL5'GRPW'                                                        
         DC    CL5'JEFF'                                                        
         DC    CL5'MKTR'                                                        
SOURCES  EQU   (*-SOURCE)/L'SOURCE                                              
         SPACE                                                                  
AGYIDTAB DC 100XL12'00'             AGENCY IDS PLUS FLAG FOR DELETED            
ENDAGYID EQU   *                                                                
         SPACE                                                                  
HEADING  SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,37,C'BATCH DELETED LIST'                                      
         SSPEC H2,37,C'------------------'                                      
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,73,REPORT                                                     
         SSPEC H3,89,C'EASI'                                                    
         SSPEC H4,73,RUN                                                        
         SSPEC H5,83,PAGE                                                       
         SSPEC H8,3,C'USER-ID'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,13,C'STATION'                                                 
         SSPEC H9,13,C'-------'                                                 
         SSPEC H8,22,C'DATE'                                                    
         SSPEC H9,22,C'--------'                                                
         SSPEC H8,32,C'SRCE'                                                    
         SSPEC H9,32,C'----'                                                    
         SSPEC H8,39,C'SEQ'                                                     
         SSPEC H9,39,C'----'                                                    
         SSPEC H8,46,C'CONVERTED'                                               
         SSPEC H9,46,C'---------'                                               
         SSPEC H8,61,C'DELETED'                                                 
         SSPEC H9,61,C'-------'                                                 
         SSPEC H8,70,C'INVOICES'                                                
         SSPEC H9,70,C'--------'                                                
         SSPEC H8,82,C'SPOTS'                                                   
         SSPEC H9,82,C'-----'                                                   
         SSPEC H8,100,C'NET'                                                    
         SSPEC H9,100,C'---'                                                    
         SSPEC H8,112,C'GROSS'                                                  
         SSPEC H9,112,C'-----'                                                  
         DC    X'00'                                                            
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* SPEZFFFD                                                                      
* CTGENFILE                                                                     
       ++INCLUDE SPGENEZ                                                        
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPEZFFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* SPEZFFAD                                                                      
       ++INCLUDE SPEZFF6D                                                       
         EJECT                                                                  
* DDGENTWA                                                                      
* DMWRKFD                                                                       
* DMWRKFK                                                                       
* SPGENSTA                                                                      
* CTGENFILE                                                                     
* SPEZFWORKD                                                                    
         PRINT OFF                                                              
         EJECT                                                                  
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* DMWRKFD                                                                       
       ++INCLUDE DMWRKFD                                                        
         EJECT                                                                  
* DMWRKFK                                                                       
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
* SPGENSTA                                                                      
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
* SPEZFWORKD                                                                    
       ++INCLUDE SPEZFWORKD                                                     
         PRINT ON                                                               
         ORG   SYSSPARE                                                         
T23011RR DS    F                                                                
SVLNNUM  DS    F                                                                
         SPACE                                                                  
* MAX PER BATCH                                                                 
         SPACE                                                                  
MAXSPTS  DS    F                   SPOTS                                        
MAXDOLN  DS    F                   NET DOLLARS                                  
MAXDOLG  DS    F                   GROSS DOLLARS                                
         SPACE                                                                  
         SPACE                                                                  
* BATCH COUNTERS                                                                
         SPACE                                                                  
BCTRS    DS   0CL16                                                             
BEZINV   DS    F                   TOTAL INVOICES                               
BEZSPT   DS    F                   TOTAL SPOTS                                  
BEZNET   DS    F                   NET DOLLARS                                  
BEZGRS   DS    F                   GROSS DOLLARS                                
         SPACE                                                                  
* TOTAL COUNTERS                                                                
         SPACE                                                                  
TCTRS    DS   0CL56                                                             
TWKRECS  DS    F                   TOTAL BATCHES ON FILE                        
TEZRECS  DS    F                   EZ BATCHES ON FILE                           
TEZDEL   DS    F                   DELETE BATCHES                               
TEZCON   DS    F                   TOTALLY CONVERTED BATCHES                    
TEZPCON  DS    F                   PATIALLY CONVERTED BATCHES                   
TEZINV   DS    F                   TOTAL INVOICES                               
TEZINVD  DS    F                   DELETED INVOICES                             
TEZSPT   DS    F                   TOTAL SPOTS                                  
TEZSPTD  DS    F                     "     "   DELETED                          
TEZNET   DS    PL8                 NET DOLLARS                                  
TEZNETD  DS    PL8                  "     "    DELETED                          
TEZGRS   DS    PL8                 GROSS DOLLARS                                
TEZGRSD  DS    PL8                   "      "    DELETED                        
         SPACE                                                                  
EZBATCT  DS    F                   COUNT OF BATCHES PROCESSED                   
         SPACE                                                                  
SVWKCMT  DS    0CL16                                                            
SVWCSTAT DS    XL1                                                              
SVWCPDAT DS    XL3                                                              
         DS    XL2                                                              
         DS    XL2                                                              
         DS    XL2                                                              
SVWCSRCE DS    CL4                                                              
         DS    XL2                                                              
         SPACE                                                                  
SVFILNO  DS    XL2                                                              
SVDATEC  DS    XL2                                                              
         SPACE                                                                  
REPORTSW DS    XL1                                                              
SVSTA    DS    CL5                                                              
SVDTE    DS    XL2                                                              
SVSRCE   DS    CL4                                                              
SVSTAT   DS    CL1                                                              
SVDRAFT  DS    CL1                                                              
SVREST   DS    CL1                                                              
SVUID    DS    CL8                                                              
SVUIDNUM DS    CL2                                                              
SVEZKEY  DS    CL42                                                             
BATMEDIA DS    CL1                 SAVED MEDIA FOR EACH BATCH                   
         SPACE                                                                  
* WORK AREA FOR ONE BATCH                                                       
         SPACE                                                                  
SVEZWORK DS    CL28                                                             
         SPACE                                                                  
* TABLE OF ALL EASI BATCHES ON WORKER FILE                                      
         SPACE                                                                  
SVLEZLSD DSECT                                                                  
SVLEZLS  DS    0CL28                                                            
SVLSORT  DS    0CL13                                                            
SVLUID   DS    XL2                 AGENCY ID                                    
SVLSTA   DS    CL3                 COMPRESSED STATION                           
SVLBDTE  DS    XL2                 BATCH (LOADED) DATE                          
SVLSRCE  DS    CL4                 SOURCE                                       
SVLSEQ   DS    CL2                 BATCH SEQ #                                  
SVLCDTE  DS    XL2                 CONVERTED DATE                               
SVLSTAT  DS    XL1                                                              
SVLSTATC EQU   X'40'               ALL INVOICES IN BATCH CONVERTED              
SVLSTATD EQU   X'20'               BATCH DELETED                                
SVLINVCT DS    XL2                 INVOICE COUNT                                
SVLSPTCT DS    XL2                 SPOT COUNT                                   
SVLNET   DS    XL4                 NET DOLLARS                                  
SVLGRS   DS    XL4                 GROSS DOLLARS                                
SVEZLNE  EQU   *-SVLEZLS                                                        
         EJECT                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
LUID     DS    CL8                                                              
         DS    CL2                                                              
LSTA     DS    CL7                                                              
         DS    CL2                                                              
LDATE    DS    CL8                                                              
         DS    CL2                                                              
LSRCE    DS    CL4                                                              
         DS    CL1                                                              
LSEQ     DS    CL6                                                              
         DS    CL3                                                              
LSTAT    DS    CL11                                                             
         DS    CL4                                                              
LDEL     DS    CL7                                                              
         DS    CL3                                                              
LINVS    DS    CL9                                                              
         DS    CL2                                                              
LSPTS    DS    CL5                                                              
         DS    CL3                                                              
LNETD    DS    CL13                                                             
         DS    CL1                                                              
LGRSD    DS    CL13                                                             
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014PPEZF11   05/01/02'                                      
         END                                                                    
